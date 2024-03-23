;;; buffer-env.el --- Buffer-local process environments -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; URL: https://github.com/astoff/buffer-env
;; Keywords: processes, tools
;; Package-Requires: ((emacs "27.1") (compat "29.1"))
;; Version: 0.6

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The purpose of this package is to adjust the `process-environment'
;; and `exec-path' variables buffer locally according to the output of
;; a shell script.  This allows the correct operation of tools such as
;; linters, compilers and language servers when working on projects
;; with special requirements which are not installed globally on the
;; system.

;; The default settings of this package are compatible with the
;; popular direnv program.  However, the package is entirely
;; independent of direnv and it's not possible to use direnv-specific
;; features in the .envrc scripts.  On the plus side, it's possible to
;; configure the package to support other environment setup methods,
;; such as .env files or Python virtualenvs.  The README file includes
;; some examples.

;; The usual way to activate this package is by including the
;; following in your init file:
;;
;;     (add-hook 'hack-local-variables-hook 'buffer-env-update)
;;
;; In this way, any buffer potentially affected by directory-local
;; variables will also be affected by buffer-env.  It is nonetheless
;; possible to call `buffer-env-update' interactively or add it only
;; to specific major-mode hooks.

;;; Code:

(require 'compat)
(require 'seq)
(eval-when-compile
  (require 'rx)
  (require 'subr-x))

(defgroup buffer-env nil
  "Buffer-local process environments."
  :group 'processes
  :link '(url-link "https://github.com/astoff/buffer-env"))

(defcustom buffer-env-script-name ".envrc"
  "File name of the script producing environment variables, or a list of such."
  :type '(choice (repeat string) string))

(defcustom buffer-env-command-alist
  `((,(rx "/.env" eos)
     . "set -a && >&2 . \"$1\" && env -0")
    (,(rx "/manifest.scm" eos)
     . "guix shell -m \"$1\" -- env -0")
    (,(rx "/guix.scm" eos)
     . "guix shell -D -f \"$1\" -- env -0")
    (,(rx "/flake.nix" eos)
     . "nix develop -c env -0")
    (,(rx "/shell.nix" eos)
     . "nix-shell \"$1\" --run \"env -0\"")
    (,(rx "/pyproject.toml" eos)
     . "\
backend=$(sed -nr 's/^build-backend\s*=\s*\"([a-z]+).*/\\1/p' \"$1\")
case $backend in
  poetry|pdm|hatchling) ${backend%ling} run env -0;;
  *) echo >&2 \"Unsupported build backend '$backend'.\" && exit 1;;
esac")
    (,(rx ".ps1" eos)
     . "powershell -c '& { param($ignore, $script) . $script > $null; Get-ChildItem env: |\
        % {\"$($_.Name)=$($_.Value)`0\"} | Write-Host -NoNewLine } '")
    (,(rx any)
     . ">&2 . \"$1\" && env -0"))
  "Alist of commands used to produce environment variables.
For each entry, the car is a regular expression and the cdr is a
shell command.  The command specifies how to execute a script and
collect the environment variables it defines.

More specifically, the command corresponding to the script file
name is executed in a shell, in the directory of the script, with
its absolute file name as argument.  The command should print a
null-separated list of environment variables, and nothing else,
to standard output."
  :type '(alist :key-type (regexp :tag "File name pattern")
                :value-type (string :tag "Shell command")))

(defcustom buffer-env-commands nil
  "Alist of commands used to produce environment variables.
For each entry, the car is a glob pattern and the cdr is a shell
command.  The command specifies how to execute a script and
collect the environment variables it defines.

Specifically, the command corresponding to the script file name
is executed in a shell, in the directory of the script, with its
absolute file name as argument.  The command should print a
null-separated list of environment variables, and nothing else,
to standard output."
  :type '(alist :key-type (string :tag "Glob pattern")
                :value-type (string :tag "Shell command")))
(make-obsolete-variable 'buffer-env-commands 'buffer-env-command-alist "0.5")

(defcustom buffer-env-safe-files nil
  "List of scripts marked as safe to execute.
Entries are cons cells consisting of the file name and a hash of
its content."
  :type 'alist)

(defcustom buffer-env-ignored-variables
  '("_=" "PS1=" "SHLVL=" "DISPLAY=" "PWD=")
  "List of environment variables to ignore."
  :type '(repeat string))

(defcustom buffer-env-extra-variables
  '("TERM=dumb")
  "List of additional environment variables."
  :type '(repeat string))

(defcustom buffer-env-verbose nil
  "Whether to display a message every time `buffer-env-update' runs successfully."
  :type 'boolean)

(defcustom buffer-env-mode-line " Env"
  "Mode line indicator for buffers affected by buffer-env."
  :risky t
  :type '(choice (const :tag "Default" " Env")
                 (const :tag "With script name"
                        (:eval (format " Env[%s]" (file-name-nondirectory
                                                   buffer-env-active))))
                 (const :tag "No indicator" nil)
                 sexp))

(defvar-local buffer-env-active nil
  "Non-nil if a buffer-local process environment has been set.
In this case, this is the name of the script used to generate it.")

(setf (alist-get 'buffer-env-active minor-mode-alist)
      '((:propertize
         buffer-env-mode-line
         help-echo "\
Process environment is buffer local\n\
mouse-1: Describe process environment\n\
mouse-2: Reset to default process environment"
         mouse-face mode-line-highlight
         local-map (keymap (mode-line keymap
                                      (mouse-1 . buffer-env-describe)
                                      (mouse-2 . buffer-env-reset))))))

(defvar buffer-env--cache (make-hash-table :test #'equal)
  "Hash table of cached entries, to accelerate `buffer-env-update'.
Keys are file names, values are lists of form
(TIMESTAMP PROCESS-ENVIRONMENT EXEC-PATH).")

(defun buffer-env--authorize (file)
  "Check if FILE is safe to execute, or ask for permission.
Files marked as safe to execute are permanently stored in
`buffer-env-safe-files' via the Custom mechanism."
  (let ((hash (with-temp-buffer
                (insert-file-contents-literally file)
                (secure-hash 'sha256 (current-buffer)))))
    (or (member (cons file hash) buffer-env-safe-files)
        (pcase (car (read-multiple-choice
                     (format-message "[buffer-env] Execute script `%s'?" file)
                     '((?! "always")
                       (?y "yes")
                       (?n "no"))
                     "\
Please decide if you trust this script and would like to execute it.

If you choose ‘yes’ or ‘no’, the decision holds in this Emacs
session only and provided the file modification time is unchanged.

If you choose `always', the decision persists for as long as the
content of the file remains unchanged.  See ‘buffer-env-safe-files’
for more details."))
          (?! (customize-save-variable
               'buffer-env-safe-files
               (push (cons file hash) buffer-env-safe-files)))
          (?y t)))))

(defun buffer-env--locate-script ()
  "Locate a dominating file named `buffer-env-script-name'."
  (unless (file-remote-p default-directory)
    (seq-some
     (lambda (name)
       (when-let ((dir (locate-dominating-file default-directory
					       name)))
	 (expand-file-name name dir)))
     (if (stringp buffer-env-script-name)
         (list buffer-env-script-name)
       buffer-env-script-name))))

(defun buffer-env--get-command (file)
  "Return the appropriate shell command to interpret script FILE."
  (or (when-let
          ((c (seq-some (pcase-lambda (`(,patt . ,command))
                          (when (string-match-p (wildcard-to-regexp patt)
                                                (file-name-nondirectory file))
                            command))
                        buffer-env-commands)))
        (prog1 c
          (lwarn 'buffer-env :warning "\
`buffer-env-commands' is obsolete, use 'buffer-env-command-alist' instead.")))
      (seq-some (pcase-lambda (`(,patt . ,command))
                  (when (string-match-p patt file)
                    command))
                buffer-env-command-alist)
      (user-error "[buffer-env] No entry of `buffer-env-command-alist' matches %s"
                  file)))

(defun buffer-env--filter-vars (vars)
  "Filter out VARS listed in `buffer-env-ignored-variables'."
  (seq-filter (lambda (var)
                (not (seq-contains-p buffer-env-ignored-variables
                                     var
                                     'string-prefix-p)))
              vars))

;;;###autoload
(defun buffer-env-update (&optional file)
  "Update the process environment buffer locally.
FILE is executed in the way prescribed by
`buffer-env-command-alist' and the buffer-local values of
`process-environment' and `exec-path' are set accordingly.

If FILE omitted, a file with base name `buffer-env-script-name'
is looked up in the current directory and its parents; nothing
happens if no such file is found.  This makes this function
suitable for use in a normal hook.

When called interactively, ask for a FILE."
  (interactive
   (list (let ((file (buffer-env--locate-script)))
           (read-file-name (format-prompt "Environment script"
                                          (when file (file-relative-name file)))
                           nil file t))))
  (when-let ((file (if file
                       (expand-file-name file)
                     (buffer-env--locate-script))))
    (let ((modtime (file-attribute-modification-time (file-attributes file)))
          (cached (gethash file buffer-env--cache)))
      (cond
       ((time-equal-p (car cached) modtime)
        (unless (eq 'ignore (cdr cached))
          (when buffer-env-verbose
            (message "[buffer-env] Environment of `%s' set from `%s' using cache"
                     (current-buffer) file))
          (setq-local process-environment (nth 1 cached)
                      exec-path (nth 2 cached)
                      buffer-env-active file)))
      ((buffer-env--authorize file)
       (pcase (with-temp-buffer
                (let* ((default-directory (file-name-directory file))
                       (message-log-max nil)
                       (errbuf (with-current-buffer (get-buffer-create " *buffer-env*")
                                 (erase-buffer)
                                 (current-buffer)))
                       (proc (make-process
                              :name "buffer-env"
                              :command (list shell-file-name
                                             shell-command-switch
                                             (buffer-env--get-command file)
                                             shell-file-name ;Argument $0
                                             file)
                              :sentinel #'ignore
                              :buffer (current-buffer)
                              :stderr errbuf)))
                  ;; Give subprocess a chance to finish
                  ;; before setting up a progress reporter
                  (sit-for 0)
                  (if (not (process-live-p proc))
                      (accept-process-output proc)
                    (let* ((msg (format-message "[buffer-env] Running `%s'..." file))
                           (reporter (make-progress-reporter msg)))
                      (while (or (accept-process-output proc 1)
                                 (process-live-p proc))
                        (progress-reporter-update reporter))
                      (progress-reporter-done reporter)))
                  (list (process-exit-status proc)
                        errbuf
                        (thread-first
                          (buffer-substring (point-min) (point-max))
                          (split-string "\0" t)
                          (buffer-env--filter-vars)
                          (nconc buffer-env-extra-variables)))))
         (`(0 ,_ ,vars)
          (setq-local process-environment vars)
          (when-let ((path (getenv "PATH")))
            (setq-local exec-path (nconc (split-string path path-separator)
                                         (list exec-directory))))
          (puthash file (list modtime process-environment exec-path) buffer-env--cache)
          (when buffer-env-verbose
            (message "[buffer-env] Environment of `%s' set from `%s'"
                     (current-buffer) file))
          (setq buffer-env-active file))
         (`(,status ,errbuf ,_)
          (buffer-env-reset)
          (puthash file `(,modtime . ignore) buffer-env--cache)
          (lwarn 'buffer-env :warning "\
Error running script %s (exit status %s).
See script output in %s for more information."
                 (buttonize file #'find-file file)
                 status
                 (buttonize (buffer-name errbuf) #'pop-to-buffer errbuf)))))
      (t
       (puthash file `(,modtime . ignore) buffer-env--cache))))))

;;;###autoload
(defun buffer-env-reset ()
  "Reset the process environment of this buffer to the default values."
  (interactive)
  (kill-local-variable 'buffer-env-active)
  (kill-local-variable 'process-environment)
  (kill-local-variable 'exec-path)
  (force-mode-line-update))

(defun buffer-env--sorted (vars)
  "Sort and remove duplicates in the list of environment VARS."
  (let ((testfn (lambda (x y)
                  (string= (progn (string-match "[^=]*" x)
                                  (match-string 0 x))
                           (progn (string-match "[^=]*" y)
                                  (match-string 0 y))))))
    (sort (seq-uniq vars testfn) 'string<)))

(defun buffer-env-describe (&optional buffer)
  "Compare buffer-local and global process environments."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((script (or buffer-env-active "??"))
          (local (buffer-env--sorted process-environment))
          (global (buffer-env--sorted (default-toplevel-value 'process-environment))))
      (help-setup-xref (list #'buffer-env-describe buffer)
		       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert "The process environment of "
                  (buttonize (buffer-name buffer) #'pop-to-buffer buffer)
                  " is buffer local.\nIt was generated by "
                  (buttonize (abbreviate-file-name script) #'find-file script)
                  ".")
          (insert "\n\nOnly in the local process environment:\n")
          (if-let ((vars (seq-difference local global)))
              (dolist (var vars) (insert "  " var ?\n))
            (insert "    (None)\n"))
          (insert "\nOnly in the global process environment:\n")
          (if-let ((vars (seq-difference global local)))
              (dolist (var vars) (insert "  " var ?\n))
            (insert "    (None)\n"))
          (insert "\nComplete process environment of the buffer:\n")
          (dolist (var local) (insert "  " var ?\n))
          (goto-char (point-min)))))))

(provide 'buffer-env)
;;; buffer-env.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; End:
