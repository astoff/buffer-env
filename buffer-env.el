;; buffer-env.el --- Buffer-local process environments -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: processes, tools
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1

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

;; This package adjusts the `process-environment' and `exec-path'
;; variables buffer locally according to the output of a shell script.
;; The idea is that the same scripts used elsewhere to set up a
;; project environment can influence external processes started from
;; buffers editing the project files.  This is important for the
;; correct operation of tools such as linters, language servers and
;; the `compile' command, for instance.

;; The default settings of the package are compatible with the popular
;; direnv program.  However, this package is entirely independent of
;; direnv and it's not possible to use direnv-specific features in the
;; .envrc scripts.  On the plus side, it's possible to configure the
;; package to support other environment setup methods, such as .env
;; files or Python virtualenvs.

;; The usual way to activate this package is by including the
;; following in your init file:
;;
;;     (add-hook 'hack-local-variables-hook 'buffer-env-update)
;;
;; This way, any buffer potentially affected by directory-local
;; variables can also be affected by buffer-env.  It is nonetheless
;; possible to call `buffer-env-update' interactively or add it only
;; to specific major-mode hooks.

;; Note that it is quite common for process-executing Emacs libraries
;; not to heed to the fact that `process-environment' and `exec-path'
;; may have a buffer-local value.  This unfortunate state of affairs
;; can be remedied by the `inheritenv' package, which see.

;;; Code:

(require 'seq)
(eval-when-compile (require 'subr-x))

(defcustom buffer-env-file ".envrc"
  "Base name of the script to produce environment variables.")

(defcustom buffer-env-command ">&2 . \"$0\" && env -0"
  "Command to produce environment variables.
This string is executed as a command in a shell, in the directory
of the envrc script, with its absolute file name as argument.
The command should print a null-separated list of environment
variables, and nothing else, to the standard output.")

(defcustom buffer-env-safe-files nil
  "List of scripts marked as safe to execute.
Entries are conses consisting of the file name and a hash of its
content.")

(defcustom buffer-env-ignored-variables
  '("_=" "PS1=" "SHLVL=" "DISPLAY=" "PWD=")
  "List of environment variables to ignore.")

(defcustom buffer-env-extra-variables
  '("TERM=dumb")
  "List of additional environment variables.")

(defcustom buffer-env-extra-exec-path
  (list exec-directory)
  "List of additional `exec-path' entries.")

(defun buffer-env-authorize (file)
  "Check if FILE is safe to execute, or ask for permission.
Files marked as safe to execute are permanently stored in
`buffer-env-safe-files' via the Custom mechanism."
  (let ((hash (with-temp-buffer
                (insert-file-contents-literally file)
                (secure-hash 'sha256 (current-buffer)))))
    (or (member (cons file hash) buffer-env-safe-files)
        (when (y-or-n-p (format "Mark ‘%s’ as safe to execute?"
                                file))
          (customize-save-variable 'buffer-env-safe-files
                                   (push (cons file hash)
                                         buffer-env-safe-files))))))

;;;###autoload
(defun buffer-env-update ()
  "Update the environment variables buffer locally.
This function searches for a file named `buffer-env-file' in the
current directory and its parents, executes it in a shell, and
then sets `process-environment' and `exec-path' buffer-locally to
match the shell environment."
  (interactive)
  (when-let* ((dir (and (stringp buffer-env-file)
                        (not (file-remote-p default-directory))
                        (locate-dominating-file default-directory
                                                buffer-env-file)))
              (file (expand-file-name buffer-env-file dir))
              ((file-readable-p file))
              ((buffer-env-authorize file))
              (out (with-temp-buffer
                     (setq default-directory dir)
                     (call-process shell-file-name nil t nil
                                   shell-command-switch
                                   buffer-env-command
                                   file)
                     (buffer-substring (point-min) (point-max))))
              (vars (split-string out (string 0) t)))
    (setq-local process-environment buffer-env-extra-variables)
    (dolist (var vars)
      (unless (seq-contains-p buffer-env-ignored-variables
                              var
                              'string-prefix-p)
        (setq process-environment (cons var process-environment))))
    (when-let* ((path (getenv "PATH")))
      (setq-local exec-path (append (split-string path path-separator)
                                    buffer-env-extra-exec-path)))))

(provide 'buffer-env)
;;; buffer-env.el ends here
