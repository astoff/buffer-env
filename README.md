buffer-env — Buffer-local process environments
==============================================

This Emacs package sets environment variables on a buffer-local basis,
according to the output of a shell script.  The idea is that the same
scripts used elsewhere to set up a project environment can influence
external processes started from buffers editing the project files.
This is important for the correct operation of tools such as linters,
language servers and the `M-x compile` command, for instance.

With the default settings, the package gathers environment variables
from `.envrc` files, just like the popular [direnv] program.  However,
buffer-env is entirely independent of direnv and it's not possible to
use certain fancy direnv-specific features in the `.envrc` scripts.
On the plus side, it's possible to configure the package to support
other environment setup methods, such as Python virtualenvs or `.env`
files (see below).

The usual way to activate this package is by including the
following in your init file:

``` elisp
(add-hook 'hack-local-variables-hook 'buffer-env-update)
```

This way, any buffer potentially affected by directory-local
variables can also be affected by buffer-env.  It is nonetheless
possible to call `buffer-env-update` interactively or add it only
to specific major-mode hooks.

Note that it is quite common for process-executing Emacs libraries not
to heed to the fact that `process-environment` and `exec-path` may
have a buffer-local value.  This unfortunate state of affairs can be
remedied by the [inheritenv] package, which see.

## Alternative settings

This package works as follows.  First, a file named `buffer-env-file`
is looked up in the current directory or one of its parents.  In case
of success, `buffer-env-command` is executed in a shell, with the
found file as argument.  This command should print a list of
null-separated environment variables (and nothing else) to stdout.
The buffer-local values of `process-environment` and `exec-path` are
then set based on that.

With this in sight, it should be possible to integrate with any of the
numerous environment management tools out there.

### Python virtualenvs

One can always source a virtualenv activation script from an `.envrc`
script, but this additional step can be avoided by calling
`buffer-env-update` manually and point to the `bin/activate` file, or
setting

``` elisp
(setq 'buffer-env-file "bin/activate")
```

or a variation thereof.  Note that it is also possible to provide an
absolute path for `buffer-env-file`, and it is possible to specify it
as a buffer- or directory-local variable.

### .env files

To gather environment variables from `.env` files in the style of
Docker, Node.js and others, use the following settings:

``` elisp
(setq 'buffer-env-file ".env")
(setq 'buffer-env-command "set -a && >&2 . \"$0\" && env -0")
```

Obviously, this assumes that the `.env` file is correct when
interpreted as a shell script, which dictates, for instance, how and
when quotes are to be used.

### Direnv

As mentioned above, the default settings are compatible with direnv,
but only as long as `.envrc` is a regular shell script.  If you need
those, you will be better served by the [envrc] library.  It is
nonetheless possible to take some advantage of direnv by setting

``` elisp
(setq 'buffer-env-command "direnv exec . env -0")
```

## Related packages

This package is essentially a knockoff of the [envrc] package by Steve
Purcell.  The main difference is that envrc depends on and tightly
integrates with the direnv program, while buffer-env is minimalist and
has no extra dependencies.

For a comparison of the buffer-local approach to environment variables
with the global approach used by most of the similar packages, see
envrc's README.

There is a large number of Emacs packages interfacing with Python's
virtualenv system.  They all seem to take the global approach and,
therefore, the comparisons and caveats in the envrc README also apply,
mutatis mutandis.

[direnv]: https://direnv.net/
[envrc]: https://github.com/purcell/envrc
[inheritenv]: https://github.com/purcell/inheritenv
