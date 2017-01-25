# mega-sdist

Handles uploading to Hackage from Stack mega repos.

Compares local code against version on Hackage. Accepts the following options:

* __--gittag__: Automatically tag as well.

Uses `stack.yaml` if present to determine which packages to
build. Only takes directories which are a subdirectory of the current
directory.
