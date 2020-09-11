# Dotfiles

This repository hosts a number of user configuration files for various applications which can be symlinked where needed.  Changes to these files should be version-controlled and accessible to any consumer of this repository.  Clone the repository onto whatever machine requires it and symlink the necessary files into the appropriate directory (commonly, this is `$HOME`).  Files added to this repository come in all shapes and sizes.  They should be well-documented with both per-line descriptions of what certain configurations do and primary source links (where possible) so that future changes can be made more easily.

### Example

```sh
$ ln --symbolic /path/to/desired/dotfile /path/where/dotfile/is/used
```

