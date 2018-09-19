### Introduction

This is a utility program for switching between different versions of a
program. It works by managing a set of symlinks for each program. The
definition of what links to create for a program, and what versions of the
program are installed on the system is called a *bundle*.

### File format

Bundle files go in `$HOME/.version` and the file name is the name of the
bundle. Each file consists of two sections: `[package]` and `[versions]`. The
*package* section defines the files and directories that make up the bundle,
where they should be installed, and how to compute the location of a particular
version. The *versions* section defines the available versions.

Example (`~/.version/gcc`):
```
[package]
prefix: /usr/local/bin
files: gcc, g++
source:
  /usr/local/bin/${TARGET}-${TAG}
  ${SOURCE}/${TARGET}

[versions]
clang /usr/bin
4.9
8
```

The package section contains three entries:

- `prefix`: root directory for the symlinks
- `files`: a comma-separated list of paths (relative to the `prefix`) of the
  symlinks to create
- `source`: a list of locations (one per line) where to find a particular
  version. These paths can use the following variables
  - `${TAG}` - the name of the version
  - `${TARGET}` - the relative path listed in `files`
  - `${SOURCE}` - the optional source annotation given after the tag in the version entry
  - `${HOME}` - the user's home directory
  When installing a version a symlink is created to the first source entry that
  is an existing file or directory.

The version section contains a list of versions, one per line, consisting of a
tag (`${TAG}`) and an optional source annotation (`${SOURCE}`).

In the example we would get the following mappings (assuming
`/usr/local/bin/gcc-clang` and `/usr/local/bin/g++-clang` do not exist):
```
version gcc clang
  /usr/local/bin/gcc -> /usr/bin/gcc
  /usr/local/bin/g++ -> /usr/bin/g++
version gcc 4.9
  /usr/local/bin/gcc -> /usr/local/bin/gcc-4.9
  /usr/local/bin/g++ -> /usr/local/bin/g++-4.9
version gcc 8
  /usr/local/bin/gcc -> /usr/local/bin/gcc-8
  /usr/local/bin/g++ -> /usr/local/bin/g++-8
```
