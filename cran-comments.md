# Test environments
* local Debian 11 (bullseye) install, R 4.3.1
* Github actions
  - macOS 12.7.3, R 4.3.2
  - Windows Server 2022, R 4.3.2
  - Ubuntu 22.04.3 LTS, R 4.3.2
  - Ubuntu 20.04.3 LTS, R-devel (2024-02-11 r85891)
* win-builder, R-devel and R-release
* macbuilder macOS 13.3.1 (22E261) R4.3.0 Patched (2023-05-18 r84451)
* Rhub
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Debian Linux, R-devel, GCC, no long double

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

  Maintainer: ‘Michal Kolesár <kolesarmi@googlemail.com>’

  New submission

  Possibly misspelled words in DESCRIPTION:
    Kolesár (20:61)
    Pinkham (20:42)

The misspelled words are the package authors' names.

## Downstream dependencies
There are currently no downstream dependencies for this package
