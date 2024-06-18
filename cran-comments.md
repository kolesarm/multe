## Test environments
* Debian GNU/Linux 12 (bookworm) install, R 4.3.1
* win-builder, R-devel and R-release
* Rhub
  - ubuntu-release R-4.4.1 (2024-06-14)


  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Debian Linux, R-devel, GCC, no long double



r-release-macos-arm64
M1mac
r-oldrel-macos-arm64

https://mac.R-project.org/macbuilder/results/1718716866-1c9f21830b3e3fb4/

* Github actions
  - macOS 12.7.3, R 4.3.2
  - Windows Server 2022, R 4.3.2
  - Ubuntu 22.04.3 LTS, R 4.3.2
  - Ubuntu 20.04.3 LTS, R-devel

* macbuilder macOS 13.3.1 (22E261) R4.3.0 Patched (2023-05-18 r84451)

## Submission note


- This patch fixes vignette compilation issues on mac-release-macos-arm64,
  r-oldrel-macos-arm64 and numerical tolerance issues in unit tests on m1mac.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs



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
