## Test environments
* Debian GNU/Linux 12 (bookworm) install, R 4.3.1
* win-builder, R-devel and R-release
* macbuilder macOS Ventura 13.3.1 R4.4.0
* Rhub
  - macOS-arm64 14.5, R-devel (2024-06-17 r86768)
  - macOS 13.6.7, R-devel (2024-06-17 r86768)
  - ubuntu-release Ubuntu 22.04.4LTS, R 4.4.1
* Github actions
  - macOS 14.5, R 4.4.1
  - Windows Server 2022, R 4.4.1
  - Ubuntu 22.04.4 LTS, R 4.4.1
  - Ubuntu 20.04.4 LTS, R-devel

## Submission note

- This patch fixes vignette compilation issues on mac-release-macos-arm64,
  r-oldrel-macos-arm64 and numerical tolerance issues in unit tests on m1mac.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are currently no downstream dependencies for this package
