# Current submission (0.1.6):

1. I changed the my email address as previous one expires soon. As required by `CRAN` policy, I have sent an email from previous address to `CRAN-submissions@r-project.org` explaining the situation. The title is

> Email change for R package `dipsaus` and `threeBrain`

Please refer to that email. 

2. I removed `LazyData` specification in `DESCRIPTION` file, as noted on the `CRAN` check website.

3. Self checked locally on `Apple M1 chip`. Online checks include `rhub` (`Linux`, `Solaris`, and `OSX Sierra`) and `Windows` (`old`, `release`, and `devel` provided by `Ligges`). All passed with 1 note as maintainer email has been changed.


# Previous submission (0.1.5):

```
Version: 0.1.5
Check: LazyData
Result: NOTE
     'LazyData' is specified without a 'data' directory
Flavors: r-devel-linux-x86_64-debian-clang, r-devel-linux-x86_64-debian-gcc, r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-devel-windows-x86_64, r-devel-windows-x86_64-gcc10-UCRT, r-patched-linux-x86_64, r-patched-solaris-x86, r-release-linux-x86_64, r-release-macos-arm64, r-release-macos-x86_64, r-release-windows-ix86+x86_64

Version: 0.1.5
Check: package dependencies
Result: NOTE
    Package suggested but not available for checking: ‘RcppRedis’
Flavor: r-patched-solaris-x86
```

