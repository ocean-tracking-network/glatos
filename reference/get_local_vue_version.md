# Get version of local installation of Innovasea program VUE.exe

Get version of local installation of Innovasea program VUE.exe

## Usage

``` r
get_local_vue_version(vue_exe_path = NULL)
```

## Arguments

- vue_exe_path:

  The full path to `VUE.exe`. If `NULL` (default) then the path to
  VUE.exe must be in the PATH environment variable of the system. See
  [`check_vue`](https://github.io/reference/check_vue.md).

## Value

A list with `version` (version number) and `long_version` (full string
returned by VUE.exe).

## Examples

``` r
if (FALSE) { # \dontrun{

# use if VUE.exe in Windows system PATH variable
get_local_vue_version()

# or specify path to VUE.exe
get_local_vue_version(
  vue_exe_path =
    "C:/Program Files (x86)/Vemco/VUE"
)
} # }
```
