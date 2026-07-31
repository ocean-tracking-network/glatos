# Get version of local installation of Innovasea program vdat.exe

Get version of local installation of Innovasea program vdat.exe

## Usage

``` r
get_local_vdat_version(vdat_exe_path = NULL)
```

## Arguments

- vdat_exe_path:

  The full path to `vdat.exe`. If `NULL` (default) then the path to
  `vdat.exe` must be in the PATH environment variable of the system. See
  [`check_vdat`](https://ocean-tracking-network.github.io/glatos/reference/check_vdat.md).

## Value

A list with `version` (version number) and `long_version` (full string
returned by vdat.exe).

## Examples

``` r
if (FALSE) { # \dontrun{

# use if vdat.exe in Windows system PATH variable
get_local_vdat_version()

# or specify path to vdat.exe
get_local_vdat_version(
  vdat_exe_path =
    "C:/Program Files/Innovasea/Fathom Connect/vdat.exe"
)
} # }
```
