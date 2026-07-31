# Get schema from local installation of Innovasea program `vdat.exe`

Get schema from local installation of Innovasea program `vdat.exe`

## Usage

``` r
get_local_vdat_template(vdat_exe_path = NULL)
```

## Arguments

- vdat_exe_path:

  The full path to `vdat.exe`. If `NULL` (default) then the path to
  `vdat.exe` must be in the PATH environment variable of the system. See
  [`check_vdat`](https://ocean-tracking-network.github.io/glatos/reference/check_vdat.md).

## Value

Schema (template) of VDAT CSV produced by installed version of
`vdat.exe`.

## Details

A bug in vdat.exe version 9 (confirmed on vdat-9.3.0) will cause this
function to return an empty list. Fixed in vdat.exe version 10
(confirmed on vdat-10.6.0).

## Examples

``` r
if (FALSE) { # \dontrun{

# use if vdat.exe in Windows system PATH variable
get_local_vdat_template()

# or specify path to vdat.exe
get_local_vdat_template(
  vdat_exe_path =
    "C:/Program Files/Innovasea/Fathom Connect/vdat.exe"
)
} # }
```
