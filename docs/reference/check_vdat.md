# Check path to Innovasea program `vdat.exe`

Check path to Innovasea program `vdat.exe`

## Usage

``` r
check_vdat(vdat_exe_path = NULL)
```

## Arguments

- vdat_exe_path:

  The full path to `vdat.exe`. If `NULL` (default) then the path to
  `vdat.exe` must be in the PATH environment variable of the system.

## Value

Character string with command for calling `vdat.exe` via `system2`'s
`command` argument.

## Examples

``` r
if (FALSE) { # \dontrun{

# use Windows system PATH variable
check_vdat()


# use path to directory containing vdat.exe
check_vdat(vdat_exe_path = "C:/Program Files/Innovasea/Fathom Connect")


# use full path to vdat.exe
check_vdat(vdat_exe_path = "C:/Program Files/Innovasea/Fathom Connect/vdat.exe")
} # }
```
