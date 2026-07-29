# Check path to Innovasea program VUE.exe

Check path to Innovasea program VUE.exe

## Usage

``` r
check_vue(vue_exe_path = NULL)
```

## Arguments

- vue_exe_path:

  The full path to `VUE.exe`. If `NULL` (default) then the path to
  VUE.exe must be in the PATH environment variable of the system.

## Value

Character string with command for calling VUE.exe via `system2`'s
`command` argument.

## Examples

``` r
if (FALSE) { # \dontrun{

# use Windows system PATH variable
check_vue()


# use path to directory containing VUE.exe
check_vue(vue_exe_path = "C:/Program Files (x86)/VEMCO/VUE")


# use full path to VUE.exe
check_vue(vue_exe_path = "C:/Program Files (x86)/VEMCO/VUE/VUE.exe")
} # }
```
