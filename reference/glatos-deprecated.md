# Deprecated functions in package glatos.

The functions listed below are deprecated and will be defunct in the
near future. When possible, alternative functions with similar
functionality are also mentioned. Help pages for deprecated functions
are available at `help("<function>-deprecated")`.

These functions still work but will be removed (defunct) in the next
version.

- `vrl2_csv`: This function is deprecated, and will be removed in the
  next version of this package. Use
  [vue_convert](https://github.io/reference/vue_convert.md) instead.

## Usage

``` r
vrl2csv(
  vrl,
  outDir = NA,
  overwrite = TRUE,
  vueExePath = NA,
  showProgress = TRUE
)
```

## Arguments

- vrl:

  A character string or vector with names of VRL file(s) or a single
  directory containing VRL files.

- outDir:

  A character string directory where CSV files will be written. If `NA`
  (default) then file(s) will be written to the current working
  directory (e.g., [`getwd()`](https://rdrr.io/r/base/getwd.html)).

- overwrite:

  Logical. If TRUE (default), output CSV file(s) will overwrite existing
  CSV file(s) with same name in `outDir`. When FALSE, '\_n' (i.e., \_1,
  \_2, etc.) will be appended to names of output files that already
  exist in `outDir`.

- vueExePath:

  An optional character string with directory containing VUE.exe. If NA
  (default) then the path to VUE.exe must be added to the PATH
  environment variable of your system. See Note below.

- showProgress:

  If TRUE (default) a progress bar and message are displayed.

## `vrl2csv`

For `vrl2csv`, use
[`vue_convert`](https://github.io/reference/vue_convert.md).
