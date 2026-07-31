# Convert an Innovasea Vemco VRL file to a VUE CSV file

Use Innovasea's command line program VUE.exe (distributed with VUE
software) to make a CSV file containing detection data from a VRL file
(receiver event data are not supported).

## Usage

``` r
vue_convert(
  src,
  out_dir = NULL,
  overwrite = FALSE,
  recursive = FALSE,
  vue_exe_path = NULL,
  skip_pattern = "-RLD_",
  show_progress = TRUE,
  diagn = FALSE
)
```

## Arguments

- src:

  Character string with path and name of a detection file (`VRL`), a
  vector of file names, or a directory containing files. If only file
  name is given, then the file must be located in the working directory.

- out_dir:

  Optional character string with directory where CSV files will be
  written. If `NULL` (default) then each file will be written to the
  same directory as its source file.

- overwrite:

  Logical. If `TRUE`, output CSV file(s) will overwrite existing CSV
  file(s) with same name in `out_dir`. If `FALSE` (default), any output
  files that already exist in `out_dir` will be skipped, with warning.

- recursive:

  Logical. If `TRUE` and `src` is a directory, then all VRL files in all
  subdirectories of `src` will be converted. Default is `FALSE`. Ignored
  if `src` is a not directory.

- vue_exe_path:

  The full path to `VUE.exe`. If `NULL` (default) then the path to
  VUE.exe must be in the PATH environment variable of the system. See
  [`check_vue`](https://github.io/reference/check_vue.md).

- skip_pattern:

  A regular expression used to exclude files from processing. Default
  value `"-RLD_"` will exclude "RAW LOG" files. Ignored if `src`
  contains file names.

- show_progress:

  Logical. Indicates if progress bar should be shown.

- diagn:

  Logical. Indicates if errors or warnings message (from vue.exe) should
  be displayed (default = `FALSE`).

## Value

A character string or vector with the full path and name of each output
file, including files that were skipped (when output file exists and
`overwrite = FALSE`).

## Details

If `src` is a directory, then all source files in that directory
(including all subdirectories if `recursive = TRUE`) with supported
extension (`"vrl"`) will be converted to CSV. Otherwise, only those
files specified in `src` will be converted. Each output CSV file will
have same name as its source file, except extension will be `csv`.

Conversion is done by system call to the Innovasea program `VUE.exe`
(included with Innovasea's VUE software; available at
<https://support.fishtracking.innovasea.com/s/downloads>). VUE.exe must
be available at the location specified by `vue_exe_path` or via system
PATH environment variable. See also
[`check_vue`](https://github.io/reference/check_vue.md).

## Note

Tested on VUE 2.8.1.

Only detection records are written. Receiver event data are not exported
because that functionality was not supported by the VUE system command
at time of writing. See `vue_convert` to extract detection and event
data.

VUE does not automatically correct timestamps for clock skew. To create
a CSV file with corrected timestamps using VUE's linear time correction
method, first time-correct each file using the VRL editor in VUE (under
Tools menu). To speed up that process, uncheck the "Import" checkbox
next to each filename, then run `vue_convert` to create a CSV for each
edited (e.g. time-corrected) VRL.

## Output

A comma-separated-values (CSV) text file in Innovasea's default VUE
Export format for each input VRL file. Each CSV is named the same
(except for extension) as the source file (e.g.,
`VR2W_109924_20110718_1.csv`).

## Author

C. Holbrook (cholbrook@glfc.org)

## Examples

``` r
if (FALSE) { # \dontrun{

# Check vue.exe
check_vue()

# all examples below assume path to VUE.exe is in system PATH environment
# variable. If not (you get an error), add input argument 'vue_exe_path'
# with path directory with VUE.exe.
# e.g.,
# vue_convert(vrl_files,
#            vue_exe_path = "C:/Program Files (x86)/Vemco/VUE")

# get path to example VRL files in glatos
vrl_files <- system.file("extdata", "detection_files_raw",
  c(
    "VR2W_109924_20110718_1.vrl",
    "VR2W180_302187_20180629_1.vrl",
    "VR2AR_546310_20190613_1.vrl",
    "VR2Tx_480022_20190613_1.vrl"
  ),
  package = "glatos"
)

# copy to temp_dir
temp_dir <- tempdir()
vrl_files2 <- file.path(temp_dir, basename(vrl_files))
file.copy(vrl_files, vrl_files2)

# uncomment to open in file browser
# utils::browseURL(temp_dir)

# call VUE.exe; default args
vue_convert(vrl_files2)

# run again and overwrite
vue_convert(vrl_files2, overwrite = TRUE)

# run again without progress bars
vue_convert(vrl_files2, overwrite = TRUE, show_progress = FALSE)


# change output directory
new_dir <- file.path(temp_dir, "testdir")
if (!dir.exists(new_dir)) dir.create(new_dir)

# write to new directory
vue_convert(vrl_files2, out_dir = new_dir)


# multiple source folders
# make new folder for each vrl file inside temp directory
new_dir2 <- file.path(
  temp_dir,
  "testdir2",
  seq_along(vrl_files2)
)
for (i in 1:length(new_dir2)) {
  if (!dir.exists(new_dir2[i])) dir.create(new_dir2[i], recursive = TRUE)
}

# redistribute files
vrl_files3 <- file.path(new_dir2, basename(vrl_files2))
file.copy(vrl_files2, vrl_files3)

# write each CSV file to same location as corresponding VRL (full path input)
vue_convert(vrl_files3)

# same but use input dir only and overwrite = TRUE
vue_convert(dirname(vrl_files3), overwrite = TRUE)

# same but write all CSV files to new location
new_dir3 <- file.path(temp_dir, "testdir3")
if (!dir.exists(new_dir3)) dir.create(new_dir3)

vue_convert(vrl_files3, out_dir = new_dir3)

# same but use input dir only and recursive = TRUE
vue_convert(
  src = file.path(temp_dir, "testdir2"),
  out_dir = new_dir3,
  overwrite = TRUE,
  recursive = TRUE
)
} # }
```
