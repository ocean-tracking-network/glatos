# Convert an Innovasea VRL or VDAT file to a Fathom CSV file

Use Innovasea's VDAT command line program `vdat.exe` (distributed with
Fathom Connect software) to make a CSV file containing data from a VRL
or VDAT file in Fathom CSV format.

## Usage

``` r
vdat_convert(
  src,
  out_dir = NULL,
  output_format = "csv.fathom",
  overwrite = FALSE,
  recursive = FALSE,
  vdat_exe_path = NULL,
  skip_pattern = "-RLD_",
  show_progress = TRUE,
  diagn = FALSE,
  export_settings = NULL
)
```

## Arguments

- src:

  Character string with path and name of a detection file (`VDAT` or
  `VRL`), a vector of file names, or a directory containing files. If
  only file name is given, then the file must be located in the working
  directory.

- out_dir:

  Optional character string with directory where CSV files will be
  written. If `NULL` (default) then each file will be written to the
  same directory as its source file.

- output_format:

  Character string with output format. Options are: `"csv.fathom"`
  (default) writes a single CSV file (for each input file) with multiple
  record types interleaved; `"csv.fathom.split"` writes a folder (for
  each input file) containing a separate CSV for each record type.

- overwrite:

  Logical. If `TRUE`, output CSV file(s) will overwrite existing CSV
  file(s) with same name in `out_dir`. If `FALSE` (default), any output
  files that already exist in `out_dir` will be skipped, with warning.

- recursive:

  Logical. If `TRUE` and `src` is a directory, then all VRL/VDAT files
  in all subdirectories of `src` will be converted. Default is `FALSE`.
  Ignored if `src` is a not directory.

- vdat_exe_path:

  The full path to `vdat.exe`. If `NULL` (default) then the path to
  vdat.exe must be in the PATH environment variable of the system. See
  [`check_vdat`](https://github.io/reference/check_vdat.md).

- skip_pattern:

  A regular expression used to exclude files from processing. Default
  value `"-RLD_"` will exclude "RAW LOG" files. Ignored if `src`
  contains file names.

- show_progress:

  Logical. Indicates if progress bar should be shown.

- diagn:

  Logical. Indicates if errors or warnings message (from vdat.exe)
  should be displayed (default = `FALSE`).

- export_settings:

  (NOT YET IMPLEMENTED). Placeholder for future specification of other
  options available via Fathom Data Export app. (E.g., 'Data Types to
  Include', 'Data Filter', 'Filename Suffix', 'Time Offset in Hours',
  'Split CSV by UTC Day'.)

## Value

A character string or vector with the full path and name of each output
file, including files that were skipped (when output file exists and
`overwrite = FALSE`).

## Details

If `src` is a directory, then all source files in that directory
(including all subdirectories if `recursive = TRUE`) with supported
extensions (currently `"vrl"` and `"vdat"`) will be converted to CSV.
Otherwise, only those files specified in `src` will be converted.

Conversion is done by system call to the Innovasea program `vdat.exe`
(included with Innovasea's Fathom Connect software; available at
<https://support.fishtracking.innovasea.com/s/downloads>). vdat.exe must
be available at the location specified by `vdat_exe_path` or via system
PATH environment variable. See also
[`check_vdat`](https://github.io/reference/check_vdat.md).

## Note

Tested on VDAT version vdat-10.6.0-20240716-1903df-release

## Output

Output depends on `output_format`:

If `output_format = "csv.fathom"`: A comma-separated-values (CSV) text
file in Innovasea's Fathom CSV format for each input VRL/VDAT file. Each
CSV is named the same (except for extension) as the source file (e.g.,
`VR2W_109924_20110718_1.csv`).

If `output_format = "csv.fathom.split"`: A directory containing a set of
CSV files for each input VRL/VDAT file. Each CSV file contains data for
one record type in Innovasea's Fathom CSV format and each file name
matches the corresponding record type (e.g, `BATTERY.csv`, `DET.csv`,
`HEALTH_VR2W.csv`). Each directory is named the same (except for
extension) as the source file (e.g.,
`VR2W_109924_20110718_1.csv-fathom-split`).

## Author

C. Holbrook, <cholbrook@glfc.org>

## Examples

``` r
if (FALSE) { # \dontrun{

# Check vdat.exe
check_vdat()

# all examples below assume path to vdat.exe is in system PATH environment
# variable. If not (you get an error), add input argument 'vdat_exe_path'
# with path directory with vdat.exe.
# e.g.,
# vdat_convert(
#   vrl_files,
#   vdat_exe_path = "C:/Program Files/Innovasea/Fathom Connect"
# )

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

# call vdatT.exe; default args
vdat_convert(vrl_files2)

# run again and overwrite
vdat_convert(vrl_files2, overwrite = TRUE)

# run again without progress bars
vdat_convert(vrl_files2, overwrite = TRUE, show_progress = FALSE)

# use split output format
vdat_convert(vrl_files2, output_format = "csv.fathom.split")

# change output directory
new_dir <- file.path(temp_dir, "testdir")
if (!dir.exists(new_dir)) dir.create(new_dir)

# write to new directory
vdat_convert(vrl_files2, out_dir = new_dir)


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
vdat_convert(vrl_files3)

# same but use input dir only and overwrite = TRUE
vdat_convert(dirname(vrl_files3), overwrite = TRUE)

# same but write all CSV files to new location
new_dir3 <- file.path(temp_dir, "testdir3")
if (!dir.exists(new_dir3)) dir.create(new_dir3)

vdat_convert(vrl_files3, out_dir = new_dir3)

# same but use input dir only and recursive = TRUE
vdat_convert(
  src = file.path(temp_dir, "testdir2"),
  out_dir = new_dir3,
  overwrite = TRUE,
  recursive = TRUE
)
} # }
```
