# R/util-vue.r

skip_on_ci()
skip_on_cran()

test_that("vue_convert works", {
  # VR2W file
  vrl_files <- system.file(
    "extdata",
    "detection_files_raw",
    c(
      "VR2W_109924_20110718_1.vrl",
      "VR2W180_302187_20180629_1.vrl",
      "VR2AR_546310_20190613_1.vrl",
      "VR2Tx_480022_20190613_1.vrl"
    ),
    package = "glatos"
  )

  temp_vrl_dir <- file.path(tempdir(), "vrl")
  dir.create(temp_vrl_dir)

  temp_vrl_files <- file.path(temp_vrl_dir, basename(vrl_files))
  file.copy(vrl_files, temp_vrl_files)

  # default args
  expect_snapshot(temp_csv_files <- vue_convert(temp_vrl_files))

  # skip existing output files
  expect_snapshot(temp_csv_files <- vue_convert(temp_vrl_files))

  # src is dir; overwrite = TRUE
  expect_snapshot(
    temp_csv_files2 <- vue_convert(temp_vrl_dir, overwrite = TRUE)
  )

  # skip RLD files
  rld_files <- tempfile(rep("VR2W-RLD_", 3), tmpdir = temp_vrl_dir)
  for (file_i in rld_files) writeLines("foo", file_i)

  expect_snapshot(
    temp_csv_files2 <- vdat_convert(temp_vrl_dir, overwrite = TRUE)
  )

  # clean up
  on.exit(
    unlink(
      c(
        temp_csv_files,
        temp_vrl_files,
        rld_files,
        temp_vrl_dir
      ),
      recursive = TRUE
    )
  )
})


test_that("vue_convert catches bad inputs", {
  # VR2W file
  vrl_files <- system.file(
    "extdata",
    "detection_files_raw",
    c(
      "VR2W_109924_20110718_1.vrl",
      "VR2W180_302187_20180629_1.vrl",
      "VR2AR_546310_20190613_1.vrl",
      "VR2Tx_480022_20190613_1.vrl"
    ),
    package = "glatos"
  )

  temp_vrl_dir2 <- file.path(tempdir(), "vrl2")
  dir.create(temp_vrl_dir2)

  # src is missing file
  expect_error(
    vue_convert(tempfile()),
    "Input 'src' not found:"
  )

  # src is empty dir
  expect_warning(
    vue_convert(temp_vrl_dir2),
    "No VRL files were found in 'src'"
  )

  temp_vrl_files2 <- file.path(temp_vrl_dir2, basename(vrl_files))
  file.copy(vrl_files, temp_vrl_files2)

  # input is (existing) file + dir
  expect_error(
    vue_convert(c(temp_vrl_files2[1], tempdir())),
    paste0(
      "Input arg 'src' must contain one or more ",
      "files or directories, but not both\\."
    )
  )

  # missing out_dir
  expect_error(
    vue_convert(temp_vrl_files2[1], out_dir = file.path(tempdir(), "1")),
    paste0("'out_dir' not found")
  )

  # more out_dir than src
  expect_error(
    vue_convert(
      temp_vrl_files2[1],
      out_dir = file.path(tempdir(), c("1", "2"))
    ),
    paste0(
      "Input 'out_dir' must be NULL, length one, or ",
      "same length at 'src'\\."
    )
  )

  # out_dir is file
  expect_error(
    vue_convert(temp_vrl_files2[1], out_dir = temp_vrl_files2[2]),
    paste0(
      "'out_dir' cannot contain full paths to files; ",
      "only directories\\."
    )
  )

  # bad input file
  temp_file2 <- file.path(tempdir(), "bad_input.vrl")
  writeLines("foo", temp_file2)

  expect_message(
    vue_convert(temp_file2, overwrite = TRUE),
    "Converting 1 VRL file\\(s\\) to VUE CSV\\.\\.\\."
  ) |>
    expect_output("^|=") |> # progressbar
    expect_message("Done\\. 0 of 1 file\\(s\\) written to disk\\.") |>
    expect_message("\n") |>
    expect_message("\\! 1 file\\(s\\) not written due to errors\\:")

  expect_warning(
    vue_convert(temp_file2, overwrite = TRUE, diagn = TRUE),
    "Error converting bad_input\\.vrl \\:"
  ) |>
    expect_message("Converting 1 VRL file\\(s\\) to VUE CSV\\.\\.\\.") |>
    expect_output("^|=") |> # progressbar
    expect_message("Done\\. 0 of 1 file\\(s\\) written to disk\\.") |>
    expect_message("\n") |>
    expect_message("\\! 1 file\\(s\\) not written due to errors\\:")

  # clean up
  on.exit(
    unlink(
      c(
        temp_vrl_files2,
        temp_vrl_dir2,
        temp_file2
      ),
      recursive = TRUE
    )
  )
})


test_that("check_vue works", {
  # Check vdat.exe
  expect_equal(check_vue(), "VUE")

  # dir contains vue.exe
  expect_equal(
    check_vue("C:/Program Files (x86)/Vemco/VUE"),
    "C:/Program Files (x86)/Vemco/VUE/VUE.exe"
  )

  # path contains vue.exe
  expect_equal(
    check_vue("C:/Program Files (x86)/Vemco/VUE/VUE.exe"),
    "C:/Program Files (x86)/Vemco/VUE/VUE.exe"
  )

  # vdat.exe not found
  expect_error(
    check_vue(tempdir()),
    "VUE\\.exe not found at specified path\\."
  )
})


test_that("get_local_vue_version works", {
  expect_type(
    temp_vue_version <- get_local_vue_version(),
    "list"
  )

  expect_equal(
    names(temp_vue_version),
    c(
      "version",
      "long_version"
    )
  )

  expect_type(temp_vue_version$version, "character")

  expect_type(temp_vue_version$long_version, "character")

  expect_error(
    get_local_vue_version(tempdir()),
    "VUE.exe not found at specified path.",
    fixed = TRUE
  )
})
