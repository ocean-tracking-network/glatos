vrl_to_tempdir <- function(test_dir) {
  ## Access internal VRL
  myVRL <- system.file(
    "extdata",
    "detection_files_raw",
    "VR2W_109924_20110718_1.vrl",
    package = "glatos"
  )

  ## Create temp_dir
  test_dir <- file.path(tempdir(), test_dir)
  if (!dir.exists(test_dir)) dir.create(test_dir)


  ## Copy internal VRL to test_dir
  good_vrl <- file.path(test_dir, basename(myVRL))
  copied <- file.copy(myVRL, good_vrl)

  list(
    test_dir = test_dir,
    vrl = good_vrl
  )
}


# Check csv from one VRL in dir with space in name
test_that("one vrl gives expected result", {
  skip_on_ci()
  skip_on_cran()

  vrl_loc <- vrl_to_tempdir("test")

  # Warns re: deprecation
  expect_warning(
    good_csv <- vrl2csv(
      vrl_loc$vrl,
      outDir = vrl_loc$test_dir,
      vueExePath = "C:/Program Files (x86)/VEMCO/VUE",
      showProgress = FALSE
    ),
    "'vrl2csv' is deprecated\\."
  )

  # invisibly returns file path
  expect_invisible(
    suppressWarnings(
      good_csv <- vrl2csv(
        vrl_loc$vrl,
        outDir = vrl_loc$test_dir,
        vueExePath = "C:/Program Files (x86)/VEMCO/VUE",
        showProgress = FALSE
      )
    )
  )
  
  expect_equal(
    normalizePath(dirname(good_csv)),
    normalizePath(vrl_loc$test_dir)
  )
  
  expect_equal(
    basename(good_csv),
    gsub(
      "\\.vrl", "\\.csv",
      basename(vrl_loc$vrl)
    )
  )

  
  # Check message and progress bar when showProgress = TRUE
  
  suppressWarnings(
    good_csv <- vrl2csv(
      vrl_loc$vrl,
      outDir = vrl_loc$test_dir,
      vueExePath = "C:/Program Files (x86)/VEMCO/VUE"
    )
  ) |> 
    expect_message("Converting 1 detection files\\.\\.\\.") |>
    expect_output("\\|======================================================================\\| 100%")
  
  # # Creates message
  # expect_message(
  #   suppressWarnings(
  #     good_csv <- vrl2csv(
  #       vrl_loc$vrl,
  #       outDir = vrl_loc$test_dir,
  #       vueExePath = "C:/Program Files (x86)/VEMCO/VUE"
  #     )
  #   ),
  #   "Converting 1 detection files\\.\\.\\."
  # )
  # 
  # # Outputs progress bar
  # expect_output(
  #   suppressWarnings(
  #     good_csv <- vrl2csv(
  #       vrl_loc$vrl,
  #       outDir = vrl_loc$test_dir,
  #       vueExePath = "C:/Program Files (x86)/VEMCO/VUE"
  #     )
  #   ),
  #   "\\|======================================================================\\| 100%"
  # )

  # Check if expected and actual results are the same
  expect_snapshot(
    readLines(good_csv, n = 10)
  )

  # Clean up
  unlink(
    vrl_loc$test_dir,
    recursive = TRUE
  )
})




test_that("one vrl in dir with space in name gives expected result", {
  skip_on_ci()
  skip_on_cran()

  vrl_loc <- vrl_to_tempdir("test path with spaces")

  # Warns re: deprecation. Deprecation warnings are suppressed after this
  expect_warning(
    good_csv <- vrl2csv(
      vrl_loc$vrl,
      outDir = vrl_loc$test_dir,
      vueExePath = "C:/Program Files (x86)/VEMCO/VUE",
      showProgress = FALSE
    ),
    "'vrl2csv' is deprecated\\."
  )

  expect_invisible(
    suppressWarnings(
      good_csv <- vrl2csv(
        vrl_loc$vrl,
        outDir = vrl_loc$test_dir,
        vueExePath = "C:/Program Files (x86)/VEMCO/VUE",
        showProgress = FALSE
      )
    )
  )
  expect_equal(
    normalizePath(dirname(good_csv)),
    normalizePath(vrl_loc$test_dir)
  )
  expect_equal(
    basename(good_csv),
    gsub(
      "\\.vrl", "\\.csv",
      basename(vrl_loc$vrl)
    )
  )

  
  # Check message and progress bar when showProgress = TRUE
  suppressWarnings(
    good_csv <- vrl2csv(
      vrl_loc$vrl,
      outDir = vrl_loc$test_dir,
      vueExePath = "C:/Program Files (x86)/VEMCO/VUE"
    )
  ) |> 
    expect_message("Converting 1 detection files\\.\\.\\.") |>
    expect_output("\\|======================================================================\\| 100%")
  

  # Check if expected and actual results are the same
  expect_snapshot(
    readLines(good_csv, n = 10)
  )

  # Clean up
  unlink(
    vrl_loc$test_dir,
    recursive = TRUE
  )
})




test_that("one good vrl in dir with corrupt vrl gives expected result", {
  skip_on_ci()
  skip_on_cran()

  ## Create corrupt VRL
  vrl_loc <- vrl_to_tempdir("test")

  renamed <- file.rename(
    vrl_loc$vrl,
    file.path(vrl_loc$test_dir, "corrupt.vrl")
  )

  write(
    c("SOMEgibberish"),
    file.path(vrl_loc$test_dir, "corrupt.vrl")
  )

  vrl_loc <- vrl_to_tempdir("test")


  expect_warning(
    out_csv <- vrl2csv(
      list.files(vrl_loc$test_dir, full.names = T),
      outDir = vrl_loc$test_dir,
      vueExePath = "C:/Program Files (x86)/VEMCO/VUE",
      showProgress = FALSE
    ),
    "deprecated"
  ) |>
    expect_warning("corrupt\\.csv was not created")

  # Check if expected and actual results are the same
  expect_snapshot(
    readLines(out_csv, n = 10)
  )

  # Clean up
  unlink(
    vrl_loc$test_dir,
    recursive = TRUE
  )
})
