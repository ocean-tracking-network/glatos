library(rmarkdown)
library(tint)

render(input = "inst/user_guides/detection_range_vignette.Rmd", 
       output_file = "detection_range_vignette.pdf",
       output_dir = "vignettes",
  output_format = tint::tintPdf(highlight = "haddock"))

render(input = "inst/user_guides/detection_range_vignette.Rmd", 
       output_file = "detection_range_vignette.html",
       output_dir = "vignettes",
  output_format = tint::tintHtml(highlight = "haddock"))
