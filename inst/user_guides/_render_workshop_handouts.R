library(rmarkdown)
library(tint)


render(input = "inst/user_guides/data_requirements.Rmd", 
  output_dir = "vignettes",
  output_format = "all")

render(input = "inst/user_guides/data_loading.Rmd", 
  output_dir = "vignettes",
  output_format = "all")


render(input = "inst/user_guides/receiver_efficiency.Rmd", 
  output_dir = "vignettes",
  output_format = "all")

render(input = "inst/user_guides/residence_index.Rmd", 
  output_dir = "vignettes",
  output_format = "all")



render(input = "inst/user_guides/detection_range_vignette.Rmd", 
  output_file = "detection_range_vignette.pdf", 
  output_format = tint::tintPdf(highlight = "haddock"))


render(input = "inst/user_guides/gganimate_vignette.Rmd", 
  output_file = "gganimate.pdf",
  output_format = tint::tintPdf(highlight = "haddock"), clean=TRUE)
