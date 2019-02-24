library(rmarkdown)

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
  output_dir = "vignettes"
  output_format = "all")

render(input = "inst/user_guides/gganimate_vignette.Rmd", 
  output_dir = "vignettes",
  output_format = "all")
