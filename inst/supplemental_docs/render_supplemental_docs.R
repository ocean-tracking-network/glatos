library(rmarkdown)

render(input = "inst/supplemental_docs/receiver_efficiency_index_handout.Rmd", 
  output_dir = "vignettes",
  output_format = "all")

render(input = "inst/supplemental_docs/residence_index_handout.Rmd", 
  output_dir = "vignettes",
  output_format = "all")


render(input = "inst/supplemental_docs/detection_range_handout.Rmd", 
  output_dir = "vignettes", 
  output_format = "all")


render(input = "inst/supplemental_docs/gganimate_handout.Rmd", 
  output_dir = "vignettes",
  output_format = "all")


#render html versions of vignettes
render(input = "vignettes/data_loading_vignette.Rmd", 
  output_dir = "vignettes",
  output_format = "html_vignette")

render(input = "vignettes/data_requirements_vignette.Rmd", 
  output_dir = "vignettes",
  output_format = "html_vignette")

