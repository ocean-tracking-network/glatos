library(rmarkdown)

render(input = "inst/supplemental_docs/workshop_handouts/receiver_efficiency.Rmd", 
  output_dir = "vignettes",
  output_format = "all")

render(input = "inst/supplemental_docs/workshop_handouts/residence_index.Rmd", 
  output_dir = "vignettes",
  output_format = "all")


render(input = "inst/supplemental_docs/workshop_handouts/detection_range_vignette.Rmd", 
  output_dir = "vignettes", 
  output_format = "all")


render(input = "inst/supplemental_docs/workshop_handouts/gganimate_vignette.Rmd", 
  output_dir = "vignettes",
  output_format = "all")
