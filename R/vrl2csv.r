#Convert VRL file to CSV
#This creates a CSV file for each VRL without direct user interaction with VUE

vrl2csv <- function(vrlDir, vueExePath="C:\\Program Files (x86)\\Vemco\\VUE"){
  if( !("VUE.exe" %in% list.files(vueExePath))) stop("VUE.exe not found at specified path.")
  shelltxt <- paste("cd \"",vueExePath,"\\\"", " & VUE --convert-files --output-format csv ", vrlDir, "\\*.vrl", sep="")
  shell(shelltxt)
}
