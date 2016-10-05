vrl2ulfx <- function(vrlDir, vueExePath="C:\\Program Files (x86)\\Vemco\\VUE"){
  if( !("VUE.exe" %in% list.files(vueExePath))) stop("VUE.exe not found at specified path.")
  shelltxt <- paste("cd \"",vueExePath,"\\\"", " & VUE --convert-files --output-format ulfx ", vrlDir, "\\*.vrl", sep="")
  shell(shelltxt)
}
