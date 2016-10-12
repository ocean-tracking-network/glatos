#Convert VRL file to CSV
#This creates a CSV file for each VRL without direct user interaction with VUE

vrl2csv <- function(vrl, vueExePath="C:\\Program Files (x86)\\Vemco\\VUE"){
  #check path to VUE.exe
  if( !("VUE.exe" %in% list.files(vueExePath))) stop("VUE.exe not found at specified path.")
  
  #if vrl is single directory, get list of vrl file names
  if(all(file.info(vrl)$isdir)) {
	if(!file.exists(vrl)) stop(paste0("File or folder ",vrl," not found.")
	if(length(vrl) > 1) stop("input argument 'vrl' cannot include more than one directory")
	if(length(vrl) == 0) stop(paste0("No VRL files found at ",vrl))
	if(length(vrl) == 1) vrl <- list.files(vrl,full.names=T, pattern=".vrl$|.VRL$|.Vrl$")
  } #end if
  
  #call vue command
  
  shelltxt <- paste0("cd \"",vueExePath,"\\\"", " & VUE --convert-files --output-format csv ", 
	paste(vrl,collapse=" "))
 # shelltxt <- paste0("cd \"",vueExePath,"\\\"", " & VUE --convert-files --output-format csv ", vrlDir, "\\*.vrl")
  shell(shelltxt)
  
  #return output vrl path(s) and file name(s)
  return(vrl)
}
