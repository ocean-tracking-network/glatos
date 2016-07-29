#simulate a random walk; parameterized by turn angle (change in heading) 
crw <- function(theta=c(0,5), stepLen=10, initPos=c(0,0), initHeading=0,nsteps=10000){
      #generate turn angles
      heading <- rnorm(nsteps, mean=theta[1],sd=theta[2])
      heading <- initHeading + cumsum(heading)
      heading <- heading %% 360
      heading_rad <- heading * pi/180 #convert to radians
        
      #create steps
      xlen <- sin(heading_rad)*stepLen #x-component vector
      ylen <- cos(heading_rad)*stepLen #y-component vector
      path <- data.frame(
           x = as.numeric(initPos)[1] + cumsum(xlen),
           y = as.numeric(initPos)[2] + cumsum(ylen))
            
  return(path)
} #end crw def  