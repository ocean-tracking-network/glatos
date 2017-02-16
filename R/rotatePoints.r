#' @export
#function to rotate points
rotatePoints <- function(x, y, theta, focus){
	pos <- cbind(x,y) #original points
	theta.rad <- theta*(pi/(180)) #convert to radians
	focus <- as.numeric(focus) #coerce to vector if not
	#make rotation matrix - for clockwise rotation
	rot <- matrix(c(cos(theta.rad), -sin(theta.rad), sin(theta.rad), cos(theta.rad)),ncol=2)
	pos[,1] <- pos[,1] - focus[1] #shift points to center at focus
	pos[,2] <- pos[,2] - focus[2] #shift points to center at focus
	pos <- t(rot%*%t(pos)) #rotate
	pos[,1] <- pos[,1] + focus[1] #shift back
	pos[,2] <- pos[,2] + focus[2] #shift back
	colnames(pos) <- c("x","y")
	return(as.data.frame(pos))
}





