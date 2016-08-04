receiverLineDetSim <- function(vel=1,delayRng=c(120,360),burstDur=5.0,recSpc=1000,maxDist=2000,rngFun,outerLim=c(0,0),nsim=1000,showPlot=FALSE)
  {
      #this is the basic 2-receiver scenario
      #outerLim is allowable space the left of first receiver and right of second receiver
      #maxDist is the maximum distance away from (perpendicular to) receiver line
  
      xLim <- c(0,sum(recSpc)+sum(outerLim))
      recLoc <- c(outerLim[1], outerLim[1] + cumsum(recSpc))
      yLim <-  c(-maxDist, maxDist)

      nTrns <- floor((diff(yLim)/vel)/delayRng[1]) #number of transmissions to simulate

      del <- matrix(runif(nTrns*nsim,delayRng[1],delayRng[2]),nrow=nsim, ncol=nTrns) #sample delays
      del <- del + burstDur #add burst duration (for Vemco)
      trans <- t(apply(del, 1, cumsum)) #time series of signal transmissions
      #"center" the fish track over the receiver line; with some randomness
      trans <- trans - matrix(runif(nsim, trans[,nTrns/2],trans[,(nTrns/2)+1]), nrow=nsim, ncol=nTrns)
      #row = simulated fish; col = signal transmission
      fsh.x <- matrix(runif(nsim, xLim[1], xLim[2]), nrow=nsim, ncol=nTrns)
      fsh.y <- matrix(trans*vel, nrow=nsim, ncol=nTrns) #convert from time to distance from start

      if(showPlot){
        plot(NA, xlim=xLim, ylim=yLim, asp=c(1,1),
          xlab="Distance (in meters) along receiver line",
          ylab="Distance (in meters) along fish path")
        #fish tracks and transmissions
        for(i in 1:nsim){
          lines(fsh.x[i,], fsh.y[i,], col="grey") #fish tracks
          points(fsh.x[i,], fsh.y[i,], pch=20, cex=0.8) #signal transmissions
        }
        #receiver locations
        points(recLoc, rep(0,length(recLoc)), pch=21, bg='red', cex=1.2)
        legend("topleft",legend=c("receiver","sim. fish path","tag transmit"),
          pch=c(21,124,20),col=c("black","grey","black"),pt.bg=c("red",NA,NA),
          pt.cex=c(1.2,1,0.8))
      }
      
      #calculate distances between transmissions and receivers
      for(i in 1:length(recLoc)){
        if(i == 1) {
          succ <- detP <- distM <- vector("list",length(recLoc))
          nDets <- matrix(NA, nrow=nsim, ncol=length(recLoc)) #col = receiver
        }
        distM[[i]] <- sqrt((fsh.x - recLoc[i])^2 + (fsh.y)^2)
        #detP[[i]] <- matrix(approx(x=rngFun$x,y=rngFun$p,xout=distM[[i]])$y, nrow=nsim)
        #detP[[i]][is.na(detP[[i]])] <- 0
        detP[[i]] <- matrix(rngFun(distM[[i]]), nrow=nsim)
        succ[[i]] <- matrix(rbinom(length(detP[[i]]), 1, detP[[i]]), nrow=nsim) #was transmission detected?
        nDets[,i] <- rowSums(succ[[i]])
      }

      maxDet <- apply(nDets, 1, max) #max detects on any receiver
      detProb <-  mean(maxDet>1)
      
      return(data.frame(detProb=detProb))
  }