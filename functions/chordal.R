
## Inter-site chordal distance matrix
distance.chord.matrix <- function(long, lat) {
  
    ## The following function computes the distance on the surface of
    ## the earth between two points point1 and point2, by measuring
    ## the length of the chord between the two points. This
    ## approximation is better than the no.curvature.distance.stretch
    ## - both the points are of the form (Longitude, Latitude)
    
    distance.chord <- function(point1, point2)
    {
        R <- 6371
        
        p1rad <- point1 * pi/180
        
        lon1 <- p1rad[1]
        lat1 <- p1rad[2]
        
        u1 <- c(cos(lat1)*cos(lon1), cos(lat1)*sin(lon1), sin(lat1))

        p2rad <- point2 * pi/180
        
        lon2 <- p2rad[1]
        lat2 <- p2rad[2]
        
        u2 <- c(cos(lat2)*cos(lon2), cos(lat2)*sin(lon2), sin(lat2))
        
        R * sqrt(sum((u1-u2)^2))        
    }

    NSITES <- length(long)
    loc <- cbind(long, lat)
    
    d <- matrix(nrow=NSITES, ncol=NSITES)	
    
    for (i in 1:(NSITES-1)) {
        
        d[i,i] <- 0.0
	for (j in (i+1):NSITES) {					
            d[j,i] <- d[i,j] <- distance.chord(loc[i,],loc[j,])
	}	
    }
    d[NSITES, NSITES] <- 0.0
    
    d
}
