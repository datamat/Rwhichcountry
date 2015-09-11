# Matthias Haeni

incountry <- function(df) {
  df$points <- NA
  df$ISO3 <- NA
  s <- getMap(resolution="high")
  uz <- as.character(unique(s$ISO3))    
  for(zt in c(1:length(uz))) {
    zz <- uz[zt]
    s <- getMap(resolution="high")
    ran <- which(s$ISO3==zz)
    if(length(ran)!=0) {
      s <- s[ran,]
      a <- SpatialPolygons(s@polygons,proj4string=s@proj4string)
      op <- length(a@polygons[[1]]@Polygons)
      for(qw in c(1:op)) {
        us <- a@polygons[[1]]@Polygons[[qw]]
        er <- as.data.frame(us@coords)
        names(er) <- c("X","Y")
        df$points <- point.in.polygon(df$X,df$Y,er$X,er$Y)
        df$ISO3[df$points>0] <- zz
      }
    }
  }
  return(df)
}