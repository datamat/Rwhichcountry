# Matthias Haeni

incountry <- function(X,Y) {
  sd <- NA
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
        tz <- point.in.polygon(X,Y,er$X,er$Y)
        if(tz>0) {sd <- zz; break; break}
      }
    }
  }
  return(sd)
}