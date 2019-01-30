setwd("~/Desktop/Anagha/UCSB/Classes/GEOG 210B/hw4")

library(dplyr)  ## Data manipulation
library(maps) ## Projections
library(maptools) ## Data management
library(sp) ## Data management
library(spdep) ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(spgwr)        ## GWR (not sure we need this for now)
library(lattice)      ## Data visualization

my2k =readRDS('h2kb.rds')
sp_point <-cbind(my2k$XCORD, my2k$YCORD)
colnames(sp_point) <-c("LONG","LAT")
proj <-CRS("+proj=utm +zone=10 +datum=WGS84")
data.sp <-SpatialPointsDataFrame(coords=sp_point,my2k,proj4string=proj)
bbox(data.sp)
par(mar=c(2,2,0.2,0.2))
plot(data.sp,pch=16, cex=.5, axes=T)

SAMPLE1 <-sp_point[sample(100,replace=F),]
SAMPLE2 <-sp_point[sample(500,replace=F),]

km2d <- function(km){
  out <-(km/1.852)/60
  return(out)
}

# Entire dataset
u.x <-runif(n=nrow(sp_point), min=bbox(sp_point)[1,1], max=bbox(sp_point)[1,2])
u.y <-runif(n=nrow(sp_point), min=bbox(sp_point)[2,1], max=bbox(sp_point)[2,2])
r <-seq(0,km2d(10),length.out=10000)
#G function
env.u <-envelope(ppp(x=u.x,y=u.y,window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Gest, r=r, nsim=99, nrank=2)
plot(env.u)
env <-envelope(ppp(x=sp_point[,1],y=sp_point[,2],window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Gest, r=r, nsim=99, nrank=2)
plot(env)
#The G values from our data are above the randomization envelope. Therefore, we have clustering in our data!
#F
Fenv.u <-envelope(ppp(x=u.x,y=u.y,window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Fest, r=r, nsim=99, nrank=2)
plot(Fenv.u)
Fenv <-envelope(ppp(x=sp_point[,1],y=sp_point[,2],window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Fest, r=r, nsim=99, nrank=2)
summary(Fenv)
plot(Fenv)
#When the F values from our data are below the envelope of the Fs under Poisson we have clustering. This means that the empty space distances among points are longer than a Poisson process (i.e., uniformity). The opposite when the F values from the data are above the Poisson envelope means the empty space distances in our data are shorter thatn Poisson distances implying a regular pattern.
#K function
Kenv <-envelope(ppp(x=sp_point[,1],y=sp_point[,2],window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Kest, r=r, nsim=99, nrank=2)
plot(Kenv)

#SAMPLE1
u.x1 <-runif(n=nrow(SAMPLE1), min=bbox(SAMPLE1)[1,1], max=bbox(SAMPLE1)[1,2])
u.y1 <-runif(n=nrow(SAMPLE1), min=bbox(SAMPLE1)[2,1], max=bbox(SAMPLE1)[2,2])
#G function
env.u1 <-envelope(ppp(x=u.x1,y=u.y1,window=owin(bbox(SAMPLE1)[1,],bbox(SAMPLE1)[2,])), fun=Gest, r=r, nsim=99, nrank=2)
plot(env.u1)
env1 <-envelope(ppp(x=SAMPLE1[,1],y=SAMPLE1[,2],window=owin(bbox(SAMPLE1)[1,],bbox(SAMPLE1)[2,])), fun=Gest, r=r, nsim=99, nrank=2)
plot(env1)
#F
Fenv.u1 <-envelope(ppp(x=u.x1,y=u.y1,window=owin(bbox(SAMPLE1)[1,],bbox(SAMPLE1)[2,])), fun=Fest, r=r, nsim=99, nrank=2)
plot(Fenv.u1)
Fenv1 <-envelope(ppp(x=SAMPLE1[,1],y=SAMPLE1[,2],window=owin(bbox(SAMPLE1)[1,],bbox(SAMPLE1)[2,])), fun=Fest, r=r, nsim=99, nrank=2)
plot(Fenv1)
Kenv1 <-envelope(ppp(x=SAMPLE1[,1],y=SAMPLE1[,2],window=owin(bbox(SAMPLE1)[1,],bbox(SAMPLE1)[2,])), fun=Kest, r=r, nsim=99, nrank=2)
plot(Kenv1)

#SAMPLE2
u.x2 <-runif(n=nrow(SAMPLE2), min=bbox(SAMPLE2)[1,1], max=bbox(SAMPLE2)[1,2])
u.y2 <-runif(n=nrow(SAMPLE2), min=bbox(SAMPLE2)[2,1], max=bbox(SAMPLE2)[2,2])
#G function
env.u2 <-envelope(ppp(x=u.x2,y=u.y2,window=owin(bbox(SAMPLE2)[1,],bbox(SAMPLE2)[2,])), fun=Gest, r=r, nsim=99, nrank=2)
plot(env.u2)
env2 <-envelope(ppp(x=SAMPLE2[,1],y=SAMPLE2[,2],window=owin(bbox(SAMPLE2)[1,],bbox(SAMPLE2)[2,])), fun=Gest, r=r, nsim=99, nrank=2)
plot(env2)
#F
Fenv.u2 <-envelope(ppp(x=u.x2,y=u.y2,window=owin(bbox(SAMPLE2)[1,],bbox(SAMPLE2)[2,])), fun=Fest, r=r, nsim=99, nrank=2)
plot(Fenv.u2)
Fenv2 <-envelope(ppp(x=SAMPLE2[,1],y=SAMPLE2[,2],window=owin(bbox(SAMPLE2)[1,],bbox(SAMPLE2)[2,])), fun=Fest, r=r, nsim=99, nrank=2)
plot(Fenv2)
Kenv2 <-envelope(ppp(x=SAMPLE2[,1],y=SAMPLE2[,2],window=owin(bbox(SAMPLE2)[1,],bbox(SAMPLE2)[2,])), fun=Kest, r=r, nsim=99, nrank=2)
plot(Kenv2)

###PART 2

poly <- as.points(c(min(sp_point[,1]),max(sp_point[,1]),max(sp_point[,1]),min(sp_point[,1])),c(max(sp_point[,2]),max(sp_point[,2]),min(sp_point[,2]),min(sp_point[,2])))
poly1 <- as.points(c(min(SAMPLE1[,1]),max(SAMPLE1[,1]),max(SAMPLE1[,1]),min(SAMPLE1[,1])),c(max(SAMPLE1[,2]),max(SAMPLE1[,2]),min(SAMPLE1[,2]),min(SAMPLE1[,2])))
poly2 <- as.points(c(min(SAMPLE2[,1]),max(SAMPLE2[,1]),max(SAMPLE2[,1]),min(SAMPLE2[,1])),c(max(SAMPLE2[,2]),max(SAMPLE2[,2]),min(SAMPLE2[,2]),min(SAMPLE2[,2])))

#Entire dataset
mserw <- mse2d(sp_point, poly=poly, nsmse=100, range=0.1)
bw <- mserw$h[which.min(mserw$mse)] 

#SAMPLE1
mserw1 <- mse2d(SAMPLE1, poly=poly1, nsmse=100, range=0.1)
bw1 <- mserw$h[which.min(mserw$mse)]

#SAMPLE2
mserw2 <- mse2d(SAMPLE2, poly=poly2, nsmse=100, range=0.1)
bw2 <- mserw$h[which.min(mserw$mse)] 

#Entire Dataset
sp_points <- SpatialPoints(coords=sp_point, proj4string=CRS("+proj=utm +zone=10 +datum=WGS84"))
grd <- Sobj_SpatialGrid(sp_points,maxDim=100)$SG
grd <- GridTopology(summary(grd)$grid[,1],cellsize=summary(grd)$grid[,2],cells.dim=summary(grd)$grid[,3])

#SAMPLE1
SAMPLE1pts <- SpatialPoints(coords=SAMPLE1, proj4string=CRS("+proj=utm +zone=10 +datum=WGS84"))
grd1 <- Sobj_SpatialGrid(SAMPLE1pts,maxDim=100)$SG
grd1 <- GridTopology(summary(grd1)$grid[,1],cellsize=summary(grd1)$grid[,2],cells.dim=summary(grd1)$grid[,3])

#SAMPLE2
SAMPLE2pts <- SpatialPoints(coords=SAMPLE2, proj4string=CRS("+proj=utm +zone=10 +datum=WGS84"))
grd2 <- Sobj_SpatialGrid(SAMPLE2pts,maxDim=100)$SG
grd2 <- GridTopology(summary(grd2)$grid[,1],cellsize=summary(grd2)$grid[,2],cells.dim=summary(grd2)$grid[,3])

#I've chosen to use only the bw-dictated minimum mean squared error to create kernel densities. Having gone through a couple of bandwidth options, bw shows the clearest diversity of points - as expected, bw creates the optimized bandwidth

#Entire Dataset
kernel1 <- spkernel2d(sp_points, poly=poly, h0=bw, grd=grd)

#SAMPLE1
kernel2 <- spkernel2d(SAMPLE1, poly=poly1, h0=bw1, grd=grd1)

#SAMPLE2
kernel3 <- spkernel2d(SAMPLE2, poly=poly2, h0=bw2, grd=grd2)


CAdf <- data.frame(kernel1=kernel1)
CAsg <- SpatialGridDataFrame(grd, data=CAdf)
spplot(CAsg, main="California Event Location Bandwidth=0.041")

CAdf2 <- data.frame(kernel2=kernel2)
CAsg2 <- SpatialGridDataFrame(grd1, data=CAdf2)
spplot(CAsg2, main="California Event Location Bandwidth=0.041")

CAdf3 <- data.frame(kernel3=kernel3)
CAsg3 <- SpatialGridDataFrame(grd2, data=CAdf3)
spplot(CAsg3, main="California Event Location Bandwidth=0.041")


