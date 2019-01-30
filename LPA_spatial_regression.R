setwd("~/Desktop/Anagha/UCSB/Classes/GEOG 210B/hw3")

install.packages("maptools", dependencies = TRUE)
install.packages("spdep", dependencies = TRUE)
install.packages("leaflet", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("sandwich", dependencies = TRUE)

library(spdep)
library(maptools)
library(leaflet)
library(RColorBrewer)
library(stargazer)

CA.poly <- readShapePoly('LPA_Pop_Char_bg.shp')
class(CA.poly)

CA.poly@data$center = CA.poly@data$LPAgrp == 4
CA.poly@data$suburb = CA.poly@data$LPAgrp == 3
CA.poly@data$exurb = CA.poly@data$LPAgrp == 2
CA.poly@data$rural = CA.poly@data$LPAgrp == 1
CA.poly@data$none = CA.poly@data$LPAgrp == 0

YCOUNTY <- CA.poly[CA.poly@data$countyname== c("Riverside"), ]

YCOUNTY@data$drvalpr =YCOUNTY@data$drvalM/YCOUNTY@data$n_pr
YCOUNTY@data$drvalpr[is.na(YCOUNTY@data$drvalpr)] <-0

drvalOLS<-lm(drvalpr~suburb+exurb+rural+HHSIZE1+HHSIZE2+HHSIZE3+HHSIZE4+HHSIZE5+HHSIZE6+HHSIZE7, data=YCOUNTY@data)
summary(drvalOLS)

library(lmtest)
library(sandwich)
bptest(drvalOLS, studentize=TRUE)

IDs<-row.names(as(YCOUNTY, "data.frame"))
coordsY<-coordinates(YCOUNTY)
plot(YCOUNTY)
sids_kn10<-knn2nb(knearneigh(coordsY, k=10), row.names=IDs)
plot(sids_kn10, coordsY, add=T)
summary (sids_kn10)

knn_w <-nb2listw(sids_kn10, style="W") ###
summary(knn_w)
structure(knn_w)
#Not working
mor10k <-sp.correlogram(sids_kn10, var=YCOUNTY@data$drvalpr, order=10, method="I")
plot(mor10k, main ="Moran's I with KNN Contiguity and Row Standardization")

LM<-lm.LMtests(drvalOLS, knn_w, test="all")

#Warning
SpaLag <-lagsarlm(drvalpr~suburb+exurb+rural+HHSIZE1+HHSIZE2+HHSIZE3+HHSIZE4+HHSIZE5+HHSIZE6+HHSIZE7, data=YCOUNTY, knn_w)
SpaErr<-errorsarlm(drvalpr~suburb+exurb+rural+HHSIZE1+HHSIZE2+HHSIZE3+HHSIZE4+HHSIZE5+HHSIZE6+HHSIZE7, data=YCOUNTY, knn_w)

SARAR<-sacsarlm(drvalpr~suburb+exurb+rural+HHSIZE1+HHSIZE2+HHSIZE3+HHSIZE4+HHSIZE5+HHSIZE6+HHSIZE7, data=YCOUNTY, knn_w)
SpaLagMix <-lagsarlm(drvalpr~suburb+exurb+rural+HHSIZE1+HHSIZE2+HHSIZE3+HHSIZE4+HHSIZE5+HHSIZE6+HHSIZE7, data=YCOUNTY, knn_w, type="mixed")

SARARMix<-sacsarlm(drvalpr~suburb+exurb+rural+HHSIZE1+HHSIZE2+HHSIZE3+HHSIZE4+HHSIZE5+HHSIZE6+HHSIZE7, data=YCOUNTY, knn_w, type="sacmixed")

