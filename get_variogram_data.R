library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(sp)
library(sf)
library(maps)
addRepo("geanders")
get_variogram<-function(){
  data(county.fips)
  mapcounty=st_as_sf(maps::map('county',plot = F,fill = T))
  tmp<-maps::map('county',plot = F,fill = T)
  tmp$x<-c(tmp$x,NA)
  tmp$y<-c(tmp$y,NA)
  px<-which(is.na(tmp$x))
  px_start<-c(0,px)
  re_x=c()
  for (i in px_start[1:length(px_start)-1]){
    mean_re=tmp$x[i:px_start[which(px_start==i)+1]] %>% mean(na.rm=T)
    re_x=c(re_x,mean_re)
  }
  py<-which(is.na(tmp$y))
  py_start<-c(0,py)
  re_y=c()
  for (i in py_start[1:length(py_start)-1]){
    mean_re=tmp$y[i:py_start[which(py_start==i)+1]] %>% mean(na.rm=T)
    re_y=c(re_y,mean_re)
  }
  
  colnames(county.fips)[2]='ID'
  Map=right_join(mapcounty,county.fips,'ID')
  Map2<-cbind(Map$ID,Map$fips,re_x,re_y) %>% data.frame()
  colnames(Map2)[1]='ID';colnames(Map2)[2]='fips'
  Map3<- Map2 %>% separate(col=ID,into=c('state','county'),sep=',')
  rain2<-rain %>% filter(storm_id=='Bonnie-2010',lag==0)
  final_re<-right_join(Map3,rain2,by='fips')
  return(final_re)
}
#write.csv(x=final_re, file="variogram_data.csv", row.names=F, quote=F)
