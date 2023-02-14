#library(av)
library(readr)
library(tidyverse)

# turn .MOV files into .AVI (only do this once)

#f <- list.files(full.names = T,pattern=".MOV")
#dir.create("images")

#for(i in f){
 # if(dir.exists("images")) unlink("images",recursive = T)
 # av_video_images(i,destdir ="images",format="tiff")
 # f.i <- list.files("images",full.names = T)
 # av_encode_video(f.i,gsub("MOV","avi",basename(i)),codec = "rawvideo")
#}

# turn .TXT files into a tibble
walk.files <- list.files()[grep("walk_.*.txt",list.files())]

walk.dat <- list() 
for(i in walk.files){
  walk.i <- read_delim(i,delim="\t") 
  met.dat <- unlist(strsplit(i,"_")) 
  speed <- met.dat[2]
  subject <- gsub(".txt","",met.dat[3])
  walk.dat[[i]] <- walk.i%>%
    mutate(speed=speed,subject=subject)
}

walk.dat <- do.call(rbind,walk.dat)
colnames(walk.dat) <- str_remove(colnames(walk.dat)," \\[pixel\\]| \\[sec\\]")

walk.dat2 <- walk.dat%>%
  select(TID,PID,x,y,subject,speed)
walk.dat2%>%
  ggplot(aes(x,y,col=speed))+geom_point()+facet_grid(subject~.)

walk.dat2 <- walk.dat2%>%
  group_by(speed,subject)%>%
  mutate(y=abs(y-max(y)))
walk.dat2%>%
  ggplot(aes(x,y,col=speed))+geom_point()+facet_grid(subject~.)

walk.dat2 <- walk.dat2%>%
  mutate(TID=replace(TID, TID==1, "hipR"))%>%
  mutate(TID=replace(TID, TID==2, "hipC"))%>%
  mutate(TID=replace(TID, TID==3, "legR"))
head(walk.dat2)

walk.wide <- walk.dat2%>%
  pivot_wider(names_from=TID,values_from=c("x","y"))
head(walk.wide)