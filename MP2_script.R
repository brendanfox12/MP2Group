library(readr)
library(tidyverse)

# turn .MOV files into .AVI (only do this once)
#library(av)

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
print(walk.files)

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
walk.dat2 <- walk.dat2%>%
  group_by(speed,subject)%>%
  mutate(y=abs(y-max(y)))

walk.dat2 <- walk.dat2 %>%
  mutate(TID=replace(TID, TID==1, "hipR")) %>%
  mutate(TID=replace(TID, TID==2, "hipC"))

walk.wide <- walk.dat2%>%
  pivot_wider(names_from=TID,values_from=c("x","y"))

# calculate pelvic tilt
walk.wide <- walk.wide %>%
  mutate(angle=((180/pi)*atan(abs(y_hipR-y_hipC)/abs(x_hipR-x_hipC))))

max.angle <- walk.wide%>%
  group_by(subject,speed)%>%
  slice(which.max(angle))

# calculate vmax
max1 <- sqrt(9.8*1)
max2 <- sqrt(9.8*0.97)
max3 <- sqrt(9.8*1.06)
max4 <- sqrt(9.8*1.1)

# calculate pendulum walking speed
v1 <- 5/4
v2 <- 5/5
v3 <- 5/4
v4 <- 5/4

# calculate Froude efficiency
(v1)^2/max1
(v2)^2/max2
(v3)^2/max3
(v4)^2/max4

