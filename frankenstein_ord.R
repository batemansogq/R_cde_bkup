#https://www.r-bloggers.com/frankenstein/

library(imager)
library(dplyr)
library(deldir)
library(ggplot2)
library(scales)

setwd('E://R/Photos')

# Download the image
file="http://ereaderbackgrounds.com/movies/bw/Frankenstein.jpg"
download.file(file, destfile = "frankenstein.jpg", mode = 'wb')

# Read and convert to grayscale
load.image("frankenstein.jpg") %>% grayscale() -> x
#load.image("frankenstein.jpg") %>% grayscale() -> v

# This is just to define frame limits
x %>% 
  as.data.frame() %>% 
  group_by() %>% 
  summarize(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y)) %>% 
  as.vector()->rw

# Filter image to convert it to bw
x %>%
  threshold("45%") %>% 
  as.data.frame() -> df

# Function to compute and plot Voronoi tesselation depending on sample size
doPlot = function(n)
{
  #Voronoi tesselation
  df %>% 
    #sample_n(n, weight=(1-value)) %>% 
    sample_n(n) %>% 
    select(x,y) %>% 
    deldir(rw=rw, sort=TRUE) %>% 
    .$dirsgs -> data
  
  # This is just to add some alpha to lines depending on its longitude
  data %>% 
    mutate(long=sqrt((x1-x2)^2+(y1-y2)^2),
           alpha=findInterval(long, quantile(long, probs = seq(0, 1, length.out = 20)))/21)-> data
  
  # A little bit of ggplot to plot results
  data %>% 
    ggplot(aes(alpha=(1-alpha))) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color="black", lwd=1) +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0), trans=reverse_trans())+
    theme(legend.position  = "none",
          panel.background = element_rect(fill="white"),
          axis.ticks       = element_blank(),
          panel.grid       = element_blank(),
          axis.title       = element_blank(),
          axis.text        = element_blank())->plot
  
  return(plot)
}

# I call the previous function and store resulting plot in jpeg format
i=5000
name=paste0("frankie",i,".jpeg")
jpeg(name, width = 600, height = 800, units = "px", quality = 100)
doPlot(i)
dev.off()

# Once all images are stored I can create gif
library(magick)
frames=c()
images=list.files(pattern="jpeg")

for (i in length(images):1)
{
  x=image_read(images[i])
  x=image_scale(x, "300")
  c(x, frames) -> frames
}
animation=image_animate(frames, fps = 2)
image_write(animation, "Frankenstein.gif")