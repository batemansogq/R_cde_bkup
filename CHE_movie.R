#https://www.r-bloggers.com/frankenstein/
# http://flowingdata.com/2016/04/12/voronoi-diagram-and-delaunay-triangulation-in-r/

library(imager)
library(dplyr)
library(deldir)
library(ggplot2)
library(scales)

# Download the image
#file="http://ereaderbackgrounds.com/movies/bw/Frankenstein.jpg"
#download.file(file, destfile = "frankenstein.jpg", mode = 'wb')

setwd('E://R/Photos')


# Read and convert to grayscale
load.image("che.jpg") %>% grayscale() -> a

# This is just to define frame limits
rw <- a %>% 
  as.data.frame() %>% 
  group_by() %>% 
  summarize(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y)) %>% 
  as.vector()

# Filter image to convert it to bw
df <- a %>%  threshold("45%") %>%  as.data.frame() 

#df <- x %>%  as.data.frame() 

ax <- df %>% select(x, y) %>% sample_frac(0.025)

vtess <- deldir(ax$x, ax$y) 

xtess <- as.data.frame(vtess$dirsgs) %>%
  mutate(long=sqrt((x1-x2)^2+(y1-y2)^2),
         alpha=findInterval(long, quantile(long, probs = seq(0, 1, length.out = 20)))/21)-> data


plot(ax, asp=1, type="n", bty="n", xlab="", ylab="", axes=FALSE)
#points(ax, pch=20, cex=0.1, col="red")

plot(vtess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)


ggplot(xtess, aes(alpha)) +
geom_segment(aes(x = x1, 
                   y = y1, 
                   xend = x2, 
                   yend = y2), color="black", lwd=1) +
scale_x_continuous(expand=c(0,0))+
scale_y_continuous(expand=c(0,0), trans=reverse_trans()) +
  theme(legend.position  = "none",
        panel.background = element_rect(fill="white"),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())


# Function to compute and plot Voronoi tesselation depending on sample size
doPlot = function(n)
{
  #Voronoi tesselation
  data <- df %>% 
    select(x,y) %>% 
    sample_n(n) %>% 
    deldir(rw=rw, sort=TRUE) %>% 
    .$dirsgs 
  
  

  
  
  # This is just to add some alpha to lines depending on its longitude
  data <- data %>% 
    mutate(long=sqrt((x1-x2)^2+(y1-y2)^2),
           alpha=findInterval(long, quantile(long, 
                                             probs = seq(0, 1, length.out = 20)))/21)
  
  # A little bit of ggplot to plot results
  plot <- data %>% 
    ggplot(aes(alpha=(1-alpha))) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color="black", lwd=1) +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0), trans=reverse_trans())+
    theme(legend.position  = "none",
          panel.background = element_rect(fill="white"),
          axis.ticks       = element_blank(),
          panel.grid       = element_blank(),
          axis.title       = element_blank(),
          axis.text        = element_blank())
  
  return(plot)
}


setwd('E://R/Photos/che')

# I call the previous function and store resulting plot in jpeg format

i=5000

for ( j in 1:25) {name=paste0("che",i,".jpeg")
  jpeg(name, width = 600, height = 800, units = "px", quality = 100)
  doPlot(i)
  dev.off()
  i = i + 500
  }

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
image_write(animation, "CHE.gif")