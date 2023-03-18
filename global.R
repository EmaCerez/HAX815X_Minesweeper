

# Packages

library("shiny")
library("png")
library("shinyjs")
library("shinyWidgets")


# Modules

source("modules/time-module.R")
source("modules/welcome-module.R")


# Functions

#source("sources/Démine.R")
#source("sources/functions.R")


# Images

n_tiles <- 26

df <- img(src="images/tiles/df.png", height=32, width=32)
lf <- img(src="images/tiles/lf.png", height=32, width=32)

db <- img(src="images/tiles/db.png", height=32, width=32)
lb <- img(src="images/tiles/lb.png", height=32, width=32)

de <- img(src="images/tiles/de.png", height=32, width=32)
le <- img(src="images/tiles/le.png", height=32, width=32)

dg <- img(src="images/tiles/dg.png", height=32, width=32)
lg <- img(src="images/tiles/lg.png", height=32, width=32)

dr <- img(src="images/tiles/dr.png", height=32, width=32)
lr <- img(src="images/tiles/lr.png", height=32, width=32)

dn <- c()
ln <- c()

for (i in 1:8){
  dn[i] <- paste0("images/tiles/d", i, ".png")
  ln[i] <- paste0("images/tiles/l", i, ".png")
}

d1 <- img(src=dn[1], height=32, width=32)
d2 <- img(src=dn[2], height=32, width=32)
d3 <- img(src=dn[3], height=32, width=32)
d4 <- img(src=dn[4], height=32, width=32)
d5 <- img(src=dn[5], height=32, width=32)
d6 <- img(src=dn[6], height=32, width=32)
d7 <- img(src=dn[7], height=32, width=32)
d8 <- img(src=dn[8], height=32, width=32)

l1 <- img(src=ln[1], height=32, width=32)
l2 <- img(src=ln[2], height=32, width=32)
l3 <- img(src=ln[3], height=32, width=32)
l4 <- img(src=ln[4], height=32, width=32)
l5 <- img(src=ln[5], height=32, width=32)
l6 <- img(src=ln[6], height=32, width=32)
l7 <- img(src=ln[7], height=32, width=32)
l8 <- img(src=ln[8], height=32, width=32)


