# required libraries
library(jpeg)

# constants
MARGINBUFFER = 0.2
ROWS = 4
COLS = 2
SHEET = matrix(1:(ROWS*COLS), ROWS, COLS, byrow = TRUE)
PAGEWIDTH = 8.5 # inches
PAGEHEIGHT = 11 # inches

# how many digits
ndigits <- function(x) floor(log10(x) + 1)

#an empty plot on x = [0, 1], y = [0, 1]
stdplot <- function(...) {
  plot(
    0, 0, type = 'n', axes = FALSE, 
    xlim = c(0, 1), ylim = c(0, 1),
    xlab = "", ylab = "",
    ...
  )
}


# test file
fnames <- list.files("testimg", full.names = TRUE)
fnames <- fnames[grep("*.jpg$|*.jpeg$", fnames, ignore.case = TRUE)]

if(length(fnames) < 1) stop("i can't find any image files")

npages <- ceiling(length(fnames) / (ROWS*COLS))
pageid <- rep(1:npages, each = ROWS*COLS)[1:length(fnames)]
outfilename <- paste0("flash-%0", ndigits(npages), "d.pdf")

fnames_bypage <- split(fnames, pageid)

print(paste("pages are going to be", npages))

for(p in 1:npages) {
print(paste("page", p))
  # set up a page
  pdf(sprintf(outfilename, p), width = PAGEWIDTH, height = PAGEHEIGHT)
  par(oma = rep(1, 4), mar = rep(MARGINBUFFER, 4))
  layout(SHEET)
  
  fnames_cur <- fnames_bypage[[p]]
  
  for(i in 1:length(fnames_cur)) {
print(paste("image", i))
    img <- jpeg::readJPEG(fnames_cur[i])
    ras <- as.raster(img[, , 1:3])
    
    # make a blank space to plot
    stdplot()
    
    # get the dimensions
    dims <- par()$usr
    xl <- dims[1]
    yb <- dims[3]
    xr <- dims[2]
    yt <- dims[4]
    
    # draw the raster
    rasterImage(ras, xl, yb, xr, yt)
  }
  
  dev.off()
}
