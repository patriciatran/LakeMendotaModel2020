# Patricia Tran
# animation from Residuals plots

install.packages("magick")
library(magick)

## list file names and read in
imgs <- list.files(path = "../Plots/", full.names = TRUE, pattern = ".png")
img_list <- lapply(imgs, image_read)

img_list

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "residuals.plots.gif")
