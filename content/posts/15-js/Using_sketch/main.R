#! load_library("p5")

setup <- function() {       
  createCanvas(400, 300)  # create a canvas of size 400 x 300 (w x h)
}

draw <- function() {        
  background(0, 0, 33)    # paint the background with the RGB colour
  fill("red")             # change fill colour to "red"
  circle(200, 150, 50)    # draw a circle (x, y, diameter)
}
