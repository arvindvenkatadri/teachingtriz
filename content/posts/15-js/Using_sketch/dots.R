#! load_library("p5")

# Make a "person" object
person <- function(id) {
  list(id = id, x = runif(1, 0, 400), 
       y = runif(1, 0, 300))
}

# Set up variables
radius <- 5
people <- map(1:50, person)

setup <- function() {
  createCanvas(400, 300)  
}

draw <- function() {
  background(0, 0, 33)    
  for (person in people) {   # Use a for loop to draw one person at a time
    fill(190, 128, 0)                
    circle(person$x, person$y, 2*radius)   
  }
  NULL
}
