library(LindenmayeR)
library(LearnGeom)
library(RgoogleMaps)
library(Julia)
library(tidyverse)
library(ggforce)

# Set up a coordinate Plane
x_min <- -6
x_max <- 6
y_min <- -6
y_max <- 12
CoordinatePlane(x_min, x_max, y_min, y_max)

# Create three points, to make a triangle
P1 <- c(-5,0) # points
P2 <- c(5,0)
P3 <- c(0,10)
poly <- CreatePolygon(P1,P2,P3) # Not drawing it
Draw(poly, colors = "orange")
LearnGeom::Circumcenter(poly,lines = TRUE)
Ortho <- c(0,3.75)
radius = DistancePoints(Ortho,P1)
radius
CreateArcPointsDist(P1, P2, radius, choice = 1, direction = "clock") %>% Draw(colors = "red")

# Parameters for a Koch Fractal
# Iterative Function
angle <- 60 # Angle of new linesegments
cut1 <- 1/3 # location along existing line segment
cut2 <- 2/3 # location along existing line segment
f <- .8 # Scale factor. Positive
it <- 4
FractalSegment(P1, P2, angle, cut1, cut2, f, it)
FractalSegment(P1, P3, -60, cut1, cut2, f, it)
FractalSegment(P3, P2, -60, cut1, cut2, f, it)

# Koch
x_min <- -6
x_max <- 6
y_min <- -12
y_max <- 6
CoordinatePlane(x_min, x_max, y_min, y_max)

P1 <- c(-5,0)
P2 <- c(5,0)
P3 <- c(0,-10)
it <- 4
Koch(P1, P2, it)
Koch(P3,P1,it)
Koch(P2,P3, it)

#
# Modified Koch curve
dictionary <- data.frame(symbol = c("F", "f", "+", "-", "[", "]"),
                         action = c("F", "f", "+", "-", "[", "]"),
                         stringsAsFactors = FALSE)
koch_morph_rules_1 <- data.frame(inp = c("F"), out = c("F+F-F-F+F"), stringsAsFactors = FALSE)
koch1 <- Lsys(init = "F", rules = koch_morph_rules_1, n = 4)
drawLsys(string = koch1, stepSize = 1, st = c(10, 50, 0), 
         drules = dictionary)
grid.text("Modified Koch Curve (n = 3)", 0.5, 0.25)

# Classic Koch snowflake
koch_morph_rules_2 <- data.frame(inp = c("F"), out = c("F-F++F-F"), stringsAsFactors = FALSE)
koch2 <- Lsys(init = "F++F++F", rules = koch_morph_rules_2, n = 4)
drawLsys(string = koch2, stepSize = 1, ang = 60, st = c(10, 25, 0), drules = dictionary)
grid.text("Classic Koch Snowflake (n = 4)", 0.5, 0.5)

# Lindemayer tree
tree_morph_rules_2 <- data.frame(inp = c("F"), out = c("F[+F][-F]"), stringsAsFactors = FALSE)
Ltree <- Lsys(init = "F", rules = tree_morph_rules_2, n = 7)
drawLsys(string = Ltree, drules = dictionary, stepSize = 1, 
         ang = 7,st = c(50,10,90))


# Penrose Snowflake
# Two methods: a) As per Fractal Grower b) My shorter method that does not work in Fractal Grower
#
dictionary <- data.frame(symbol = c("F", "f", "+", "-", "[", "]"),
                         action = c("F", "f", "+", "-", "[", "]"),
                         stringsAsFactors = FALSE)
penrose_rules_fg <- data.frame(inp = c("F"), out = c("F- -F- -F- - - - - F + F- -F"), stringsAsFactors = FALSE)
# Axiom = F--F--F--F--F
penrose1 <- Lsys(init = "F--F--F--F--F", rules = penrose_rules_fg, n = )
drawLsys(string = penrose1, stepSize = 3, ang = 36, st = c(25, 90, 0), drules = dictionary)



# My Way
dictionary <- data.frame(symbol = c("F", "f", "+", "-", "[", "]"),
                         action = c("F", "f", "+", "-", "[", "]"),
                         stringsAsFactors = FALSE)
penrose_rules_fg <- data.frame(inp = c("F"), out = c("F- -F + F + F- -F"), stringsAsFactors = FALSE)
# Axiom = F--F--F--F--F
penrose2 <- Lsys(init = "F--F--F--F--F", rules = penrose_rules_fg, n = 2)
drawLsys(string = penrose2, stepSize = 3, ang = 36, st = c(50, 50, 0), drules = dictionary)
