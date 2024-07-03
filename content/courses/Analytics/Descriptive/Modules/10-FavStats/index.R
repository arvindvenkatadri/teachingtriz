library(tidyverse)
library(mosaic)
library(skimr)
library(kableExtra)



ggplot2::theme_set(new = theme_classic(base_size = 14, base_family = "roboto"))
library(checkdown)
library(epoxy)
library(explore) # fake data generation
library(grateful)
# library(conflicted)
# conflicts_prefer(dplyr::filter, dplyr::last, dplyr::glimpse, base::max)

mpg %>% 
  head(10) %>%
  kbl(
    # add Human Readable column names
    col.names = c("Manufacturer", "Model", "Engine\nDisplacement", 
                    "Model\n Year", "Cylinders", "Transmission",
                    "Drivetrain", "City\n Mileage", "Highway\n Mileage",
                    "Fuel", "Class\nOf\nVehicle"), 
    caption = "MPG Dataset") %>%
  kable_styling(bootstrap_options = c("striped", "hover", 
                                      "condensed", "responsive"),
                full_width = F, position = "center")


glimpse(mpg)


skimr::skim(mpg) # explicitly stating package name


inspect(mpg)



mpg_describe <- inspect(mpg)
mpg_describe$categorical
mpg_describe$quantitative


# From Vincent Arel-Bundock's dataset website
# https://vincentarelbundock.github.io/Rdatasets
# 
# read_csv can read data directly from the net
# Don't use read.csv()
docVisits <- read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/DoctorVisits.csv")


docVisits <- read_csv("data/DoctorVisits.csv")


docVisits %>%
  head(10) %>%
  kbl(caption = "Doctor Visits Dataset",
      # Add Human Readable Names if desired
      # col.names(..names that you may want..)
      ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover",
                          "condensed", "responsive"),
    full_width = F, position = "center")


glimpse(docVisits)

skim(docVisits) %>% kbl()

inspect(docVisits)

diamonds %>% dplyr::count(cut)
diamonds %>% mosaic::count(color) # does the same thing! Counts!
diamonds %>% count(clarity)

### All combinations of cut, color, clarity
### Overwhelming??
diamonds %>% 
  count(across(where(is.ordered)))


## Counting by the obvious factor variables
docVisits %>% count(gender)
docVisits %>% count(private)
docVisits %>% count(freepoor)
docVisits %>% count(freerepat)
docVisits %>% count(lchronic)
docVisits %>% count(nchronic)


# Now for all Combinations...
# Maybe too much to digest...
docVisits %>% count(across(where(is.character)))
# Shall we try counting by some variables that might be factors?
# Even if they are labeled as <dbl>?
# 
docVisits %>% count(illness)
docVisits %>% count(health)


diamonds %>% 
  group_by(clarity) %>% 
  summarize(average_price = mean(price), count = n())

diamonds %>% 
  group_by(clarity, color) %>% 
  summarize(average_price = mean(price), count = n())

# Perhaps the best method for us!
diamonds %>% 
  mosaic::favstats(price ~ clarity, data = .) # Don't use fav_stats with formula!!!

# Be aware of the first column format here!
diamonds %>% 
  mosaic::favstats(price ~ clarity + cut, data = .) # Don't use fav_stats with formula!!!


docVisits %>%
  group_by(gender) %>% 
  summarize(average_visits = mean(visits), count = n())
##
docVisits %>%
  group_by(gender) %>% 
  summarize(average_visits = mean(visits), count = n())
##
docVisits %>% 
  group_by(freepoor,nchronic) %>% 
  summarise(mean_income = mean(income),
            average_visits = mean(visits),
            count = n())
##
docVisits %>% 
  mosaic::favstats(income ~ gender, data = .) # Don't use fav_stats with formula!!!
##
docVisits %>% 
  mosaic::favstats(income ~ freepoor + nchronic, data = .) # Don't use fav_stats with formula!!!


mpg %>% 
  head(10) %>%
  kbl(col.names = c("Manufacturer", "Model", "Engine\nDisplacement", 
                    "Model\n Year", "Cylinders", "Transmission",
                    "Drivetrain", "City\n Mileage", "Highway\n Mileage",
                    "Fuel", "Class\nOf\nVehicle"), 
      longtable = FALSE, centering = TRUE,
      caption = "MPG Dataset") %>%
    kable_styling(bootstrap_options = c("striped", "hover", 
                                        "condensed", "responsive"),
                  full_width = F, position = "center")



skim(mpg) %>%
  kbl(align = "c", caption = "Skim Output for mpg Dataset") %>%
kable_paper(full_width = F)
  

library(usedthese)
used_here()


#scan_packages()
cite_packages(
  output = "table",
  out.dir = ".",
  out.format = "html",
  pkgs = c("mosaic", "palmerpenguins", "skimr")
) %>%
  knitr::kable(format = "simple")

