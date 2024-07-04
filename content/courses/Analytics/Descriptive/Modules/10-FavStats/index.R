library(tidyverse)
library(mosaic)
library(skimr)
library(knitr)
library(kableExtra)



ggplot2::theme_set(new = theme_classic(base_size = 14, base_family = "roboto"))
library(checkdown)
library(epoxy)
library(explore) # fake data generation
library(grateful)
# library(conflicted)
# conflicts_prefer(dplyr::filter, dplyr::last, dplyr::glimpse, base::max)

literacy <- readxl::read_xlsx("../../../../../materials/Data/US_literacy_SETables.xlsx",sheet = "S1",skip = 3) %>% 
  select(-c(2,3),-contains("S.E.")) %>% 
  rename("Numbers" = `...1`,
         "BelowLevel1" = `Estimate...4`,
         "Level1" = `Estimate...6`,
         "Level2" = `Estimate...8`,
         "Level3" = `Estimate...10`,
         "Level4/5" = `Estimate...12`) %>% 
  filter(str_detect(pattern = "Number",Numbers))

literacy %>% 
  kbl(caption = "US Population: Reading and Numeracy Levels", digits = 2,
      align = "c",centering = T,
      col.names = c("Year", "Below Level #1", "Level #1", "Level #2", "Level #3", "Levels #4 and #5")) %>% 
  kable_paper(full_width = F, html_font = "Noto") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "float_right") %>% 
  column_spec(2:4, bold = T) %>%
  row_spec(1:2, bold = T, color = "white", background = "#D7261E") %>% 
    footnote(general = "SOURCE: U.S. Department of Education, National Center for Education Statistics, Program for the International Assessment of Adult Competencies (PIAAC), U.S. PIAAC 2017, U.S. PIAAC 2012/2014.")

read_csv("../../../../../materials/Data/pronouns.csv") %>% 
  #filter(No == "1") %>% 
  kbl() %>%
  kable_paper(c("striped","hover","responsive"), full_width = T)
  

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


mpg_modified <- mpg %>% 
  dplyr::mutate(cyl = as_factor(cyl),
                fl = as_factor(fl),
                drv = as_factor(drv),
                class = as_factor(class),
                trans = as_factor(trans))
glimpse(mpg_modified)


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

mpg_modified %>% dplyr::count(cyl)
mpg_modified %>% mosaic::count(drv) # does the same thing! Counts!
mpg_modified %>% count(fl)

### All combinations of cut, color, clarity
### Overwhelming??
mpg_modified %>% 
  count(across(where(is.factor)))


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


mpg_modified %>% 
  group_by(cyl) %>% 
  summarize(average_hwy = mean(hwy), count = n())

mpg_modified %>% 
  group_by(cyl, fl) %>% 
  summarize(average_hwy = mean(hwy), count = n())

# Perhaps the best method for us!
mpg_modified %>% 
  mosaic::favstats(hwy ~ cyl, data = .) # Don't use fav_stats with formula!!!

# Be aware of the first column format here!
mpg_modified %>% 
  mosaic::favstats(hwy ~ cyl + fl, data = .) # Don't use fav_stats with formula!!!


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

