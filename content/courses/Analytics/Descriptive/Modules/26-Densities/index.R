options(paged.print = TRUE)
library(tidyverse)
library(mosaic)
library(ggformula)

#install.packages("remotes")
#library(remotes)
#remotes::install_github("wilkelab/ggridges")
library(ggridges)
library(skimr)

library(palmerpenguins) # Our new favourite dataset


library(checkdown)
library(epoxy)
library(TeachHist)
library(TeachingDemos)
library(visualize) # Plot Densities, Histograms and Probabilities as areas under the curve
library(grateful)
library(MKdescr)
library(shinylive) # To create a Shiny app in a Quarto HTML doc
# Will not work if webr is also used in the SAME Quarto doc!
library(sysfonts)
library(gfonts)
library(kableExtra)
# library(conflicted)
# conflicted::conflicts_prefer(dplyr::filter, dplyr::count, dplyr::last, dplyr::glimpse, base::max)


# https://stackoverflow.com/questions/74491138/ggplot-custom-fonts-not-working-in-quarto

# Chunk options
knitr::opts_chunk$set(
 fig.width = 7,
 fig.asp = 0.618, # Golden Ratio
 #out.width = "80%",
 fig.align = "center"
)
### Ggplot Theme
### https://rpubs.com/mclaire19/ggplot2-custom-themes

theme_custom <- function(){ 
    font <- "Roboto Condensed"   #assign font family up front
    
    theme_classic(base_size = 14) %+replace%    #replace elements we want to change
    
    theme(
      panel.grid.minor = element_blank(),    #strip minor gridlines
      text = element_text(family = font),
      #text elements
      plot.title = element_text(             #title
                   family = font,            #set font family
                   #size = 20,               #set font size
                   face = 'bold',            #bold typeface
                   hjust = 0,                #left align
                   #vjust = 2                #raise slightly
                   margin=margin(0,0,10,0)
),               
      
      plot.subtitle = element_text(          #subtitle
                   family = font,            #font family
                   #size = 14,               #font size
                   hjust = 0,
                   margin=margin(2,0,5,0)
),               
      
      plot.caption = element_text(           #caption
                   family = font,            #font family
                   size = 8,                 #font size
                   hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
                   family = font,            #font family
                   size = 10                 #font size
),
      
      axis.text = element_text(              #axis text
                   family = font,            #axis family
                   size = 8)               #font size
    )
}

# Set graph theme
theme_set(new = theme_custom())
#

read_csv("../../../../../materials/Data/pronouns.csv") %>% 
  filter(No == "1") %>% 
  kbl() %>%
  kable_paper("hover", full_width = T)
  

glimpse(penguins)


skim(penguins)


## Set graph theme
theme_set(new = theme_custom())
##
penguins <- penguins %>% drop_na()

gf_density( ~ body_mass_g, data = penguins) %>%
  gf_labs(title = "Plot A: Penguin Masses", caption = "ggformula")

###
penguins %>% gf_density( ~ body_mass_g, fill = ~ species, color = "black") %>%
  gf_refine(scale_color_viridis_d(option = "magma", aesthetics = c("colour", "fill"))) %>%
  gf_labs(title = "Plot B: Penguin Body Mass by Species", caption = "ggformula")

###
penguins %>%
  gf_density(
    ~ body_mass_g,
    fill = ~ species,
    color = "black",
    alpha = 0.3
  ) %>%
  gf_facet_wrap(vars(sex)) %>%
  gf_labs(title = "Plot C: Penguin Body Mass by Species and facetted by Sex", caption = "ggformula")

###
penguins %>%
  gf_density( ~ body_mass_g, fill = ~ species, color = "black") %>%
  gf_facet_wrap(vars(sex), scales = "free_y", nrow = 2) %>%
  gf_labs(title = "Plot D: Penguin Body Mass by Species and facetted by Sex",
          subtitle = "Free y-scale",
          caption = "ggformula") %>%
  gf_theme(theme(axis.text.x = element_text(angle = 45, hjust = 1)))



## Set graph theme
theme_set(new = theme_custom())
## Remove the rows containing NA (11 rows!)
penguins <- penguins %>% drop_na()

ggplot(data = penguins) + 
  geom_density(aes(x = body_mass_g)) + 
  labs(title = "Plot A: Penguin Masses",caption = "ggplot")

###
penguins %>% 
  ggplot() + 
  geom_density(aes(x = body_mass_g, fill = species),
                   color = "black") + 
  scale_color_viridis_d(option = "magma",
                        aesthetics = c("colour", "fill")) +
  labs(title = "Plot B: Penguin Body Mass by Species",
       caption = "ggplot")

###
penguins %>% ggplot() + 
  geom_density(aes(x = body_mass_g, fill = species),
                   color = "black",
                   alpha = 0.3) + 
  facet_wrap(vars(sex)) + 
  labs(title = "Plot C: Penguin Body Mass by Species and facetted by Sex",caption = "ggplot") 

###
penguins %>% ggplot() + 
  geom_density(aes(x = body_mass_g, fill = species), 
                   color = "black") + 
  facet_wrap(vars(sex), scales = "free_y", nrow = 2) + 
  labs(title = "Plot D: Penguin Body Mass by Species and facetted by Sex", 
       subtitle = "Free y-scale", caption = "ggplot") %>%
  theme(theme(axis.text.x = element_text(angle = 45,hjust = 1)))


## Set graph theme
theme_set(new = theme_custom())
##


gf_density_ridges(drv ~ hwy, fill = ~ drv, 
                  alpha = 0.3, 
                  rel_min_height = 0.005, data = mpg) %>% 
  gf_refine(scale_y_discrete(expand = c(0.01, 0)),
            scale_x_continuous(expand = c(0.01, 0))) %>% 
  gf_labs(title = "Ridge Plot")


race_df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv")
rank_df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv")


glimpse(race_df)
glimpse(rank_df)


skim(race_df)


skim(rank_df)


# inspect(race_df) # does not work with hms and difftime variables
inspect(rank_df)


race_df %>% count(country) %>% arrange(desc(n))
rank_df %>% count(nationality) %>% arrange(desc(n))


rank_df %>% 
  filter(rank %in% c(1,2,3)) %>%
  count(nationality) %>% arrange(desc(n))


longest_races <- race_df %>%
  slice_max(n = 5, order_by = distance) # Longest distance races
longest_races

longest_races %>%
  left_join(., rank_df, by  = "race_year_id") %>% # total participants in longest 4 races
  filter(rank %in% c(1:10)) %>% # Top 10 ranks
  count(nationality) %>% arrange(desc(n))



## Set graph theme
theme_set(new = theme_custom())
##

rank_df %>%
  gf_histogram(~ time_in_seconds, bins = 75) %>%
  gf_labs(title = "Histogram of Race Times")



## Set graph theme
theme_set(new = theme_custom())
##

race_df %>%
  gf_histogram(~ distance, bins =  50) %>%
  gf_labs(title = "Histogram of Race Distances")


race_df %>%
  filter(distance == 0)


race_times <- race_df %>%
  count(start_time) %>% arrange(desc(n))
race_times


# Demo purposes only!

## Set graph theme
theme_set(new = theme_custom())
##

race_start_factor <- race_df %>%
  mutate(
    start_day_time =
      case_when(
        start_time > hms("02:00:00") &
          start_time <= hms("06:00:00") ~ "early_morning",
        
        start_time > hms("06:00:01") &
          start_time <= hms("10:00:00") ~ "late_morning",
        
        start_time > hms("10:00:01") &
          start_time <= hms("14:00:00") ~ "mid_day",
        
        start_time > hms("14:00:01") &
          start_time <= hms("18:00:00") ~ "afternoon",
        
        start_time > hms("18:00:01") &
          start_time <= hms("22:00:00") ~ "evening",
        
        start_time > hms("22:00:01") &
          start_time <= hms("23:59:59") ~ "night",
        
        start_time >= hms("00:00:00") &
          start_time <= hms("02:00:00") ~ "postmidnight",
        
        .default =  "other"
      )
  ) %>%
  mutate(start_day_time = 
           as_factor(start_day_time) %>%
           fct_collapse(.f = ., 
               night = c("night", "postmidnight")))
##
# Join with rank_df
race_start_factor %>%
  left_join(rank_df, by = "race_year_id") %>%
  drop_na(time_in_seconds) %>%
  gf_histogram(
    ~ time_in_seconds,
    bins = 75,
    fill = ~ start_day_time,
    color = ~ start_day_time,
    alpha = 0.5
  ) %>%
  gf_facet_wrap(vars(start_day_time), ncol = 2, scales = "free_y") %>%
  gf_labs(title = "Race Times by Start-Time")


pop <- read_csv("data/populations.csv")
pop
inspect(pop)


## Set graph theme
theme_set(new = theme_custom())
##

##
gf_histogram(~ value, data = pop, title = "Long Tailed Histogram") 
##
gf_density(~ value, data = pop, title = "Long Tailed Density")


## Set graph theme
theme_set(new = theme_custom())
##

gf_histogram(~ log10(value), data = pop, title = "Histogram with Log transformed x-variable") 
##
gf_density(~ log10(value), data = pop, title = "Density with Log transformed x-variable")


# Build dataset with different distributions
library(hrbrthemes)
data <- data.frame(
  type = c( rep("edge peak", 1000), rep("comb", 1000), rep("normal", 1000), rep("uniform", 1000), rep("bimodal", 1000), rep("skewed", 1000) ),
  value = c( rnorm(900), rep(3, 100), rnorm(360, sd=0.5), rep(c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75), 80), rnorm(1000), runif(1000), rnorm(500, mean=-2), rnorm(500, mean=2), abs(log(rnorm(1000))) )
)

# Represent it
data %>%
  ggplot( aes(x=value)) +
    geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    facet_wrap(~type, scale="free_x") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )

library(TeachHist)

## Set graph theme
theme_set(new = theme_custom())
##

p1 <- TeachHistDens(Mean = 60, Sd = 5)
p3 <- TeachHistDens(Mean = 10, Sd = 5)
p2 <- TeachHistDens(Mean = 60, Sd = 15)
p4 <- TeachHistDens(Mean = 10, Sd = 15)



library(usedthese)
used_here()


#scan_packages()
cite_packages(
  output = "table",
  out.dir = ".",
  out.format = "html",
  pkgs = c("ggridges", "NHANES", "TeachHist",
           "TeachingDemos", "visualize")
) %>%
  knitr::kable(format = "simple")

