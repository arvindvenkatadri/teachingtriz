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
library(downloadthis)


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
),    plot.title.position = "plot",               
      
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
  

theme_set(new = theme_custom())
lincoln_weather %>% 
  gf_density_ridges_gradient(Month ~ `Max Temperature [F]`,
                             group = ~ Month) %>% 
  gf_refine(scale_fill_viridis_c(name = "Temperature [F]", option = "B")) %>% 
  gf_labs(title = "Weather in Lincoln, Nebraska")

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



library(rtrek)
star_trek_books <- stBooks
star_trek_books %>% download_this(output_name = "star_trek_books", output_extension = ".csv", button_label = "Start Trek Book data", button_type = "default", icon = "fa fa-save")



library(resampledata3)
data(MathAnxiety)
MathAnxiety %>% 
 download_this(output_name = "MathAnxiety", output_extension = ".csv", button_label = "Math Anxiety data", button_type = "default", icon = "fa fa-save")


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

