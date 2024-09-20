library(tidyverse,quietly = TRUE,warn.conflicts = FALSE)
##
theme_custom <- function(){ 
  font <- "Roboto Condensed"   #assign font family up front
  
  theme_classic(base_size = 14) %+replace%    #replace elements we want to change
    
    theme(
      panel.grid.minor = element_blank(),    #strip minor gridlines
      text = element_text(family = font),
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 16,               #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        #vjust = 2                #raise slightly
        margin=margin(0,0,10,0)
      ),               
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14,                #font size
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
update_geom_defaults(geom = "text", aes(family = "Roboto Condensed"))
#

