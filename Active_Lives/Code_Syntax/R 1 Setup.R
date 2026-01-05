## ---- Setup
# Install if not already installed
#install.packages("GGally")
#install.packages("psych")
rm(list = ls())
# Packages 
library(car)
library(tidyverse)
library(lubridate)
library(patchwork)
library(sf)
library(jsonlite)
library(httr)
library(scales)
library(tidyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(dplyr)
library(purrr)
library(DT)
library(htmlwidgets)
library(htmltools)
library(epitools)
library(leaflet) 
library(ggiraph)
library(ggVennDiagram)
library(venn)
library(VennDiagram)
library(RColorBrewer)
library(gridExtra)
library(grid)
#library(ggsankey)
library(writexl)
library(ggalluvial)
library(knitr)
library(gt)
library(openxlsx)
library(mapview)
library(webshot)
library(DescTools)
library(treemap)
library(treemapify)
library(plotly)
library(GGally)
library(ggplot2)
library(psych)
library(data.table)
library(janitor)
library(UpSetR)

theme_bnssg <- function(base_size = 12, base_family = "sans",base_colour = "black"){theme_bw() %+replace% theme(
  axis.title.x = element_text(size = 16, color = '#1c1f63', face = 'bold', family = "sans", margin = margin(t = 0, r = 20, b = 0, l = 0)), #x Axis Titles
  axis.title.y = element_text(size = 16, color = '#1c1f63', angle = 90, face = 'bold', family = "sans", margin = margin(t = 0, r = 20, b = 0, l = 0)), #y Axis Titles
  axis.text = element_text(size = 12,  family = "sans", color = 'black'), #Axis text
  panel.border = element_blank(), #remove plot border
  panel.grid.major.x = element_blank(), #no major vertical lines
  panel.grid.major.y = element_line(linetype = 'dotted', size = 1), #dotted major horizontal lines
  panel.grid.minor = element_blank(), #no minor lines
  legend.justification='left', #legend left
  legend.direction='horizontal', #legend to be horizontal
  legend.title = element_blank(), #No legend title
  legend.text = element_text(size = 12, family = "sans",),
  legend.key.size = unit(0.3, "cm"),
  plot.title = element_text(size = 16, color = '#1c1f63', face="bold", margin = margin(b = 10, t=10), hjust=0),
  plot.subtitle = element_text(size = 10, margin = margin(b = 10), hjust=0, color = "grey20"),
  # Customize facet title appearance
  strip.background = element_blank(),  # Set background to white
  strip.text = element_text(face = "bold", family = "sans", size = 12),  # Set font to Arial 12 for facet titles
  plot.title.position = "plot", #align to left of plot not y-axis
  legend.position = "top", #legend position to top
  legend.location = "plot") 
}

bnssgtheme <- theme_bnssg

bnssg_colours <- c(
  `white`           = "#FFFFFF",
  `midnight_blue`   = "#1C1F62",
  `dark_violet`     = "#D091FD",
  `royal_blue`      = "#045EDA",
  `grass_green`     = "#008247",
  `brilliant_purple`= "#8F00B6",
  `vivid_blue`      = "#049FC1",
  `lime_green`      = "#9EF101",
  `teal`            = "#73D4D3")

bnssg_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (bnssg_colours)
  
  bnssg_colours[cols]
}


##Set colour 'names'scheme' names:
bnssg_palettes <- list(
  "main"  = bnssg_cols("midnight_blue", "dark_violet", "royal_blue", "grass_green", "brilliant_purple",
                       "vivid_blue", "lime_green", "teal"),
  "blpkgrn" = c("#1C1F62", "#045EDA", "#0DCFFA", "#D091FD", "#8F00B6", "#008080", "#35FFA4", "#9EF101"),
  "blgrn" = c("#1C1F62", "#045EDA", "#0DCFFA", "#E5FFB5", "#005730"  ),
  "blpk" = c("#1C1F62","#045EDA","#0DCFFA","#D091FD", "#8F00B6"),
  "pkgrn" = c("#D091FD","#8F00B6", "#008080", "#9EF101","#005730"),
  "blue_2" = c("#1C1F62","#0DCFFA"),
  "blue_3" = c("#1C1F62","#045EDA", "#0DCFFA"),
  "blue_4" = c("#1C1F62","#045EDA", "#0DCFFA","#FFFFFF"),
  "blue_5" = c("#1C1F62","#045EDA", "#0DCFFA", "#888BDD","#0346A3"),
  "pink_5" = c("#420272", "#8F00B6", "#F3C4FF", "#E06CFF", "#CD11FF"),
  "pink_3"= c("#420272", "#8F00B6", "#F3C4FF"),
  "green_5" = c("#003B3D", "#008247","#E5FFB5","#00AD5F","#54B7B7"),
  "green_3" = c("#003B3D", "#008247","#E5FFB5"),
  "mapcol" = c("#1C1F62", "#045EDA", "#0DCFFA","#8F00B6","#9EF101","#F3C4FF","#008247"),
  "locality" = c("#1C1F62", "#045EDA", "#0DCFFA","#8F00B6","#F3C4FF","#008247"),
  "uniq" = c("#1C1F62", "#D091FD", "#0DCFFA","grey","#00AD5F")

)

locality_colors <- c(
  "Inner City & East" = "#1C1F62",
  "South Bristol" = "#0DCFFA",
  "North & West" = "#045EDA",
  "Woodspring" = "#008247",
  "Weston Worle & Villages" = "#F3C4FF",
  "South Gloucestershire" = "#8F00B6"  )


##Function so that the colours can be found from the scheme name
bnssg_pal <- function(palette, reverse = FALSE, ...) {
  pal <- bnssg_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for BNSSG colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_bnssg <- function(palette = "main", discrete = FALSE, reverse = FALSE, ...) {
  pal <- bnssg_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("color", paste0("bnssg_", palette), palette = pal, ...)
  } else {
    scale_colour_gradientn(colors = pal(256), ...)
  }
}

#' Fill scale constructor for BNSSG colors
#'
#' @param palette Character name of palette in bnssg_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_bnssg <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bnssg_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("bnssg_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

## collapse rows
collapse_rows_df <- function(df, variable){
  group_var <- enquo(variable)
  df %>%
    group_by(!! group_var) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}

##small number supression
sns_table <- function(df) {
  df <- df %>% mutate(across(where(is.numeric), ~ifelse(. < 10, "<10", scales::comma(.))))
  return(df)
}

sns_table_specific  <-  function(df, ...) {
  cols_to_replace <- c(...)
  df <- df %>%
    mutate(across(all_of(cols_to_replace), ~ifelse(. < 10, "<10", scales::comma(.))))
  return(df)
}

sns_chart <- function(df) {
  df <- df %>%
    mutate(across(where(is.numeric), ~ifelse(. < 10, 0, .)))
  return(df)
}

sns_chart_specific  <-  function(df, ...) {
  cols_to_replace <- c(...)
  df <- df %>%
    mutate(across(all_of(cols_to_replace), ~ifelse(. < 10, 0, .)))
  return(df)
}

suppress_percent <- function(df, condition_column, target_column) {
  df <- df %>%
    mutate(across(all_of(c(condition_column, target_column)), 
                  ~ifelse(is.na(df[[condition_column]]) | df[[condition_column]] < 10 & (. < 10 | is.na(.)), 
                          "<10", format(as.numeric(.), big.mark = ","))))
  
  return(df)
}

#rounding to nearest 10 function
mround <- function(x, multiple) {
  round(x / multiple) * multiple
}

#new ISR function
calculate_isr <- function(data, subgroup_column, events_column, title_column) {
  data <- data %>%
    select(subgroup = !!(sym(subgroup_column)), age_band, sex, people, events = !!sym(events_column))
  
  bnssg <- data %>%
    group_by(age_band, sex) %>%
    summarise(
      bnssg_people = sum(people),
      bnssg_events = sum(events)
    ) %>% ungroup
  
  data <- data %>%
    left_join(bnssg, by =c ("age_band", "sex")) %>%
    mutate(bnssg_rate = (bnssg_events / bnssg_people)*1000) %>%
    mutate(expected = (people*bnssg_rate)/1000) %>%
    filter(!is.na(subgroup))
  
  isr <- data %>%
    group_by(subgroup) %>%
    summarise(
      actual = sum(events),
      population = sum(people),
      expected = sum(expected)
    ) %>% ungroup() %>%
    #mutate(ISR = round((actual / expected) * 100, 2)) %>%
    mutate(ISR = round(ifelse(actual == 0, 0, actual / expected) * 100, 2)) %>%
    mutate(var_isr = actual/expected^2) %>%
    mutate(
      Upper_95_CI = ISR + (((ISR / ifelse(sqrt(actual) == 0, NA, sqrt(actual))) * 1.96) * ifelse(sqrt(actual) == 0, 0, 1)),
      Lower_95_CI = ISR - (((ISR / ifelse(sqrt(actual) == 0, NA, sqrt(actual))) * 1.96) * ifelse(sqrt(actual) == 0, 0, 1))
    ) %>%
    mutate(
      Upper_95_CI = round(Upper_95_CI, 2),
      Lower_95_CI = round(Lower_95_CI, 2)
    ) %>%
    mutate(Significance = if_else(Upper_95_CI > 100 & Lower_95_CI > 100, 'Y',
                                  if_else(Upper_95_CI < 100 & Lower_95_CI < 100, 'Y', 'N')))
  
  isr$subgroup <- factor(isr$subgroup)
  
  axis_title <- title_column
  
  
  isr_plot <- ggplot(isr,aes(reorder(subgroup, ISR), ISR)) + 
    bnssgtheme() +
    geom_bar(aes(fill = Significance), stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI), width = 0.5, colour="black", size= 0.5) + 
    theme(axis.ticks.y = element_blank()) +
    geom_text(aes(label = str_wrap(round(ISR,0), width = 10), 
                  y = Lower_95_CI - 0.3*ISR),
              position = position_dodge(width = 0.9), hjust = 0, size=4, family = "sans") +
    labs(y = "Age / sex ISR (BNSSG = 100)", fill = "Significant") +  # Include fill legend title here
    xlab(axis_title)+
    scale_fill_manual(values = c("N" = "grey", "Y" = "#045EDA")) +
    guides(fill = guide_legend(nrow = 1)) +
    geom_hline(yintercept = 100, linetype = "solid", color = "#1C1F62", linewidth= 0.2) +
    coord_flip() +
    ylim(0, max(isr$Upper_95_CI) + 10) +
    theme(legend.title=element_text(size = 12, family = "sans"))+
    labs(title = paste0(title_column," ISRs,\nBNSSG, ", isr_chart_date),
         subtitle = proj_title) 
  
  isr_plot2 <- ggplot(isr, aes(subgroup, ISR)) + 
    bnssgtheme() +
    geom_bar(aes(fill = Significance), stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI), width = 0.5, colour="black", size= 0.5) + 
    theme(axis.ticks.y = element_blank()) +
    geom_text(aes(label = str_wrap(round(ISR,0), width = 10), 
                  y = Lower_95_CI - 0.3*ISR),
              position = position_dodge(width = 0.9), hjust = 0, size=4, family = "sans") +
    labs( y = "Age / sex ISR (BNSSG = 100)", fill = "Significant") +  # Include fill legend title here
    xlab(axis_title)+
    scale_fill_manual(values = c("N" = "grey", "Y" = "#045EDA")) +
    guides(fill = guide_legend(nrow = 1)) +
    geom_hline(yintercept = 100, linetype = "solid", color = "#1C1F62", linewidth= 0.2) +
    coord_flip() +
    ylim(0, max(isr$Upper_95_CI) + 10) +
    theme(legend.title=element_text(size = 12, family = "sans"))+
    labs(title = paste0(title_column," ISRs,\nBNSSG, ", isr_chart_date),
         subtitle = proj_title) 
  # Assigning names to the plot and dataframe
  plot_name <- paste0(subgroup_column, "_", events_column, "_ISR_plot")
  plot_name2 <- paste0(subgroup_column, "_", events_column, "_ISR_plot2")
  tbl_name <- paste0(subgroup_column, "_", events_column, "_ISR_tbl")
  
  assign(plot_name, isr_plot, envir = .GlobalEnv)
  assign(plot_name2, isr_plot2, envir = .GlobalEnv)
  assign(tbl_name, isr, envir = .GlobalEnv)
  
  return(invisible(NULL))  # Returning invisible NULL to suppress printing of plot and dataframe
}


options(scipen=999)

#load data created in the 'Dataload.R' file:
#load(file.path('02 Data_Images','lsoa_data.RData'))
#load(file.path('02 Data_Images','lsoa_sf.RData'))
load(file.path('02 Data_Images','combined_lsoa_indicators.RData'))
load(file.path('02 Data_Images','activity_joined.RData'))
load(file.path('02 Data_Images','activity.RData'))
load(file.path('02 Data_Images','lsoa_lookup.RData'))
load(file.path('02 Data_Images','green.RData'))
load(file.path('02 Data_Images','green_21.RData'))
load(file.path('02 Data_Images','lsoa_shape.RData'))
load(file.path('02 Data_Images','lsoa_shape_england.RData'))


#specific project titles and dates for text, tables and charts 
proj_title <- "Actice Lives, Data science professional practice 1" #amend as necessary
ltc_field_name <- "Actice Lives"  #replace with field name of LTC of interest
ltc_name <- "Actice Lives" #replace with term for LTC to be used for charts / text etc
ltc_name_sc <- "actice Lives" #small case
data_date <- "01/09/2025" #amend as necessary

chart_date <- "Novenber 2022-23"

script_path <- "C:/Annes offline R/lsoa clustering/01 Scripts"
data_path <- "C:/Annes offline R/lsoa clustering/02 Data_Images"
project_path <- "C:/Annes offline R/lsoa clustering"






#######