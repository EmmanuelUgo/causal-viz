## Loading libraries

library(tidyverse)
library(showtext)

font_add_google(name = "quicksand",  family = "quicksand")

## Generating data from the original plot

tbl <- tibble(x_axis = c(37.42, 29.11, 28.79, 27.59, 26.47, 23.00, 21.34, 20.54, 20.22, 19.93),
       y_axis = c("Parker and Lynch", "SGS Consulting", "CFS", "TekWissen", "Motion Recruitment",
                  "Talent Solutions", "Kforce", "True Search", "Ultimate Staffing", "Solvo Global USA")) %>% 
  rowid_to_column() %>% 
  mutate(col = case_when(
    rowid %in% seq(1,10,2) ~ "orange",
    TRUE ~ "purple"
  ))

## Arranging the location of the text (inside the bar plots)
place_company <- tibble(x = c(7,7,2.7,4.8,8.2,7,3.3,5.5,7.2,7), y = tbl$y_axis)

## Creating a structure for the percentages, and rank change
perc <- tibble(x = 45, x_new = 55,
  label = c("37.42%", "29.11%", "28.79%", "27.59%", "26.47%", "23.00%", "21.34%",
            "20.54%", "20.22%", "19.93%"),
  label_new = c("0",paste0(sprintf('\u2191'),41),paste0(sprintf('\u2193'),1),"NEW",
                paste0(sprintf('\u2193'),2),paste0(sprintf('\u2193'),1),"NEW","NEW",
                paste0(sprintf('\u2191'),5),"NEW"),
  y = tbl$y_axis,
  col = tbl$col,
  col_new = c("black","green","red","black","red","red","black","black","green","black"))

## Structure for the ranks
ranks <- tibble(rank = 1:10) %>% 
  mutate(rank = glue::glue("{rank}th"),
         rank = as.character(rank),
         rank = case_when(
           rank == "1th" ~ "1st",
           rank == "2th" ~ "2nd",
           rank == "3th" ~ "3rd",
           TRUE ~ rank
         )) %>% 
  rowid_to_column() %>% 
  mutate(x = -4, y = tbl$y_axis) 

## Setting color scheme
cols <- c("orange" = "#DB4F09", "purple" = "#0F0A2EEE", 
          "green" = "#069E2D", "black" = "#050401", "red" = "#C81D25" )


showtext_auto()

ggplot() +
  
  geom_segment(data = tbl, aes(x = 0, xend = x_axis,
                               y =  fct_reorder(y_axis, x_axis), yend =  fct_reorder(y_axis, x_axis),
                               col = col, group = 1), lineend="round", lwd= 8) +
  geom_text(data = place_company, aes(x, y, label = y, family = "quicksand", fontface = "bold"), col = "white", size = 3) +
  geom_text(data = perc, aes(x, y, label = label, col = col,  family = "quicksand", fontface = "bold"), size = 3.5) +
  geom_text(data = ranks, aes(x, y, label = rank,  family = "quicksand"), size = 3) +
  geom_text(data = perc, aes(x_new, y, label = label_new, col = col_new), size = 3.5) +
  
  
  annotate("text", x = -2.8, y = 10.75, label = "Rank", size = 2.5, family = "quicksand" ) +
  annotate("text", x = 25, y = 11.5, label = "February 2022", size = 4.2, col = cols[1],  family = "quicksand" ) +
  annotate("text", x = 43, y = 11, label = "Employee\nShares", size = 2.5,  family = "quicksand", hjust = 0 ) +
  annotate("text", x = 53, y = 11, label = "Rank\nChange", size = 2.5,  family = "quicksand", hjust = 0) +
  annotate("segment", x = 51, xend = 51, y = 0.7, yend = 10.3,  color = "white") +
  
  geom_hline(yintercept = 1.5, color = "white") +
  geom_hline(yintercept = 2.5,  color = "white") +
  geom_hline(yintercept = 3.5,  color = "white") +
  geom_hline(yintercept = 4.5,  color = "white") +
  geom_hline(yintercept = 5.5,  color = "white") +
  geom_hline(yintercept = 6.5,  color = "white") +
  geom_hline(yintercept = 7.5,  color = "white") +
  geom_hline(yintercept = 8.5,  color = "white") +
  geom_hline(yintercept = 9.5,  color = "white") +

  
  expand_limits(y = c(0, 12)) +
  scale_x_continuous(limits = c(-4, 60)) +
  scale_color_manual(values = cols, aesthetics = "col") +
  
  
  labs(
    title = "\nThe USA's Most Active\nStaffing & Recruiting Professionals on Social",
    caption = "Companies with 500+ employees on LinkedIn",
    x = NULL,
    y = NULL
  ) +
  
  theme(
    legend.position = "null",
    text = element_text(family = "quicksand"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic", family = "serif", size = 7.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "#EDE8E8A5")) 

