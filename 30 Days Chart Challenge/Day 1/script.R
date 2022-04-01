
library(tidyverse)


tribble(~action, ~time,
        ###############
        "Getting hyped about the challange", 0.7,
        "Searching for inspiration", 0.28,
        "Creating this random viz", 0.02) %>% 
  mutate(action = factor(action,
                         levels = c("Getting hyped about the challange",
                                    "Searching for inspiration",
                                    "Creating this random viz"))) %>% 
  ggplot(aes(x = "", y = time, fill = action)) +
  geom_col(color = "white") +
  coord_polar(theta = 'y') +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "quicksand")
  )+
  labs(
    fill = NULL,
    title = "Today in a Nutshell"
  ) +
  
  scale_fill_viridis_d(option = "E")
  
  scale_fill_manual(
    values = c("#8cb369","#354f52","#90be6d")
  )
  

         