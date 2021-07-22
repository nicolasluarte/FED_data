library(tidyverse)
library(ggplot2)

# load data -------------------------------------------------------------------

list.files(path = "../data", full.names = TRUE) %>%
	map_dfr(~read_csv(.)) -> raw_data

# preprocess ------------------------------------------------------------------

raw_data %>%
	mutate(date = lubridate::mdy_hms(`MM:DD:YYYY hh:mm:ss`)) %>%
	select(-`MM:DD:YYYY hh:mm:ss`) %>%
	mutate(Mouse = as.factor(Mouse)) -> data

# fix pellet count

data %>%
	group_by(Mouse) %>%
	mutate(PelletCount = seq(1, n())) -> data

# summarise data
 

# plots -----------------------------------------------------------------------

data %>%
	ggplot(aes(date, PelletCount)) +
	geom_point() +
	facet_grid(~Mouse)




