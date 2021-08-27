pacman::p_load(
	       tidyverse,
	       ggplot2,
	       gridExtra,
	       cowplot,
	       timetk,
	       ggpubr,
	       imputeTS,
	       lubridate
	       )

# load data -------------------------------------------------------------------

list.files(path = "../data",
	   pattern = "^[0-9]{3}_clean.csv$",
	   full.names = TRUE,
	   recursive = TRUE) %>%
	map_dfr(~read_csv(.)) -> raw_data

# pre-proc --------------------------------------------------------------------

raw_data %>%
	mutate(
	       date = mdy_hms(`MM:DD:YYYY hh:mm:ss`),
	       Mouse = as.factor(Mouse)
	       ) %>%
	select(-`MM:DD:YYYY hh:mm:ss`) -> data

# add lights off period -------------------------------------------------------

add_lights  <- function(date_object, light_init, light_end){
	hour_vector <- hour(date_object)
	logic  <- if_else(
			  hour_vector >= light_init & hour_vector <= light_end,
			  "light",
			  "dark"
			  )
	return(logic)
}

lights_labels <- function(light_init){
	lab <- seq(light_init, 24 + (light_init -1)) %% 24
	return(lab)
}

data %>%
	mutate(lights = add_lights(date, 7, 19)) -> data

# summarise data --------------------------------------------------------------

data %>%
	group_by(h = hour(date), lights) %>%
	summarise(
		  mean_PelletCount = mean(PelletCount),
		  sem_PelletCount = sd(PelletCount) / sqrt(7)
			  ) %>%
	ungroup() -> data_hourly_intake

# plot summarised data --------------------------------------------------------

data_hourly_intake %>%
	mutate(
	       plot_x = factor(h, levels = lights_labels(7))
	       ) %>%
	ggplot(aes(plot_x, mean_PelletCount, group = 1)) +
	geom_errorbar(aes(
			ymin = mean_PelletCount - sem_PelletCount,
			ymax = mean_PelletCount + sem_PelletCount
			), color = "grey70", width = 0.3) +
	geom_line(aes(color = lights), size = 3) +
	geom_point(color = "black") +
	theme_pubr() +
	labs(color = "Light Period") +
	xlab("Hour") +
	ylab("Mean pellet intake per hour") +
	scale_color_manual(labels = c("Dark", "Light"), values = c("gray", "gray90"))

# test area -------------------------------------------------------------------

