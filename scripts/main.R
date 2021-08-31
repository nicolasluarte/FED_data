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
	   pattern = "^[0-9]{3}_all.csv$",
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
			  "dark",
			  "light"
			  )
	return(logic)
}

lights_labels <- function(light_init){
	lab <- seq(light_init, 24 + (light_init -1)) %% 24
	return(lab)
}

data %>%
	mutate(lights = add_lights(date, 12, 23)) -> data

# pellets per day -------------------------------------------------------------

data %>%
	group_by(
		 Mouse, day = day(date)
		 ) %>%
	summarise(
		  pellets_per_day = n()
		  ) %>%
	ungroup() -> pellets_per_day

pellets_per_day %>%
	filter(day != 30) %>%
	ggplot(aes(day, pellets_per_day)) +
	geom_point() +
	geom_line() +
	facet_grid(~Mouse)

# import weights --------------------------------------------------------------

weights_raw <- read_csv('../data/weights.csv')
weights_raw %>%
	mutate(Mouse = as.factor(Mouse)) %>%
	group_by(Mouse) %>%
	summarise(
		  mean_weight = mean(weight)
		  ) -> weights

left_join(weights, pellets_per_day) -> pellets_per_day_weight

pellets_per_day_weight %>%
	filter(Mouse %in% c(1, 2, 3, 4, 5, 6, 7)) %>%
	filter(day %in% c(16, 17, 18, 19, 20)) %>%
	group_by(Mouse) %>%
	summarise(
		  baseline = mean(pellets_per_day / mean_weight)
		  ) -> baseline_a

pellets_per_day_weight %>%
	filter(Mouse %in% c(8, 9, 0)) %>%
	filter(day %in% c(23, 24, 25, 26, 27)) %>%
	group_by(Mouse) %>%
	summarise(
		  baseline = mean(pellets_per_day / mean_weight)
		  ) -> baseline_b

bind_rows(baseline_a, baseline_b) -> baseline

left_join(baseline, pellets_per_day_weight) -> pellets_baseline

pellets_baseline %>%
	mutate(per_gr = ((pellets_per_day / mean_weight) / baseline) * 100 ) -> pellets_baseline

pellets_baseline %>%
	filter(day >= 23, day != 30) %>%
	ggplot(aes(day, per_gr)) +
	geom_point() +
	geom_line() +
	geom_hline(yintercept = 100) +
	facet_grid(~Mouse) +
	ylab("100% baseline intake") +
	xlab("Days") +
	theme_pubr()
ggsave("baseline_intake.png", width = 14)

# weight plot -----------------------------------------------------------------

weights_all <- read_csv('../data/weights_all.csv') %>%
	mutate(Mouse = as.factor(Mouse))

left_join(weights, weights_all) %>%
	mutate(delta_weight = abs(mean_weight - weight) / ((mean_weight + weight) / 2) * 100) -> weight_delta

weight_delta %>%
	filter(measurement %in% c(4, 5, 6)) %>%
	ggplot(aes(measurement, delta_weight, color = Mouse)) +
	geom_point() +
	geom_line() +
	ylim(c(-15, 15)) +
	scale_x_discrete(limits = c(4, 5, 6)) +
	xlab("Days after baseline") +
	ylab("Percent delta weight") +
	theme_pubr()
ggsave("baseline_weight.png", width = 14)

# intake raster ---------------------------------------------------------------

data %>%
	mutate(
	       day = day(date),
	       hh = format(as.POSIXct(strptime(date, "%Y-%m-%d %H:%M:%S", tz = "")), format = "%H:%M:%S"),
	       hh = hms(hh),
	       intake = "1"
	       ) -> tt

tt %>%
	filter(day >= 23, day != 30) %>%
	ggplot(aes(hh, as.factor(intake))) +
	geom_point(pch = "|") +
	geom_rect(aes(xmin = hms("12:00:00"),
		      xmax = hms("23:59:59"),
		      ymin = -Inf,
		      ymax = Inf
		      ), alpha = 0.005, color = "grey20") +
	scale_x_time() +
	theme_pubr() +
	xlab("Hour") +
	ylab("Pellet removal") +
	theme(axis.text.y = element_blank(),
	      axis.text.x = element_text(color = "grey20", size = 8, hjust = .5, vjust = .5, face = "plain"),
	      strip.background = element_rect(fill = 'white'),
	       strip.text.x = element_text(size = 12, color = "black", face = "bold")) +
	facet_wrap(~Mouse, ncol = 2)
ggsave("raster.png", width = 14, height = 5)

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

