pacman::p_load(
	       tidyverse,
	       ggplot2,
	       gridExtra,
	       cowplot,
	       timetk,
	       ggpubr,
	       lubridate,
	       ggforce,
	       hms
	       )

# load data -------------------------------------------------------------------

list.files(path = "../data",
	   pattern = "^[0-9]{3}.csv$",
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
data %>%
	drop_na(data) -> data

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

# mice weights ----------------------------------------------------------------

read_csv("../data/mice_weight.csv") %>%
	mutate(
	       date = dmy(date),
	       mice = as.factor(mice)
	       ) -> weights

weights %>%
	filter(
	       date < "2021-09-06"
	       ) %>%
	group_by(mice) %>%
	summarise(baseline = mean(weight)) %>%
	ungroup() -> weight_baseline

weights %>%
	left_join(weight_baseline, by = "mice") %>%
	filter(date >= "2021-09-06") %>%
	mutate(delta_weight = ((weight - baseline) / ((weight + baseline) / 2)) * 100) -> delta_weight

# add groups to delta weight

delta_weight %>%
	mutate(group = if_else(mice %in% c(234, 235, 236, 245, 265), "Uncertainty", "Certainty")) -> delta_weight

delta_weight %>%
	group_by(group, date) %>%
	summarise(
		  group_delta_weight = mean(delta_weight),
		  sem = sd(delta_weight) / sqrt(n())
		  ) %>%
	ungroup() -> group_delta_weight

delta_weight %>%
	ggplot(aes(date, delta_weight, group = mice, color = mice)) +
	geom_point() +
	geom_line() +
	geom_point(
		   data = group_delta_weight,
		   aes(date, group_delta_weight),
		   inherit.aes = FALSE
		   ) +
	geom_line(
		   data = group_delta_weight,
		   aes(date, group_delta_weight),
		   inherit.aes = FALSE,
		   size = 1.5
		   ) +
	geom_errorbar(
		   data = group_delta_weight,
		   aes(
		       date,
		       ymin = group_delta_weight - sem,
		       ymax = group_delta_weight + sem
		       ),
		      width = 0.3,
		   inherit.aes = FALSE
		   ) +
	facet_wrap(~group) +
	theme_pubr() +
	ylab("% weight delta") +
	xlab("Date")
ggsave("weight.png", width = 14)

# mice intake -----------------------------------------------------------------

data %>%
	mutate(group = if_else(Mouse %in% c(1, 2, 3, 6, 0), "Uncertainty", "Certainty")) %>%
	mutate(day = as.Date(date)) %>%
	filter(day != "2021-09-10") %>%
	group_by(Mouse, day, group) %>%
	summarise(pellets = n()) %>%
	ungroup() -> intake

intake %>%
	filter(day >= "2021-08-30", day < "2021-09-06") %>%
	group_by(Mouse, group) %>%
	summarise(baseline_pellets = mean(pellets)) %>%
	ungroup() -> baseline_intake

intake %>%
	filter(day >= "2021-09-06") %>%
	left_join(baseline_intake %>% select(-group), by = "Mouse") %>%
	mutate(delta_pellets = ((pellets - baseline_pellets) / (pellets + baseline_pellets / 2)) * 100) %>%
	ungroup() -> delta_intake

delta_intake %>%
	filter(day < "2021-09-23", delta_pellets > -50) %>%
	group_by(day, group) %>%
	summarise(
		  mean_pellet = mean(delta_pellets),
		  sem = sd(delta_pellets) / sqrt(n())
		  ) -> group_delta_intake

delta_weight %>%
	filter(date >= "2021-09-13") %>%
	group_by(mice) %>%
	summarise(weight = mean(weight)) %>%
	mutate(Mouse = mice) -> mice_w

write_csv(delta_intake, "delta_intake.csv")

delta_intake %>%
	filter(day != "2021-09-23", delta_pellets > -50) %>%
	mutate(
	       Mouse = as.factor(recode(Mouse,
		`0` = 265,
		`1` = 234,
		`2` = 235,
		`3` = 236,
		`4` = 243,
		`5` = 244,
		`6` = 245,
		`7` = 246,
		`8` = 263,
		`9` = 264
	       ))
	       ) %>%
	ggplot(aes(day, delta_pellets, group = Mouse, color = Mouse)) +
	geom_point() +
	geom_line() +
	geom_point(
		   data = group_delta_intake,
		   aes(day, mean_pellet),
		   inherit.aes = FALSE
		   ) +
	geom_line(
		   data = group_delta_intake,
		   aes(day, mean_pellet),
		   inherit.aes = FALSE,
		   size = 1.5
		   ) +
	geom_errorbar(
		   data = group_delta_intake,
		   aes(day, ymin = mean_pellet - sem, ymax = mean_pellet + sem),
		   inherit.aes = FALSE,
		   width = 0.3
		   ) +
	facet_wrap(~group) +
	theme_pubr() +
	xlab("Date") +
	ylab("Percent delta pellet intake")
ggsave("intake.png", width = 14)

# time between intakes --------------------------------------------------------

# clean data
data %>%
	filter(MotorTurns < 10) %>%
	mutate(
	       hh = as_hms(date),
	       day = day(date)
	       ) -> data_hms

data_hms %>%
	group_by(Mouse, day) %>%
	mutate(IRI = c(0, diff(hh))) -> IRI

IRI %>% 
	mutate(IRI_corr = IRI - Delay) %>%
	filter(IRI_corr > 0) -> IRI

IRI %>%
	filter(IRI < 600, date >= "2021-09-13", date < "2021-09-23") %>%
	mutate(group = if_else(Mouse %in% c(1, 2, 3, 6, 0), "Uncertainty", "Certainty")) -> retrieval

retrieval %>%
	ggplot(aes(IRI_corr, color = group)) +
	geom_density() +
	theme_pubr() +
	xlab("Retrieval time") +
	ylab("Density") +
	labs(color = "Condition")
ggsave("IRI.png", width = 14)

retrieval %>%
	filter(group == "Uncertainty") %>%
	group_by(Delay) %>%
	summarise(mean_iri = mean(IRI_corr), sem = sd(IRI_corr) / sqrt(5)) %>%
	ggplot(aes(Delay, mean_iri, group = 1)) +
	geom_point() +
	geom_errorbar(aes(ymin = mean_iri - sem, ymax = mean_iri + sem), width = 30) +
	geom_line()

retrieval %>%
	filter(group == "Uncertainty") %>%
	group_by(Mouse, Delay) %>%
	summarise(mean_iri = mean(IRI_corr)) %>%
	ggplot(aes(Delay, mean_iri, color = Mouse)) +
	geom_point() +
	geom_line()
ggsave("ex.png", width = 14)

retrieval %>%
	filter(group == "Uncertainty") %>%
	group_by(Mouse, Delay) %>%
	summarise(mean_iri = (mean(IRI_corr))) %>%
	ungroup() %>%
	group_by(Mouse) %>%
	mutate(scaled_iri = scale(mean_iri)) -> per_mice

per_mice %>%
	group_by(Delay) %>%
	summarise(iri_group = mean(scaled_iri), sem = sd(scaled_iri) / sqrt(n())) -> group

per_mice %>%
	mutate(
	       Mouse = as.factor(recode(Mouse,
		`0` = 265,
		`1` = 234,
		`2` = 235,
		`3` = 236,
		`4` = 243,
		`5` = 244,
		`6` = 245,
		`7` = 246,
		`8` = 263,
		`9` = 264
	       ))) %>%
	ggplot(aes(Delay, scaled_iri, color = Mouse)) +
	geom_point() +
	geom_line() +
	geom_point(data = group, inherit.aes = FALSE, aes(Delay, iri_group)) +
	geom_line(data = group, inherit.aes = FALSE, aes(Delay, iri_group), size = 1.5) +
	geom_errorbar(data = group, inherit.aes = FALSE, aes(x = Delay,
							     ymin = iri_group - sem,
							     ymax = iri_group + sem),
		      width = 30, size = 1.5) +
	theme_pubr() +
	ylab("Time from delivery to retrieval") +
	xlab("FED programmed delay")
ggsave("delay.png", width = 14)








 






# delay histograms ------------------------------------------------------------

data %>%
	mutate(
	       day = day(date)
	       ) -> data

data %>%
	group_by(Mouse, day) %>%
	mutate(
	       IRI = diff(c(0, date))
	       ) -> removal_times

removal_times %>%
	filter(IRI < 260, IRI > 0) %>%
	ggplot(aes(IRI)) +
	geom_histogram(aes()) +
	scale_y_continuous() +
	scale_x_discrete(limits = c(30, 60, 120, 240)) +
	theme_pubr(base_size = 22) +
	ylab("Percent") +
	xlab("Removal times") +
	theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"))
ggsave("IRI.png")

# per mice
removal_times %>%
	filter(IRI < 260, IRI > 0) %>%
	ggplot(aes(IRI)) +
	geom_histogram(aes()) +
	scale_x_discrete(limits = c(30, 60, 120, 240)) +
	theme_pubr(base_size = 22) +
	facet_wrap(~Mouse, scales = "free") +
	ylab("Percent") +
	xlab("Removal times") +
	theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"))
ggsave("IRI_permice.png")

# pellets per day -------------------------------------------------------------

data %>%
	group_by(
		 Mouse, day = day(date)
		 ) %>%
	summarise(
		  pellets_per_day = n()
		  ) %>%
	ungroup() -> pellets_per_day

# mean pellet per day, not corrected

pellets_per_day %>%
	group_by(Mouse) %>%
	summarise(mean_intake = mean(pellets_per_day)) %>%
	arrange(desc(mean_intake)) -> pellets_per_day
group_a <- pellets_per_day %>% filter(Mouse %in% c(9, 8, 4, 7, 5))
group_b <- pellets_per_day %>% filter(Mouse %in% c(0, 3, 2, 1, 6))

data_extra <- read_csv("manual_intake.csv") %>%
	mutate(Mouse = as.factor(Mouse))

pellets_per_day <- bind_rows(pellets_per_day %>% filter(day != 30), data_extra)

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
	filter(day >= 23) %>%
	ggplot(aes(day, per_gr)) +
	geom_point() +
	geom_line() +
	geom_hline(yintercept = 100) +
	facet_grid(~Mouse) +
	ylab("100% baseline intake") +
	xlab("Days") +
	scale_x_discrete(limits = c(24, 27, 30)) +
	theme_pubr()
ggsave("baseline_intake.png", width = 14)

# weight plot -----------------------------------------------------------------

weights_all <- read_csv('../data/weights_all.csv') %>%
	mutate(Mouse = as.factor(Mouse))

left_join(weights, weights_all) %>%
	mutate(delta_weight = abs(mean_weight - weight) / ((mean_weight + weight) / 2) * 100) -> weight_delta

weight_delta %>%
	group_by(measurement) %>%
	mutate(global_mean = mean(delta_weight)) %>%
	mutate(sem = sd(delta_weight) / sqrt(10)) %>%
	ungroup() -> weight_delta 

weight_delta %>%
	filter(measurement %in% c(4, 5, 6, 7, 8, 9)) %>%
	ggplot(aes(measurement, delta_weight, color = Mouse)) +
	geom_point() +
	geom_line() +
	geom_line(aes(measurement, global_mean), size = 2, color = "red") +
	geom_errorbar(aes(ymin = global_mean - sem, ymax = global_mean + sem), color = "red", width = 0.3) +
	ylim(c(-15, 15)) +
	scale_x_discrete(limits = c(4, 5, 6, 7, 8, 9)) +
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
	geom_rect(aes(xmin = hms("12:00:00"),
		      xmax = hms("23:59:59"),
		      ymin = -Inf,
		      ymax = Inf
		      ), color = "grey20") +
	geom_point(pch = "|") +
	scale_x_time() +
	theme_pubr() +
	xlab("Hour") +
	ylab("Pellet removal") +
	theme(axis.text.y = element_blank(),
	      axis.text.x = element_text(color = "grey20", size = 5, hjust = .5, vjust = .5, face = "plain"),
	      strip.background = element_rect(fill = 'white'),
	       strip.text.x = element_text(size = 12, color = "black", face = "bold")) +
	facet_grid(day ~ Mouse)
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

