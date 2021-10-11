pacman::p_load(
	       tidyverse,
	       ggplot2,
	       gridExtra,
	       cowplot,
	       timetk,
	       ggpubr,
	       lubridate,
	       ggforce,
	       hms,
	       imputeTS,
	       chron
	       )

# load data -------------------------------------------------------------------

list.files(path = "../data/FR_FED_OCTUBRE_2021",
	   pattern = "*.csv",
	   full.names = TRUE,
	   recursive = TRUE) %>%
	set_names() %>%
	map_dfr(read_csv, .id = "file_name")  %>%
	mutate(date = str_extract(string = file_name, pattern = "(?<=1/)(.*)(?=_)"),
	       hour = str_extract(string = file_name, pattern = "(?<=[0-9]_)(.*)(?=.csv)"),
		date = ymd(date)) -> raw_data

raw_data %>%
	mutate(
	       ID = as.factor(ID),
	       session = (day(date)),
	       spout = case_when(
				 session %% 2 == 0 & sensor == 0 ~ "water",
				 session %% 2 == 0 & sensor == 1 ~ "sucrose",
				 session %% 2 != 0 & sensor == 0 ~ "sucrose",
				 session %% 2 != 0 & sensor == 1 ~ "water"
				 ),
	       group = if_else(ID %in% c(234, 235, 236, 245, 265), "Uncertainty", "Certainty")
	       ) -> data

data %>%
	group_by(ID, session, spout, group) %>%
	summarise(licks = max(actividad), events = max(evento)) -> dat_summ

dat_summ %>%
	group_by(session, spout, group) %>%
	summarise(licks_m = mean(licks), events_m = mean(events),
	seml = sd(licks) / sqrt(5), seme = sd(events) / sqrt(5)) -> dat_summ_group

dat_summ %>%
	ggplot(aes(session, licks, color = ID, group = ID)) +
	geom_point() +
	geom_line() +
	geom_line(inherit.aes = FALSE, data = dat_summ_group, aes(session, licks_m)) +
	geom_point(inherit.aes = FALSE, data = dat_summ_group, aes(session, licks_m)) +
	geom_errorbar(inherit.aes = FALSE, data = dat_summ_group, aes(session, licks_m,
								      ymin = licks_m - seml,
								      ymax = licks_m + seml)) +
	facet_wrap(group~spout) +
	theme_pubr()
ggsave("licks.png", width = 14)

dat_summ %>%
	ggplot(aes(session, events, color = ID, group = ID)) +
	geom_point() +
	geom_line() +
	geom_line(inherit.aes = FALSE, data = dat_summ_group, aes(session, events_m)) +
	geom_point(inherit.aes = FALSE, data = dat_summ_group, aes(session, events_m)) +
	geom_errorbar(inherit.aes = FALSE, data = dat_summ_group, aes(session, events_m,
								      ymin = events_m - seme,
								      ymax = events_m + seme)) +
	facet_wrap(group~spout) +
	theme_pubr()
ggsave("events.png", width = 14)


