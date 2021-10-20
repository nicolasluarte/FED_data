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
	       chron,
	       lme4,
	       lmerTest
	       )

# load data -------------------------------------------------------------------

list.files(path = "../data/lickometer_data",
	   pattern = "*.csv",
	   full.names = TRUE,
	   recursive = TRUE) %>%
	set_names() %>%
	map_dfr(read_csv, .id = "file_name")  %>%
	mutate(date = str_extract(string = file_name, pattern = "2021[0-9]{4}"),
	       hour = str_extract(string = file_name, pattern = "(?<=[0-9]_)(.*)(?=.csv)"),
		date = ymd(date)) -> raw_data

raw_data %>%
	mutate(
	       type = as.factor(if_else(date >= "2021-10-11", "PR", "FR")),
	       ID = as.factor(ID),
	       session = (day(date)),
	       session = session - min(session),
	       spout = case_when(
				 session %% 2 == 0 & sensor == 0 ~ "water",
				 session %% 2 == 0 & sensor == 1 ~ "sucrose",
				 session %% 2 != 0 & sensor == 0 ~ "sucrose",
				 session %% 2 != 0 & sensor == 1 ~ "water"
				 ),
	       group = if_else(ID %in% c(234, 235, 236, 245, 265), "Uncertainty", "Certainty")
	       ) -> data

data %>%
	group_by(ID, session, spout, group, type) %>%
	summarise(licks = max(actividad), events = max(evento)) -> dat_summ

dat_summ %>%
	group_by(session, spout, group, type) %>%
	summarise(licks_m = mean(licks), events_m = mean(events),
	seml = sd(licks) / sqrt(5), seme = sd(events) / sqrt(5)) -> dat_summ_group

dat_summ_group %>%
	filter(spout == "sucrose") %>%
	ggplot(aes(session, licks_m, ymin = licks_m - seml, ymax = licks_m + seml, color = group)) +
	geom_line() +
	geom_point() +
	geom_errorbar() +
	facet_wrap(~type, scales = "free_x") +
	mytheme
ggsave("licks.png", width = 14)

dat_summ_group %>%
	filter(spout == "sucrose") %>%
	ggplot(aes(session, events_m, ymin = events_m - seme, ymax = events_m + seme, color = group)) +
	geom_line() +
	geom_point() +
	geom_errorbar() +
	facet_wrap(~type, scales = "free_x") +
	mytheme
ggsave("events.png", width = 14)

mytheme <- theme_pubr() + theme(
  panel.margin = unit(0, "lines"),
  panel.border = element_rect(colour = rgb(1.0, 0, 0, 0.5), fill=NA, size=1)
)

mdl_fr <- lmer(licks ~ session * group + (1 | ID), data = dat_summ %>% filter(spout == "sucrose", type == "FR"))
anova(mdl_fr)
summary(mdl_fr)

mdl_fr_e <- lmer(events ~ session * group + (1 | ID), data = dat_summ %>% filter(spout == "sucrose", type == "FR"))
anova(mdl_fr_e)
summary(mdl_fr_e)

mdl_pr <- lmer(licks ~ session * group + (1 | ID), data = dat_summ %>% filter(spout == "sucrose", type == "PR"))
anova(mdl_pr)
summary(mdl_pr)

mdl_pr_e <- lmer(events ~ session * group + (1 | ID), data = dat_summ %>% filter(spout == "sucrose", type == "PR"))
anova(mdl_pr_e)
summary(mdl_pr_e)

#- licke per time block

data %>%
	mutate(time = gsub("(.{2})", "\\1 ", hour) %>%
	gsub(" ", ":", .) %>%
	gsub('.{1}$', '', .),
	time = lubridate::hms(time)
	) -> lickometer

lickometer %>%
	group_by(date, ID, sensor) %>%
	mutate(
	       init_ms = tiempo - min(tiempo),
	       block = as.factor(if_else(init_ms >= 30 * 60000, "2", "1"))
	       ) -> lickometer

lickometer %>%
	filter(date == "2021-10-10" | date == "2021-10-14", spout == "sucrose") %>%
	group_by(group, type, block, ID) %>%
	summarise(
		  licks = n(),
		  events = sum(evento > 0)
		  ) %>%
	ungroup() %>%
	group_by(group, type, block) %>%
	summarise(
		  mlicks = mean(licks),
		  mevents = mean(events),
		  seml = sd(licks) / sqrt(n()),
		  seme = sd(events) / sqrt(n())
		  ) %>%
	ggplot(aes(block, mlicks, color = block, ymin = mlicks - seml, ymax = mlicks + seml)) +
	geom_col() +
	geom_errorbar() +
	facet_grid(group~type) +
	xlab("30 minutes") +
	ylab("Mean licks") +
	theme_pubr()
ggsave("licks_30.png", width = 14)

lickometer %>%
	filter(date == "2021-10-10" | date == "2021-10-14", spout == "sucrose") %>%
	group_by(group, type, block, ID) %>%
	summarise(
		  licks = n(),
		  events = sum(diff(evento))
		  ) %>%
	ungroup() %>%
	group_by(group, type, block) %>%
	summarise(
		  mlicks = mean(licks),
		  mevents = mean(events),
		  seml = sd(licks) / sqrt(n()),
		  seme = sd(events) / sqrt(n())
		  ) %>%
	ggplot(aes(block, mevents, color = block, ymin = mevents - seme, ymax = mevents + seme)) +
	geom_col() +
	geom_errorbar() +
	facet_grid(group~type) +
	xlab("30 minutes") +
	ylab("Mean events") +
	theme_pubr()
ggsave("events_30.png", width = 14)


dat_summ %>%
	ggplot(aes(session, licks, color = ID, group = ID)) +
	geom_point() +
	geom_line() +
	geom_line(inherit.aes = FALSE, data = dat_summ_group, aes(session, licks_m)) +
	geom_point(inherit.aes = FALSE, data = dat_summ_group, aes(session, licks_m)) +
	geom_errorbar(inherit.aes = FALSE, data = dat_summ_group, aes(session, licks_m,
								      ymin = licks_m - seml,
								      ymax = licks_m + seml)) +
	facet_wrap(~spout) +
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

# licks in and out of time ----------------------------------------------------


# calculate time from last event
data %>%
	mutate(time = gsub("(.{2})", "\\1 ", hour) %>%
	gsub(" ", ":", .) %>%
	gsub('.{1}$', '', .),
	time = lubridate::hms(time)
	) -> lickometer
lickometer %>%
	group_by(date, ID) %>%
	mutate(
	       init_ms = tiempo - tiempo[1],
	       block = as.factor(if_else(init_ms >= 30 * 60000, "2", "1"))
	       ) -> lickometer

lickometer %>%
	filter(spout == "sucrose") %>%
	group_by(date, ID, sensor, evento) %>%
	mutate(
	       time_from_event = tiempo - min(tiempo),
	       check = min(init_ms),
	       time_out = time_from_event <= 20000 & time_from_event >= 1000
		       ) -> lickometer_post

lickometer_post %>%
	group_by(ID, type, group) %>%
	summarise(
		  ratio_time_out = sum(time_out) / sum(!time_out)
		  ) %>%
	ungroup() %>%
	group_by(type, group) %>%
	summarise(
		  ratio_time_out_m = mean(ratio_time_out),
		  sem = sd(ratio_time_out) / sqrt(n())
		  ) %>%
	ungroup() -> time_out

lickometer_post %>%
	group_by(ID, type, group) %>%
	summarise(
		  ratio_time_out = sum(time_out) / sum(!time_out)
		  ) %>%
	ungroup() %>%
	ungroup() -> time_out_p

time_out %>%
	ggplot(aes(group, ratio_time_out_m,
		   ymin = ratio_time_out_m - sem,
		   ymax = ratio_time_out_m + sem,
		   fill = group
		   )) +
	geom_col() +
	geom_errorbar(width = 0.3) +
	geom_point(data = time_out_p, inherit.aes = FALSE, aes(group, ratio_time_out)) +
	geom_label(data = time_out_p, inherit.aes = FALSE, aes(group, ratio_time_out, label = ID)) +
	facet_wrap(~type, scales = "free") +
	theme_pubr() +
	ggsci::scale_fill_npg()

