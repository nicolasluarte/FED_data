pacman::p_load(
	       tidyverse,
	       ggplot2,
	       gridExtra,
	       cowplot,
	       timetk,
	       ggpubr,
	       lubridate,
	       ggforce,
	       lme4,
	       lmerTest,
	       ggrepel,
	       sjPlot
	       )

# example

read_csv("PR_SESSIONS.csv") -> df

# groups

df %>%
	mutate(
		group = if_else(ID %in% c(234, 235, 236, 245, 265), "Uncertainty", "Certainty")
	       ) -> df

# ILI

df %>%
	group_by(ID, sensor, group, date) %>%
	mutate(ILI = diff(c(tiempo[1], tiempo))) -> df

# breakpoints

df %>%
	group_by(date, ID, spout) %>%
	mutate(
	       tiempo = tiempo - tiempo[1],
	       time_breaks = cut(tiempo, breaks = c(-Inf, 1800000, Inf), labels = c("First 30 minutes", "Last 30 minutes"))
	) -> df

# ILI histogram

df %>%
	filter(ILI < 250) %>%
	ggplot(aes(ILI, color = group)) +
	geom_density() +
	theme_pubr() +
	theme(text = element_text(size=20)) +
	ylab("Density") +
	xlab("Inter lick interval (ms)") +
	labs(color = "Groups")
ggsave("ILI_histogram.png", width = 14)

df %>%
	filter(ILI < 250) %>%
	ggplot(aes(ILI, color = group)) +
	geom_density() +
	theme_pubr() +
	theme(text = element_text(size=20)) +
	ylab("Density") +
	xlab("Inter lick interval (ms)") +
	labs(color = "Groups") +
	facet_wrap(~time_breaks)
ggsave("ILI_histogram_breaks.png", width = 14)

# meals check negative values

threshold <- 500
df %>%
	group_by(ID, sensor, date, group) %>%
	mutate(
	       cluster = if_else(ILI <= threshold, "cluster", "pause"),
	       cluster_index = as.numeric(as.factor(cluster)) %>%
	       { if_else(. == 1, 1, 0) }
       ) %>%
	mutate(
	       cluster_index = cumsum(cluster_index != lag(cluster_index, default = 0) %>%
				      { if_else(cluster == "pause", 0, .)}) %>% as.factor()
	       ) %>%
	filter(cluster == "cluster", ILI > 0, spout == "sucrose") -> clusters

# meal size

clusters %>%
	filter(spout == "sucrose") %>%
	group_by(date, ID, cluster_index, group, time_breaks) %>%
	summarise(
		  cluster_size = n(),
		  last_time = max(tiempo)
		  ) %>%
	ungroup() %>%
	group_by(date, ID) %>%
	mutate(last_time = scale(last_time)) %>%
	ungroup() -> cluster_size

# check licks within meal vs the rest

df %>%
	filter(spout == "sucrose") %>%
	group_by(ID, session, group) %>%
	summarise(
		  total_licks = max(actividad),
		  total_events = max(evento),
		  ratio_le = total_licks / (total_events + 1)
		  ) -> ratio

ratio %>%
	group_by(group, ID) %>%
	summarise(
		  ratio_le_mean = mean(ratio_le),
		  ) -> ratio_m

ratio %>%
	group_by(group) %>%
	summarise(
		  sem = sd(ratio_le) / sqrt(n()),
		  ratio_mean = mean(ratio_le)
		  ) -> ratio_p

ratio %>%
	group_by(group, ID) %>%
	summarise(
		  sem = sd(ratio_le) / sqrt(n()),
		  ratio_mean = mean(ratio_le)
		  ) -> ratio_i

ratio_p %>%
	ggplot(aes(group, ratio_mean)) +
	geom_col(stat = "identity") +
	geom_errorbar(
		      aes(
			  ymin = ratio_mean - sem,
			  ymax = ratio_mean + sem
			  ),
		      width = 0.3
		      ) +
	geom_point(data = ratio_i) +
	theme_pubr() +
	xlab("Group") +
	ylab("Licks / Events") +
	theme(text = element_text(size=20))
ggsave("ratio_plot.png", width = 14)


tibble(e = e, l = cumsum(l)) %>%
	rename(total_events = e) %>%
	mutate(optimal = l / e) -> optimal

ratio %>%
	left_join(optimal, by = "total_events") %>%
	mutate(opt_diff = (ratio_le - optimal)
	       ) -> ratio2

ratio_mdl <- lmer(opt_diff ~ group  + (1 | ID), data = ratio2) 
summary(ratio_mdl)


l <- c(5,7,11,17,25,35,47,61,77,95,115,137,161,187,215,245)
e <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16) - 1
tibble(e = e, l = cumsum(l)) %>%
	mutate(ratio = l / e) %>%
	ggplot(aes(e, ratio)) +
	geom_line() +
	geom_hline(yintercept = 85, color = "red") +
	geom_hline(yintercept = 60.5, color = "green") +
	geom_text(aes(x = 10, y = 65, label = "Uncertainty")) +
	geom_text(aes(x = 10, y = 90, label = "Certainty")) +
	theme_pubr() +
	xlab("Events") +
	ylab("Licks / Events") +
	theme(text = element_text(size=20))
ggsave("optimal_ratio.png", width = 14)


# ====

cluster_size %>%
	group_by(ID, group, time_breaks, date) %>%
	summarise(
		  sem = sd(cluster_size) / sqrt(n()),
		  cluster_size_mean = mean(cluster_size)
		  ) %>%
	ungroup() -> cluster_size_rm

cluster_size %>%
	group_by(ID, group, time_breaks) %>%
	summarise(
		  sem = sd(cluster_size) / sqrt(n()),
		  cluster_size_mean = mean(cluster_size)
		  ) %>%
	ungroup() -> cluster_size_i

cluster_size %>%
	group_by(group, time_breaks) %>%
	summarise(
		  sem = sd(cluster_size) / sqrt(n()),
		  cluster_size_mean = mean(cluster_size)
		  ) %>%
	ungroup() -> cluster_size_p

cluster_size_p %>%
	ggplot(aes(group, cluster_size_mean, group = time_breaks)) +
	geom_bar(stat = "identity", position = position_dodge(width = 1)) +
	geom_errorbar(aes(ymin = cluster_size_mean - sem,
			  ymax = cluster_size_mean + sem),
		      position = position_dodge(width = 1)) +
	geom_point(data = cluster_size_i,
		   aes(group, cluster_size_mean),
		   position = position_dodge(width = 1)) +
	theme_pubr() +
	theme(text = element_text(size=20)) +
	ylab("Mean meal size") +
	xlab("") +
	labs(color = "Groups") +
	facet_wrap(~time_breaks)
ggsave("meal_size_30min.png", width = 14)

# mixed linear model

null <- lmer(cluster_size ~ 1 + (1 | ID), data = cluster_size)
meal_size_model <- lmer(cluster_size ~ group * last_time + (1 | ID), data = cluster_size)
summary(meal_size_model)

anova(null, meal_size_model)

plot_model(meal_size_model,
	axis.labels = c("Group:Time", "Time within session", "Group (uncertainty)"),
	show.values = TRUE,
	show.p = TRUE,
	title = "",
	vline.color = "blue",
	width = 0.5
	) +
	theme_pubr() +
	theme(text = element_text(size=20))
ggsave("mdl_meal_size.png", width = 14)

options(browser = "firefox")
tab_model(meal_size_model)

# cdf

cluster_size %>%
	ggplot(aes(cluster_size, color = group)) +
	geom_density() +
	theme_pubr() +
	theme(text = element_text(size=20)) +
	ylab("Density") +
	xlab("Meal size n") +
	labs(color = "Groups")
ggsave("meal_size_density.png", width = 14)

cluster_size %>%
	ggplot(aes(cluster_size, color = group)) +
	geom_density() +
	theme_pubr() +
	theme(text = element_text(size=20)) +
	ylab("Density") +
	xlab("Meal size n") +
	labs(color = "Groups") +
	facet_wrap(~time_breaks)
ggsave("meal_size_density_30min.png", width = 14)

# number of clusters

clusters %>%
	filter(spout == "sucrose") %>%
	group_by(ID, group, date, time_breaks) %>%
	summarise(
		  cluster_number = max(as.numeric(cluster_index)),
		  last_time = max(tiempo)
		  ) %>%
	ungroup() %>%
	group_by(date, ID) %>%
	mutate(last_time = scale(last_time)) %>%
	ungroup() -> cluster_number

cluster_number %>%
	group_by(ID, group, time_breaks) %>%
	summarise(
		  sem = sd(cluster_number) / sqrt(n()),
		  cluster_number_mean = mean(cluster_number)
		  ) %>%
	ungroup() -> cluster_number_i

cluster_number %>%
	group_by(group, time_breaks) %>%
	summarise(
		  sem = sd(cluster_number) / sqrt(n()),
		  cluster_number_mean = mean(cluster_number)
		  ) %>%
	ungroup() -> cluster_number_p

cluster_number_p %>%
	ggplot(aes(group, cluster_number_mean, group = time_breaks)) +
	geom_bar(stat = "identity", position = position_dodge(width = 1)) +
	geom_errorbar(aes(ymin = cluster_number_mean - sem,
			  ymax = cluster_number_mean + sem),
		      position = position_dodge(width = 1)) +
	geom_point(data = cluster_number_i,
		   aes(group, cluster_number_mean),
		   position = position_dodge(width = 1)) +
	theme_pubr() +
	theme(text = element_text(size=20)) +
	ylab("Mean meal number") +
	xlab("") +
	labs(color = "Groups") +
	facet_wrap(~time_breaks)
ggsave("meal_number_30min.png", width = 14)

meal_number_model <- lmer(cluster_number ~ group * last_time + (1 | ID), data = cluster_number)
summary(meal_number_model)

plot_model(meal_number_model,
	axis.labels = c("Group:Time", "Time within session", "Group (uncertainty)"),
	show.values = TRUE,
	show.p = TRUE,
	title = "",
	vline.color = "blue",
	width = 0.5
	) +
	theme_pubr() +
	theme(text = element_text(size=20))
ggsave("mdl_meal_number.png", width = 14)

# meal time

clusters %>%
	filter(spout == "sucrose") %>%
	group_by(date, ID, cluster_index, group) %>%
	summarise(
		  cluster_time = sum(ILI),
		  ) %>%
	ungroup() -> cluster_time

cluster_time %>%
	group_by(ID, group) %>%
	summarise(
		  sem = sd(cluster_time) / sqrt(n()),
		  cluster_time_mean = mean(cluster_time)
		  ) %>%
	ungroup() -> cluster_time_i

cluster_time %>%
	group_by(group) %>%
	summarise(
		  sem = sd(cluster_time) / sqrt(n()),
		  cluster_time_mean = mean(cluster_time)
		  ) %>%
	ungroup()

# lick frequency

df %>%
	mutate(interval = as.numeric(as.factor(tiempo %/% 1000))) %>%
	group_by(ID, date, interval, group, time_breaks) %>%
	summarise(
		  hz = n()
		  ) -> hz

hz %>%
	group_by(group, time_breaks) %>%
	summarise(
		  hz_mean = mean(hz)
		  )



# mean inter meal time

# where are the meals?

clusters %>%
	group_by(date) %>%
	mutate(tiempo = tiempo - tiempo[1], tick = as.factor(1)) %>%
	ungroup() %>%
	group_by(date, ID) %>%
	mutate(scale_act = scale(actividad)) %>%
	ggplot(aes(tiempo, scale_act, color = group, group = date)) +
	geom_line() +
	facet_wrap(~ID)




# load data -------------------------------------------------------------------

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
	filter(ID != 246) %>%
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
	group_by(date, ID, sensor, evento) %>%
	mutate(
	       time_from_event = tiempo - min(tiempo),
	       check = min(init_ms),
	       time_out = time_from_event <= 20000
		       ) -> lickometer_post


lickometer_post %>%
	filter(type == "PR", spout == "sucrose", ID == 236) %>%
	select(ID, actividad, evento, time_out, time_from_event) %>%
	View


lickometer_post %>% filter(type == "PR") %>%
	group_by(date, ID, evento, spout) %>%
	summarise(intime = sum(time_out), total = n()) %>%
	ungroup() %>%
	mutate(
	       real_intime = case_when(
				       evento == 0 ~ 5,
				       evento == 1 ~ 7,
				       evento == 2 ~ 11,
				       evento == 3 ~ 17,
				       evento == 4 ~ 25,
				       evento == 5 ~ 35,
				       evento == 6 ~ 47,
				       evento == 7 ~ 61,
				       evento == 8 ~ 77,
				       evento == 9 ~ 95,
				       evento == 10 ~ 115,
				       evento == 11 ~ 137,
				       evento == 12 ~ 161,
				       evento == 13 ~ 187,
				       evento == 14 ~ 215,
				       evento == 15 ~ 245
				       ),
	       ff = total - real_intime,
	       mark = if_else(ff < 0, "del", "keep")
	       ) %>% 
	select(date, ID, evento, spout, mark) -> test

lickometer_post %>%
	left_join(test, by = c("date", "ID", "evento", "spout")) %>%
	filter(type == "PR") -> clean_data

clean_data %>% dim()

clean_data %>% 
	filter(mark == "keep") -> valid

valid %>%
	group_by(date, ID, spout) %>%
	mutate(actividad = 1:n()) -> licks_fix

licks_fix %>%
	group_by(date, ID, spout, evento) %>%
	mutate(idx = factor(evento)) %>%
	mutate(evento = (as.numeric(idx))) -> events_fix

events_fix %>%
	select(
	       ID,
	       sensor,
	       tiempo,
	       actividad,
	       evento,
	       exito,
	       session
	       ) %>%
	write_csv(., "PR_SESSIONS.csv")






events_fix %>%
	group_by(date, ID, sensor, evento) %>%
	mutate(
	       time_from_event = tiempo - min(tiempo),
	       check = min(init_ms),
	       time_out = time_from_event <= 20000
		       ) -> lickometer_post_2


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
	ungroup() 



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
	geom_label_repel(data = time_out_p, inherit.aes = FALSE, aes(group, ratio_time_out, label = ID)) +
	facet_wrap(~type, scales = "free") +
	theme_pubr() +
	ggsci::scale_fill_npg()
ggsave("ratio.png", width = 14)

lickometer_post %>%
	group_by(date, ID, type, group) %>%
	summarise(
		  ratio_time_out = sum(time_out) / sum(!time_out)
		  ) %>%
	ungroup() %>%
	group_by(date, type, group) %>%
	summarise(
		  ratio_time_out_m = mean(ratio_time_out),
		  sem = sd(ratio_time_out) / sqrt(n())
		  ) %>%
	ungroup() -> time_out_date

time_out_date %>%
	filter(type == "PR") %>%
	ggplot(aes(date, ratio_time_out_m,
		   ymin = ratio_time_out_m - sem,
		   ymax = ratio_time_out_m + sem,
		   color = group
		   )) +
	geom_line() +
	geom_point() +
	geom_errorbar(width = 0.3) +
	theme_pubr()
ggsave("ratio_days.png", width = 14)


lickometer_post %>%
	group_by(date, ID, sensor, type) %>%
	summarise(
		  events = max(evento)
	)
