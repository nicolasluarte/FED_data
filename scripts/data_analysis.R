library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(timetk)
library(ggpubr)
library(imputeTS)

# load data -------------------------------------------------------------------

list.files(path = "../data",
	   pattern = "^[0-9]{3}_clean.csv$",
	   full.names = TRUE,
	   recursive = TRUE) %>%
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

# add pellet count
data %>%
	mutate(freq = 1) -> data

# add day
data %>%
	mutate(day = lubridate::day(date)) -> data

# add hour minute second
data %>%
	mutate(hour = lubridate::hour(date) +
			lubridate::minute(date) / 60 +
			lubridate::second(date) / 3600
		) -> data

# add light/dark
data %>%
	mutate(lights = if_else(
				hour >= 19 | hour <= 7,
				"dark",
				"light"
				)) -> data

# intake per day
data %>%
	filter(day %in% c(26, 27, 28, 29, 30, 31, 1, 2, 3, 4, 5, 6)) %>%
	group_by(Mouse, day) %>%
	summarise(daily_pellet = n()) %>%
	mutate(day = seq(1, n())) %>%
	mutate(fail = if_else(Mouse %in% c(244, 246, 234, 235) & day %in% c(6, 7) | Mouse == 246 & day == 12, "FAIL", "GOOD")) %>%
	ungroup() -> daily_pellet
daily_pellet$daily_pellet[daily_pellet$daily_pellet < 5] <- NA
daily_pellet$daily_pellet[daily_pellet$Mouse %in% c(234, 243) & daily_pellet$day == 2] <- NA
daily_pellet$daily_pellet[daily_pellet$Mouse %in% c(234) & daily_pellet$day == 11] <- NA
daily_pellet$daily_pellet[daily_pellet$Mouse %in% c(243) & daily_pellet$day == 3] <- NA
daily_pellet %>%
	group_by(Mouse) %>%
	mutate(pellet_int = round(na_interpolation(daily_pellet, option = "spline"))) -> daily_pellet
daily_pellet$pellet_int[daily_pellet$Mouse %in% c(234) & daily_pellet$day == 3] <- 75
daily_pellet$pellet_int[daily_pellet$Mouse %in% c(234) & daily_pellet$day == 12] <- 60
daily_pellet$pellet_int[daily_pellet$Mouse %in% c(234) & daily_pellet$day == 8] <- 30

# plot daily intake
daily_pellet %>%
	ggplot(aes(day, pellet_int, color = Mouse)) +
	geom_point(aes(shape = as.factor(fail)), size = 5) +
	geom_line() +
	facet_grid(~Mouse) +
	theme_pubr() +
	annotate("rect", xmin = 6, xmax = 7, ymin = 0, ymax = Inf,
		 fill = "red",
		 colour = "red",
		 alpha = 0.2) +
	ylab("Daily pellet intake") +
	xlab("Days") +
	scale_shape(name = "Status") +
	scale_shape_manual(values = c(4, 1)) +
	scale_x_discrete(limits = 1:12) +
	ggtitle("Ingesta diaria 26-Jul a 6-Ago")
ggsave("daily_intake.png")



# add hour bins
data %>%
	mutate(hour_bin = cut(hour, 24)) -> data

# pellet sum per bin
data %>%
	filter(day %in% c(27, 28, 29)) %>%
	group_by(hour_bin, day) %>%
	summarise(pellet_intake = sum(freq)) %>%
	ungroup() %>%
	group_by(hour_bin) %>%
	summarise(pellet_mean = mean(pellet_intake), pellet_sem = sd(pellet_intake) / sqrt(n())) %>%
	mutate(hour_bin = as.numeric(hour_bin)) %>%
	ungroup() -> pellet_intake

# pellet light/dark
data %>%
	filter(day %in% c(27, 28, 29)) %>%
	group_by(lights, day) %>%
	summarise(pellet_intake = sum(freq)) %>%
	ungroup() %>%
	group_by(lights) %>%
	summarise(pellet_mean = mean(pellet_intake), pellet_sem = sd(pellet_intake) / sqrt(n()))

data %>%
	filter(hour_bin != 12, hour_bin != 13) %>%
	filter(day %in% c(27, 28, 29)) %>%
	group_by(hour_bin, lights) %>%
	summarise(pellet_intake = sum(freq)) 

	ungroup() %>%
	group_by(lights) %>%
	summarise(pellet_mean = mean(pellet_intake))

-> diff_intake

t.test(pellet_intake ~ lights, paired = TRUE, data = diff_intake)

# add inter removal interval
data %>%
	mutate(IRI = c(0, as.numeric(diff(date), units = "secs"))) -> data

# summarise data
data %>%
	group_by(Mouse, day) %>%
	summarise_by_time(
			  .date_var = date,
			  .by = "hour",
			  removed = n()
			  ) -> pellets_count_day

data %>%
	group_by(Mouse) %>%
	summarise_by_time(
			  .date_var = date,
			  .by = "hour",
			  removed = mean(n())
			  ) -> pellets_count


# plots -----------------------------------------------------------------------

data %>%
	filter(hour > 13 | hour < 12) %>%
	ggplot(aes(hour, as.factor(freq))) +
	geom_point(pch = "|") +
	scale_x_continuous(breaks = seq(0, 24)) +
	theme_pubr() +
	coord_fixed(ratio = 0.5) +
	ylab("") +
	facet_grid(as.factor(Mouse) ~ .)  -> A
pellet_intake %>%
	filter(hour_bin != 12, hour_bin != 13) %>%
	ggplot(aes(hour_bin, pellet_mean, ymin = pellet_mean - pellet_sem,
		   ymax = pellet_mean + pellet_sem)) +
	geom_line() +
	geom_errorbar() +
	scale_x_continuous(breaks = seq(0, 24)) +
	theme_pubr() +
	xlab("") +
	theme(axis.text.x = element_blank()) -> B
grid.arrange(B, A, nrow = 2) -> intake
ggsave("intake.png", intake)


pellets_count_day %>%
	ggplot(aes(lubridate::hour(date), removed, group = Mouse)) +
	geom_rect(aes(xmin=0, xmax=6,ymin=-Inf,ymax=Inf),alpha=0.1,fill="gray") +
	geom_rect(aes(xmin=18, xmax=24,ymin=-Inf,ymax=Inf),alpha=0.1,fill="gray") +
	geom_point() +
	geom_line() +
	geom_point(data = data, aes(lubridate::hour(date), freq), color = "red", pch = "|", stroke = 0.01) +
	facet_grid(Mouse~day) +
	theme_pubr() +
	scale_x_continuous(breaks = round(seq(0, 24, by = 2),1))
ggsave("intake.png", width = 14)

data %>%
	filter(IRI < 600) %>%
	ggplot(aes(IRI)) +
	geom_histogram() +
	scale_x_continuous(breaks = seq(0, 600, by = 150)) +
	facet_grid(~Mouse) +
	theme_pubr() 
ggsave("IRI.png")




data %>%
	filter(MotorTurns < 500) %>%
	ggplot(aes(MotorTurns)) +
	geom_histogram() +
	facet_grid(~Mouse) +
	scale_x_continuous(breaks = seq(0, 500, by = 100)) +
	theme_pubr()
ggsave("motorturns.png")

data %>%
	group_by(Mouse) %>%
	summarise(m = mean(MotorTurns), s = sd(MotorTurns))

data %>%
	group_by(Mouse) %>%
	summarise(m = sum(MotorTurns < 60), t = m / n())

data %>%
	ungroup() %>%
	summarise(m = sum(MotorTurns < 10), t = m / n())

data %>%
	filter(Mouse %in% c(234, 235)) %>%
	ungroup() %>%
	summarise(m = median(IRI))

data %>%
	ungroup() %>%
	filter(Mouse %in% c(234, 235)) %>%
	summarise(m = sum(IRI < 150) / n())

data %>%
	ungroup() %>%
	filter(day %in% c(20, 21)) %>%
	group_by(lights, Mouse, day) %>%
	summarise(m = n()) %>%
	summarise(avg = mean(m), s = sd(m)/sqrt(n())) %>%
	ggplot(aes(lights, avg, ymin = avg - s, ymax = avg + s)) +
	geom_point() +
	geom_errorbar() +
	facet_wrap(~Mouse) +
	theme_pubr()

data %>%
	ungroup() %>%
	mutate(ss = hms::as_hms(date)) %>%
	ggplot(aes(ss, freq)) +
	geom_point(pch = "|") +
	facet_grid(Mouse ~ day) +
	geom_vline(aes(xintercept = lubridate::hms("18:00:00"))) +
	geom_vline(aes(xintercept = lubridate::hms("06:00:00"))) +
	theme_pubr()
ggsave("ticks.png", width = 12)

# daily intake data -----------------------------------------------------------

daily <- read_csv("~/FED_data/data/all_daily.csv")
daily %>%
	mutate(date = lubridate::mdy(`MM:DD:YYYY hh:mm:ss`)) %>%
	select(-`MM:DD:YYYY hh:mm:ss`) %>%
	mutate(Mouse = as.factor(Mouse)) %>%
	mutate(day = lubridate::day(date)) -> daily
daily %>%
	filter(day < 13) %>%
	group_by(Mouse) %>%
	mutate(mean_intake = mean(PelletCount)) -> daily_plot

daily_plot %>%
	ggplot(aes(date, PelletCount, color = as.factor(Issue), group = Mouse)) +
	geom_point(size = 3) +
	geom_line() +
	geom_hline(aes(yintercept = mean_intake, linetype = "Mean intake"), color = "grey70", size = 3, alpha = 0.3) +
	facet_grid(~Mouse) +
	scale_color_manual(values=c("blue", "red")) +
	labs(color = "FED Error", linetype = " ") +
	ggtitle("Ingesta diaria del 2 de Agosto al 12 de agosto") +
	scale_x_date(date_labels="%d",date_breaks  ="1 day") +
	theme_pubr()
ggsave("weekly_intake.png", width = 20)
