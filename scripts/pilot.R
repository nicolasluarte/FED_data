library(tidyverse)
library(ggplot2)
library(timetk)
library(ggpubr)

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

# add pellet count
data %>%
	mutate(freq = 1) -> data

# add light/dark

data %>%
	mutate(lights = if_else(
				lubridate::hour(date) >= 16 | lubridate::hour(date) <= 6,
				"dark",
				"light"
				)) -> data

# add day
data %>%
	mutate(hour = lubridate::hour(date)) %>%
	mutate(day = lubridate::day(date)) -> data

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




