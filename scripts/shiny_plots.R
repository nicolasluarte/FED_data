options(
	browser = "brave",
	shiny.autoreload = TRUE
)
pacman::p_load(
	      shiny,
	      ggplot2,
	      tidyverse,
	      lubridate
	      )

# Define UI for app that draws a histogram ----
ui <- fluidPage(
		actionButton("do", "Load data"),
		sliderInput(
			   inputId = "days",
			   label = "Number of data points",
			   min = 1,
			   max = 120,
			   value = c(1, 10)
			   ),
		hr(),
		plotOutput("plot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
	observeEvent(
		     input$do,
		     {
			data <- read_csv("https://raw.githubusercontent.com/nicolasluarte/FED_data/main/data/all_daily.csv")
		        data %>%
				mutate(n_data = row_number()) -> data
			output$plot <- renderPlot(
						  {
							  data %>%
								  slice(input$days[1]:input$days[2]) %>%
							  ggplot(aes(n_data, PelletCount)) +
								geom_point()
						  }
			)
		     }
		     )

}

shinyApp(ui, server)
