library(ggplot2)
library(dplyr)
library(shinyWidgets)

server <- shinyServer(function(input, output) {
  
pens <- read.csv("data/penalties.csv") %>%
  mutate(end_y = 100 - end_y)

y_min <- reactive ({ ifelse(!is.null(input$plot_brush$xmin), input$plot_brush$xmin, 0)})
y_max <- reactive ({ ifelse(!is.null(input$plot_brush$xmax), input$plot_brush$xmax, 100)})
z_min <- reactive ({ ifelse(!is.null(input$plot_brush$ymin), input$plot_brush$ymin, 0)})
z_max <- reactive ({ ifelse(!is.null(input$plot_brush$ymax), input$plot_brush$ymax, 100)})

penalties <- reactive({penalties <- subset(pens, end_y >= y_min() & 
                          end_y <= y_max() &
                          end_z >= z_min() &
                          end_z <= z_max())
  
  if(input$foot_sel != "Both"){
    penalties <- subset(penalties, body_part == input$foot_sel)
  }
penalties
  })

num_pen <- reactive({nrow(penalties())})
scored <- reactive ({paste0(round((nrow(subset(penalties(), outcome == "goal"))/num_pen())*100, 2),"%")})
missed <- reactive ({paste0(round((nrow(subset(penalties(), outcome != "goal"))/num_pen())*100, 2),"%")})


output$plot1 <- renderPlot({
ggplot() + theme(axis.title = element_blank(),
                 axis.ticks = element_blank(),
                 axis.text = element_blank(),
                 panel.grid = element_blank(),
                 panel.background = element_rect(fill = 'white', colour = 'white')) +
    geom_rect(aes(xmin=43, xmax=57, ymin=-10, ymax=50), fill = "#009933") +
    geom_rect(aes(xmin=43, xmax=44, ymin=4, ymax=20), fill = "lightblue") +
    geom_rect(aes(xmin=44, xmax=56, ymin=4, ymax=20), fill = "grey") +
    geom_rect(aes(xmin=56, xmax=57, ymin=4, ymax=20), fill = "salmon") +
    geom_rect(aes(xmin=43, xmax=57, ymin=20, ymax=50), fill = "black") +
  geom_segment(aes(x=43, xend=57, y=0, yend=0), size=0.2, colour="white") +
  geom_rect(aes(xmin=44.7, xmax=45.2, ymin=0, ymax=38), colour = "white", fill = "white") +
  geom_rect(aes(xmin=54.8, xmax=55.3, ymin=0, ymax=38), colour = "white", fill = "white") +
  geom_rect(aes(xmin=44.7, xmax=55.3, ymin=38, ymax=42), colour = "white", fill = "white") +
  scale_x_continuous(limits = c(43,57)) +
  scale_y_continuous(limits = c(-10, 50)) +
  geom_rect(aes(xmin=51, xmax=54.5, ymin=-10, ymax=-0.5), colour = "black", fill = "red") +
  geom_rect(aes(xmin=45.5, xmax=49, ymin=-10, ymax=-0.5), colour = "black", fill = "limegreen") +
  geom_rect(aes(xmin=49, xmax=51, ymin=-10, ymax=-0.5), colour = "black", fill = "navy") +
  annotate("text", x = 47.25, y = -3, label = "Scored", colour = "black", size = 8) + 
  annotate("text", x = 52.75, y = -3, label = "Missed", colour = "white", size = 8) +
  annotate("text", x = 47.25, y = -7.5, label = scored(), colour = "black", size = 8) + 
  annotate("text", x = 52.75, y = -7.5, label = missed(), colour = "white", size = 8) +
    annotate("text", x = 50, y = -3, label = "Penalties", colour = "white", size = 8) + 
    annotate("text", x = 50, y = -7.5, label = num_pen(), colour = "white", size = 8)
})

#Close the server definition
})

##################
# User Interface #
##################

#generic line initiating the UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  titlePanel("Penalty Conversion rates"),
  
  radioGroupButtons(inputId = "foot_sel",
                    label = NULL,
                    choiceNames = c("Both", "Right Foot", "Left Foot"),
                    choiceValues = c("Both", "right foot", "left foot"),
                    selected = "Both",
                    width = '350px'),

plotOutput("plot1",
           brush = "plot_brush"
)

))

##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)