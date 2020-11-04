#########################################################################################################
#
# Machine Learning Based Estimation of Average Treatment Effects under Unconfoundedness
# 
# Elias Moor, ETHZ
#
# Description: This is the main Shiny app file containing ui and server
#
#########################################################################################################

# Clear memory
rm(list = ls())

# Loading libraries and R files
library(shiny)
library(ggplot2)
library(shinythemes)
library(png)
library(dplyr)
library(stringr)
library(tidyr)
library(xtable)

source(file="f_getEstimatorNames.R", local=T)[[1]] 
source(file="app_choices.R", local=T)[[1]] 

#############################################################################################################
# Define UI 
#############################################################################################################

ui <- fluidPage(
  
  theme = shinytheme("paper"),
  
  titlePanel("ML Based Estimation of Average Treatment Effects"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Left side panel
    sidebarPanel( 
      
      tabsetPanel(type = "tabs",
                  tabPanel("General Settings", source(file = "UI_Side_General.R", local = TRUE)[1]),
                  tabPanel("DGP Settings",  source(file = "UI_Side_DGP.R", local = TRUE)[1]),
                  tabPanel("Estimation Settings", source(file = "UI_Side_Estimation.R", local = TRUE)[1]))
      
      , width = 3), 
    
    # Outputs
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Home", withMathJax(includeMarkdown("Home.Rmd"))),
                  tabPanel("Objectives", withMathJax(includeMarkdown("RO.Rmd"))),
                  tabPanel("Graph",
                           uiOutput(outputId = "mle_info_title_graph"),
                           plotOutput(outputId = "mle_graph"),
                           uiOutput(outputId = "mle_info_caption_graph"),
                           conditionalPanel(condition = "input.comparison_mode != 'analysis'",
                                            uiOutput(outputId = "mle_info_title_hist"),
                                            plotOutput(outputId = "mle_hist",  width = "100%"),
                                            uiOutput(outputId = "mle_info_caption_hist")),
                           radioButtons(inputId  = "performance_graph", 
                                        label    = "Performance measure:",
                                        choices  = c("RMSE"  = "RMSE", 
                                                     "MAE"   = "MAE",
                                                     "|Bias|"  = "MeanBias",
                                                     "SD"    = "SD"), 
                                        selected = "RMSE",
                                        inline = TRUE),
                           radioButtons(inputId  = "trimPS",
                                        label    = "Trim Propensity Score:",
                                        choices  = c("No"  = "0",
                                                     "0.01"  = "0.01",
                                                     "0.05"  = "0.05",
                                                     "0.1"  = "0.1",
                                                     "MaxPS" = "MaxPS",
                                                     "99QPS" = "99QPS",
                                                     "MaxPS and 0.01" = "MaxPS001"),
                                        selected = "MaxPS001",
                                        inline = TRUE),
                           uiOutput(outputId = "mle_info_bottom")),
                  tabPanel("Table", 
                           tableOutput(outputId = "mle_table"),
                           conditionalPanel(condition = "input.comparison_mode != 'analysis'",
                                           tableOutput(outputId = "mle_fs_table")),
                           uiOutput(outputId = "mle_info_bottom2")),
                  tabPanel("Data",
                           conditionalPanel(condition = "input.datatype == 'simulate_diamond1'",
                                            withMathJax(includeMarkdown("Data_Diamond1.Rmd"))),
                           conditionalPanel(condition = "input.datatype == 'simulate_busso'",
                                            withMathJax(includeMarkdown("Data_Sim_Lalonde.Rmd"))),
                           conditionalPanel(condition = "input.datatype == 'real_nsw'",
                                            withMathJax(includeMarkdown("Data_realNSW.Rmd")))),
                  tabPanel("Background", withMathJax(includeMarkdown("Background1.Rmd")),
                           tags$img(src="structure.png", width = "60%", height = "60%"), 
                           withMathJax(includeMarkdown("Background2.Rmd"))),
                  #tabPanel("Poster", tags$iframe(style="height:600px; width:100%", src="Poster.pdf")),
                  selected = "Graph")
      
    ) # end of main panel
  ) 
)

#############################################################################################################
# Define server function
#############################################################################################################

server <- function(input, output) {
  
  get_data <- reactive({
    
    source(file="SERVER_getData.R", local=T)[[1]] 
    
    data
    
  })
  
  get_info <- reactive({
    
    source(file="SERVER_getInfo.R", local=T)[[1]] 
    
    info
    
  })
  
  get_data_subset_part1 <- reactive({
    
    source(file="SERVER_filterData1.R", local=T)[[1]] 
    
    data_subset_list1
    
  })
  
  get_data_subset_part2 <- reactive({
    
    data_subset_list1 <- get_data_subset_part1()
    
    source(file="SERVER_filterData2.R", local=T)[[1]] 
    
    data_subset_list
    
  })
  
  get_data_subset1 <- reactive({
    
    validate({
      
      data_subset1 <- get_data_subset_part1()
      
      need( (nrow(data_subset1$info) > 0 && nrow(data_subset1$info) > 0),
            "No simulations are available for this specification! Please select another specification.")
    })
    
    data_subset1 <- get_data_subset_part1()
    data_subset1
    
  })
  
  get_data_subset <- reactive({
    
    validate({
      
      data_subset <- get_data_subset_part2()
      
      need( (nrow(data_subset$data.conv) > 0 && nrow(data_subset$data.ml) > 0),
            "No simulations are available for this specification! Please select another specification.")
    })
    
    data_subset <- get_data_subset_part2()
    data_subset
    
  })
  
  get_hist_data <- reactive({
    
    source(file="SERVER_histData.R", local=T)[[1]] 
    
    hist_data
    
  })
  
  get_data_within <- reactive({
    
    source(file="SERVER_withinData.R", local=T)[[1]] 
    
    data_within
    
  })
  
  get_data_between <- reactive({
    
    source(file="SERVER_betweenData.R", local=T)[[1]]
    
    data_between
    
  })
  
  get_data_analysis <- reactive({
    
    source(file="SERVER_analysisData.R", local=T)[[1]] 
    
    data_analysis
    
  })
  
  make_plot <- reactive({
    
    source(file="SERVER_makePlot.R", local=T)[[1]]
    
    mle_graph <- mle_graph + theme_light() + 
                             theme(legend.title = element_blank(),
                                   axis.text=element_text(size=14),  
                                   axis.title=element_text(size=14), 
                                   legend.text=element_text(size=14), 
                                   strip.text.x = element_text(size=14))
    
    yrange <- layer_scales(mle_graph)$y$range$range[2]-layer_scales(mle_graph)$y$range$range[1]
    
    if (input$comparison_mode == "within") {
      
      mle_graph <- mle_graph +
        geom_bar(stat = "identity" , width = 0.7, color = "dodgerblue3" , fill = "dodgerblue3") +
        geom_hline(yintercept = 0) +
        ylim(min(min(data_within_graph$relChange)-0.3*yrange, 0), max(max(data_within_graph$relChange) + 0.3*yrange, 0)) + 
        geom_text(aes(y = relChange + 0.03*yrange*sign(relChange) + ifelse(relChange==0, -0.03*yrange, 0), 
                      label = relChange, hjust = ifelse(relChange>0, 0, 1)), position = position_dodge(width = 0.9),
                      size = 5)
      
    } else if (input$comparison_mode == "between") {
        
        format.nb <- ifelse(input$datatype == "real_nsw", "%.0f", "%.2f")
        
        if (input$performance_graph == "MeanBias" |  input$performance_graph == "SD"){
          mle_graph <- mle_graph  + theme(legend.key.size = unit(0.5, "cm")) +
            ylim(- 0.1*yrange, 1.35*max(data_between_graph$value)) + 
            geom_text(aes(y = abs(value) + 0.05*yrange, label = sprintf(format.nb,abs(value)), hjust = 0), size = 5) + # y = value + 0.06
            scale_fill_manual(values=c("ML" = "#00BA38", "Conv" = "#F8766D", "OLS" = "#619CFF"))   
        } else {
          
          ylim.factor <- ifelse(input$datatype == "real_nsw", 0.5, 0.4)
          
          mle_graph <- mle_graph  + theme(legend.key.size = unit(0.5, "cm")) +
            ylim(- ylim.factor*yrange, 1.35*max(data_between_graph$value)) + 
            geom_text(aes(y = value + 0.05*yrange, label = sprintf(format.nb,value), hjust = 0), size = 5) + # y = value + 0.06
            geom_text(aes(y = -0.05*yrange, label = paste0("[", sprintf(format.nb,abs(MeanBias)), ", ", sprintf(format.nb,SD), "]"), hjust = 1), size = 4.7) + # y = value + 0.06
            scale_fill_manual(values=c("ML" = "#00BA38", "Conv" = "#F8766D", "OLS" = "#619CFF"))   
        }
      
    } else if (input$comparison_mode == "analysis") {
      
      mle_graph <- mle_graph + theme(strip.text.y = element_text(size=12), legend.key.size = unit(0.3, "cm"), legend.position="right",
                                     legend.box.spacing = unit(0, "lines"), legend.box = "vertical",
                                     legend.spacing = unit(0, "lines")) + 
        ylim(0, max(data_analysis_graph$value) + 0.3*yrange) + 
        geom_point(size=3, position = position_dodge(0.15)) + geom_line(size = 0.6, position = position_dodge(0.15))
    }
    
    mle_graph
    
  })
  
  make_table <- reactive({
    
    source(file="SERVER_makeTable.R", local=T)[[1]]
    
    mle_table
    
  })
  
  make_fs_table <- reactive({
    
    source(file="SERVER_makefsTable.R", local=T)[[1]]
    
    mle_fs_table
    
  })
  
  make_hist <- reactive({
    
    source(file="SERVER_makeHist.R", local=T)[[1]] 
    
    mle_hist <- mle_hist + theme_light() + 
      geom_col(position = position_dodge2(padding = 0.2)) +
      geom_line(data=hist_data_true, aes(x = bin, y = value, color = group), size = 0.8) +
      geom_point(data=hist_data_true, aes(x = bin, y = value, color = group), size = 2) +
      theme(legend.title = element_blank(),
            axis.text=element_text(size=12),  
            axis.title=element_text(size=12), 
            legend.text=element_text(size=12), 
            strip.text.x = element_text(size=12),
            legend.position="bottom")
    
    mle_hist
    
    
  })
  
  make_info_title_graph <- reactive({
    
    source(file="SERVER_makeInfoTitleGraph.R", local=T)[[1]] 
    
  })
  
  make_info_caption_graph <- reactive({
    
    source(file="SERVER_makeInfoCaptionGraph.R", local=T)[[1]] 
    
  })
  
  make_info_title_hist <- reactive({
    
    source(file="SERVER_makeInfoTitleHist.R", local=T)[[1]] 
    
  })
  
  make_info_caption_hist <- reactive({
    
    source(file="SERVER_makeInfoCaptionHist.R", local=T)[[1]] 
    
  })
  
  make_info_bottom <- reactive({
    
    source(file="SERVER_makeInfoBottom.R", local=T)[[1]] 
    
  })
  
  sampleSplits_reac <- reactive({
    
    if (input$crossFitFolds == 1) {
      sampleSplits_val <- "1"
    } else {
      sampleSplits_val <- input$sampleSplits
    }
    sampleSplits_val
  })
  
  ############################################################################
  
  # Create the output table
  output$mle_table <- renderTable({
    
    make_table()
    
  }, rownames = FALSE, caption = "Note: Simulation based on 5000 replications.")
  
  # Create the first stage output table
  output$mle_fs_table <- renderTable({
    
    make_fs_table()
    
  }, caption = "Note: Correlations (Cor) and RMSE of predicted conditional outcome means (Y1/Y0) and propensity scores (PS) vs. true conditional outcome means and propensity scores.")
  
  
  output$mle_info_title_graph  <- renderUI({
    
    make_info_title_graph()
    
  })
  
  output$mle_info_caption_graph  <- renderUI({
    
    make_info_caption_graph()
    
  })
  
  output$mle_info_title_hist  <- renderUI({
    
    make_info_title_hist()
    
  })
  
  output$mle_info_caption_hist  <- renderUI({
    
    make_info_caption_hist()
    
  })
  
  output$mle_info_bottom <- output$mle_info_bottom2 <- renderUI({
    
    make_info_bottom()
    
  })
  
  
  output$mle_graph <- renderPlot({
    
    print(make_plot())
    
  })
  
  output$mle_hist <- renderPlot({
    
    print(make_hist())
    
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)