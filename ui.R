library(shiny)

shinyUI(fluidPage(
  
  
  tags$head(
    # Set the title of the document.
    tags$title("Visualising Independence"),
    
    # Link to Open Sans webfont.
    tags$link(rel = "stylesheet",
              href = "//fonts.googleapis.com/css?family=Open+Sans:400,700"),
    
    # Custom CSS styles must be included inline.
    # Shiny by default includes bootstrap.min.css and shiny.css after any custom CSS
    # files included using tags$link. So if we want to overwrite some of those CSS rules
    # they need to be included inline.
    tags$style(HTML("
      body {
        font-family: 'Open Sans', Arial, sans-serif;
      }
      
      .control-label {
        font-weight: bold;
      }
      
      .tab-content {
        overflow: visible;
      }
      
      #file_progress {
      display: none;
      }

      #error {
        color: red;
      }

      .help-info {
        max-width: 640px;
      }
      
      .help-info p {
        margin-bottom: 15px;
        line-height: 150%;
      }

      p.sample-file {
        font-family:Consolas, Courier, monospace;
        padding: 8px 15px;
        background-color: #EEE;
      }
    "))
  ),
  
  # Make a big header.
  titlePanel("Visualising Independence"),
  
  # Use a sidebar layout. Options will be in a left-hand sidebar,
  # content will be to the right of the sidebar.
  sidebarLayout(
    sidebarPanel(
      # File input element
      fileInput('file', '',
                accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
      # Use br tags as a quick way to adjust vertical spacing between elements.
      tags$br(),
      tags$br(),
      
      # Header checkbox - corresponds to input$header in server.R
      # Selected by default.
      checkboxInput('header', "File contains a header (raw data only)", TRUE),
      tags$br(),
      
      # Data type radio buttons.
      radioButtons(inputId = 'fileType',
                   label = "Data Type",
                   choices = c('Raw (CSV)' = 'raw', Counts = 'counts'),
                   selected = "raw"),

      # UI elements for other options. Initially these are not displayed.
      # Their values are assigned to the output variable in server.R. NULL values
      # mean that nothing is output.
      uiOutput('showDataUI'),
      uiOutput('independenceUI'),
      uiOutput('factorSelectors'),
      uiOutput('swapFactorsUI'),
      
      tags$br(),
      # Element for display error messages.
      textOutput('error', container = tags$p)
      
    ),
    
    
    # The content panel.
    mainPanel(
      
      # We will use a tabs layout for this app.
      tabsetPanel(
        
        # First panel displays the eikosogram output
        tabPanel("Eikosogram",
                 imageOutput('eikosogram')),
        
        # Second tab displays the counts grid output.
        tabPanel("Counts Grid",
                 imageOutput('countsGrid')),
        
        # Third tab provides information about how to use the app, such as
        # accepted file formats and what each UI option does.
        tabPanel("Help",
                 
                 tags$h3("File format"),
                 
                 
                 tags$h5("Raw format"),
                 
                 tags$p("A CSV file, with variables as columns and observations as rows.",
                        "If the first row is not a header that gives variable names, you'll need to deselect the 'File contains a header' option."),
          
                 tags$p("Example:"),
                 
                 tags$p(
                   "course,sex,age,eyes",
                   tags$br(),
                   "201,female,20,brown",
                   tags$br(),
                   "BIOSCI_209,female,30,green",
                   tags$br(),
                   "208,male,22,blue",
                   tags$br(),
                   "BIOSCI_209,male,26,brown",
                   class = "sample-file"),
                 
                 
                 tags$h5("Counts format"),
                 
                 tags$p("This format allows you to read in a table of counts directly.",
                        "The first two lines should specify the names of the factors in the table.",
                        "It does not matter which order these two lines are in."),
                 
                 tags$p("The third line should give the names (levels) of the column factor, ",
                        "and then line 4 onwards should specify the rows of the table, ",
                        "where the first value in each line gives the level of the row factor, ",
                        "and the following values give the counts."),
                 
                 tags$p("Example:"),
                 
                 tags$p("rows=EyeColour",
                        tags$br(),
                        "cols=Gender",
                        tags$br(),
                        ",female,male",
                        tags$br(),
                        "blue,37,37",
                        tags$br(),
                        "brown,132,101",
                        tags$br(),
                        "green,23,15",
                        tags$br(),
                        "hazel,17,18",
                        tags$br(),
                        "other,16,12",
                        class = "sample-file"),
                 
                 
                 
                 tags$h3("Using this tool"),
                 
                 
                 tags$p(
                   "First upload a file. ",
                   "Specify the data type of your file using the Data Type options."
                 ),
                 
                 tags$p(
                   "For raw CSV files, the factors in the dataset will be detected automatically, ",
                   "and all factors with no more than 5 levels will be displayed in the factor selection boxes.", 
                   "A numerically-coded factor will be recognised as a factor as long as the levels are non-negative integers and there are no more than 5 levels.",
                   "Other numeric variables that do not meet these conditions are ignored.",
                   "We place a restriction on the number of factor levels because having a large number of levels make the diagrams more difficult to interpret."
                 ),
                 
                 tags$p(
                   "You can display counts for each cell (and column total) by selecting the Counts options under 'Show data labels'.",
                   "Similarly, you can instead choose to display proportions within each column by selecting the Proportions option."
                 ),
                 
                 tags$p(
                   "Tick the 'Show independence' checkbox to show what we would expect to see if the two factors were independent."
                 ),
                 
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 
                 class = "help-info"
                 
        ) # end tabPanel
      ) # end tabsetPanel
    ) # end mainPanel
  ) # end sidebarLayout
)) # end fluidPage, shinyUI
