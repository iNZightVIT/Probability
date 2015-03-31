
# We need the colorspace package - install it if it is not present.
if (!("colorspace" %in% rownames(installed.packages())))
  install.packages("colorspace")


# Read in the required libraries.
library(shiny)
library(grid)
library(colorspace)


# Read in the required R code files.
source("utils.R")
source("eikosogram_functions.R")
source("counts_grid_functions.R")




shinyServer(function(input, output, session) {
  
  # A place to store reactive values.
  values = reactiveValues()
  
  # TRUE is uploaded file is valid, FALSE otherwise. Initially FALSE because no file has been uploaded.
  values$file = FALSE
  
  # A list containing information needed for drawing plots, or NULL if input data is invalid.
  values$data = NULL
  
  # Assign error messages to this component. We will have a function that listens for changes to this
  # value and will display the error message when one is set.
  values$error = NULL
  
  # Store the names of all factors in the data file that was uploaded.
  values$factors = NULL
  
  
  
  # Listens for changes to input$file (whether or not a valid file was uploaded)
  # and also listens for changes to input$fileType, e.g. if the Data type option
  # is changed in the UI. Also takes a dependency on input$header, meaning that this
  # is also re-executed when that option is toggled on/off. Use isolate to write
  # to our reactive values storage variable, so that the function is not re-run
  # when those values change.
  
  observe({
    
    inFile = input$file
    
    # Initially set error and data to NULL, without taking a dependency on those values
    isolate({
      values$error = NULL
      values$data = NULL
    })
    
    # If the uploaded file is valid
    if (!is.null(inFile)) {
      
      fileType = input$fileType
      
      # If the selected file type is raw
      if (fileType == "raw") {
        
        # need to use a try-catch statement with read.csv in case the file format
        # is invalid (in which case read.csv throws an error)
        result = tryCatch({read.csv(inFile$datapath,
                                    header = input$header)
                          },
                          error = function(e) {
                            NULL
                          })
        if (is.null(result)) {
          # The file was was not able to be read using read.csv
          isolate({
            values$file = FALSE
            values$error = "Invalid file format! Did you mean to select data type counts?"
          })
        } else {
          # The raw data file was successfully read into R
          isolate({
            values$file = TRUE
            values$data = result
          })
        }
      }
      else {
        # Selected file type counts
        
        # read_counts is found in utils.R
        result = read_counts(inFile$datapath)
        
        if (is.null(result)) {
          # Invalid ounts file format
          isolate({
            values$file = FALSE
            values$error = "Incorrect file format."
          })
        } else {
          # Successfully read in counts file.
          isolate({
            values$file = TRUE
            values$data = result
          })
        }
      }
    }
    else {
      # Only true when the app is first loaded and no file has been uploaded yet.
      isolate({
        values$file = FALSE
        values$error = "Please upload a file."
      })
    }
    
  }) # end observe
  
  
  
  
  # Listens for changes in values$data and determines which variables are
  # factors. Sets values$factors if everything is OK (using isolate, so as to make
  # sure that the function does not take a dependency on values$factors).
  # If there are less than 2 factors, we set values$error (again, using isolate).
  # Factors with more than 5 levels are ignored.
  
  observe({
    
    if (!is.null(values$data)) {
      # Only do something if the data is valid
      
      if (isolate(input$fileType) == "raw") {
        # Raw file type
      
        # Get factor variable names. The is_factor() function is found in utils.R
        factorNames = names(which(sapply(values$data, is_factor)))
        
        # Restrict number of levels
        maxLevels = 5
        
        # Calculate the number of levels in each factor
        levels = sapply(
          lapply(values$data[,factorNames], unique),
          length
        )
        
        # Final names of the valid factors that will be shown in the UI selection boxes.
        factorNames = factorNames[which(levels <= maxLevels)]
        
        if (length(factorNames) < 2) {
          # Need at least 2 factors!
          # Set an error message if this is not the case.
          isolate({
            values$error = "Data file needs more than 2 factors!"
            values$factors = NULL
          })
        } else {
          # Set error to NULL and set values$factors. 
          isolate({
            values$error = NULL
            values$factors = factorNames
          })
        }
      }
      else {
        # Counts file format.
        isolate({
          values$error = NULL
          values$factors = values$data$factorNames
        })
      }
    }
    
  }) # end observe
  
  
  
  # Error message as a text element.
  output$error <- renderText({
    values$error
  })
  


  # Factor selection UI elements.
  # Takes a dependency on values$factors.
  # Should only be run when values$factors changes, which only
  # occurs when values$data changes.
  
  output$factorSelectors = renderUI({
    
    f = values$factors
    ui = list()
    
    if (is.null(f)) {
      # If no valid factors, set UI elements to NULL to display nothing.
      ui[[1]] = NULL
      ui[[2]] = NULL
    }
    else {
      # Otherwise create 2 UI elements. Initially, set the first checkbox to
      # show the first factor and the second checkbox to show the second factor.
      fList = as.list(f)
      names(fList) = f
      ui[[1]] = selectInput(inputId = "factor1",
                            label = "Factor 1",
                            choices = fList,
                            selected = f[1])
      ui[[2]] = selectInput(inputId = "factor2",
                            label = "Factor 2",
                            choices = fList,
                            selected = f[2])
    }
    
    ui
  }) # end observe



  # Listen for changes to input$swapFactors. This occurs when the Swap factors
  # UI button is clicked and its value is incremented by 1.
  
  observe({
    
    # If the UI button is not displayed or its value is zero (has not been clicked yet), do nothing
    if (is.null(input$swapFactors) || input$swapFactors == 0)
      return()
    
    # Switch the values of the factor selection boxes. Use isolate to avoid this function
    # taking a dependency on input$factor1 and input$factor2. 
    isolate({
      updateSelectInput(session, "factor1", selected = input$factor2)
      updateSelectInput(session, "factor2", selected = input$factor1)
    })
    
    # If the current file type is counts, we also need to transpose the
    # countsMatrix element of values$data. We do this to the matrix because transposing
    # an R table object does not actually do what we would expect. We get a table later on
    # using as.table and setting the rownames and colnames manually.
    if (isolate(input$fileType) == "counts") {
      isolate({
        values$data$countsMatrix = t(values$data$countsMatrix)
      })
    }
    
  }) # end observe
  


  
  # Show independence UI checkbox.
  independenceOpt = reactive({
    if (!values$file)
      NULL
    else
      list(checkboxInput(inputId = 'independence',
                         label = 'Show independence',
                         value = input$independence),
           tags$br())
  })

  
  
  
  # Show data labels UI radio button group.
  showDataOpt = reactive({
    if (!values$file)
      NULL
    else
      list(radioButtons(inputId = 'showData',
                        label = 'Show data labels',
                        choices = c('None' = 'None',
                                    'Counts' = 'Counts',
                                    'Proportions' = 'Proportions'),
                        selected = "None"),
           tags$br())
  })


  
  
  # Swap factors UI button.
  swapFactorsButton = reactive({
    if (!values$file)
        NULL
    else
      list(actionButton(inputId = 'swapFactors',
                        label = 'Swap factors'),
           tags$br())
  })




  # This value is recalculated when any of the reactive values inside the containing function
  # change. It also makes this variable a reactive value itself, so that other reactive contexts
  # can take a dependency on this value.
  eikosogramData = reactive({
    
    d = values$data
    
    # If data invalid, or inpur$factor1 or input$factor2 are NULL or not valid names, return 
    # list with data set to NULL.
    if (is.null(d)
        || is.null(input$factor1)
        || is.null(input$factor2)
        || !(input$factor1 %in% isolate(values$factors))
        || !(input$factor1 %in% isolate(values$factors))
        )
      return(list(data = NULL, error = " "))
    
    # If we make it here, all values are valid so far.
    # Set initial values of eik and error.
    # The eik variable will store information needed to draw an eikosogram.
    error = eik = NULL
    
    # The selected factors MUST be different. If not, set an error message.
    if (input$factor1 == input$factor2)
      error = "Please select 2 different factors."
    
    # Update the reactive error value (through isolate).
    # An error message will be printed if the value was not NULL.
    isolate({values$error = error})
    
    # If error is NULL, calculate the table of counts for the
    # two factors that are selected.
    if (is.null(error)) {
      
      if (isolate(input$fileType) == "raw") {
        tableOfCounts = table(d[,input$factor1],
                              d[,input$factor2])
      } else {
        tableOfCounts = as.table(d$countsMatrix)
      }
      
      # Get the eikosogram data.
      eik = get_eikosogram_data(
        tableOfCounts,
        factorNames = c(input$factor1, input$factor2),
        showIndependence = ifelse(is.null(input$independence),
                                  FALSE,
                                  input$independence),
        showCounts = (input$showData == 'Counts'),
        showProportions = (input$showData == 'Proportions')
      )
      
    }
    
    # Return data and error information in a list.
    list(data = eik,
         error = error)
    
  }) # end eikosogramData reactive context.


  
  
  # Eikosogram image output for the UI.
  # If data element of eikosogramData is invalid, the image
  # will be a tiny white square to make it look like there
  # is no output. We use the png device for drawing because it
  # allows us to specify an exact size for our image.
  output$eikosogram = renderImage({
    
    eik = eikosogramData()
    outfile = tempfile("eikosogram", fileext = ".png")
    
    if (!is.null(eik$data)) {
      sizeCm = 20
    } else {
      sizeCm = 1
    }
    
    png(outfile, units = "cm",
        width = sizeCm,        
        height = sizeCm,
        res = 72)
    
    if (!is.null(eik$data)) {
      draw_eikosogram(eik$data, sizeCm)
    }
    
    dev.off()
    
    list(src = outfile)
    
  }, deleteFile = TRUE)


  
  
  # Counts grid image for the UI. If the data element of eikosogramData is
  # invalid, then clearing we cannot proceed with drawing the counts grid either,
  # and like before we use a tiny white square image instead.
  output$countsGrid = renderImage({
    
    # Use validity of eikosogram data to determine whether or not
    # to send a valid image. Consider changing structure of data later
    eik = eikosogramData()
    outfile = tempfile("counts_grid", fileext = ".png")
    
    if (!is.null(eik$data)) {
      sizeCm = 20
    } else {
      sizeCm = 1
    }
    
    png(outfile, units = "cm",
        width = sizeCm,        
        height = sizeCm,
        res = 72)
    
    if (!is.null(eik$data)) {
      draw_counts_grid(eik$data$counts,
                       factorNames = c(input$factor1, input$factor2),
                       showIndependence = input$independence,
                       showCounts = (input$showData == 'Counts'),
                       showProportions = (input$showData == 'Proportions'),
                       sizeCm = sizeCm)
    }
    
    dev.off()
    
    list(src = outfile)
    
  }, deleteFile = TRUE)



  
  # Show independence UI checkbox element.
  output$independenceUI = renderUI({
    independenceOpt()
  })

  # Show data labels UI radio button elements.
  output$showDataUI = renderUI({
    showDataOpt()
  })
  
  # Swap factors UI button element.
  output$swapFactorsUI = renderUI({
    swapFactorsButton()
  })
  
  
  
}) # end shinyServer
