##
## This function takes an R table of counts and returns a list
## containing information we will use to draw an eikosogram.
##
## Return value: A list containing the following elements
##
##    x: List of sublists. Each sublist represents a category on
##       the x axis, contains information used during drawing
##
##    xLevels: Names of the x categories (levels of factor 1)
##
##    yLevels: Names of the y categories (levels of factor 2)
##

get_eikosogram_data = function(tableCounts,
                               factorNames,
                               showIndependence = FALSE,
                               showCounts = FALSE,
                               showProportions = FALSE) {
  
  if (showIndependence)
    tableCounts = get_expected_counts_independence(tableCounts)
  
  # List to store the eikosogram data
  eik_data = list()
  
  # Sublist to store each x group list
  eik_data$counts = tableCounts
  eik_data$x = list()
  eik_data$names = factorNames
  eik_data$showCounts = showCounts
  eik_data$showProportions = showProportions
  
  # Initial x0 coordinate for rectangles
  x0 = 0
  
  rowCounts = rowSums(tableCounts)
  colCounts = colSums(tableCounts)
  N = sum(tableCounts)
  
  # Iterate through the levels of factor 1
  for (i in seq_along(rowCounts)) {
    
    # Current level of factor 1
    xLevel = names(rowCounts)[i]
    
    # x1 coordinate for the rectanges (as a proportion along x-axis)
    x1 = cumsum(rowCounts)[i] / N
    
    # Store the level name in a list
    # This list will ultimately contain all required data for this factor level
    xData = list(
      xLevel = xLevel,
      xProp = x1 - x0,
      xCount = rowCounts[i]
    )
    
    # List that will be used to store data for each rectangle
    # There will be one element per level of factor 2 (y variable)
    rects = list()
    
    # Initial y coordinate for first rectangle
    y0 = 0
    
    # Iterate through the levels of factor 2
    for (j in seq_along(colCounts)) {
      
      # The current level of factor 2
      yLevel = names(colCounts)[j]
      
      # The top y-coordinate of the rectangle for this (xLevel,yLevel) combination
      y1 = cumsum(tableCounts[xLevel,])[j] / rowCounts[i]
      
      # Store all the data needed to draw the rectange in a list
      rect = list(
        x0 = x0,
        x1 = x1,
        y0 = y0,
        y1 = y1,
        yLevel = yLevel,
        prop = tableCounts[xLevel, yLevel] / N,
        count = tableCounts[xLevel, yLevel],
        columnProp = tableCounts[xLevel, yLevel] / rowCounts[i],
        yProp = y1 - y0
      )
      
      # Store the list in the rects list
      rects[[j]] = rect
      
      # Set the bottom coordinate for the next rectangle
      y0 = y1
      
    } # end factor-2 iteration
    
    # Set the left coordinate for the next set of rectangles (next level of factor 1 (x))
    x0 = x1
    
    # Store all rects in the xData list
    xData$rects = rects
    
    # Store all the data for this level of factor 1 in the top-level list
    eik_data$x[[i]] = xData
    
  } # end factor-1 iteration
  
  # Add the names of the factor levels for x axis and y axis
  eik_data$xLevels = names(rowCounts)
  eik_data$yLevels = names(colCounts)
  
  # Return list of data
  eik_data
}








##
## This function takes an a list returned by get_eikosogram_data()
## and uses it to draw an eikosogram.
##
## Return value: none
##

draw_eikosogram = function(eik_data, sizeCm) {
  
  # Number x levels
  nx = length(eik_data$xLevels)
  
  # Number y levels
  ny = length(eik_data$yLevels)
  
  # Get a palette of colours
  nColors = 7 # max levels plus 2
  eik_colours = hsv(seq(0, 1, length = nColors + 1)[-(nColors + 1)],
                    0.25,
                    1)
  
  # New page of grid output
  grid.newpage()
  
  sz = unit(sizeCm - 4, "cm")
  
  # Plotting area is square
  pushViewport(viewport(width = sz,
                        height = sz,
                        x = 0.5*sz,
                        y = unit(sizeCm - 1, "cm") - 0.5*sz,
                        default.unit = "npc",
                        gp = gpar(fill = "white")))
  
  # Outline plotting area
  grid.rect()
  
  # Iterate through x levels
  for (i in 1:nx) {
    
    # Data for the current x level
    xGroup = eik_data$x[[i]]
    
    # Label on the x axis
    grid.text(
      label = xGroup$xLevel,
      x = (xGroup$rects[[1]]$x0 + xGroup$rects[[1]]$x1)/2,
      y = unit(-0.6, "cm"),
      gp = gpar(fontsize = 12)
    )
    
    # Proportion on the top x axis
    if (eik_data$showProportions || eik_data$showCounts
        && xGroup$xCount > 0) {
      value = ifelse (eik_data$showProportions,
                      sprintf("%.3f", xGroup$xProp),
                      xGroup$xCount)
      grid.text(
        label = value,
        x = (xGroup$rects[[1]]$x0 + xGroup$rects[[1]]$x1)/2,
        y = unit(1, "npc") + unit(0.6, "cm"),
        gp = gpar(fontsize = 12)
      )
    }
    
    # Iterate through y levels
    for (j in 1:ny) {
      
      # Rectangular area information for this (x,y) factor combination
      rect = xGroup$rects[[j]]
      
      # Colour of the rectangle to be drawn
      col = eik_colours[j]
      
      # Draw a rectangle for this (x,y) factor combination
      grid.rect(
        x = rect$x0,
        y = rect$y0,
        width = rect$x1 - rect$x0,
        height = rect$y1 - rect$y0,
        gp = gpar(fill = col),
        just = c("left", "bottom")
      )
      
      # Display the count or proportion if either option is selected
      if ((eik_data$showProportions || eik_data$showCounts)
          && rect$count > 0) {
        
        value = ifelse (eik_data$showProportions,
                        sprintf("%.3f", rect$columnProp),
                        round(rect$count, 1))
        grid.text(
          label = value,
          x = rect$x0 + (rect$x1 - rect$x0)/2,
          y = rect$y0 + (rect$y1 - rect$y0)/2,
          just = "centre",
          gp = gpar(fontsize = 12)
        )
        
      }
      
    } # end y-iterate
    
  } #end x-iterate
  
  
  # X axis label
  grid.text(
    eik_data$names[1],
    x = 0.5,
    y = unit(-1.6, "cm"),
    gp = gpar(font = 2)
  )
  
  # Legend label
  grid.text(
    label = eik_data$names[2],
    x = unit(1, "npc") + unit(0.75, "cm"),
    y = unit(1, "npc"),
    just = c("left", "top"),
    gp = gpar(font = 2)
  )
  
  # Legend boxes
  grid.rect(
    x = unit(1, "npc") + unit(0.75, "cm"),
    y = unit(1, "npc") - unit(0.3 + (ny:1 * 0.8), "cm"),
    width = unit(0.5, "cm"),
    height = unit(0.5, "cm"),
    just = c("left", "center"),
    gp = gpar(fill = eik_colours,
              col = "grey")
  )
  
  # Legend categories labels
  grid.text(
    label = rev(eik_data$yLevels),
    x = unit(1, "npc") + unit(1.5, "cm"),
    y = unit(1, "npc") - unit(0.3 + (1:ny * 0.8), "cm"),
    just = c("left", "center"),
    gp = gpar(fontsize = 11)
  )
  
}
