##
## This function draws a grid (table) of counts, where each
## count is show with a circle. Unlike the eikosogram, the sizes
## of each cell are equal, it is only the number of icons in each
## cell that vary.
##
##
## Arguments are as follows:
##
##   counts              Table of counts.
##
##   factorNames         The names of the factors in the table of counts. If the table
##                       was created using table(data$Gender, data$EyeColour), then set this to
##                       c("Gender", "EyeColour"). (Can't get this info from counts table directly).
##
##   showIndependence    Set to true to show what we would expect to see if the two factors
##                       were independent.
##
##   showCounts          Set to true to display counts in each cell and at the top of each grid column.
##
##   showProportions     Set to true to display proportions instead of counts. (Note: only one of
##                       showCounts and showProportions can be TRUE, not both).
##
##   sizeCm              The width and height of the plot, in centimetres.
##
##
## There is no value returned.
##

draw_counts_grid = function(counts,
                            factorNames,
                            showIndependence = FALSE,
                            showCounts = FALSE,
                            showProportions = FALSE,
                            sizeCm = 20) {
  
  if (showIndependence)
    counts = get_expected_counts_independence(counts)
  
  countsRowSums = rowSums(counts)
  countsColSums = colSums(counts)
  
  ceilCounts = ceiling(counts)
  
  # Border color of circle icons
  iconBorder = "#97BDDE"
  
  # Fill color of circle icons
  iconFill = "#C2DCF2"
  
  nObs = sum(counts)
  
  # The row names of the table will go along the x axis, and the column names
  # will go along the y axis, in agreement with the eikosogram. This means
  # that nRows is actually the number of columns in the table, and nCols is
  # actually the number of columns. 
  
  nRows = ncol(counts)
  nCols = nrow(counts)
  maxCount = max(round(counts))
  
  gridRowSums = countsColSums
  gridColSums = countsRowSums
  
  # Size of the actual plotting region
  plotSize = sizeCm - 4
  
  # Dimensions of cells, and set cell padding (mm)
  cellWidth = 10 * plotSize / nCols
  cellHeight = 10 * plotSize / nRows
  cellPadding = 1
  
  iconProps = get_icon_size(maxCount, cellWidth, cellHeight, cellPadding)
  
  # If too many counts and not enough space to draw them, stop function execution
  if (iconProps$size < 1)
    stop("Too many counts!")
  
  # Radius of the circles to be used for each unit
  iconRadius = iconProps$size/2
  
  
  # Start a new page of grid output
  grid.newpage()
  
  # Plotting area is square, with a layout consisting of
  # rows and columns to give a grid.
  pushViewport(viewport(width = unit(plotSize, "cm"),
                        height = unit(plotSize, "cm"),
                        layout = grid.layout(nRows, nCols),
                        x = unit(plotSize/2, "cm"),
                        y = unit(sizeCm - 1 - plotSize/2, "cm"),
                        default.unit = "npc",
                        gp = gpar(fill = "white")))
  

  # Loop through the rows of the grid
  for (i in 1:nRows) {
    
    # Loop through the columns within this row
    for (j in 1:nCols) {
      
      # New viewport for this cell
      pushViewport(viewport(layout.pos.row = nRows - i + 1,  # Drawing from the bottom upwards, not top-down!
                            layout.pos.col = j,
                            name = paste("cell", i, j)))
      
      # Cell border
      grid.rect()
      
      # The current cell count. Remember our grid rows and columns are reversed relative to the
      # table of counts, so we have to get the count by [column, row].
      count = counts[j, i]
      c_count = ceilCounts[j, i]

      # Continue to next cell if count is zero
      if (c_count == 0) {
        upViewport()
        next
      }

      # initial icon x position
      xPos = (iconProps$totalSize/2 + cellPadding) - iconProps$totalSize
      # initial icon y position
      yPos = cellHeight - cellPadding - iconProps$totalSize/2
      
      # counter to count icons drawn per row of icons
      counter = 0
      
      # For each unit, draw a count.
      for (k in 1:c_count) {
        
        # Update x position
        xPos = xPos + iconProps$totalSize
        
        # If we have reached the end of the row
        if (counter == iconProps$perRow) {
          # Reset the counter
          counter = 0
          # Reset x position to start of row
          xPos = iconProps$totalSize/2 + cellPadding
          # Update y position for the next row
          yPos = yPos - iconProps$totalSize
        }
        
        # Draw a circle
        grid.circle(x = unit(xPos, "mm"),
                    y = unit(yPos, "mm"),
                    r = unit(iconRadius, "mm"),
                    gp = gpar(col = iconBorder,
                              fill = iconFill))
        
        # Increment count of icons drawn in this row
        counter = counter + 1
        
      }

      if (c_count - count > 0) {
        # We have a partial unit if this condition is true. This should only happen
        # when showIndependence is true and we are displaying expected counts.
        # In this situation, we draw an extra icon then use a white rectangle to
        # obscure a portion of it.
        partialCount = count - floor(count)
        obscureAt = (xPos - iconRadius) + (2 * iconRadius * partialCount)
        
        grid.rect(x = unit(obscureAt, "mm"),
                  y = unit(yPos, "mm"),
                  width = unit(2 * iconRadius * (1 - partialCount), "mm"),
                  height = unit(2 * iconRadius, "mm"),
                  hjust = 0,
                  gp = gpar(col = "white", fill = "white"))
      }
      
      
      if (showCounts || showProportions) {
        # Displaying count or proportion in the cell
        value = ifelse(showCounts,
                       round(count, 1),
                       sprintf("%.3f", count / gridColSums[j]))
        
        grid.text(x = unit(1, "npc") - unit(2, "mm"),
                  y = unit(2, "mm"),
                  label = value,
                  just = c("right", "bottom"),
                  gp = gpar(fontface = "bold"))
      }
      
      
      upViewport()
    }
    
  }
  
  # Levels of the factors
  horizLevels = rev(colnames(counts))
  vertLevels = rownames(counts)
  
  # Loop through the rows, drawing a label to the right of the plotting area
  # for each level of the grid-rows factor
  
  for (i in 1:nRows) {
    yPos = (i - 1/2) * cellHeight
    grid.text(x = unit(1, "npc") + unit(3, "mm"),
              y = unit(yPos, "mm"),
              label = rev(horizLevels)[i],
              just = "left",
              gp = gpar(fontsize = 12,
                        col = "#555555"))
  }
  
  # Loop through the columns, drawing a label below the plotting area
  # for each level of the grid-columns factor. Also, draw grid-column counts
  # or grid-column proportions (of total sample size) above the plotting area
  # if either showCounts or showProportions is selected.
  
  for (i in 1:nCols) {
    
    xPos = (i - 1/2) * cellWidth
    
    grid.text(x = unit(xPos, "mm"),
              y = unit(-3, "mm"),
              label = vertLevels[i],
              just = "top",
              gp = gpar(fontsize = 12,
                        col = "#555555"))
    
    if (showCounts || showProportions) {
      value = ifelse(showCounts,
                     round(gridColSums[i], 1),
                     sprintf("%.3f", gridColSums[i] / nObs))
    
      grid.text(x = unit(xPos, "mm"),
                y = unit(1, "npc") + unit(3, "mm"),
                label = value,
                just = "bottom",
                gp = gpar(fontsize = 12,
                          col = "#555555"))
    }
    
  }
  
  # Draw label (name) of horizontal factor
  grid.text(factorNames[1],
            x = 0.5,
            y = unit(-1.6, "cm"),
            gp = gpar(font = 2))
  
  # Draw label (name) of vertical factor
  grid.text(factorNames[2],
            x = unit(1, "npc") + unit(3, "mm"),
            y = 1,
            just = c("left", "top"),
            gp = gpar(font = 2))

}







get_icon_size = function(maxCount, cellWidth, cellHeight, cellPadding) {
  
  # Spacing around each icon (mm)
  iconSpacing = 2
  
  # Initial size of each icon (mm)
  iconSize = 8
  
  # Initial total size (incl. spacing) of each icon (mm)
  totalIconSize = iconSize + iconSpacing * 2
  
  # Now we need to adjust the size of the icons so that we maximise
  # icon size whilst ensuring all icons fit in their cells.
  
  while (TRUE) {
    
    # Number of icons per row based on current size
    iconsPerRow = floor((cellWidth - 2 * cellPadding) / totalIconSize)
    
    # Number of rows that can be accomodated by the cell height
    numIconRows = floor((cellHeight - 2 * cellPadding) / totalIconSize)
    
    # The maximum number of icons any given cell can accomodate
    maxIconsInCell = iconsPerRow * numIconRows
    
    # If we have enough space for all cells
    if (maxIconsInCell >= maxCount)
      break
    
    # Adjust icon size
    if (iconSize > 10)
      iconSize = iconSize - 1
    else
      iconSize = iconSize - 0.5
    
    
    # Adjust icon spacing (10 percent of icon size)
    iconSpacing = max(0.25, iconSize * 0.1)
    
    # New total icon size
    totalIconSize = iconSize + iconSpacing * 2
    
    # Icons are getting too small. Too many counts!
    if (iconSize < 1)
      break
  }
  
  list(size = iconSize,
       totalSize = totalIconSize,
       spacing = iconSpacing,
       perRow = iconsPerRow)
  
}












