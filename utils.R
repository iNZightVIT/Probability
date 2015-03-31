
get_expected_counts_independence = function(counts) {
  
  factor2_proportions = colSums(counts) / sum(counts)
  factor1_counts = rowSums(counts)
  counts_if_independent = c(factor2_proportions %*% t(factor1_counts))
  t( t(counts^0) * counts_if_independent )
  
}




is_factor = function(x) {
  
  if (class(x) == "factor")
    return(TRUE)
  
  # Also accept non-negative integer variables
  # We will impose a limit on displayed factors later,
  # meaning that any count variables that are determined
  # by this function to be factors won't be displayed
  # to the user.
  
  isInteger = all(unique(x) == round(unique(x)))
  isNonNegative = min(x) >= 0
  
  isInteger && isNonNegative
  
}




read_counts = function(filePath) {
  
  lines = readLines(filePath)
  nLines = length(lines)
  
  factorNames = list()
  
  # First two lines should give rows factor name and columns factor name
  # Doesn't matter which order.
  
  for (i in 1:2) {
  
    if (length(grep("^((rows)|(cols))=(.+)", lines[i])) > 0) {
      
      rowsOrCols = gsub("^(rows|cols)=(.+)", "\\1", lines[i])
      name = gsub("^(rows|cols)=(.+)", "\\2", lines[i])
      
      factorNames[[rowsOrCols]] = name
      
    }
    
  }

  
  
  # Third line should contain column factor headers (levels) separated by commas
  # First  character should be comma (empty value)
  # But we will make sure it works even if the comma is missed out
  
  columnLevels = unlist(strsplit(lines[3], ","))
  
  if (length(columnLevels) == length(unlist(strsplit(lines[4], ",")))) {
    # There is a comma at the start of column levels, so remove it
    columnLevels = columnLevels[-1]
  }
  
  
  
  # Does the fourth row, minus the first value, contain all numbers?
  # If not, this may suggest wrong file type was selected.
  dataIsValid = !any(
    is.na(
      as.numeric(
        unlist(strsplit(lines[4], ","))[-1]
      )
    )
  )
  
  if (dataIsValid) {
    rowLevels = character(nLines - 3)
    rowCounts = list()
    
    for (i in 4:nLines) {
      row = lines[i]
      rowValues = unlist(strsplit(row, ","))
      rowLevels[i - 3] = rowValues[1]
      rowCounts[[i-3]] = as.numeric(rowValues[-1])
    }
  
    # Check the number of values in each row matches number of column levels found
    dataIsValid = all(sapply(rowCounts, length) == length(columnLevels))
  }
  
  
  if (dataIsValid) {
    
    countsMatrix = matrix(unlist(rowCounts),
                          ncol = length(columnLevels),
                          byrow = TRUE)
    
    rownames(countsMatrix) = rowLevels
    colnames(countsMatrix) = columnLevels
    
    countsTable = as.table(countsMatrix)
    
    result = list(factorNames = unname(unlist(factorNames)),
                  countsMatrix = countsMatrix)
    
  } else {
    result = NULL
  }
  
  result
  
}