#######################
#### SIC Lab Utils ####
#######################

#### Data Cleaning #####

  ## Retaining the rational zero

    ratzero <- function(x,y){
      # (x is the column you're trying to center. y is the rational zero.)
      initcen <- scale(x, center = TRUE, scale = TRUE) # Centering on Zero
      adj <- (y - mean(x, na.rm = T)) / sd(x, na.rm = T)
      initcen - adj # The value of a moderate person
    }  


#### Data Analysis ####
    
    # Cleaning ANES data. You'll still need to make sure that they're coded in a way that makes sense after running this. 
      # (ANES coding varies, so we can't automate making sure that the numbers make sense.)
    
    makenumeric <- function(colsn, anes = FALSE, na.strings = NULL){
      # "x" is the column (or columns) of the variables that you'd like to clean.
      # This checks if it's ANES
      ifelse(anes == FALSE, 
             ifelse(is.null(na.strings), # Did they enter NA strings? 
                    colsn.r <- as.data.frame(colsn),
                    colsn.r <- lapply(as.data.frame(colsn), function(x) gsub(na.strings, NA, x)) # If it's not ANES data
             ),
             colsn.r <- lapply(as.data.frame(colsn), function(x) replace(x, grep("[-]", x), NA)) # If it is ANES data
      )
      colsn.r <- lapply(colsn.r, function(x) gsub("[^0-9\\.]", "", x)) # Removes any alphabetic characters
      colsn.d <- as.data.frame(colsn.r)
      colnum <- ncol(as.data.frame(colsn))
      ifelse(colnum == 1,
             return("Error: Please enter at least two columns."),
             colsn.d[,1:colnum] <- sapply(colsn.d[,1:colnum], as.numeric)
      )
      # Adding an indication that these are numeric. 
      colnames(colsn.d) <- paste(colnames(colsn.d), "n", sep = ".")
      colsn.d
    }


#### Plotting ####
    
# llamas
