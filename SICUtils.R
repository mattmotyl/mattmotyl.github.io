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
    
    anesclean <- function(colsn){
      # "x" is the column (or columns) of the variables that you'd like to clean.
      colsn.r <- lapply(colsn, function(x) replace(x, grep("[-]", x), NA))
      colsn.d <- as.data.frame(lapply(colsn.r, function(x) factor(x)))
      colnum <- ncol(colsn)
      colsn.d[,1:colnum] <- sapply(colsn.d[,1:colnum], as.numeric)
      # Adding an indication that these are numeric. 
      colnames(colsn.d) <- paste(colnames(colsn.d), "n", sep = ".")
      colsn.d
    }


#### Plotting ####
    

