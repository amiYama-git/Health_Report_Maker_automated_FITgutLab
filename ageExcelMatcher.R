
#agecompare(20,30,10) #age, hand, vo2

VO2table <- function(){
  # VO2_threshold_df <- data.frame(
  #   Agecol     = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79"),
  #   Very Poor       = c(34.1, 32.6, 31.4, 29.1, 26.2, 24.9), #very poor 
  #   Poor       = c(37.8, 36.8, 35.0, 31.7, 29.2, 27.5),
  #   Fair       = c(41.1, 39.7, 38.2, 34.6, 31.4, 29.6),
  #   Good       = c(45.3, 43.9, 41.1, 37.4, 34.5, 31.9),
  #   Excellent  = c(54.4, 52.5, 51.1, 46.0, 43.9, 43.9),
  #   stringsAsFactors = FALSE
  # )
  
  VO2_threshold_df <- data.frame(
    Agecol     = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79"),
    VeryPoor   = c(18.8, 15.0, 13.7, 13.0, 12.2, 10.7), #very poor 
    Poor       = c(25.6, 18.6, 16.6, 15.2, 14.2, 12.9),
    Fair       = c(31.6, 21.6, 18.8, 16.9, 15.7, 14.5),
    Good       = c(37.5, 24.5, 21.8, 18.8, 17.4, 15.9),
    Excellent  = c(46.0, 32.0, 27.3, 22.4, 20.3, 18.0),
    stringsAsFactors = FALSE
  )
  
  return(VO2_threshold_df)
}

handtable <- function(){
  agedf <- data.frame(
    Agecol = c("18-24", "25-29", "30-34","35-39", "40-44", "45-49","50-54", "55-59", "60-64","65-59", "70-74", "75-79", "80-85"),
    Poor   = c(17.6, 20.2, 20.5, 20.0, 22.8, 17.7, 19.7, 16.9, 15.9, 11.7, 15.2, 12.6, 14.5), #percentile 10 , very poor
    Poor   = c(22.4, 25.4, 23.9, 24.5, 26.5, 25.2, 24.6, 20.7, 19.2, 19.3, 19.5, 15.7, 16.6), #percentile 25 
    Fair   = c(28.4, 29.6, 29.8, 30.3, 30.4, 28.7, 28.2, 24.1, 24.4, 22.2, 22.5, 18.2, 19.5), #percentile 50 
    Good   = c(33.8, 33.6, 33.0, 33.0, 33.8, 34.4, 32.7, 30.2, 28.1, 25.0, 23.9, 22.4, 21.8), #percentile 75 
    Excellent = c(38.0, 39.7, 37.1, 38.0, 37.4, 37.6, 35.2, 32.2, 31.8, 31.2, 27.5, 27.8, 27.0), #percentile 90
    stringsAsFactors = FALSE
  )
  return(agedf)
}

#tables <- list(handtable, VO2table)
which <- 0

results <- NA


agecompare <- function(age, handgrip, VO2){
  which <- 0
  
  results <- NA
  evaluated <- NA
  
  actualcalc <- function(age,handgrip,VO2){
    
    for(a in 1:2){
      
      row <- NA
      differenceNOW <- NA
      differenceFIN <- 100
      percentile <- NA
      df_table_rn <- NA
      value <- NA
      checkcol <- NA
      
      which <- which + 1 
      if(which ==1){
        df_table_rn <- handtable()
        value <- handgrip
      }
      else{
        df_table_rn <- VO2table()
        value <- VO2
      }
      
      print("ji")
      print(df_table_rn)
      print(age)
      print(handgrip)
      print(VO2)
      
      for(i in 1:length(df_table_rn$Agecol)){
        print("in here")
        range <- strsplit(as.character(df_table_rn$Agecol[i]), "-")[[1]]
        print(range[1])
        print(range[2])
        if(age >= as.numeric(range[1]) && age <= as.numeric(range[2])){
          row = i + 1 #the first col is not included in the count apparently 
          cat("ageMinMax",range[1],"-", range[2],"row:",row, "\n")
          break
        }
      }
      adjustrow <- row-1
      cat("done:", adjustrow, "\n")
      
      length <- length(df_table_rn[adjustrow,])
      cat("length",length(df_table_rn[adjustrow,]),"\n")
      for(i in 2:length){
        cat("value of i",i,"\n")
        cat("Value: ", value)
        cat("df_table_rn[adjustrow, i]",df_table_rn[adjustrow, i],"\n")
        differenceNOW <- abs(df_table_rn[adjustrow,i] - value)
        cat("difference",differenceNOW,"\n")
        if(differenceNOW <= differenceFIN){
          differenceFIN <- differenceNOW
          percentilePlace <- i-1
          checkcol <- i
          cat("FINnow",differenceFIN,"\n")
        }
      }
      cat("FINFIN",differenceFIN, "\n")
      cat("percentile place: ", percentilePlace, "\n")
      
      percentile <- c("Poor", "Poor", "Fair", "Good", "Excellent")
      cat("universal: ",percentile[percentilePlace],"\n")
      results[[a]] <- percentile[percentilePlace]
      
    }
    
    cat("handgrip results : ", results[1], "\n", "vo2 results: ", results[2])
    #evaluated <- list(results[1], results[2], (textgenerate(results[1], results[2])))
    evaluated <-  list(results[1], results[2])
    print(evaluated)
    return(evaluated)
  }
  return(actualcalc(age,handgrip,VO2))
}

textgenerate <- function(hand, vo2){
  generaltext <- "Below is a breakdown of your total body weight, fat mass, free-fat mass, and total fat percentage. Under the Fitness report, you will find information for your true working zones calculated from the fitness test."
  additional <- ""
  if(hand == "Poor"|| vo2 == "Poor"){ 
    if(hand == "Poor" && vo2 == "Poor"){ #both hand and vo2 is poor 
      #nothing
    }
    else if(hand == "Poor"){ #only the hand is poor 
      additional <- paste("Your VO2peak is", tolower(vo2), "considering your age, and biological sex.")
    }
    else{ #only the vo2 is poor 
      additional <- paste("Your strength is", tolower(hand), "considering your age, and biological sex")
    }
  }
  
  else if(hand != vo2){ # the hand and vo2 is not the same, and is fair, good, Excellent
    additional <- paste("Your strength is", tolower(hand), "and your VO2 is", tolower(vo2) ,"considering your age, and biological sex")
  }
  else{ #same 
    additional <- paste("Your strength and VO2 is", tolower(vo2), "considering your age, and biological sex")
  }
  
  finaltext <- paste(generaltext, additional)
  
  return(finaltext)
  
}

