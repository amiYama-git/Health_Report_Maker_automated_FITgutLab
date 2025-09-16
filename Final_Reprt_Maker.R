library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(zoo)
## for UI, this will warn the user about things, but do not worry about it. 
library(shiny)
library(bslib)
library(tibble)
library(rlang)
library(knitr)
library(pdftools)
library(stringr)
library(magick)
#library(later)
library(qpdf)
library(kableExtra)

#must be downloaded first via terminal: 
#/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

app_dir <- dirname(normalizePath(rstudioapi::getSourceEditorContext()$path))
print(app_dir)


#basic UI setup:

ui <- page_fillable(
  card(
    card_header("Input values and corresponding files"),
    
    fileInput(
      "raw_excel_file", 
      label = "Select EXCEL file for VO2"
    ),
    fileInput(
      "raw_DXA_file", 
      label = "Select PDF file"
    ),
    div(
      style = "margin-top: 20px; text-align: center;",
      actionButton("check_excel_pdf", "Submit Excel and PDF, check for corrupt files"),
      textOutput("corrcheck")
    ),
    textInput(
      "partname",
      "Enter Name of Participant",
    ),
    textInput(
      "partID",
      "Enter Participant ID",
    ),
    helpText("Do not include FGW. Just the number"),
    numericInput(
      "age", 
      "Enter Participant age:", 
      NA,
      min = NA,
      max = NA
    ),
    numericInput(
      "restingHR", 
      "Resting Heart Rate:", 
      NA,
      min = NA,
      max = NA
    ),
    numericInput(
      "hand_strength", 
      "Hand grip strength:", 
      NA,
      min = NA,
      max = NA
    ),
    textInput(
      "BloodPressure", 
      "Blood Pressure:", 
    ),
    helpText("format: ###/##"),
    numericInput(
      "Trigly", 
      "Triglycerides:", 
      NA,
      min = NA,
      max = NA
    ),
    numericInput(
      "Cholesterol", 
      "Total Cholesterol:", 
      NA,
      min = NA,
      max = NA
    ),
    numericInput(
      "HDLprotein",  
      "High Density Lipoproteins:", 
      NA,
      min = NA,
      max = NA
    ),
    numericInput(
      "FastingGlu", 
      "Fasting Glucose:", 
      NA,
      min = NA,
      max = NA
    ),
    numericInput(
      "WaistCir", 
      "Average Waist Circumference:", 
      NA,
      min = NA,
      max = NA
    ),
  ),
  div(
    style = "margin-top: 20px; text-align: center;",
    actionButton("submitBtnDone", "Generate Report"),
    textOutput("excelMessage")
  )
)


#server logic
server <- function(input,output){
  
  raw_excel_file_rmd <- reactiveVal(NULL) # for plot making 
  raw_DXA_file_rmd <- reactiveVal(NULL)
  checked <- reactiveVal(NULL)
  blankcheck <- reactiveVal(NULL)
  name_rmd <- reactiveVal(NULL)
  participantID_rmd <- reactiveVal(NULL)
  age_rmd <- reactiveVal(NULL)
  restingHR_rmd <- reactiveVal(NULL) #not 0 or "0", so req() will trigger. for visit B info
  excelMessage <- reactiveVal(NULL)
  corrcheck <- reactiveVal(NULL)
  #valueMessage <- reactiveVal(NULL)
  beforeclose <- reactiveVal(NULL)
  # done <- reactiveVal(0)
  list_of_data <- reactiveVal(NULL)
  ##for sending to rmd
  HRR6070_rmd <- reactiveVal(NULL)
  wattage_rmd <- reactiveVal(NULL)
  HR.Z1_rmd <- reactiveVal(NULL)
  HR.Z2_rmd <- reactiveVal(NULL)
  HR.Z3_rmd <- reactiveVal(NULL)
  HR.Z4_rmd <- reactiveVal(NULL)
  HR.Z5_rmd <- reactiveVal(NULL)
  hand_strength_rmd <- reactiveVal(NULL)
  BloodPressure_rmd <- reactiveVal(NULL)#
  Trigly_rmd <- reactiveVal(NULL)
  Cholesterol_rmd <- reactiveVal(NULL)
  HDLprotein_rmd <- reactiveVal(NULL)
  FastingGlu_rmd <- reactiveVal(NULL)
  WaistCir_rmd <- reactiveVal(NULL)#
  VO2max_rmd <- reactiveVal(NULL)
  plot_rmd <- reactiveVal(NULL)
  image_extract_text <- reactiveVal(NULL) #image,fatPercent,totalMass, fatMass, leanMass
  handcompareText <- reactiveVal(NULL)
  VO2compareText <- reactiveVal(NULL)
  DXAscanimage_rmd <- reactiveVal(NULL)
  fatPercent_rmd <- reactiveVal(NULL)
  totalMass_rmd <- reactiveVal(NULL)
  fatMass_rmd <- reactiveVal(NULL)
  leanMass <- reactiveVal(NULL)
  
  #----------main UI------- observe -----
  
  # loadfiles <- function(excel, DXA){
  #   print("submit was clicked for the files")
  #   # raw_excel_file_rmd(input$raw_excel_file)
  #   # raw_DXA_file_rmd(input$raw_DXA_file)
  #   if(!is.na(raw_excel_file_rmd()) && !is.na(raw_DXA_file_rmd())){
  #     if(raw_DXA_file_rmd()$pages == 3){
  #       excelMessage("files set!")
  #     }
  #     else{
  #       excelMessage("wrong PDF file")
  #     }
  #   }
  #   else{
  #     excelMessage("One or more files are missing")
  #   } 
  # }
  
  output$excelMessage <- renderText({
    excelMessage()
  })
  
  output$corrcheck <- renderText({
    corrcheck()
  })
  
  programcheck <- function(){
    showModal(modalDialog(
      title = "Have you used this program before? If you click No, it will run a check to see if all packages are installed; If not, they will be installed.", 
      actionButton("usedProgram","Yes"),
      actionButton("NotUsedProgram","No"),
      easyClose = FALSE,
      fotter = NULL
    ))
    
    observeEvent(input$usedProgram,{
      removeModal()
      makermd()
    })
    
    observeEvent(input$NotUsedProgram,{
      showModal(modalDialog(
        title = "Checking if all packages are installed, this may take a while....If prompted to restart R, please restart and click \"Yes\" next time.",
        easyClose = FALSE,
        footer = NULL
      ))
      
      #print("i think it comes here")
      
      withProgress(message = "checking packages",{
        if (!tinytex::is_tinytex()) {
          showModal(modalDialog(
            title = "Installing LaTeX",
            "TinyTeX (LaTeX system) will now be installed. This may take several minutes and could require restarting R.",
            easyClose = FALSE,
            footer = NULL
          ))
          tinytex::install_tinytex()
          
          incProgress(0.5, detail = "Step 1: install_tinytex() confirmed and/or installed.")
          
          showModal(modalDialog(
            title = "Installing ghostscript",
            "ghostscript will now be installed. This may take several minutes and could require restarting R.",
            easyClose = FALSE,
            footer = NULL
          ))
          
          #print("return")
          
          # install.packages(c("pdftools", "stringr", "magick"))
          system("brew install ghostscript")
          return() #user must restart. 
        }
        
        tinytex::tlmgr_install(c("tabu", "booktabs", "xcolor", "colortbl", "multirow"))
        
      })
      removeModal() #input$NotUsedProgram modal
      makermd()
      removeModal() #Have you used this program before? 
    })
  }
  
  #----------submit button to make the final report ------- observe -----
  
  observeEvent(input$submitBtnDone,{
    #by then, the excel and pdf file checks are completed. 
    if(checked() != 0){
      showModal(modalDialog(
        title = "Excel file or PDF is invalid, or the \"Submit files\" button above was not clicked",
        easyClose = TRUE,
        footer = NULL
      ))
      return(invisible(NULL))
    }
    
    blankcheck(0)
    
    checkexist <- list()
    
    age_rmd(input$age)
    checkexist[[1]] <- age_rmd()
    restingHR_rmd(input$restingHR)
    checkexist[[2]] <- restingHR_rmd()
    hand_strength_rmd(input$hand_strength)
    checkexist[[3]] <- hand_strength_rmd()
    BloodPressure_rmd(input$BloodPressure)
    checkexist[[4]] <- BloodPressure_rmd()
    Trigly_rmd(input$Trigly)
    checkexist[[5]] <- Trigly_rmd()
    Cholesterol_rmd(input$Cholesterol)
    checkexist[[6]] <- Cholesterol_rmd()
    print("check point here")
    HDLprotein_rmd(input$HDLprotein)
    checkexist[[7]] <- HDLprotein_rmd()
    FastingGlu_rmd(input$FastingGlu)
    checkexist[[8]] <- FastingGlu_rmd
    WaistCir_rmd(input$WaistCir) 
    checkexist[[9]] <- WaistCir_rmd()
    # #raw_excel_file_rmd(input$raw_excel_file)
    # checkexist[[10]]<- raw_excel_file_rmd()
    # #raw_DXA_file_rmd(input$raw_DXA_file)
    # checkexist[[11]] <- raw_DXA_file_rmd()
    #### alr inputted in check_excel_pdf. 
    name_rmd(input$partname)
    checkexist[[10]] <- name_rmd()
    participantID_rmd(input$partID)
    checkexist[[11]] <- participantID_rmd()
    
    #not checking pdf and excel again. 
    for (i in 1:length(checkexist)) {
      cat("checkexist one by one value of i: ",i)
      #cat("checkexist one by one ",checkexist[[i]])
      print(i)
      if (is.na(checkexist[[i]])) {
        excelMessage("One or more field is blank")
        print("this is printing")
        blankcheck(1)
      }
    }
    
    agecheck <- age_rmd()
    
    if(agecheck < 20){
      blankcheck(1)
      excelMessage("Participant age is younger than 20")
    }
    
    blankcheck_value <- blankcheck()
    
    if(blankcheck_value == 1){
      showModal(modalDialog(
        title = "One or more field is blank OR participant age is younger than 20",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    pdfcheck<- raw_DXA_file_rmd()
    info <- pdftools::pdf_info(pdfcheck$datapath) #safe now 
    if(info$pages == 3 && blankcheck_value == 0){
      programcheck()
      list_of_data(list(hand_strength_rmd(), BloodPressure_rmd(), Trigly_rmd(), Cholesterol_rmd(), HDLprotein_rmd(),FastingGlu_rmd(), WaistCir_rmd(), name_rmd(), participantID_rmd()))
    }
    else if(info$pages != 3){
      excelMessage("Potentially wrong PDF file. check file.")
    }
  
  })
  
  observeEvent(input$check_excel_pdf,{
    print("in observeEvent(input$check_excel_pdf")
    checked(1)
    # number <- checked()
    # cat("checked()", number)
    
    modal_sec_close <- function(title, msg) {
      showModal(modalDialog(title = title, tags$p(msg), easyClose = TRUE))
    }
    
    excelcheck <- input$raw_excel_file
    pdfcheck <- input$raw_DXA_file
    
    safe <- tryCatch({
      
      checked(1)
      
      check_if_valid <- function(file, filename){
        print("check if valid")
        if (is.null(file) || is.null(file$datapath) || !nzchar(file$datapath) || !file.exists(file$datapath)) {
          stop(sprintf("%s is missing or not uploaded correctly.", filename), call. = FALSE)
        }
      }
      
      print("does it come here?--1")
      
      check_if_valid(excelcheck, "excel") #the excel file path in here
      print("does it come here?--1.5")
      check_if_valid(pdfcheck, "pdf") #the pdf file path in here
      
      print("does it come here?--2")
      
      readxl::read_xlsx(excelcheck$datapath, n_max = 1)  # will error on corrupt/unreadable Excel
      
      print("does it come here?--3")
      
      info <- pdftools::pdf_info(pdfcheck$datapath)      # will error on bad PDF/path
      if (is.na(info$pages) || info$pages < 1) {
        stop("DXA PDF appears empty or unreadable.", call. = FALSE)
      }
      
      print("does it come here?--4")
      
    },
    error = function(e) {
      modal_sec_close("File check failed, reupload another file. ", conditionMessage(e))
      NULL
    })
    
    # abort observer on any failure
    print("does it come here?--5")
    if (is.null(safe)) return(invisible(NULL))
    print("does it come here?--6")
    
    checked(0)
    print("does it come here?--7")
    raw_excel_file_rmd(input$raw_excel_file)
    raw_DXA_file_rmd(input$raw_DXA_file)
    corrcheck("No files are corrupt, proceed.")
    print("does it come here?--8")
  })
  
  output$beforeclose <- renderText({
    beforeclose()
  })
  
  
  #----------calculate and make excel file using .rmd if fields are not blank--
  makermd <- function(){
    
    showModal(modalDialog(
      title = "Final Report is being made",
      easyClose = FALSE,
      fotter = NULL
    ))
    
    withProgress(message = "progress....",{
      
      incProgress(0.25, detail = "25%")
      
      calcfunc(raw_excel_file_rmd(),restingHR_rmd())
      incProgress(0.25, detail = "50%")
      
      image_and_compare(raw_DXA_file_rmd(), hand_strength_rmd(), age_rmd(), VO2max_rmd())#list_of_data <- list(HR.Z1_rmd(),HR.Z2_rmd(),HR.Z3_rmd(),HR.Z4_rmd(),HR.Z5_rmd(),VO2max_rmd(),plot_rmd())
      incProgress(0.25, detail = "75%")
      
      renderfunc(list_of_data())
      incProgress(0.25, detail = "100%")
      
      removeModal()
      
    })
    
    showModal(modalDialog(
      title = "Information required for VISIT B, input in Redcap",
      p(paste("Heart rate variability at 60%-70%:", HRR6070_rmd())),
      p(paste("Wattage for 60%-70%:", wattage_rmd())),
      easyClose = TRUE,
      fotter = NULL
      
    ))
  }
  #----------calculate and make excel file using .rmd if fields are not blank--
  
  
  #----------calculate and make excel file using .rmd if fields are not blank--
  
  calcfunc <- function(excel_file,RHR){
    print("see if this works")
    #datapath_bike_excel <- 
    
    bike.test.raw <-read_xlsx(excel_file$datapath)
    
    # Cleaning the data that you uploaded from the raw data from the VO2peak test from the participant
    bike.test <- select(bike.test.raw,"t","HR", "Phase", VO2_kg = "VO2/Kg","VO2","VCO2","Power","RQ", "METS", phase_time ="Phase time")
    bike.test <- bike.test %>% 
      filter(Phase != "RECOVERY" & Phase != "WARMUP" & Phase != "REST") %>%
      tail(-1)
    
    bike.test$VO2 <- as.numeric(bike.test$VO2)
    bike.test$VCO2 <- as.numeric(bike.test$VCO2)
    bike.test$HR <- as.numeric(bike.test$HR)
    
    # Plot the ventilatory thresholds
    bike.test$Observation <- 1:nrow(bike.test)
    
    bikeplot <- (ggplot(bike.test) + 
                   geom_line(aes(x = Observation, y = VO2, color = "VO2"), size = 0.5, alpha=0.8) + 
                   geom_line(aes(x = Observation, y = VCO2, color = "VCO2"), size = 0.5, alpha=0.8) + 
                   geom_line(aes(x = Observation, y = 20*HR, color ="HR"), size = 0.5, alpha = 0.8) +
                   scale_color_manual(values = c("VO2" = "blue", "VCO2" = "orange", "HR" = "red")) + 
                   scale_y_continuous(sec.axis = sec_axis(~./20, name = "bpm")) +
                   labs(x = "Time, seconds", y = "mL/min", color = "") +
                   theme_minimal())
    
    print("type of bikeplot")
    print(class(bikeplot))          # Should return: "gg" "ggplot"
    
    #Information required for VISIT B
    #Calculate the HRR 60-70% and the equivalent watts
    max.HR <- as.numeric(max(bike.test$HR, na.rm = TRUE))
    rest.HR <- RHR
    #rest.HR <- readline(prompt = "Enter Resting HR;   ") # This needs to be enter manually need to figure it out
    rest.HR <- as.numeric(rest.HR)
    
    HRR.60 <-(((max.HR-rest.HR)*0.60) + rest.HR)
    
    #print(HRR.60)
    
    HRR.70 <-(((max.HR-rest.HR)*0.70) + rest.HR)
    #print(HRR.70)
    HRR6070 <- paste0(HRR.60,"-",HRR.70)
    #print(HRR6070)
    #Enter the data in redcap under the VISIT B for the participant under 
    # the HR range for 60 to 70% HRR. 
    
    HRR.watts <- bike.test[bike.test$HR >= HRR.60 & bike.test$HR <= HRR.70, ]
    power_watts <- as.numeric(HRR.watts$Power)
    power_range <- as.character(range(power_watts))
    powerwattage <- paste0(power_range[1],"-", power_range[2])
    
    print("type of bikeplot2")
    print(class(bikeplot)) 
    
    #Enter the data in redcap under the VISIT B for the participant under 
    # the power (watts) range for 60 to 70% HRR. 
    
    #########FINAL REPORT INFORMATION############
    #to complete table Exercise Prescription (i.e. What are the working zones):
    HR.Z1 <- (((max.HR-rest.HR) * c(0.50, 0.60) + rest.HR)) #Zone 1
    HR.Z2 <- (((max.HR-rest.HR) * c(0.60, 0.70) + rest.HR)) #Zone 2
    HR.Z3 <- (((max.HR-rest.HR) * c(0.70, 0.80) + rest.HR)) #Zone 3 
    HR.Z4 <- (((max.HR-rest.HR) * c(0.80, 0.90) + rest.HR)) #Zone 4
    HR.Z5 <- (((max.HR-rest.HR) * c(0.90, 1.0) + rest.HR)) #Zone 5
    
    HR_table <- data.frame(
      Z1 = HR.Z1,
      Z2 = HR.Z2,
      Z3 = HR.Z3,
      Z4 = HR.Z4,
      Z5 = HR.Z5
    )
    ## 5, 2 dimensional vector output. 
    
    row.names(HR_table) <- c("Minimum, HR", "Maximum, HR")
    print(HR_table)
    
    ### Calculating the VO2peak. This is calculated as the average value of the highest three consecutive values of the VO2/kg
    # You need to enter the value of step 4 under fitness report, cardiovascular fitness report
    bike.test$VO2_kg <- as.numeric(bike.test$VO2_kg)
    print(max(bike.test$VO2_kg))
    
    print("type of bikeplot3")
    print(class(bikeplot)) 
    
    # Step 1: Calculate the sum of every three consecutive values
    consecutive_sums <- rollapply(bike.test$VO2_kg, width = 3, FUN = sum, align = "right", fill = NA)
    
    # Step 2: Find the maximum sum of three consecutive values
    max_sum <- max(consecutive_sums, na.rm = TRUE)
    
    # Step 3: Calculate the average of the highest three consecutive values
    average_max_three <- max_sum / 3
    
    # Step 4: Print the result
    print(average_max_three)
    
    #populate reactiveVals
    HRR6070_rmd(HRR6070)
    wattage_rmd(powerwattage)
    HR.Z1_rmd(HR.Z1)
    HR.Z2_rmd(HR.Z2) 
    HR.Z3_rmd(HR.Z3)
    HR.Z4_rmd(HR.Z4)
    HR.Z5_rmd(HR.Z5)
    VO2max_rmd(average_max_three)
    #######VO2max_rmd(10)
    print(bikeplot)
    print("type of bikeplot4")
    print(class(bikeplot)) 
    plot_rmd(bikeplot)
    
    print("after this?")
    
    #populate rest of the list_of_data()
    completeList <- list()
    for(i in 1:length(list_of_data())){
      completeList[[i]] <- list_of_data()[[i]]
    }
    addon <- length(list_of_data())
    for(i in 1:5){
      name <- paste0("HR.Z",i,"_rmd")
      name_func <- get(name)()  # set the "string" into the function
      #print(name_func[1])
      #print(name_func[2])
      completeList[[addon + i]] <- name_func 
    } #done inputting until HR.Z1-5
    lengthNOW <- length(completeList)
    completeList[[lengthNOW + 1]] <- VO2max_rmd()
    completeList[[lengthNOW + 2]] <- plot_rmd()
    list_of_data(completeList)
    
    #print("length of the list")
    print(length(list_of_data()))
    #print("type of bikeplot5")
    print(class(bikeplot)) 
    #print(list_of_data()[14])
    #print("here here")
  }
  
  #----------calculate and make excel file using .rmd if fields are not blank--
  
  #----------extracting images and comparing with the population.
  
  image_and_compare <- function(DXA, hand, age, VO2){
    
    options(error = function() {
      traceback(2)
      stop()
    })
    
    #temp <- list() 
    temp <- vector("list", 8)
    print("goes in here?")
    DXApath <- DXA$datapath
    
    print(DXA$datapath)
    
    #----------------
    
    DXAtext <- pdf_text(DXApath)
    matches <- str_match(DXAtext, regex("(?i)total[^\\d\\n]*((\\d+(\\.\\d+)?[^\\d]*){5})"))
    # Extract just the numeric values from the matched group
    numbers <- str_extract_all(matches[1, 1], "\\d+(\\.\\d+)?")[[1]]
    numbers <- as.numeric(numbers[1:5])  # Get first 5 num, returns a vector 
    temp[[1]] = numbers[1] #fatPercent
    cat("temp[[1]] = ",temp[[1]])
    temp[[2]] = numbers[3] #totalMass
    cat("temp[[2]] = ",temp[[2]])
    temp[[3]] = numbers[4] #fatMass
    cat("temp[[3]] = ",temp[[3]])
    temp[[4]] = numbers[5] #leanMass
    cat("temp[[4]] = ",temp[[4]])
    print("checkpoint 1")
    
    DXApage <- image_read_pdf(DXApath, pages = 1, density = 300)
    
    # geometry(pixelsizex, pixelsizey, fromx,fromy) +x means more from the bottom.
    DXAscanimage <- image_crop(DXApage, geometry = "457x1300+140+580")
    temp_file <- tempfile(fileext = ".png")
    image_write(DXAscanimage, path = temp_file, format = "png") #DXAscanimage is the image object, and image_write() will save it to tmp_file in PNG format.
    print("temp_file")
    print(temp_file)
    
    
    temp[[5]] <- temp_file 
    print("checkpoint 2")
    
    print(DXAscanimage)
    cat("fatPercent: ", temp[[1]], "totalMass: ", temp[[2]], "fatMass: ", temp[[3]], "leanMass: ", temp[[4]] )
    print("checkpoint 3")
    
    #----------------
    
    #temp <- image_and_healthValues(image_extract_text(DXA$datapath)) #in a list: temp: image,fatPercent,totalMass, fatMass, leanMass. add the path instead of eh actual pdf. 
    
    # 1-5 return(DXAscanimage,fatPercent, totalMass, fatMass, leanMass)
    #print(app_dir)
    source(file.path(app_dir, "ageExcelMatcher.R"))
    
    #--------
    # BEFORE calling agecompare()
    f <- file.path(app_dir, "ageExcelMatcher.R")
    message("Sourcing: ", normalizePath(f))
    
    # Load the functions into the *current* server/reactive environment
    sys.source(f, envir = environment())
    
    # Now *print* the checks so you can see them
    message("exists(agecompare) = ", exists("agecompare", inherits = TRUE))
    print(getAnywhere("agecompare"))  # will show where it's defined (or not)
    
    stopifnot(exists("agecompare", inherits = TRUE))  # fail here if not loaded
    
    #--------
    eval <- agecompare(age, hand, VO2)
    #eval <- agecompare(20,30,10) #age, hand, vo2
    
    ############# 
    cat("comparison", eval[[1]], eval[[2]])
    print("checkpoint 4")
    
    temp[[6]] <- eval[[1]] #compares hand grip strength to age group
    temp[[7]] <- eval[[2]]#compares VO2 to age group
    #temp[[8]] <- eval[[3]]
    
    print(temp[[6]])
    print(temp[[7]])
    
    print("checkpoint 5")
    #populate rest of the list_of_data()
    completeList <- list()
    completeList <- list_of_data() #transfer the data we have in now
    
    # for(i in seq_along(list_of_data())){
    #   completeList[[i]] <- list_of_data()[[i]] #transfer the data we have in now
    # }
    print("checkpoint 6")
    
    addon <- length(list_of_data())
    #print(addon)
    for(i in seq_len(length(temp))){ #temp length 8 
      # pos <- addon + i
      #print(pos)
      completeList[[addon + i]] <- temp[[i]] 
      #cat("complete list addon+1, addon+i is: ",pos, "value inputted is: ", temp[[i]] )
    }
    print("checkpoint 7")
    
    lengthNOW <- length(completeList)
    list_of_data(completeList)
    print("checkpoint 8")
  }
  
  
  #----------extracting images and comparing with the population.
  
  renderfunc <- function(list_of_data){

    rmd_path <- file.path(app_dir, "Final_Reort_Maker.Rmd") #
    print(list_of_data())
    print("checkpoint 9")
    
    final_report_name <- paste0("pre_processed", participantID_rmd(), ".pdf")
    preprocessed_dir <- normalizePath(file.path(dirname(rmd_path), "pre_processed_files"))
    cat("preprocessed_dir: ", preprocessed_dir)
    
    rendered_pdf <- rmarkdown::render(
      input = rmd_path,
      output_file = final_report_name,
      output_dir = preprocessed_dir,
      params = list(
        Hand = list_of_data()[[1]],
        BP = list_of_data()[[2]],
        TAG = list_of_data()[[3]],
        totCHOL = list_of_data()[[4]],
        HDL = list_of_data()[[5]],
        FasGlu = list_of_data()[[6]],
        Waist = list_of_data()[[7]],
        Name = list_of_data()[[8]],
        participantID = list_of_data()[[9]],
        HR_Z1 = list_of_data()[[10]], # HR.Z1_rmd() is in list_of_data
        HR_Z2 = list_of_data()[[11]],
        HR_Z3 = list_of_data()[[12]],
        HR_Z4 = list_of_data()[[13]],
        HR_Z5 = list_of_data()[[14]],
        VO2max = list_of_data()[[15]],
        plot = list_of_data()[[16]],
        fatPercent = list_of_data()[[17]],
        totalMass = list_of_data()[[18]],
        fatMass = list_of_data()[[19]],
        leanMass = list_of_data()[[20]],
        DXAimage = list_of_data()[[21]],
        handEval = list_of_data()[[22]],
        VO2Eval = list_of_data()[[23]]
        
      ),
      envir = new.env(parent = globalenv()) #make new env so the passed in values wont hold shiny bg.
    )
    
    print("file exists?")
    file.exists("gen_rec.pdf") #check if they exist
    normalizePath(file.path(preprocessed_dir, "gen_rec.pdf"))#check if they exist
    
    main_pdf <- normalizePath(rendered_pdf, mustWork = TRUE) #which should be appdir + preprocess, so same as preprocessed_dir + the pdf name 
    cat("main_pdf: ", main_pdf)
    cat("dirname(main_pdf):", dirname(main_pdf)) #preprocessed file
    
    rec_pdf  <- file.path(preprocessed_dir, "gen_rec.pdf") #gen rec in the same file as preprocessed 
    cat("rec_pdf: ", rec_pdf) #right, + pdfname 
    
    #checking if file exits
    if (!file.exists(rec_pdf)) stop("gen_rec.pdf not found at: ", rec_pdf)
    # Will error if either path is invalid or not a PDF
    
    main_info <- pdf_info(main_pdf) #info of these files, main/rec_pdf both include path until file 
    rec_info  <- pdf_info(rec_pdf)
    
    ##fine until here
    
    outpdf_path <- file.path(app_dir,"Final_Reports")
    cat("file.path(app_dir,\"Final_Reports\")", outpdf_path )
    
    out_pdf <- file.path(
      file.path(app_dir,"Final_Reports"), #into the Final_Reports folder. 
      paste0("Final_Report_FGW", participantID_rmd(),".pdf")
    )
    
    pdftools::pdf_combine(
      input  = c(main_pdf, rec_pdf),       # or c(main_pdf, rec_pdf) for all pages
      output = out_pdf
    )
    
    system2("open", out_pdf)
    
    
    # pdf_combine(
    #   input  = c(final_report_name, "gen_rec.pdf"),   # merge gen recc
    #   output = paste0("Final_Report_FGW", participantID_rmd(),".pdf")
    # )
    
  }
}


#running the UI/app
shinyApp(ui=ui, server=server)


