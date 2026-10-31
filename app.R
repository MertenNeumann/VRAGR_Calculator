##############
###Packages###
##############
library("haven")
library("shiny")
library("dplyr")
library("ggplot2")
library("readxl")
library("gginnards")

#####################
###VRAG-R function###
#####################

# VRAG-R - Calculator
# by Merten Neumann
#
# based on
#
# Rettenberger, M., Hertz, P. G. & Eher, R. (2017). 
#   Die deutsche Version des Violence Risk Appraisal Guide-Revised (VRAG-R) 
#   (Elektronische Schriftenreihe der Kriminologischen Zentralstelle e. V. (KrimZ), Band 8). 
#    Wiesbaden: Kriminologische Zentralstelle e.V.
#
# Harris, G. T., Rice, M. E., Quinsey, V. L. & Cormier, C. A. (2015). 
#   Violent offenders: Appraising and managing risk (3rd ed.). 
#   Washington: American Psychological Association. https://doi.org/10.1037/14572-000

#For single case
compute_vrag <- function(Item_1,
                         Item_2,
                         Item_3_1, Item_3_2, Item_3_3, Item_3_4, Item_3_5, Item_3_6,
                         Item_4,
                         Item_5_1, Item_5_2, Item_5_3, Item_5_4, Item_5_5, Item_5_6, Item_5_7, Item_5_8, Item_5_9, Item_5_10,
                         Item_5_11, Item_5_12, Item_5_13, Item_5_14, Item_5_15, Item_5_16, Item_5_17, Item_5_18, Item_5_19, Item_5_20,
                         Item_5_21, Item_5_22, Item_5_23, Item_5_24, Item_5_25, Item_5_26, Item_5_27,
                         Item_6,
                         Item_7,
                         Item_8_1, Item_8_2, Item_8_3, Item_8_4, Item_8_5, Item_8_6, Item_8_7, Item_8_8, Item_8_9, Item_8_10,
                         Item_8_11, Item_8_12, Item_8_13, Item_8_14, Item_8_15, Item_8_16, Item_8_17, Item_8_18,
                         Item_9,
                         Item_10_1, Item_10_2, Item_10_3, Item_10_4, Item_10_5, Item_10_6, Item_10_7, Item_10_8, Item_10_9, Item_10_10,
                         Item_10_11, Item_10_12,
                         Item_11,
                         Item_12_1, Item_12_2, Item_12_3, Item_12_4,Item_12_5) {
  
  #Packages
  require(dplyr)
  
  #Function for missings
  fill_missing_vrag <- function(Row) {
    
    sum_no_missing <- sum(Row, na.rm = TRUE)
    
    if(sum_no_missing >=0 ) {Max_Values <- c(2, 4, 4, 1, 5, 4, 2, 4, 6, 5, 3, 6)}else{Max_Values <- c(-2, -3, -2, -1, -3, -2, -7, -2, -2, -2, -2, -6)}
    
    idx_missing <- is.na(Row)
    
    max_no_missing <- sum(Max_Values[!idx_missing])
    
    prop_no_missing <- sum_no_missing/max_no_missing
    
    add_missing <- sum(Max_Values[idx_missing]) * prop_no_missing
    
    if(sum(idx_missing > 4)) {new_value <- NA}else{new_value <- round(sum_no_missing + add_missing)}
    
    new_value
    
  }
  
  
  #Item 1
  Item_1 = ifelse(Item_1 == "Yes", -2, 2)
  
  #Item 2
  Item_2 = case_when(Item_2 == "None" ~ -3, 
                     Item_2 == "Moderate" ~ 1, 
                     Item_2 == "Severe" ~ 4)
  
  #Item 3
  Item_3_1 <- ifelse(Item_3_1 == "Yes", 1, 0)
  Item_3_2 <- ifelse(Item_3_2 == "Yes", 1, 0)
  Item_3_3 <- ifelse(Item_3_3 == "Yes", 1, 0)
  Item_3_4 <- ifelse(Item_3_4 == "Yes", 1, 0)
  Item_3_5 <- ifelse(Item_3_5 == "Yes", 1, 0)
  Item_3_6 <- ifelse(Item_3_6 == "Yes", 1, 0)
  
  Item_3_notNA <- sum(!is.na(c(Item_3_1, Item_3_2, Item_3_3, Item_3_4, Item_3_5, Item_3_6)))
  
  Item_3 <- (sum(Item_3_1, Item_3_2, Item_3_3, Item_3_4, Item_3_5, Item_3_6, na.rm = TRUE)/Item_3_notNA)*6
  Item_3 = case_when(Item_3 < 3 ~ -2,
                     Item_3 == 3 ~ 0,
                     Item_3 == 4 ~ 1,
                     Item_3 > 4 ~ 4)
  
  #Item 4
  Item_4 <- ifelse(Item_4 == "Yes", -1, 1)
  
  #Item 5
  Item_5 <- Item_5_1*7 +
            Item_5_2*3 +
            Item_5_3*2 +
            Item_5_4*5 +
            Item_5_5*1 +
            Item_5_6*5 +
            Item_5_7*5 +
            Item_5_8*2 +
            Item_5_9*1 +
            Item_5_10*1 +
            Item_5_11*1 +
            Item_5_12*5 +
            Item_5_13*1 +
            Item_5_14*2 +
            Item_5_15*1 +
            Item_5_16*1 +
            Item_5_17*1 +
            Item_5_18*1 +
            Item_5_19*1 +
            Item_5_20*1 +
            Item_5_21*1 +
            Item_5_22*1 +
            Item_5_23*1 +
            Item_5_24*1 +
            Item_5_25*1 +
            Item_5_26*1 +
            Item_5_27*1
  
  Item_5 <- case_when(Item_5 == 0 ~ -3,
                      Item_5 == 1 | Item_5 == 2 ~ -1,
                      Item_5 > 2 & Item_5 < 9 ~ 1,
                      Item_5 > 8 & Item_5 < 18 ~ 3,
                      Item_5 > 17 ~ 5)
  
  #Item 6
  Item_6 <- ifelse(Item_6 == "Yes", 4, -2)
  
  #Item 7
  Item_7 <- case_when(Item_7 < 26 ~ 2,
                      Item_7 > 25 & Item_7 < 31 ~ 1,
                      Item_7 > 30 & Item_7 < 34 ~ -1,
                      Item_7 > 33 & Item_7 < 39 ~ -2,
                      Item_7 > 38 & Item_7 < 46 ~ -4,
                      Item_7 > 45 ~ -7)
  
  #Item 8
  Item_8 <- Item_8_1*28 +
            Item_8_2*7 +
            Item_8_3*6 +
            Item_8_4*6 +
            Item_8_5*7 +
            Item_8_6*5 +
            Item_8_7*3 +
            Item_8_8*2 +
            Item_8_9*3 +
            Item_8_10*15 +
            Item_8_11*12 +
            Item_8_12*10 +
            Item_8_13*6 +
            Item_8_14*6 +
            Item_8_15*2 +
            Item_8_16*8 +
            Item_8_17*5 +
            Item_8_18*4
  
  Item_8 <- case_when(Item_8 == 0 ~ -2,
                      Item_8 > 0 & Item_8 < 5 ~ 2,
                      Item_8 > 4 & Item_8 < 19 ~ 3,
                      Item_8 > 18 ~ 4)
  
  #Item 9
  Item_9 <- case_when(Item_9 == 0 ~ -2,
                      Item_9 == 1 ~ 2,
                      Item_9 == 2 ~ 3,
                      Item_9 == 3 | Item_9 == 4 ~ 4,
                      Item_9 > 4 ~ 6)
  
  #Item 10
  Item_10_1 <- ifelse(Item_10_1 == "Yes", 1, 0)
  Item_10_2 <- ifelse(Item_10_2 == "Yes", 1, 0)
  Item_10_3 <- ifelse(Item_10_3 == "Yes", 1, 0)
  Item_10_4 <- ifelse(Item_10_4 == "Yes", 1, 0)
  Item_10_5 <- ifelse(Item_10_5 == "Yes", 1, 0)
  Item_10_6 <- ifelse(Item_10_6 == "Yes", 1, 0)
  Item_10_7 <- ifelse(Item_10_7 == "Yes", 1, 0)
  Item_10_8 <- ifelse(Item_10_8 == "Yes", 1, 0)
  Item_10_9 <- ifelse(Item_10_9 == "Yes", 1, 0)
  Item_10_10 <- ifelse(Item_10_10 == "Yes", 1, 0)
  Item_10_11 <- ifelse(Item_10_11 == "Yes", 1, 0)
  Item_10_12 <- ifelse(Item_10_12 == "Yes", 1, 0)
  
  Item_10 <- sum(Item_10_1, Item_10_2, Item_10_3, Item_10_4, Item_10_5, Item_10_6,
                 Item_10_7, Item_10_8, Item_10_9, Item_10_10, Item_10_11, Item_10_12)
  
  Item_10 <- case_when(Item_10 == 0 ~ -2,
                       Item_10 == 1 ~ 0,
                       Item_10 > 1 & Item_10 < 5 ~ 4,
                       Item_10 > 4 ~ 5)
  
  #Item 11
  Item_11 <- case_when(Item_11 == "None" ~ -2,
                       Item_11 == "Only14" ~ -1,
                       Item_11 == "Other" ~ 2,
                       Item_11 == "Female14" ~ 3)
  
  #Item 12
  Item_12 <- sum(Item_12_1, Item_12_2, Item_12_3, Item_12_4, Item_12_5, na.rm = TRUE)
  
  Item_12_NA <- sum(is.na(c(Item_12_1, Item_12_2, Item_12_3, Item_12_4, Item_12_5)))
  
  if(Item_12_NA == 1) {
    
    Item_12 <- case_when(Item_12 == 8 ~ 10,
                         Item_12 == 7 ~ 8.7,
                         Item_12 == 6 ~ 7.5,
                         Item_12 == 5 ~ 6.2,
                         Item_12 == 4 ~ 5.0,
                         Item_12 == 3 ~ 3.7,
                         Item_12 == 2 ~ 2.5,
                         Item_12 == 1 ~ 1.2,
                         Item_12 == 0 ~ 0)
    
  }else{if(Item_12_NA > 1){Item_12 <- NA}}
  
  Item_12 <- case_when(Item_12 == 0 ~ -6,
                       Item_12 >= 1 & Item_12 < 2.5 ~ -3,
                       Item_12 >= 2.5 & Item_12 < 3.5 ~ 2,
                       Item_12 >= 3.5 & Item_12 < 7.5 ~ 3,
                       Item_12 >= 7.5 ~ 6)
  
  
  
  #Compute Score
  Item_Scores <- c(Item_1, Item_2, Item_3, 
                   Item_4, Item_5, Item_6, 
                   Item_7, Item_8, Item_9, 
                   Item_10, Item_11, Item_12)
  
  Num_Missing <- sum(is.na(Item_Scores))
  
  if(any(is.na(Item_Scores))) {
    
    Vrag_score <- ifelse(Num_Missing > 4, NA, fill_missing_vrag(Item_Scores))
    
  }else{
    
    Vrag_score <- sum(Item_Scores)
    
  }
  
  #Risk classification
  Risk_Category <- case_when(Vrag_score <= -24 ~ 1,
                             Vrag_score > -24 & Vrag_score <= -17 ~ 2,
                             Vrag_score > -17 & Vrag_score <= -11 ~ 3,
                             Vrag_score > -11 & Vrag_score <= -4 ~ 4,
                             Vrag_score > -4 & Vrag_score <= 3 ~ 5,
                             Vrag_score > 3 & Vrag_score <= 11 ~ 6,
                             Vrag_score > 11 & Vrag_score <= 17 ~ 7,
                             Vrag_score > 17 & Vrag_score <= 26 ~ 8,
                             Vrag_score > 26 ~ 9)
  
  #Percentile
  Percentile <- case_when(Vrag_score == -34 ~ 1.2,
                          Vrag_score == -33 ~ 1.4,
                          Vrag_score == -32 ~ 1.9,
                          Vrag_score == -31 ~ 2.6,
                          Vrag_score == -30 ~ 3.7,
                          Vrag_score == -29 ~ 4.9,
                          Vrag_score == -28 ~ 5.7,
                          Vrag_score == -27 ~ 7.0,
                          Vrag_score == -26 ~ 8.8,
                          Vrag_score == -25 ~ 9.6,
                          Vrag_score == -24 ~ 11.2,
                          Vrag_score == -23 ~ 13.2,
                          Vrag_score == -22 ~ 14.5,
                          Vrag_score == -21 ~ 16,
                          Vrag_score == -20 ~ 17.7,
                          Vrag_score == -19 ~ 20.1,
                          Vrag_score == -18 ~ 21.7,
                          Vrag_score == -17 ~ 24,
                          Vrag_score == -16 ~ 26.5,
                          Vrag_score == -15 ~ 28.4,
                          Vrag_score == -14 ~ 30.1,
                          Vrag_score == -13 ~ 31.3,
                          Vrag_score == -12 ~ 33.2,
                          Vrag_score == -11 ~ 35.6,
                          Vrag_score == -10 ~ 36.8,
                          Vrag_score == -9 ~ 38.5,
                          Vrag_score == -8 ~ 39.5,
                          Vrag_score == -7 ~ 41.2,
                          Vrag_score == -6 ~ 42.2,
                          Vrag_score == -5 ~ 43.8,
                          Vrag_score == -4 ~ 45.5,
                          Vrag_score == -3 ~ 46.5,
                          Vrag_score == -2 ~ 48.1,
                          Vrag_score == -1 ~ 49.7,
                          Vrag_score == 0 ~ 51.4,
                          Vrag_score == 1 ~ 53.3,
                          Vrag_score == 2 ~ 54.5,
                          Vrag_score == 3 ~ 56,
                          Vrag_score == 4 ~ 57.6,
                          Vrag_score == 5 ~ 59.2,
                          Vrag_score == 6 ~ 60.7,
                          Vrag_score == 7 ~ 62.1,
                          Vrag_score == 8 ~ 63.9,
                          Vrag_score == 9 ~ 65,
                          Vrag_score == 10 ~ 66.1,
                          Vrag_score == 11 ~ 67.4,
                          Vrag_score == 12 ~ 69.7,
                          Vrag_score == 13 ~ 71.7,
                          Vrag_score == 14 ~ 73.6,
                          Vrag_score == 15 ~ 75,
                          Vrag_score == 16 ~ 76.7,
                          Vrag_score == 17 ~ 78.1,
                          Vrag_score == 18 ~ 79.8,
                          Vrag_score == 19 ~ 81.4,
                          Vrag_score == 20 ~ 83.2,
                          Vrag_score == 21 ~ 83.9,
                          Vrag_score == 22 ~ 84.8,
                          Vrag_score == 23 ~ 86.1,
                          Vrag_score == 24 ~ 86.9,
                          Vrag_score == 25 ~ 88.1,
                          Vrag_score == 26 ~ 89.3,
                          Vrag_score == 27 ~ 90.4,
                          Vrag_score == 28 ~ 91.2,
                          Vrag_score == 29 ~ 92.4,
                          Vrag_score == 30 ~ 93.6,
                          Vrag_score == 31 ~ 94.3,
                          Vrag_score == 32 ~ 95.1,
                          Vrag_score == 33 ~ 95.8,
                          Vrag_score == 34 ~ 96.6,
                          Vrag_score == 35 ~ 97.3,
                          Vrag_score == 36 ~ 97.7,
                          Vrag_score == 37 ~ 98,
                          Vrag_score == 38 ~ 98.2,
                          Vrag_score == 39 ~ 98.8,
                          Vrag_score == 40 ~ 99,
                          Vrag_score == 41 ~ 99.5,
                          Vrag_score == 42 ~ 99.8,
                          Vrag_score == 43 ~ 99.9,
                          Vrag_score == 44 ~ 100)
  
  #Probability for recedivism
  Prob_5 <- case_when(Risk_Category == 1 ~ 9,
                      Risk_Category == 2 ~ 12,
                      Risk_Category == 3 ~ 16,
                      Risk_Category == 4 ~ 20,
                      Risk_Category == 5 ~ 26,
                      Risk_Category == 6 ~ 34,
                      Risk_Category == 7 ~ 45,
                      Risk_Category == 8 ~ 58,
                      Risk_Category == 9 ~ 76)
  
  Prob_12 <- case_when(Risk_Category == 1 ~ 15,
                       Risk_Category == 2 ~ 24,
                       Risk_Category == 3 ~ 33,
                       Risk_Category == 4 ~ 42,
                       Risk_Category == 5 ~ 51,
                       Risk_Category == 6 ~ 60,
                       Risk_Category == 7 ~ 69,
                       Risk_Category == 8 ~ 78,
                       Risk_Category == 9 ~ 87)
  
  #Export
  Out_list <- list("Score" = Vrag_score,
                   "Risk Category" = Risk_Category,
                   "Percentile" = Percentile,
                   "Probability for violent revedivism after 5 years" = Prob_5,
                   "Probability for violent revedivism after 5 years" = Prob_12,
                   "Number of missing items" = Num_Missing,
                   "Items" = list(
                     "Item 1" = Item_1,
                     "Item 2" = Item_2,
                     "Item 3" = Item_3,
                     "Item 4" = Item_4,
                     "Item 5" = Item_5,
                     "Item 6" = Item_6,
                     "Item 7" = Item_7,
                     "Item 8" = Item_8,
                     "Item 9" = Item_9,
                     "Item 10" = Item_10,
                     "Item 11" = Item_11,
                     "Item 12" = Item_12))
  
  Out_list
  
}

#For data set
compute_vrag_data <- function(Data, Outcome = "Score", Shiny_Progress = FALSE) {
  
  #Packages
  if(Shiny_Progress) {require("shiny")}
  
  #Helper function for one row
  compute_vrag_row <- function(Row, Outcome) {
    
    Res_raw <- compute_vrag(Row[, 1],
                            Row[, 2],
                            Row[, 3], Row[, 4], Row[, 5], Row[, 6], Row[, 7], Row[, 8],
                            Row[, 9],
                            Row[, 10], Row[, 11], Row[, 12], Row[, 13],Row[, 14], Row[, 15], Row[, 16], Row[, 17], Row[, 18], Row[, 19],
                            Row[, 20], Row[, 21], Row[, 22], Row[, 23], Row[, 24], Row[, 25], Row[, 26], Row[, 27], Row[, 28], Row[, 29],
                            Row[, 30], Row[, 31], Row[, 32], Row[, 33], Row[, 34], Row[, 35], Row[, 36],
                            Row[, 37],
                            Row[, 38],
                            Row[, 39], Row[, 40], Row[, 41], Row[, 42], Row[, 43], Row[, 44], Row[, 45], Row[, 46], Row[, 47], Row[, 48],
                            Row[, 49], Row[, 50], Row[, 51], Row[, 52], Row[, 53], Row[, 54], Row[, 55], Row[, 56],
                            Row[, 57],
                            Row[, 58], Row[, 59], Row[, 60], Row[, 61], Row[, 62], Row[, 63], Row[, 64], Row[, 65], Row[, 66], Row[, 67],
                            Row[, 68], Row[, 69],
                            Row[, 70],
                            Row[, 71], Row[, 72], Row[, 73], Row[, 74],Row[, 75])
    
    Res <- Res_raw[[Outcome]]
    
    Res
    
  }
  
  Out_Data <- Data
  
  #Computation with progress bar
  if(Shiny_Progress) {
    
    withProgress(message = "Computing", value = 0, {
      
      N <- nrow(Data)*length(Outcome)
      Inc_idx <- ceiling(N/100)
      Run <- 0
      
      
      for(j in Outcome) {
        
        Out_Data[, j] <- rep(NA, times = nrow(Data))
        
        for(i in 1:nrow(Data)) {
          
          Out_Data[i, j] <- compute_vrag_row(Data[i,], j)
          
          Run <- Run + 1
          
          if(Run %% Inc_idx == 0) {setProgress(value = Run/N, detail = paste(as.character(round((Run/N) * 100, digits = 0)), "%"))}
          
        }
      }
      
    })
    
    
  }else{
    
    #Computation without progress bar
    for(j in Outcome) {
      
      Out_Data[, j] <- rep(NA, times = nrow(Data))
      
      for(i in 1:nrow(Data)) {
        
        Out_Data[i, j] <- compute_vrag_row(Data[i,], j)
        
      }
    }
  }
  
  #Export
  Out_Data
  
}



#Lookup Item 2
Lookup_item2 <- c("None", "Moderate", "Severe", NA)
names(Lookup_item2) <- c("No problems", 
                         "Slight or moderate discipline or attendance problems",
                         "Severe (i.e., frequent or serious) behavior or attendance problems (e.g., truancy or disruptive behavior that persisted over several years or resulted in expulsion)",
                         "No information")


#Lookup Item 4
Lookup_item4 <- c("No", "Yes", NA)
names(Lookup_item4) <- c("Never married", 
                         "Ever married (or lived common law in the same home for at least 6 months)",
                         "No information")

#Lookup Item 11
Lookup_item11 <- c("None", "Only14", "Other", "Female14", NA)
names(Lookup_item11) <- c("No known hands-on sex offenses",
                          "All known hands-on sex offense(s) exclusively against children under age 14",
                          "Committed hands-on sex offense(s) but not known to fit another category here",
                          "At least one hands-on sex offense against a female victim 14 or older",
                          "No information")



####################
###User Interface###
####################
ui <- navbarPage(
  
  #Application title
  "VRAG-R - Calculator",
  
  #Position of Navbar
  position = "fixed-top",
  collapsible = TRUE,
  
  
  #Tab for Single Calculation
  tabPanel(title = "Single calculation",
           
           div(style = "height:80px"),
           
           #Title
           h2("Single calculation"),
           
           p("Here you can compute the VRAG-R score for a single case. Please be aware that this application should only be used as a computation
               aid. To obtain information on how to administer the VRAG-R and how to score the individual items, please study the test manual and/or 
               attend an appropriate training event."),
           
           ###Item inputs###
           
           sidebarLayout(position = "right",
             
             mainPanel = mainPanel(width = 6,
               
               #Item 1
               h3("Item 1 - Parents"),
               
               p("Lived with both biological parents to age 16 (except for death
                of parent). Score no if offender did not live continuously with both biological
                parents until age 16 except for death of one or both
                parents. In case of death of parent(s), score as yes."),
               
               selectInput("item1", label = NULL, 
                           choices = list("No", "Yes", "No information"),
                           selected = "No information"),
               
               #Item 2
               h3("Item 2 - Elementary school"),
               
               p("Elementary school maladjustment (up to and including eighth grade)."),
               
               radioButtons("item2", label = NULL, 
                            choices = list("No problems", 
                                           "Slight or moderate discipline or attendance problems",
                                           "Severe (i.e., frequent or serious) behavior or attendance problems (e.g., truancy or disruptive behavior that persisted over several years or resulted in expulsion)",
                                           "No information"),
                            selected = "No information"),
               
               #Item 3
               h3("Item 3 - History of alcohol or drug problems"),
               
               p(div(HTML("Please choose which of the following can be answered <em>yes</em>."))),
               
               selectInput("item3_1", 
                           label = "Alcohol problem before age 18", 
                           choices = list("No", "Yes", "No information"),
                           selected = "No information"),
               selectInput("item3_2", 
                           label = "Illegal drug problem before age 18", 
                           choices = list("No", "Yes", "No information"),
                           selected = "No information"),
               selectInput("item3_3", 
                           label = "Alcohol involved in a prior offense", 
                           choices = list("No", "Yes", "No information"),
                           selected = "No information"),
               selectInput("item3_4", 
                           label = "Illegal drug involved in a prior offense", 
                           choices = list("No", "Yes", "No information"),
                           selected = "No information"),
               selectInput("item3_5", 
                           label = "Alcohol involved in current offense", 
                           choices = list("No", "Yes", "No information"),
                           selected = "No information"),
               selectInput("item3_6", 
                           label = "Illegal drug involved in current offense", 
                           choices = list("No", "Yes", "No information"),
                           selected = "No information"),            
               
               
               #Item 4
               h3("Item 4 - Marital status"),
               
               p("Marital status at time of index offense."),
               
               radioButtons("item4", label = NULL, 
                            choices = list("Ever married (or lived common law in the same home for at least 6 months)",
                                           "Never married",
                                           "No information"),
                            selected = "No information"),
               
               
               
               ),
             
             sidebarPanel = sidebarPanel(width = 6,
               
               #Sidebar Title
               h2("Results"),
               
               p("This section presents the results of the VRAG-R based on the answers above. Missing items are handeled as advised in the
            manual. This includes the prorating techniques for the items 3 and 12 as well as the prorating adjustment to the final
            score. However, as also advised by the manual, the final score is only calculated if no more than four items are missing."),
               
               #Outputs
               splitLayout(cellWidths = c("30%", "70%"),
                           verbatimTextOutput("Vrag_score"),
                           plotOutput("Vrag_percentile")),
               
               #Download
               h2("Download"),
               
               p("Here, you can download the plot."),
               
               splitLayout(cellWidths = c("50%", "50%"),
                           numericInput("plotwidth", label = "Width (in)", value = 20),
                           numericInput("plotdpi", label = "DPI", value = 300)),
               splitLayout(cellWidths = c("50%", "50%"),
                           numericInput("plotheight", label = "Height (in)", value = 10),
                           numericInput("plotfont", label = "Fontsize", value = 20)),
               selectInput("plotformat", 
                           label = "Format", 
                           choices = list("png", "jpeg", "pdf", "svg", "tiff"),
                           selected = "png"),
               downloadButton("plotdownload", label = "Download plot")
               
             )
           ),
          
           #Item 5
           h3("Item 5 - Nonviolent offenses"),
           
           p("Criminal history score for convictions or charges for nonviolent offenses before index offense 
               (from the Cormier–Lang system). Please provide the number of occurences for each offense category."),
           
           
           radioButtons("item5_info", 
                        label = "Any information on nonviolent criminal history?", 
                        choices = list("No information",
                                       "Information available")),
           
           conditionalPanel(condition = "input.item5_info == 'Information available'",
                            
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_1", label = "Robbery (bank, store)", value = 0),
                                        numericInput("item5_2", label = "Robbery (purse snatching)", value = 0),
                                        numericInput("item5_3", label = "Threatening (uttering threats)", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_4", label = "Arson and fire setting (church, house, barn)", value = 0),
                                        numericInput("item5_5", label = "Arson and fire setting (garbage can), false alarm of fire", value = 0),
                                        numericInput("item5_6", label = "Severe theft (includes car theft and possession of stolen property over a value of $5,000)", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_7", label = "Mischief to public or private property over a value of $5,000", value = 0),
                                        numericInput("item5_8", label = "Break and enter and commit indictable offense (burglary)", value = 0),
                                        numericInput("item5_9", label = "Break and enter (includes break and enter with intent to commit an offense)", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_10", label = "Theft (includes possession of stolen property under a value of $5,000)", value = 0),
                                        numericInput("item5_11", label = "Mischief to public or private property under a value of $5,000", value = 0),
                                        numericInput("item5_12", label = "Fraud (extortion, embezzlement)", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_13", label = "Fraud (forged check, impersonation)", value = 0),
                                        numericInput("item5_14", label = "Indecent exposure", value = 0),
                                        numericInput("item5_15", label = "Causing a disturbance", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_16", label = "Conspiracy", value = 0),
                                        numericInput("item5_17", label = "Contributing to juvenile delinquency", value = 0),
                                        numericInput("item5_18", label = "Criminal negligence, criminal harassment", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_19", label = "Criminal trespass, trespassing at night", value = 0),
                                        numericInput("item5_20", label = "Dangerous driving, impaired driving (driving while intoxicated)", value = 0),
                                        numericInput("item5_21", label = "Fail to provide necessities of life", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_22", label = "Obstructing peace officer (including resisting arrest), obstructing justice", value = 0),
                                        numericInput("item5_23", label = "Possession of housebreaking tools", value = 0),
                                        numericInput("item5_24", label = "Possession/misuse/careless use of/carrying a prohibited or restricted weapon", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item5_25", label = "Procuring or living on the avails of the prostitution of another person", value = 0),
                                        numericInput("item5_26", label = "Trafficking in narcotics", value = 0),
                                        numericInput("item5_27", label = "Wearing a disguise with the intent to commit an offense", value = 0))
                            
           ),
           
           #Item 6
           h3("Item 6 - Conditional release"),
           
           p("Failure on conditional release (includes parole violation or revocation, breach of or failure to comply 
                with recognizance or probation, bail violation, and any new charges, including the index offense, 
                while on a conditional release)."),
           
           selectInput("item6", label = NULL, 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           
           #Item 7
           h3("Item 7 - Age"),
           
           p("Age at index offense (at most recent birthday)."),
           
           radioButtons("item7_info", 
                        label = NULL, 
                        choices = list("No information",
                                       "Information available")),
           
           conditionalPanel(condition = "input.item7_info == 'Information available'",
                            numericInput("item7", label = NULL, value = 20)),
           
           #Item 8
           h3("Item 8 - Violent offenses"),
           
           p("Criminal history score for convictions or charges for violent offenses before index offense 
               (from the Cormier–Lang system). Please provide the number of occurences for each offense category."),
           
           radioButtons("item8_info", 
                        label = "Any information on violent criminal history?", 
                        choices = list("No information",
                                       "Information available")),
           
           conditionalPanel(condition = "input.item8_info == 'Information available'",
                            
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item8_1", label = "Homicide (murder, manslaughter, criminal negligence causing death)", value = 0),
                                        numericInput("item8_2", label = "Attempted murder", value = 0),
                                        numericInput("item8_3", label = "Kidnapping, abduction, and forcible confinement", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item8_4", label = "Aggravated assault, choking, administering a noxious substance", value = 0),
                                        numericInput("item8_5", label = "Wounding, assault CBH with intent to wound", value = 0),
                                        numericInput("item8_6", label = "Assault CBH, Criminal negligence CBH, Impaired driving CBH", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item8_7", label = "Assault with a weapon", value = 0),
                                        numericInput("item8_8", label = "Assault, assaulting a peace officer", value = 0),
                                        numericInput("item8_9", label = "Threatening with a weapon, dangerous use of/pointing firearm", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item8_10", label = "Aggravated sexual assault, sexual assault CBH", value = 0),
                                        numericInput("item8_11", label = "Sexual assault with a weapon", value = 0),
                                        numericInput("item8_12", label = "Sexual assault, gross indecency (vaginal or anal penetration; victim forced to fellate offender)", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item8_13", label = "Sexual assault (attempted rape, indecent assault)", value = 0),
                                        numericInput("item8_14", label = "Gross indecency (offender fellates or performs cunnilingus on victim)", value = 0),
                                        numericInput("item8_15", label = "Sexual assault (sexual interference, invitation to sexual touching)", value = 0)),
                            splitLayout(cellWidths = c("33%", "33%", "33%"),
                                        numericInput("item8_16", label = "Armed robbery (bank, store)", value = 0),
                                        numericInput("item8_17", label = "Robbery with violence", value = 0),
                                        numericInput("item8_18", label = "Armed robbery (not a bank or store)", value = 0))
                            
           ),
           
           #Item 9
           h3("Item 9 - Prior admissions to correctional institutions"),
           
           p("Number of prior admissions (of one day or more) to correctional
              institutions (youth detention center, jail, federal or provincial
              correctional facility) for offenses before the index offense. Do
              not count transfers from one facility to another, or overnight
              stays in jail unless part of a sentence or presentence custody."),
           
           radioButtons("item9_info", 
                        label = NULL, 
                        choices = list("No information",
                                       "Information available")),
           
           conditionalPanel(condition = "input.item9_info == 'Information available'",
                            numericInput("item9", label = NULL, value = 0)),
           
           #Item 10
           h3("Item 10 - Conduct disorder"),
           
           p("Pertaining to behavior before age 15."),
           p(div(HTML("Please choose which of the following can be answered <em>yes</em>."))),
           
           selectInput("item10_1", 
                       label = "Repeated truancy", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_2", 
                       label = "School suspension or expulsion", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_3", 
                       label = "Delinquency brought to the attention of juvenile court", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_4", 
                       label = "Running away from home at least twice", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_5", 
                       label = "Persistent lying", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_6", 
                       label = "Repeated sexual intercourse within a casual relationship", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_7", 
                       label = "Repeated drunkenness or substance abuse", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_8", 
                       label = "Thefts", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_9", 
                       label = "Vandalism", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_10", 
                       label = "School grades markedly below expectations in relation to estimated or known IQ", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_11", 
                       label = "Chronic violations at home and/or school other than truancy", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           selectInput("item10_12", 
                       label = "Involvement in fights", 
                       choices = list("No", "Yes", "No information"),
                       selected = "No information"),
           
           #Item 11
           h3("Item 11 - Sex offending history"),
           
           p("Consider entire history up to and including index offense,
              and count all offenses for which there is convincing evidence
              whether it resulted in a charge or conviction or not."),
           
           radioButtons("item11", label = NULL, 
                        choices = list("No known hands-on sex offenses",
                                       "All known hands-on sex offense(s) exclusively against children under age 14",
                                       "Committed hands-on sex offense(s) but not known to fit another category here",
                                       "At least one hands-on sex offense against a female victim 14 or older",
                                       "No information"),
                        selected = "No information"),
           
           #Item 12
           h3("Item 12 - Antisociality"),
           
           p("Score Facet 4 of Psychopathy Checklist—Revised (PCL–R)."),
           
           selectInput("item12_1", 
                       label = "Poor Behavioral Controls", 
                       choices = list("0", "1", "2", "No information"),
                       selected = "No information"),
           selectInput("item12_2", 
                       label = "Early Behavioral Problems", 
                       choices = list("0", "1", "2", "No information"),
                       selected = "No information"),
           selectInput("item12_3", 
                       label = "Juvenile Delinquency", 
                       choices = list("0", "1", "2", "No information"),
                       selected = "No information"),
           selectInput("item12_4", 
                       label = "Revocation of Conditional Release", 
                       choices = list("0", "1", "2", "No information"),
                       selected = "No information"),
           selectInput("item12_5", 
                       label = "Criminal Versatility", 
                       choices = list("0", "1", "2", "No information"),
                       selected = "No information"),
  
          div(style = "height:90px")),
  
  #Tab for calculation on data set
  tabPanel(title = "Calculation on dataset", 
           
           div(style = "height:80px"),
           
           #Title
           h2("Calculation on dataset"),
           
           #Instructions
           p("This section provides the possibility to calculate VRAG-R scores for a whole dataset in one go. Follow the instructions below to get your scores."),
           
           #Upload  
           h3("1. Data Upload"),
           
           p("Select the format of your data using the drop-down menu. The following formats are supported: ",
             a(".xlsx (Excel)", href = "https://www.rdocumentation.org/packages/readxl/versions/0.1.1/topics/read_excel"), ", ",
             a(".csv", href = "https://www.rdocumentation.org/packages/readr/versions/2.1.3/topics/read_delim"), ", ",
             a(".csv (European)", href = "https://www.rdocumentation.org/packages/readr/versions/2.1.3/topics/read_delim"), ", ",
             a(".txt", href = "https://www.rdocumentation.org/packages/readr/versions/2.1.3/topics/read_delim"), ", ",
             a(".sav (SPSS)", href = "https://www.rdocumentation.org/packages/haven/versions/2.5.1/topics/read_spss"), "."),
           
           selectInput("data_format", label = "Choose your data format", choices = list("xlsx (Excel)", "csv", "csv (European)", "txt", "sav (SPSS)")),
           
           p("Use this button to upload your data."),
           fileInput("vrag_input", label = "Upload dataset", accept = c(".xlsx", ".csv", ".txt", ".sav")),
           
           #Variable names
           h3("2. Choose variables"),
           
           p("The data must contain a variable for each possible data entry within the VRAG-R. Below, you can choose which variables from your dataset are to be used for the calculation. 
             Please give the exact variable name for each item. You find further information about the necessary format of the variables in parantheses. The categorical variables may only contain 
             the category names that are indicated here. In addition, the categorical variables and binary variables (No/Yes) must be provided as variables of character strings (no labelled numerics in SPSS). 
             Every item may also contain missing values (blank cells in Excel and correctly defined missings in SPSS)."),
           
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem1", "Item 1 - Parents (No/Yes)", value = "Item_1"),
                       textInput("nameitem2", "Item 2 - Elementary school (None/Moderate/Severe)", value = "Item_2")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem3_1", "Item 3 - Alcohol problems < 18 (No/Yes)", value = "Item_3_1"),
                       textInput("nameitem3_2", "Item 3 - Drug problems < 18 (No/Yes)", value = "Item_3_2"),
                       textInput("nameitem3_3", "Item 3 - Alcohol in prior offense (No/Yes)", value = "Item_3_3")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem3_4", "Item 3 - Drug in prior offense (No/Yes)", value = "Item_3_4"),
                       textInput("nameitem3_5", "Item 3 - Alcohol in current offense (No/Yes)", value = "Item_3_5"),
                       textInput("nameitem3_6", "Item 3 - Drug in current offense (No/Yes)", value = "Item_3_6")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem4", "Item 4 - Marital status (No/Yes)", value = "Item_4")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_1", "Item 5 - Robbery bank (Count)", value = "Item_5_1"),
                       textInput("nameitem5_2", "Item 5 - Robbery purse (Count)", value = "Item_5_2"),
                       textInput("nameitem5_3", "Item 5 - Threatening (Count)", value = "Item_5_3")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_4", "Item 5 - Arson house (Count)", value = "Item_5_4"),
                       textInput("nameitem5_5", "Item 5 - Arson garbage (Count)", value = "Item_5_5"),
                       textInput("nameitem5_6", "Item 5 - Severe theft (Count)", value = "Item_5_6")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_7", "Item 5 - Severe mischief (Count)", value = "Item_5_7"),
                       textInput("nameitem5_8", "Item 5 - Burglary (Count)", value = "Item_5_8"),
                       textInput("nameitem5_9", "Item 5 - Break and enter intent (Count)", value = "Item_5_9")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_10", "Item 5 - Theft (Count)", value = "Item_5_10"),
                       textInput("nameitem5_11", "Item 5 - Mischief (Count)", value = "Item_5_11"),
                       textInput("nameitem5_12", "Item 5 - Fraud extortion (Count)", value = "Item_5_12")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_13", "Item 5 - Fraud check (Count)", value = "Item_5_13"),
                       textInput("nameitem5_14", "Item 5 - Indecent exposure (Count)", value = "Item_5_14"),
                       textInput("nameitem5_15", "Item 5 - Disturbance (Count)", value = "Item_5_15")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_16", "Item 5 - Conspiracy (Count)", value = "Item_5_16"),
                       textInput("nameitem5_17", "Item 5 - Juvenile del. (Count)", value = "Item_5_17"),
                       textInput("nameitem5_18", "Item 5 - Negligence (Count)", value = "Item_5_18")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_19", "Item 5 - Trespassing (Count)", value = "Item_5_19"),
                       textInput("nameitem5_20", "Item 5 - Dangerous driving (Count)", value = "Item_5_20"),
                       textInput("nameitem5_21", "Item 5 - Fail to provide (Count)", value = "Item_5_21")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_22", "Item 5 - Obstructing officer (Count)", value = "Item_5_22"),
                       textInput("nameitem5_23", "Item 5 - Housebreaking tools (Count)", value = "Item_5_23"),
                       textInput("nameitem5_24", "Item 5 - Weapon (Count)", value = "Item_5_24")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem5_25", "Item 5 - Prostitution (Count)", value = "Item_5_25"),
                       textInput("nameitem5_26", "Item 5 - Narcotics (Count)", value = "Item_5_26"),
                       textInput("nameitem5_27", "Item 5 - Disguise (Count)", value = "Item_5_27")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem6", "Item 6 - Conditional release (No/Yes)", value = "Item_6"),
                       textInput("nameitem7", "Item 7 - Age (Age)", value = "Item_7")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem8_1", "Item 8 - Homicide (Count)", value = "Item_8_1"),
                       textInput("nameitem8_2", "Item 8 - Attempted murder (Count)", value = "Item_8_2"),
                       textInput("nameitem8_3", "Item 8 - Kidnapping (Count)", value = "Item_8_3")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem8_4", "Item 8 - Aggravated assault (Count)", value = "Item_8_4"),
                       textInput("nameitem8_5", "Item 8 - Wounding (Count)", value = "Item_8_5"),
                       textInput("nameitem8_6", "Item 8 - Assault CBH (Count)", value = "Item_8_6")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem8_7", "Item 8 - Assault with weapon (Count)", value = "Item_8_7"),
                       textInput("nameitem8_8", "Item 8 - Assault (Count)", value = "Item_8_8"),
                       textInput("nameitem8_9", "Item 8 - Threat with weapon (Count)", value = "Item_8_9")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem8_10", "Item 8 - Aggravated sex. assault (Count)", value = "Item_8_10"),
                       textInput("nameitem8_11", "Item 8 - Sex. assault with weapon (Count)", value = "Item_8_11"),
                       textInput("nameitem8_12", "Item 8 - Sex. assault penetration (Count)", value = "Item_8_12")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem8_13", "Item 8 - Sex. assault attemted rape (Count)", value = "Item_8_13"),
                       textInput("nameitem8_14", "Item 8 - Gross indecency (Count)", value = "Item_8_14"),
                       textInput("nameitem8_15", "Item 8 - Sex. assault interference (Count)", value = "Item_8_15")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem8_16", "Item 8 - Armed robbery bank/store (Count)", value = "Item_8_16"),
                       textInput("nameitem8_17", "Item 8 - Robbery with violence (Count)", value = "Item_8_17"),
                       textInput("nameitem8_18", "Item 8 - Armed robbery (Count)", value = "Item_8_18")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem9", "Item 9 - Prior admissions to correctional institutions (Count)", value = "Item_9")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem10_1", "Item 10 - Truancy (No/Yes)", value = "Item_10_1"),
                       textInput("nameitem10_2", "Item 10 - School suspension (No/Yes)", value = "Item_10_2"),
                       textInput("nameitem10_3", "Item 10 - Delinquency court (No/Yes)", value = "Item_10_3")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem10_4", "Item 10 - Running away (No/Yes)", value = "Item_10_4"),
                       textInput("nameitem10_5", "Item 10 - Lying (No/Yes)", value = "Item_10_5"),
                       textInput("nameitem10_6", "Item 10 - Sexual intercourse (No/Yes)", value = "Item_10_6")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem10_7", "Item 10 - Drunkenness/drugs (No/Yes)", value = "Item_10_7"),
                       textInput("nameitem10_8", "Item 10 - Theft (No/Yes)", value = "Item_10_8"),
                       textInput("nameitem10_9", "Item 10 - Vandalism (No/Yes)", value = "Item_10_9")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem10_10", "Item 10 - Bad school grades (No/Yes)", value = "Item_10_10"),
                       textInput("nameitem10_11", "Item 10 - Other violations (No/Yes)", value = "Item_10_11"),
                       textInput("nameitem10_12", "Item 10 - Fights (No/Yes)", value = "Item_10_12")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem11", "Item 11 - Sex offending history (None/Only14/Other/Female14)", value = "Item_11")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem12_1", "Item 12 - Behavioral controls (0-2)", value = "Item_12_1"),
                       textInput("nameitem12_2", "Item 12 - Early problems (0-2)", value = "Item_12_2"),
                       textInput("nameitem12_3", "Item 12 - Juvenile delinquency (0-2)", value = "Item_12_3")),
           splitLayout(cellWidths = c("33%", "33%", "33%"),
                       textInput("nameitem12_4", "Item 12 - Conditional release (0-2)", value = "Item_12_4"),
                       textInput("nameitem12_5", "Item 12 - Criminal versatility (0-2)", value = "Item_12_5")),
           
           
           #Compute Scores
           h3("3. Compute scores"),
           
           p("Use this button to compute the VRAG scores. This may take a moment (depending on the size of your data). 
              Observe the progress bar in the lower right-hand corner to check on the progress of the calculation."),
           
           actionButton("calc", "Calculate VRAG-R scores"),
           
           div(style = "height:30px"),
           
           #Diagnostics
           h3("4. Check the distributions"),
           
           p("The plot shows the distribution of the VRAG-Score within your sample. Use it to check if the computation was successful."),
           
           plotOutput("Distplot"),
           
           #Download
           h3("5. Download"),
           
           div(style = "border-style: solid; border-color: black; border-width: 2px; padding: 10px; border-radius: 8px;",
               p("This button can be used to save the computed VRAG-R scores as a .csv-file (European)."),
               downloadButton("downloadData", "Download")),
           
           div(style = "height:30px")
           
           
      ),
  
  #About the app
  tabPanel(title = "About",
           
           div(style = "height:70px"),
           
           p("This VRAG-R calculator is based on the German manual by", 
             a("Rettenberger, Hertz and Eher (2017)", href = "https://krimpub.krimz.de/frontdoor/deliver/index/docId/40/file/bm-online8.pdf"),  
             "and the original discription by ", 
             a("Harris, Rice, Quinsey and Cormier (2015).", href = "https://psycnet.apa.org/record/2014-38766-000")),
           p("Please do not upload more data than necessary and adhere to existing data protection regulations.
               This app is running on shinyapps.io. More information regarding this cloud service can be found", 
             a("here.", href = "https://docs.rstudio.com/shinyapps.io/security-and-compliance.html")),
           p("The R-functions that are used to calculate the VRAG-R scores in this app are accessible on  ", a("github.", href = "https://github.com/MertenNeumann/VRAGR_Calculator")),
           div(style = "height:30px"),
           p("Have Fun!"),
           p("Merten Neumann"),
           p(a("Researchgate", href = "https://www.researchgate.net/profile/Merten-Neumann"), ",",
             a("Scholar", href = "https://scholar.google.com/citations?user=QjfiK8gAAAAJ&hl=de"))
           
  )
  
)

##################
###Server logic###
##################

server <- function(input, output) {
  
  #Compute single score
  Result_list_single <- reactive({
    
    Item_1 <- na_if(input$item1, "No information")
    
    Item_2 <- unname(Lookup_item2[input$item2])
    
    Item_3_1 <- na_if(input$item3_1, "No information")
    Item_3_2 <- na_if(input$item3_2, "No information")
    Item_3_3 <- na_if(input$item3_3, "No information")
    Item_3_4 <- na_if(input$item3_4, "No information")
    Item_3_5 <- na_if(input$item3_5, "No information")
    Item_3_6 <- na_if(input$item3_6, "No information")
    
    Item_4 <- unname(Lookup_item4[input$item4])
    
    Item_5_1 <- ifelse(input$item5_info == "No information", NA, input$item5_1)
    Item_5_2 <- ifelse(input$item5_info == "No information", NA, input$item5_2)
    Item_5_3 <- ifelse(input$item5_info == "No information", NA, input$item5_3)
    Item_5_4 <- ifelse(input$item5_info == "No information", NA, input$item5_4)
    Item_5_5 <- ifelse(input$item5_info == "No information", NA, input$item5_5)
    Item_5_6 <- ifelse(input$item5_info == "No information", NA, input$item5_6)
    Item_5_7 <- ifelse(input$item5_info == "No information", NA, input$item5_7)
    Item_5_8 <- ifelse(input$item5_info == "No information", NA, input$item5_8)
    Item_5_9 <- ifelse(input$item5_info == "No information", NA, input$item5_9)
    Item_5_10 <- ifelse(input$item5_info == "No information", NA, input$item5_10)
    Item_5_11 <- ifelse(input$item5_info == "No information", NA, input$item5_11)
    Item_5_12 <- ifelse(input$item5_info == "No information", NA, input$item5_12)
    Item_5_13 <- ifelse(input$item5_info == "No information", NA, input$item5_13)
    Item_5_14 <- ifelse(input$item5_info == "No information", NA, input$item5_14)
    Item_5_15 <- ifelse(input$item5_info == "No information", NA, input$item5_15)
    Item_5_16 <- ifelse(input$item5_info == "No information", NA, input$item5_16)
    Item_5_17 <- ifelse(input$item5_info == "No information", NA, input$item5_17)
    Item_5_18 <- ifelse(input$item5_info == "No information", NA, input$item5_18)
    Item_5_19 <- ifelse(input$item5_info == "No information", NA, input$item5_19)
    Item_5_20 <- ifelse(input$item5_info == "No information", NA, input$item5_20)
    Item_5_21 <- ifelse(input$item5_info == "No information", NA, input$item5_21)
    Item_5_22 <- ifelse(input$item5_info == "No information", NA, input$item5_22)
    Item_5_23 <- ifelse(input$item5_info == "No information", NA, input$item5_23)
    Item_5_24 <- ifelse(input$item5_info == "No information", NA, input$item5_24)
    Item_5_25 <- ifelse(input$item5_info == "No information", NA, input$item5_25)
    Item_5_26 <- ifelse(input$item5_info == "No information", NA, input$item5_26)
    Item_5_27 <- ifelse(input$item5_info == "No information", NA, input$item5_27)
    
    Item_6 <- na_if(input$item6, "No information")
    
    Item_7 <- ifelse(input$item7_info == "No information", NA, input$item7)
    
    Item_8_1 <- ifelse(input$item8_info == "No information", NA, input$item8_1)
    Item_8_2 <- ifelse(input$item8_info == "No information", NA, input$item8_2)
    Item_8_3 <- ifelse(input$item8_info == "No information", NA, input$item8_3)
    Item_8_4 <- ifelse(input$item8_info == "No information", NA, input$item8_4)
    Item_8_5 <- ifelse(input$item8_info == "No information", NA, input$item8_5)
    Item_8_6 <- ifelse(input$item8_info == "No information", NA, input$item8_6)
    Item_8_7 <- ifelse(input$item8_info == "No information", NA, input$item8_7)
    Item_8_8 <- ifelse(input$item8_info == "No information", NA, input$item8_8)
    Item_8_9 <- ifelse(input$item8_info == "No information", NA, input$item8_9)
    Item_8_10 <- ifelse(input$item8_info == "No information", NA, input$item8_10)
    Item_8_11 <- ifelse(input$item8_info == "No information", NA, input$item8_11)
    Item_8_12 <- ifelse(input$item8_info == "No information", NA, input$item8_12)
    Item_8_13 <- ifelse(input$item8_info == "No information", NA, input$item8_13)
    Item_8_14 <- ifelse(input$item8_info == "No information", NA, input$item8_14)
    Item_8_15 <- ifelse(input$item8_info == "No information", NA, input$item8_15)
    Item_8_16 <- ifelse(input$item8_info == "No information", NA, input$item8_16)
    Item_8_17 <- ifelse(input$item8_info == "No information", NA, input$item8_17)
    Item_8_18 <- ifelse(input$item8_info == "No information", NA, input$item8_18)
    
    Item_9 <- ifelse(input$item9_info == "No information", NA, input$item9)
    
    Item_10_1 <- na_if(input$item10_1, "No information")
    Item_10_2 <- na_if(input$item10_2, "No information")
    Item_10_3 <- na_if(input$item10_3, "No information")
    Item_10_4 <- na_if(input$item10_4, "No information")
    Item_10_5 <- na_if(input$item10_5, "No information")
    Item_10_6 <- na_if(input$item10_6, "No information")
    Item_10_7 <- na_if(input$item10_7, "No information")
    Item_10_8 <- na_if(input$item10_8, "No information")
    Item_10_9 <- na_if(input$item10_9, "No information")
    Item_10_10 <- na_if(input$item10_10, "No information")
    Item_10_11 <- na_if(input$item10_11, "No information")
    Item_10_12 <- na_if(input$item10_12, "No information")
    
    Item_11 <- unname(Lookup_item11[input$item11])
    
    Item_12_1 <- as.numeric(na_if(input$item12_1, "No information"))
    Item_12_2 <- as.numeric(na_if(input$item12_2, "No information"))
    Item_12_3 <- as.numeric(na_if(input$item12_3, "No information"))
    Item_12_4 <- as.numeric(na_if(input$item12_4, "No information"))
    Item_12_5 <- as.numeric(na_if(input$item12_5, "No information"))
    
    Result <- compute_vrag(Item_1,
                           Item_2,
                           Item_3_1, Item_3_2, Item_3_3, Item_3_4, Item_3_5, Item_3_6,
                           Item_4,
                           Item_5_1,Item_5_2,Item_5_3,Item_5_4,Item_5_5, Item_5_6, Item_5_7, Item_5_8, Item_5_9, Item_5_10,
                           Item_5_11, Item_5_12, Item_5_13, Item_5_14, Item_5_15, Item_5_16, Item_5_17, Item_5_18, Item_5_19, Item_5_20,
                           Item_5_21, Item_5_22, Item_5_23, Item_5_24, Item_5_25, Item_5_26, Item_5_27,
                           Item_6,
                           Item_7,
                           Item_8_1, Item_8_2, Item_8_3, Item_8_4, Item_8_5, Item_8_6, Item_8_7, Item_8_8, Item_8_9, Item_8_10,
                           Item_8_11, Item_8_12, Item_8_13, Item_8_14, Item_8_15, Item_8_16, Item_8_17, Item_8_18,
                           Item_9,
                           Item_10_1, Item_10_2, Item_10_3, Item_10_4, Item_10_5, Item_10_6, Item_10_7, Item_10_8, Item_10_9, Item_10_10,
                           Item_10_11, Item_10_12,
                           Item_11,
                           Item_12_1, Item_12_2, Item_12_3, Item_12_4,Item_12_5)
    
    Result
    
  })
  
  #Build plot
  Result_plot <- reactive({
    
    Result <- Result_list_single()
    
    crit_value <- qnorm(Result[["Percentile"]]/100)
    
    Result_Plot <- ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) +
      stat_function(fun = dnorm,
                    geom = "line",
                    color = "black") +
      stat_function(fun = dnorm,
                    geom = "area",
                    fill = "grey",
                    xlim = c(-3.5, crit_value)) +
      geom_segment(x = crit_value, xend = crit_value,
                   y = 0, yend = dnorm(crit_value), 
                   color = "red") +
      annotate(geom = "text", x = 2.0, y = 0.3, label = paste(c('VRAG-R score = ', Result[['Score']], 
                                                                '\nRisk category = ', Result[['Risk Category']],
                                                                '\nPercentile = ', Result[['Percentile']],
                                                                '\nMissing items = ', Result[['Number of missing items']]),
                                                              collapse = ''),
               size = 5) +
      xlab("z score") +
      ylab("density") +
      theme_classic()+
      theme(text = element_text(size = 15))
    
    Result_Plot
    
  })
  
  #Display single results
  output$Vrag_score <- renderText({
    
    Result <- Result_list_single()
    
    paste(c("Main Results",
            "\nVRAG-R score = ", Result[["Score"]], 
            "\nRisk category = ", Result[["Risk Category"]],
            "\nPercentile = ", Result[["Percentile"]],
            "\nMissing items = ", Result[["Number of missing items"]],
            "\n",
            "\nItem Values",
            "\nItem 1 = ", Result[["Items"]][["Item 1"]],
            "\nItem 2 = ", Result[["Items"]][["Item 2"]],
            "\nItem 3 = ", Result[["Items"]][["Item 3"]],
            "\nItem 4 = ", Result[["Items"]][["Item 4"]],
            "\nItem 5 = ", Result[["Items"]][["Item 5"]],
            "\nItem 6 = ", Result[["Items"]][["Item 6"]],
            "\nItem 7 = ", Result[["Items"]][["Item 7"]],
            "\nItem 8 = ", Result[["Items"]][["Item 8"]],
            "\nItem 9 = ", Result[["Items"]][["Item 9"]],
            "\nItem 10 = ", Result[["Items"]][["Item 10"]],
            "\nItem 11 = ", Result[["Items"]][["Item 11"]],
            "\nItem 12 = ", Result[["Items"]][["Item 12"]]), collapse = "")
    
  })
  
  #Display plot
  output$Vrag_percentile <- renderPlot({
    
    Result_plot()
    
  })
  
  #Download plot
  output$plotdownload <- downloadHandler(
    
    filename = function() {paste("Vrag-", Sys.Date(), ".", input$plotformat, sep="")},
    content = function(file) {
      
      Result <- Result_list_single()
      
      Plot <- Result_plot() +
        theme(text = element_text(size = input$plotfont))
      
      Plot <- delete_layers(Plot, "GeomText")
      
      Plot <- Plot +
        annotate(geom = "text", x = 2.0, y = 0.3, label = paste(c('VRAG-R score = ', Result[['Score']], 
                                                                  '\nRisk category = ', Result[['Risk Category']],
                                                                  '\nPercentile = ', Result[['Percentile']],
                                                                  '\nMissing items = ', Result[['Number of missing items']]),
                                                                collapse = ''),
                 size = input$plotfont/3)
      
      ggsave(filename = file,
             plot = Plot,
             height = input$plotheight,
             width = input$plotwidth,
             dpi = input$plotdpi)
      
    }
  )
  
  #Compute VRAG on dataset
  Dist_risk <- eventReactive(input$calc, {
    
    req(input$vrag_input)
    
    Var_idx <- c(input$nameitem1,
                 input$nameitem2,
                 input$nameitem3_1, input$nameitem3_2, input$nameitem3_3, input$nameitem3_4, input$nameitem3_5, input$nameitem3_6,
                 input$nameitem4,
                 input$nameitem5_1,input$nameitem5_2,input$nameitem5_3,input$nameitem5_4,input$nameitem5_5, input$nameitem5_6, input$nameitem5_7, input$nameitem5_8, input$nameitem5_9, input$nameitem5_10,
                 input$nameitem5_11, input$nameitem5_12, input$nameitem5_13, input$nameitem5_14, input$nameitem5_15, input$nameitem5_16, input$nameitem5_17, input$nameitem5_18, input$nameitem5_19, input$nameitem5_20,
                 input$nameitem5_21, input$nameitem5_22, input$nameitem5_23, input$nameitem5_24, input$nameitem5_25, input$nameitem5_26, input$nameitem5_27,
                 input$nameitem6,
                 input$nameitem7,
                 input$nameitem8_1, input$nameitem8_2, input$nameitem8_3, input$nameitem8_4, input$nameitem8_5, input$nameitem8_6, input$nameitem8_7, input$nameitem8_8, input$nameitem8_9, input$nameitem8_10,
                 input$nameitem8_11, input$nameitem8_12, input$nameitem8_13, input$nameitem8_14, input$nameitem8_15, input$nameitem8_16, input$nameitem8_17, input$nameitem8_18,
                 input$nameitem9,
                 input$nameitem10_1, input$nameitem10_2, input$nameitem10_3, input$nameitem10_4, input$nameitem10_5, input$nameitem10_6, input$nameitem10_7, input$nameitem10_8, input$nameitem10_9, input$nameitem10_10,
                 input$nameitem10_11, input$nameitem10_12,
                 input$nameitem11,
                 input$nameitem12_1, input$nameitem12_2, input$nameitem12_3, input$nameitem12_4,input$nameitem12_5)
    
    #xlsx
    if(input$data_format == "xlsx (Excel)") {
      
      vragData_raw <- read_excel(input$vrag_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(vragData_raw) <- c("Item_1",
                               "Item_2",
                               "Item_3_1", "Item_3_2", "Item_3_3", "Item_3_4", "Item_3_5", "Item_3_6",
                               "Item_4",
                               "Item_5_1", "Item_5_2", "Item_5_3", "Item_5_4", "Item_5_5", "Item_5_6", "Item_5_7", "Item_5_8", "Item_5_9", "Item_5_10",
                               "Item_5_11", "Item_5_12", "Item_5_13", "Item_5_14", "Item_5_15", "Item_5_16", "Item_5_17", "Item_5_18", "Item_5_19", "Item_5_20",
                               "Item_5_21", "Item_5_22", "Item_5_23", "Item_5_24", "Item_5_25", "Item_5_26", "Item_5_27",
                               "Item_6",
                               "Item_7",
                               "Item_8_1", "Item_8_2", "Item_8_3", "Item_8_4", "Item_8_5", "Item_8_6", "Item_8_7", "Item_8_8", "Item_8_9", "Item_8_10",
                               "Item_8_11", "Item_8_12", "Item_8_13", "Item_8_14", "Item_8_15", "Item_8_16", "Item_8_17", "Item_8_18",
                               "Item_9",
                               "Item_10_1", "Item_10_2", "Item_10_3", "Item_10_4", "Item_10_5", "Item_10_6", "Item_10_7", "Item_10_8", "Item_10_9", "Item_10_10",
                               "Item_10_11", "Item_10_12",
                               "Item_11",
                               "Item_12_1", "Item_12_2", "Item_12_3", "Item_12_4", "Item_12_5")
      
    }
    
    #csv
    if(input$data_format == "csv") {
      
      vragData_raw <- read_csv(input$vrag_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(vragData_raw) <- c("Item_1",
                               "Item_2",
                               "Item_3_1", "Item_3_2", "Item_3_3", "Item_3_4", "Item_3_5", "Item_3_6",
                               "Item_4",
                               "Item_5_1", "Item_5_2", "Item_5_3", "Item_5_4", "Item_5_5", "Item_5_6", "Item_5_7", "Item_5_8", "Item_5_9", "Item_5_10",
                               "Item_5_11", "Item_5_12", "Item_5_13", "Item_5_14", "Item_5_15", "Item_5_16", "Item_5_17", "Item_5_18", "Item_5_19", "Item_5_20",
                               "Item_5_21", "Item_5_22", "Item_5_23", "Item_5_24", "Item_5_25", "Item_5_26", "Item_5_27",
                               "Item_6",
                               "Item_7",
                               "Item_8_1", "Item_8_2", "Item_8_3", "Item_8_4", "Item_8_5", "Item_8_6", "Item_8_7", "Item_8_8", "Item_8_9", "Item_8_10",
                               "Item_8_11", "Item_8_12", "Item_8_13", "Item_8_14", "Item_8_15", "Item_8_16", "Item_8_17", "Item_8_18",
                               "Item_9",
                               "Item_10_1", "Item_10_2", "Item_10_3", "Item_10_4", "Item_10_5", "Item_10_6", "Item_10_7", "Item_10_8", "Item_10_9", "Item_10_10",
                               "Item_10_11", "Item_10_12",
                               "Item_11",
                               "Item_12_1", "Item_12_2", "Item_12_3", "Item_12_4", "Item_12_5")
      
    }
    
    #csv2
    if(input$data_format == "csv (European)") {
      
      vragData_raw <- read_csv2(input$vrag_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(vragData_raw) <- c("Item_1",
                               "Item_2",
                               "Item_3_1", "Item_3_2", "Item_3_3", "Item_3_4", "Item_3_5", "Item_3_6",
                               "Item_4",
                               "Item_5_1", "Item_5_2", "Item_5_3", "Item_5_4", "Item_5_5", "Item_5_6", "Item_5_7", "Item_5_8", "Item_5_9", "Item_5_10",
                               "Item_5_11", "Item_5_12", "Item_5_13", "Item_5_14", "Item_5_15", "Item_5_16", "Item_5_17", "Item_5_18", "Item_5_19", "Item_5_20",
                               "Item_5_21", "Item_5_22", "Item_5_23", "Item_5_24", "Item_5_25", "Item_5_26", "Item_5_27",
                               "Item_6",
                               "Item_7",
                               "Item_8_1", "Item_8_2", "Item_8_3", "Item_8_4", "Item_8_5", "Item_8_6", "Item_8_7", "Item_8_8", "Item_8_9", "Item_8_10",
                               "Item_8_11", "Item_8_12", "Item_8_13", "Item_8_14", "Item_8_15", "Item_8_16", "Item_8_17", "Item_8_18",
                               "Item_9",
                               "Item_10_1", "Item_10_2", "Item_10_3", "Item_10_4", "Item_10_5", "Item_10_6", "Item_10_7", "Item_10_8", "Item_10_9", "Item_10_10",
                               "Item_10_11", "Item_10_12",
                               "Item_11",
                               "Item_12_1", "Item_12_2", "Item_12_3", "Item_12_4", "Item_12_5")
      
    }
    
    #txt
    if(input$data_format == "txt") {
      
      vragData_raw <- read_delim(input$vrag_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(vragData_raw) <- c("Item_1",
                               "Item_2",
                               "Item_3_1", "Item_3_2", "Item_3_3", "Item_3_4", "Item_3_5", "Item_3_6",
                               "Item_4",
                               "Item_5_1", "Item_5_2", "Item_5_3", "Item_5_4", "Item_5_5", "Item_5_6", "Item_5_7", "Item_5_8", "Item_5_9", "Item_5_10",
                               "Item_5_11", "Item_5_12", "Item_5_13", "Item_5_14", "Item_5_15", "Item_5_16", "Item_5_17", "Item_5_18", "Item_5_19", "Item_5_20",
                               "Item_5_21", "Item_5_22", "Item_5_23", "Item_5_24", "Item_5_25", "Item_5_26", "Item_5_27",
                               "Item_6",
                               "Item_7",
                               "Item_8_1", "Item_8_2", "Item_8_3", "Item_8_4", "Item_8_5", "Item_8_6", "Item_8_7", "Item_8_8", "Item_8_9", "Item_8_10",
                               "Item_8_11", "Item_8_12", "Item_8_13", "Item_8_14", "Item_8_15", "Item_8_16", "Item_8_17", "Item_8_18",
                               "Item_9",
                               "Item_10_1", "Item_10_2", "Item_10_3", "Item_10_4", "Item_10_5", "Item_10_6", "Item_10_7", "Item_10_8", "Item_10_9", "Item_10_10",
                               "Item_10_11", "Item_10_12",
                               "Item_11",
                               "Item_12_1", "Item_12_2", "Item_12_3", "Item_12_4", "Item_12_5")
      
    }
    
    #sav  
    if(input$data_format == "sav (SPSS)") {
      
      vragData_raw <- read_sav(input$vrag_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(vragData_raw) <- c("Item_1",
                               "Item_2",
                               "Item_3_1", "Item_3_2", "Item_3_3", "Item_3_4", "Item_3_5", "Item_3_6",
                               "Item_4",
                               "Item_5_1", "Item_5_2", "Item_5_3", "Item_5_4", "Item_5_5", "Item_5_6", "Item_5_7", "Item_5_8", "Item_5_9", "Item_5_10",
                               "Item_5_11", "Item_5_12", "Item_5_13", "Item_5_14", "Item_5_15", "Item_5_16", "Item_5_17", "Item_5_18", "Item_5_19", "Item_5_20",
                               "Item_5_21", "Item_5_22", "Item_5_23", "Item_5_24", "Item_5_25", "Item_5_26", "Item_5_27",
                               "Item_6",
                               "Item_7",
                               "Item_8_1", "Item_8_2", "Item_8_3", "Item_8_4", "Item_8_5", "Item_8_6", "Item_8_7", "Item_8_8", "Item_8_9", "Item_8_10",
                               "Item_8_11", "Item_8_12", "Item_8_13", "Item_8_14", "Item_8_15", "Item_8_16", "Item_8_17", "Item_8_18",
                               "Item_9",
                               "Item_10_1", "Item_10_2", "Item_10_3", "Item_10_4", "Item_10_5", "Item_10_6", "Item_10_7", "Item_10_8", "Item_10_9", "Item_10_10",
                               "Item_10_11", "Item_10_12",
                               "Item_11",
                               "Item_12_1", "Item_12_2", "Item_12_3", "Item_12_4", "Item_12_5")
      
    }
    
    #Compute scores
    vragData_out <- compute_vrag_data(Data = vragData_raw, 
                                      Outcome = c("Score", "Risk Category", "Number of missing items"),
                                      Shiny_Progress = TRUE)
    
    vragData_out
    
    
  })
  
  #Display distribution of computed scores
  output$Distplot <- renderPlot({
    
    Data_Plot <- Dist_risk()
    
    ggplot(data = Data_Plot, aes_string(x = "Score")) +
      geom_histogram(aes(y = ..density..), fill = "lightgrey", color = "black") +
      geom_density(color = "red") +
      annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 7, 
               label = paste0("N = ", nrow(Data_Plot), "\n",
                              "NA = ", sum(is.na(Data_Plot[, "Score"])), " (", round((sum(is.na(Data_Plot[, "Score"]))/nrow(Data_Plot)) * 100, digits = 1), "%)", "\n",
                              "M = ", round(mean(Data_Plot[, "Score"], na.rm = TRUE), digits = 2), "\n",
                              "Med = ", round(median(Data_Plot[, "Score"], na.rm = TRUE), digits = 2), "\n",
                              "SD = ", round(sd(Data_Plot[, "Score"], na.rm = TRUE), digits = 2))) +
      xlab("Score") +
      theme_classic(base_size = 18)
    
  })
  
  #Download dataset
  output$downloadData <- downloadHandler(
    filename = "VRAG_Output.csv",
    content = function(file) {
      
      req(input$vrag_input)
      
      Data_Download <- Dist_risk()
      
      write.csv2(Data_Download, file, row.names = FALSE)
      
    })
  
  
}

#########################
###Run the application###
#########################

shinyApp(ui = ui, server = server)
