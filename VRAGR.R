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

###For single case###
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
                   "Probability for violent recidivism after 5 years" = Prob_5,
                   "Probability for violent recidivism after 12 years" = Prob_12,
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

###For data set###
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


###Test functions###

#Simulate single case
sim_vrag_case <- function() {
  
  require(MASS)
  
  Item_1 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_2 <<- sample(c("None", "Moderate", "Severe"), size = 1, replace = TRUE)
  Item_3_1 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_3_2 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_3_3 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_3_4 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_3_5 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_3_6 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_4 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_5_1 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_2 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_3 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_4 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_5 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_6 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_7 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_8 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_9 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_10 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_11 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_12 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_13 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_14 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_15 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_16 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_17 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_18 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_19 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_20 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_21 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_22 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_23 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_24 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_25 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_26 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_5_27 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_6 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_7 <<- sample(c(14:90), size = 1, replace = TRUE)
  Item_8_1 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_2 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_3 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_4 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_5 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_6 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_7 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_8 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_9 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_10 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_11 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_12 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_13 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_14 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_15 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_16 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_17 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_8_18 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_9 <<- rnegbin(1, mu = 0.3, theta = 5)
  Item_10_1 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_2 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_3 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_4 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_5 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_6 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_7 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_8 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_9 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_10 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_11 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_10_12 <<- sample(c("No", "Yes"), size = 1, replace = TRUE)
  Item_11 <<- sample(c("None", "Only14", "Other", "Female14"), size = 1, replace = TRUE)
  Item_12_1 <<- sample(c(0:2), size = 1, replace = TRUE)
  Item_12_2 <<- sample(c(0:2), size = 1, replace = TRUE)
  Item_12_3 <<- sample(c(0:2), size = 1, replace = TRUE)
  Item_12_4 <<- sample(c(0:2), size = 1, replace = TRUE)
  Item_12_5 <<- sample(c(0:2), size = 1, replace = TRUE)
  
}

#Simulate data set
sim_vrag_data <- function(N, prob_missing = 0.1) {
  
  require(MASS)
  
  infuse_missing <- function(Data, p_missing = 0.1) {
    
    Missing_Mat <- matrix(data = rep(FALSE, times = prod(dim(Data))), 
                          nrow = nrow(Data),
                          ncol = ncol(Data))
    
    for(j in 1:ncol(Missing_Mat)) {
      
      Missing_Mat[ ,j] <- sample(c(FALSE, TRUE), 
                                 size = nrow(Missing_Mat), 
                                 replace = TRUE, 
                                 prob = c(1 - p_missing, p_missing))
      
    }
    
    Data[Missing_Mat] <- NA
    
    Data
    
    
  }
  
  Out_Data <- data.frame(Item_1 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_2 = sample(c("None", "Moderate", "Severe"), size = N, replace = TRUE),
                         Item_3_1 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_3_2 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_3_3 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_3_4 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_3_5 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_3_6 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_4 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_5_1 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_2 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_3 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_4 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_5 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_6 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_7 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_8 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_9 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_10 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_11 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_12 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_13 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_14 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_15 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_16 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_17 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_18 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_19 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_20 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_21 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_22 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_23 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_24 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_25 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_26 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_5_27 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_6 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_7 = sample(c(14:90), size = N, replace = TRUE),
                         Item_8_1 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_2 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_3 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_4 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_5 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_6 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_7 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_8 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_9 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_10 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_11 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_12 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_13 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_14 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_15 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_16 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_17 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_8_18 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_9 = rnegbin(N, mu = 0.3, theta = 5),
                         Item_10_1 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_2 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_3 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_4 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_5 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_6 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_7 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_8 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_9 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_10 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_11 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_10_12 = sample(c("No", "Yes"), size = N, replace = TRUE),
                         Item_11 = sample(c("None", "Only14", "Other", "Female14"), size = N, replace = TRUE),
                         Item_12_1 = sample(c(0:2), size = N, replace = TRUE),
                         Item_12_2 = sample(c(0:2), size = N, replace = TRUE),
                         Item_12_3 = sample(c(0:2), size = N, replace = TRUE),
                         Item_12_4 = sample(c(0:2), size = N, replace = TRUE),
                         Item_12_5 = sample(c(0:2), size = N, replace = TRUE))
  
  Out_Data <- infuse_missing(Out_Data, p_missing = prob_missing)
  
  Out_Data
  
}
