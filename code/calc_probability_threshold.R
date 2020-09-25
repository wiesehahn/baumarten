## ---------------------------
##
## Script name: calc_probability_threshold.R
##
## Purpose of script: calculate probability threshold below which random forest classifications are more likely to be false than correct
##
## Author:  Jens Wiesehahn
##
## Date Created: 2020-09-17
##
## Email: jens.wiesehahn@nw-fva.de
##
## ---------------------------
##
## Notes: outcome is a data.frame named 'threshold' including two values (probability threshold and data proportion below threshold (cutoff))
##
## ---------------------------

library(here)
library(dplyr)
library(tidyr)
library(groupdata2)
library(randomForest)


#### load and filter reference data ####
##----------------------------------------------------------------------------------------------------------------------------------------------------
  train_test <- read.csv(here("data/reference/train_test/S2B_MSIL2A_20170823T103019_N0205_R108_T32UNC_20170823T103018_Dsen2_TopCorSlope10K06ndvi.csv"))

  # add unique polygon id
  train_test <- train_test %>% unite("PolygonID", c("Gebiet","ID"), remove = FALSE)

  # as factor
  cols <- c("ID", "BA", "Baumart", "wbz", "Gebiet", "PolygonID")
  train_test[cols] <- lapply(train_test[cols], as.factor)
  levels(train_test$Baumart) <- c("BU", "DGL", "FI", "KI", "LAE", "TEI")

  # rename
  ref <- train_test %>%
    rename_with(~ gsub("S2B_MSIL2A_20170823T103019_N0205_R108_T32UNC_20170823T103018_Dsen2_TopCorSlope10K06ndvi", "band", .x, fixed = TRUE))

  # subset
  ref <- ref %>%
    select(-ID, -X, -BA, - wbz, -Gebiet)
##----------------------------------------------------------------------------------------------------------------------------------------------------



  #### train and validate rf model ####
##----------------------------------------------------------------------------------------------------------------------------------------------------

  # model parameter
  ntree <- 500
  nodesize <- 1
  splitvariables <- 2
  # make reproducible
  set.seed(42)


  # using 5-fold reference data to create 5 times random forest model based on 4/5 of reference data and validate on 1/5
  # reference data is folded by entire polygons and balanced by tree species
  satData <- ref

  # fold data by polygons balanced by tree species
  kfold <- 5
  satData_folded <- fold(satData, kfold, cat_col = 'Baumart', id_col = 'PolygonID', method = 'n_dist')
  satData_folded <- satData_folded %>% ungroup()

  # create model formula
  predictors <- names(satData_folded %>% select(contains("band")))
  f <- as.formula(paste("Baumart", paste(predictors, collapse=" + "), sep=" ~ "))

  # init model loop
  vali.df <- data.frame()
  i <- 1
  invisible(gc())

  # loop through reference data folds: 5 x build model with trainset (4/5) and make predictions on testset (1/5)
  for (i in 1:kfold){

    vali  <- satData_folded %>% filter(.folds == i) %>% select(Baumart, contains("band"), contains("_VI"), PolygonID)
    traini<- satData_folded %>% filter(.folds != i) %>% select(Baumart, contains("band"), contains("_VI"))

    rf <- randomForest::randomForest(
      formula   = f,
      data      = traini,
      nodesize  = nodesize,
      mtry      = splitvariables,
      ntree = ntree,
      probability = T,
      xtest=vali %>% select(contains("band"), contains("_VI")),
      ytest=vali$Baumart,
      proximity=F, importance=F, keep.forest=T)

    pred <- predict(rf, vali, type="vote")

    pred.df <- as.data.frame(pred)
    vali.fold <- merge(vali %>% select(PolygonID, Baumart), pred.df, by=0, all=TRUE)
    vali.df <- rbind(vali.df, vali.fold)

    invisible(gc())
  }

  # add column for result
  vali.df <- vali.df %>%
    rename(reference = "Baumart") %>%
    mutate(prediction = colnames(vali.df[4:9])[max.col(vali.df[4:9], ties.method = "first")],
           result = if_else(reference != prediction,"incorrect", "correct"))


  vali.long.6class<- vali.df %>%
    reshape2::melt(id.vars = c("Row.names", "PolygonID", "reference", "prediction", "result"),
                   variable.name = "species",
                   value.name = "probability")
##----------------------------------------------------------------------------------------------------------------------------------------------------




  #### calculate threshold ####
##----------------------------------------------------------------------------------------------------------------------------------------------------

  # create column with 1 for cumulative counting
  vali.long.6class$rec <- 1

  # calculate cumulative count with increasing probability for correct predictions
  correct.count.6class <- vali.long.6class %>%
    filter(prediction == species, result == "correct") %>%
    arrange(probability) %>%
    mutate(count=cumsum(rec)) %>%
    select(-rec)

  # calculate cumulative count with increasing probability for incorrect predictions
  incorrect.count.6class <- vali.long.6class %>%
    filter(prediction == species, result == "incorrect") %>%
    arrange(probability) %>%
    mutate(count=cumsum(rec)) %>%
    select(-rec)

  # calculate probability threshold for which correct and incorrect classifications are equal
  threshold <- inner_join(correct.count.6class %>% select(probability, count),
                                  incorrect.count.6class %>% select(probability, count),
                                  by = c("probability")) %>%
    mutate(diff = count.x - count.y) %>%
    filter(diff == 0) %>%
    group_by(diff) %>%
    # calculate threshold as median of probabilites where correct and inorrect classifications are equal (can be many)
    summarize(threshold= median(probability), cutoff = 2*median(count.x)/nrow(vali.df)) %>%
    select(-diff)
##----------------------------------------------------------------------------------------------------------------------------------------------------

  print(threshold)
