pacman::p_load(e1071)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3, 
                        summaryFunction = mnLogLoss,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 1000,
                        eta = c(0.001,0.01,0.02),
                        max_depth = c(2,4,6),
                        gamma = c(0.01,0.1),
                        colsample_bytree = c(0.4,0.6,0.8),
                        min_child_weight = c(0,0.5,1),
                        subsample = c(0.5,0.75,1)
)
set.seed(45)
xgb_tune <-train(result~.,
                 data=train[,4:128],
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 metric="logLoss",
                 nthread =3
)

options("max.print"=100000)
min(xgb_tune$results$logLoss)
#Get row with mimnimum logloss
xgb_tune$results[which.min(xgb_tune$results$logLoss),]

#transfer to params
params.tune <- list(booster = "gbtree", objective = "binary:logistic", tree_method="hist", 
               eta=0.01, gamma=0.01, max_depth=4, min_child_weight=1, subsample=1, colsample_bytree=1)

xgb2 <- xgb.train(params = params.tune, data = dtrain, nrounds = 1000, watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "logloss")

min(xgb2$evaluation_log$val_logloss)
xgb2$evaluation_log[which.min(xgb2$evaluation_log$val_logloss),]

xgb2 <- xgb.train(params = params.tune, data = dtrain, nrounds = 767, watchlist = list(val=dtest,train=dtrain), 
                  print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "logloss")


#validate test error
xgpred2 <- predict(xgb2,dtest)
MultiLogLoss(test$result,xgpred2)
#confusionMatrix(xgpred1,test$result)



                      