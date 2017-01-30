### Preparing Test file for submission
test<-read.csv("./Data/test.csv")
test_processed <- predict(preProcValues, test)
sum(is.na(test_processed))
Loan_ID<-as.character(test_processed$Loan_ID)
test_processed$Loan_ID<-NULL


## Process and one hot encode Test Data
dmy_test <- dummyVars(" ~ .", data = test_processed,fullRank = T,sep = "_")
test_transformed <- data.frame(predict(dmy_test, newdata = test_processed))
numeric_cols_test<-c("ApplicantIncome","CoapplicantIncome","LoanAmount","Loan_Amount_Term","Credit_History")
test_fact<-lapply(test_transformed[,!names(test_transformed) %in% numeric_cols_test],as.factor)
test_transformed<-cbind(Loan_ID,data.frame(test_fact),test_transformed[,numeric_cols_test])
colnames(test_transformed)<-sub(make.names(names(test_transformed)),pattern = "[.]",replacement = "")
test_transformed$Loan_ID<-as.character(test_transformed$Loan_ID)

testing<-test_transformed
testing$Married_No<-as.factor(ifelse(testing$Married_Yes==1,0,1))
str(testing)


## Predict using the XGBoost Model
predict_submit_xg<-predict(model_xgb,data.matrix(testing[,predictors]),type="raw")
predict_submit_xg<-ifelse(predict_submit_xg=="Yes","Y","N")
table(predict_submit_xg)

final<-data.frame("Loan_ID"=testing$Loan_ID,"Loan_Status"=predict_submit_xg)
write.csv(x = final,file = "Sample_Submission_XGBoost.csv",row.names = F)

#   predict_submit
#   N   Y 
#   59 308


## Predict using the Ensemble Model
predict_submit_ens<-predict(greedy_ensemble,testing[,predictors],type="raw")
predict_submit_ens<-ifelse(predict_submit_ens=="Yes","Y","N")
table(predict_submit_ens)

final<-data.frame("Loan_ID"=testing$Loan_ID,"Loan_Status"=predict_submit_ens)
write.csv(x = final,file = "Sample_Submission_Ensemble.csv",row.names = F)

#   predict_submit
#   N   Y 
#   61 306


## Predict using the Stacking Model
predict_submit_stk<-predict(gbm_ensemble,testing[,predictors],type="raw")
predict_submit_stk<-ifelse(predict_submit_stk=="Yes","Y","N")
table(predict_submit_stk)

final<-data.frame("Loan_ID"=testing$Loan_ID,"Loan_Status"=predict_submit_stk)
write.csv(x = final,file = "Sample_Submission_Stacking.csv",row.names = F)

#   predict_submit
#   N   Y 
#   59 308
