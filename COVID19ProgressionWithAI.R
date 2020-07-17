################################################################################################
## Research topic: 
## Early identification of COVID-19 progression to severe illness using artificial intelligence
################################################################################################

setwd("D:\\work\\Data Analysis related\\2020\\CoronavirusPredictiveModel\\datasets")
dt.train <- readxl::read_xlsx("train.xlsx", 1)  # import dataset
dt.test <- readxl::read_xlsx("test.xlsx", 1)  # import dataset

#### 1. Features selection (Boruta algorithm) and model construction ####

library(Boruta)

## 1.1 Features selection for clinical variables only
vars.clinical <- names(dt.train)[c(3:27)]  # including 8 clinical factors & 17 laboratory indicators
fmla <- as.formula(paste("diseaseProgression ~ ", paste0("`",vars.clinical,"`",collapse = "+")))

set.seed(20200612) # change the seeds and peform the following code k times (k=5)
feature.selection <- Boruta(fmla, data = dt.train, doTrace = 1) 
feature.selection$timeTaken 
table(feature.selection$finalDecision) # confirmed, tentative, rejected
fNames <- getSelectedAttributes(feature.selection) # get the confirmed features'name
fNames
# re-run the above codes k times and record the confirmed features each time
# construct the clinical-based model (model-C) accordingly
model.C <- glm(diseaseProgression ~ albumin + urea, family = binomial(link = "logit"), data=dt.train)


## 1.2 Features selection for clinical variables and radiographic features from doctors
vars.radioDoc <- names(dt.train)[c(28:75)]  # 48 radiographic features extracted by radiologists
fmla <- as.formula(paste("diseaseProgression ~ ", paste0("`",c(vars.clinical,vars.radioDoc),"`",collapse = "+")))

set.seed(20200612) # change the seeds and peform the following code k times (k=5)
feature.selection <- Boruta(fmla, data = dt.train, doTrace = 1) 
feature.selection$timeTaken 
table(feature.selection$finalDecision) # confirmed, tentative, rejected
fNames <- getSelectedAttributes(feature.selection) # get the confirmed features'name
fNames
# re-run the above codes k times and record the confirmed features each time

Model.R.Doc <- glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL, family = binomial(link = "logit"), data=dt.train)
# LSinBSofRLLL: Lesion size in basal segment of RLLL


## 1.3 Features selection for clinical variables and radiographic features from AI
vars.radioAI <- names(dt.train)[c(76:201)]  # 126 radiographic features extracted by radiologists
fmla <- as.formula(paste("diseaseProgression ~ ", paste0("`",c(vars.clinical,vars.radioAI),"`",collapse = "+")))

set.seed(20200612) # change the seeds and peform the following code k times (k=5)
feature.selection <- Boruta(fmla, data = dt.train, doTrace = 1) 
feature.selection$timeTaken 
table(feature.selection$finalDecision) # confirmed, tentative, rejected
fNames <- getSelectedAttributes(feature.selection) # get the confirmed features'name
fNames
# re-run the above codes k times and record the confirmed features each time
# albumin, urea and percentage of CT values of [-200~600) in left lung was selected

# catergorize PCTLLn200T600 (percentage of CT values of [-200~600) in left lung) to keep variable in AI model to be as similar as it in Doctor-based model
quantile(dt.train$PCTLLn200T600)[3] # p50
dt.train$PCTLLn200T600.new <- factor(ifelse(dt.train$PCTLLn200T600 >= quantile(dt.train$PCTLLn200T600)[3], 
                                              "1", "0"), levels=c("0","1"))
dt.test$PCTLLn200T600.new <- factor(ifelse(dt.test$PCTLLn200T600 >= quantile(dt.train$PCTLLn200T600)[3], 
                                            "1", "0"), levels=c("0","1"))
table(dt.train$PCTLLn200T600.new)
table(dt.test$PCTLLn200T600.new)
Model.R.AI <- glm(diseaseProgression ~ albumin + urea + PCTLLn200T600.new, family = binomial(link = "logit"), data=dt.train)


## 1.4 Model-AI-Mimic-Doc
# We additionally established an AI-mimic-doctor model (Model-AI-Mimic-Doc) by using the same indicators (LSinBSofRLLL, catergorical variable) in Model-R-Doc 
# but the original radiographic feature was replaced by AI-derived values (LSinBSofRLLL.AI, continuous variable). 
# Deliberately we re-classed its value to keep the two models be comparable as much as possible. 
dt.train$LSinBSofRLLL.new <- factor(ifelse(dt.train$LSinBSofRLLL.AI == 0, "none", 
                                           ifelse(dt.train$LSinBSofRLLL.AI< 4*3.14*1.5*0.5*0.5/3,"<(1.5*0.5*0.5)cm3",">=(1.5*0.5*0.5)cm3")),
                                    levels=c("none","<(1.5*0.5*0.5)cm3",">=(1.5*0.5*0.5)cm3")) 
# Assume lesion size < 3cm extracted by doctor is similar to a ellipsoid whose voulme is 3*1*1 cm in diameter*major axis*minor axis deriven by AI
# Of note, the volume of ellipsoid is equal to 4*3.14*(diameter/2)*(major axis/2)*(minor axis/2)/3
table(dt.train$LSinBSofRLLL.new)

dt.test$LSinBSofRLLL.new <- factor(ifelse(dt.test$LSinBSofRLLL.AI == 0, "none", 
                                           ifelse(dt.test$LSinBSofRLLL.AI< 4*3.14*1.5*0.5*0.5/3,"<(1.5*0.5*0.5)cm3",">=(1.5*0.5*0.5)cm3")),
                                    levels=c("none","<(1.5*0.5*0.5)cm3",">=(1.5*0.5*0.5)cm3")) 
# Assume lesion size < 3cm extracted by doctor is similar to a ellipsoid whose voulme is 3*1*1 cm in diameter*major axis*minor axis deriven by AI
# Of note, the volume of ellipsoid is equal to 4*3.14*(diameter/2)*(major axis/2)*(minor axis/2)/3
table(dt.train$LSinBSofRLLL.new)
table(dt.test$LSinBSofRLLL.new)

Model.AI.Doc <- glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL.new, family = binomial(link = "logit"), data=dt.train)


#### 2. Summary of each model ####
model.C <- glm(diseaseProgression ~ albumin + urea, family = binomial(link = "logit"), data=dt.train)
Model.R.Doc <- glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL, family = binomial(link = "logit"), data=dt.train)
Model.R.AI <- glm(diseaseProgression ~ albumin + urea + PCTLLn200T600.new, family = binomial(link = "logit"), data=dt.train)
Model.AI.Doc <- glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL.new, family = binomial(link = "logit"), data=dt.train)

k <- length(summary(model.C)$coefficients[,1]) # re-run the following codes three times by changing model names
beta <- numeric()
OR <- as.character()
p <-  as.character()
varname <- as.character()
for (i in 1:k) {
  smry <- summary(model.C) # remember to change model names
  varname[i] <- rownames(smry$coefficients)[i]
  beta[i] <- sprintf("%.2f", smry$coefficients[i,1])
  OR[i] <- paste(sprintf("%.2f",exp(smry$coefficients[i,1])),"(", sprintf("%.2f",exp(smry$coefficients[i,1]-smry$coefficients[i,2]*1.96)),"-",
                 sprintf("%.2f",exp(smry$coefficients[i,1]+smry$coefficients[i,2]*1.96)),")",sep = "")
  pvalue <- smry$coefficients[i,4]
  
  if (pvalue < 0.001) {p[i] <- paste( sprintf("%.3f", pvalue), "***", sep = "")}   
  else if (pvalue < 0.01) {p[i] <- paste( sprintf("%.3f", pvalue), "**", sep = "")} 
  else if (pvalue < 0.05) {p[i] <- paste( sprintf("%.3f", pvalue), "*", sep = "")}    
  else {p[i] <- sprintf("%.3f", pvalue)}
}

d1 <- data.frame(varname,beta,OR,p)
colnames(d1) <- c("variables", "beta", "OR(95%CI)", "p value")
write.csv(d1,file = "D:\\work\\Data Analysis related\\2020\\CoronavirusPredictiveModel\\Results\\ResultsForPredictionOfSevereness\\multivariate1.1.csv")
# coefficients and 95%Ci for four models would then be stored (i.e Table 2 in manuscripts).


#### 3. ROC comparison (deLong Test)####
library(pROC)

## in training set 
modelroc <- list()
se <- as.character()
sp <- as.character()
aucAnd95CI <- as.character()
p <- as.character()

for (i in 1:4) {
    if (i==1) { fmla <- as.formula(diseaseProgression ~ albumin + urea)}
    else if (i==2) { fmla <- as.formula(diseaseProgression ~ albumin + urea + LSinBSofRLLL)}
    else if (i==3) { fmla <- as.formula(diseaseProgression ~ albumin + urea + PCTLLn200T600.new)}
    else if (i==4) { fmla <- as.formula(diseaseProgression ~ albumin + urea + LSinBSofRLLL.new)}
  
  model <- glm(fmla, family = binomial(link = "logit"), data=dt.train)
  smry <- summary(model)
  pre <- predict(model,dt.train) 
  pre <- pre-smry$coefficients[[1]] # minus intercept
  
  modelroc[[i]] <- roc(dt.train$diseaseProgression,pre)
  plt <- plot(modelroc[[i]], print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
              grid.col=c("green", "red"), max.auc.polygon=TRUE,
              auc.polygon.col="skyblue", print.thres=TRUE)
  mediancutoff[i] <- sprintf("%.3f", median(na.omit(plt$thresholds)))
  index <- which((plt$sensitivities + plt$specificities-1) == max(plt$sensitivities + plt$specificities-1))
  se[i] <- sprintf("%.3f", plt$sensitivities[index])
  sp[i] <- sprintf("%.3f", plt$specificities[index])
  
  auc <- auc(modelroc[[i]])
  ci <- ci(modelroc[[i]])
  aucAnd95CI[i] <- paste(sprintf("%.3f", auc),"(",sprintf("%.3f", ci)[1],"-",sprintf("%.3f", ci)[3],")",sep="")
  
  if (i==1) { p[i] <- "Ref"}
  else { 
    delongTest <- roc.test(modelroc[[i]],modelroc[[1]],method = 'delong')
    pvalue <- delongTest$p.value
    if (pvalue < 0.001) {p[i] <- paste( sprintf("%.3f", pvalue), "***", sep = "")}   
    else if (pvalue < 0.01) {p[i] <- paste( sprintf("%.3f", pvalue), "**", sep = "")} 
    else if (pvalue < 0.05) {p[i] <- paste( sprintf("%.3f", pvalue), "*", sep = "")}    
    else {p[i] <- sprintf("%.3f", pvalue)}
  }
}
modelname <- c("model.C","Model.R.Doc","Model.R.AI","Model.AI.Doc")
d2 <- data.frame(modelname,mediancutoff,se,sp,aucAnd95CI,p)
colnames(d2) <- c("Models", "Cutoff Value","Sensitivity", "Specificity", "AUC(95%CI)","P value")
write.csv(d2, row.names = F, file = "D:\\work\\Data Analysis related\\2020\\CoronavirusPredictiveModel\\Results\\ResultsForPredictionOfSevereness\\ModelComparison1.csv")


## in test set (using the same cutoff in training set)
modelroc <- list()
se <- as.character()
sp <- as.character()
aucAnd95CI <- as.character()
p <- as.character()

for (i in 1:4) {
  if (i==1) { fmla <- as.formula(diseaseProgression ~ albumin + urea)}
  else if (i==2) { fmla <- as.formula(diseaseProgression ~ albumin + urea + LSinBSofRLLL)}
  else if (i==3) { fmla <- as.formula(diseaseProgression ~ albumin + urea + PCTLLn200T600.new)}
  else if (i==4) { fmla <- as.formula(diseaseProgression ~ albumin + urea + LSinBSofRLLL.new)}
  
  model <- glm(fmla, family = binomial(link = "logit"), data=dt.train) # get coefficients from Training set
  
  smry <- summary(model)
  pre <- predict(model,dt.test) 
  pre <- pre-smry$coefficients[[1]] # minus intercept  
  modelroc[[i]] <- roc(dt.test$diseaseProgression,pre)
  plt <- plot(modelroc[[i]], print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
              grid.col=c("green", "red"), max.auc.polygon=TRUE,
              auc.polygon.col="skyblue", print.thres=TRUE)
  group <- factor(ifelse(pre < as.numeric(mediancutoff)[i], "noneProgressed","progressed"),
                  levels=c( "noneProgressed","progressed"))
  cfMtr <- caret::confusionMatrix(group, dt.test$diseaseProgression, positive="progressed")
  cfMtr$table[1,2]
  cfMtr$table[2,1]
  se[i] <- sprintf("%.3f", cfMtr$table[2,2]/(cfMtr$table[2,2]+cfMtr$table[1,2]))
  sp[i] <- sprintf("%.3f", cfMtr$table[1,1]/(cfMtr$table[1,1]+cfMtr$table[2,1]))
  
  auc <- auc(modelroc[[i]])
  ci <- ci(modelroc[[i]])
  aucAnd95CI[i] <- paste(sprintf("%.3f", auc),"(",sprintf("%.3f", ci)[1],"-",sprintf("%.3f", ci)[3],")",sep="")
  
  if (i==1) { p[i] <- "Ref"}
  else { 
    delongTest <- roc.test(modelroc[[i]],modelroc[[1]],method = 'delong')
    pvalue <- delongTest$p.value
    if (pvalue < 0.001) {p[i] <- paste( sprintf("%.3f", pvalue), "***", sep = "")}   
    else if (pvalue < 0.01) {p[i] <- paste( sprintf("%.3f", pvalue), "**", sep = "")} 
    else if (pvalue < 0.05) {p[i] <- paste( sprintf("%.3f", pvalue), "*", sep = "")}    
    else {p[i] <- sprintf("%.3f", pvalue)}
  }
}
modelname <- c("model.C","Model.R.Doc","Model.R.AI","Model.AI.Doc")

d2.1 <- data.frame(modelname,mediancutoff,se,sp,aucAnd95CI,p) # use median cutoff 
colnames(d2.1) <- c("Models", "Cutoff Value","Sensitivity", "Specificity", "AUC(95%CI)","P value")
write.csv(d2.1, row.names = F, file = "D:\\work\\Data Analysis related\\2020\\CoronavirusPredictiveModel\\Results\\ResultsForPredictionOfSevereness\\ModelComparisonin2.csv")

#### 4. Risk score building and visualization ####
library(ggplot2)
library(dplyr)
library(scales)

## in training set
mediancutoff1 <- as.numeric(mediancutoff)

model.C <- glm(diseaseProgression ~ albumin + urea, family = binomial(link = "logit"), data=dt.train)
Model.R.Doc <- glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL, family = binomial(link = "logit"), data=dt.train)
Model.R.AI <- glm(diseaseProgression ~ albumin + urea + PCTLLn200T600.new, family = binomial(link = "logit"), data=dt.train)
Model.AI.Doc <- glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL.new, family = binomial(link = "logit"), data=dt.train)

smry <- summary(model.C)
pre <- predict(model.C,dt.train) 
dt.train$riskScore <- pre-smry$coefficients[[1]] # minus intercept
dt.train.order <- dt.train[order(dt.train$riskScore),]  
fig.trnRS1 <- dt.train.order %>% dplyr::mutate(ids=row_number(dt.train.order$riskScore)) %>% 
  dplyr::mutate(ids=as.factor(ids), group=as.factor(diseaseProgression)) %>% 
  ggplot2::ggplot(aes(x=ids, y=(riskScore-mediancutoff1[1]), fill=group)) +
  geom_col(position = 'dodge', color="black") +
  scale_fill_manual(values = c("#FFCC33","#6699FF"), name=NULL, 
                    labels=c("mild-mild","mild-severe")) +
  labs(x = "", y = "",
       title=" Training set", font.title = c(25, "bold", "black"),
       subtitle = " a) Risk score for every patient based on Model-C", font.subtitlle = c(12, "plain", "black")) + 
  theme_bw() +   
  theme(axis.text.x  = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.border = element_blank()) +  
  theme(axis.line.y = element_line(size = 0.5)) + 
  geom_hline(aes(yintercept=0), colour="black", size = 0.5) + 
  theme(legend.position = c(0.03, 1),legend.justification = c(0, 1)) +
  theme(legend.background = element_blank()) +
  scale_y_continuous(labels = c(mediancutoff1[1]-2.5,mediancutoff1[1], mediancutoff1[1]+5,
                                mediancutoff1[1]+10, mediancutoff1[1]+15), breaks=c(-2.5,0,5,10,15))


smry <- summary(Model.R.Doc)
pre <- predict(Model.R.Doc,dt.train) 
dt.train$riskScore <- pre-smry$coefficients[[1]] # minus intercept
dt.train.order <- dt.train[order(dt.train$riskScore),] 
fig.trnRS2 <- dt.train.order %>% dplyr::mutate(ids=row_number(dt.train.order$riskScore)) %>% 
  dplyr::mutate(ids=as.factor(ids), group=as.factor(diseaseProgression)) %>% 
  ggplot2::ggplot(aes(x=ids, y=(riskScore-mediancutoff1[2]), fill=group)) +
  geom_col(position = 'dodge', color="black") +
  scale_fill_manual(values = c("#FFCC33","#6699FF"), name=NULL, 
                    labels=c("mild-mild","mild-severe")) +
  labs(x = "", y = "",
       subtitle = " b) Risk score for every patient based on Model-R-Doc", font.subtitlle = c(12, "plain", "black")) + 
  theme_bw() +   
  theme(axis.text.x  = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.border = element_blank()) +  
  theme(axis.line.y = element_line(size = 0.5)) + 
  geom_hline(aes(yintercept=0), colour="black", size = 0.5) + 
  theme(legend.position = c(0.03, 1),legend.justification = c(0, 1)) +
  theme(legend.background = element_blank()) +
  scale_y_continuous(labels = c(mediancutoff1[2]-2.5,mediancutoff1[2], mediancutoff1[2]+5,
                                mediancutoff1[2]+10, mediancutoff1[2]+15, mediancutoff1[2]+20), breaks=c(-2.5,0,5,10,15,20))


smry <- summary(Model.R.AI)
pre <- predict(Model.R.AI,dt.train) 
dt.train$riskScore <- pre-smry$coefficients[[1]] # minus intercept
dt.train.order <- dt.train[order(dt.train$riskScore),] 
fig.trnRS3 <- dt.train.order %>% dplyr::mutate(ids=row_number(dt.train.order$riskScore)) %>% 
  dplyr::mutate(ids=as.factor(ids), group=as.factor(diseaseProgression)) %>% 
  ggplot2::ggplot(aes(x=ids, y=(riskScore-mediancutoff1[3]), fill=group)) +
  geom_col(position = 'dodge', color="black") +
  scale_fill_manual(values = c("#FFCC33","#6699FF"), name=NULL, 
                    labels=c("mild-mild","mild-severe")) +
  labs(x = "", y = "",
       subtitle = " c) Risk score for every patient based on Model-AI-Mimic-Doc", font.subtitlle = c(12, "plain", "black")) + 
  theme_bw() +   
  theme(axis.text.x  = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.border = element_blank()) +  
  theme(axis.line.y = element_line(size = 0.5)) +
  geom_hline(aes(yintercept=0), colour="black", size = 0.5) + 
  theme(legend.position = c(0.03, 1),legend.justification = c(0, 1)) +
  theme(legend.background = element_blank()) +
  scale_y_continuous(labels = c(mediancutoff1[3]-2.5,mediancutoff1[3], mediancutoff1[3]+5,
                                mediancutoff1[3]+10, mediancutoff1[3]+15), breaks=c(-2.5,0,5,10,15))


smry <- summary(Model.AI.Doc)
pre <- predict(Model.AI.Doc,dt.train) 
dt.train$riskScore <- pre-smry$coefficients[[1]] # minus intercept
dt.train.order <- dt.train[order(dt.train$riskScore),] 
fig.trnRS4 <- dt.train.order %>% dplyr::mutate(ids=row_number(dt.train.order$riskScore)) %>% 
  dplyr::mutate(ids=as.factor(ids), group=as.factor(diseaseProgression)) %>% 
  ggplot2::ggplot(aes(x=ids, y=(riskScore-mediancutoff1[4]), fill=group)) +
  geom_col(position = 'dodge', color="black") +
  scale_fill_manual(values = c("#FFCC33","#6699FF"), name=NULL, 
                    labels=c("mild-mild","mild-severe")) +
  labs(x = "", y = "",
       subtitle = " d) Risk score for every patient based on Model-R-AI", font.subtitlle = c(12, "plain", "black")) + 
  theme_bw() +   
  theme(axis.text.x  = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.border = element_blank()) +  
  theme(axis.line.y = element_line(size = 0.5)) + 
  geom_hline(aes(yintercept=0), colour="black", size = 0.5) + 
  theme(legend.position = c(0.03, 1),legend.justification = c(0, 1)) +
  theme(legend.background = element_blank()) +
  scale_y_continuous(labels = c(mediancutoff1[4]-2.5,mediancutoff1[4], mediancutoff1[4]+5,
                                mediancutoff1[4]+10, mediancutoff1[4]+15, mediancutoff1[4]+20), breaks=c(-2.5,0,5,10,15,20))


## in test set

smry <- summary(model.C)
pre <- predict(model.C,dt.test) 
dt.test$riskScore <- pre-smry$coefficients[[1]] # minus intercept
dt.test.order <- dt.test[order(dt.test$riskScore),]  

fig.vldRS1 <- dt.test.order %>% dplyr::mutate(ids=row_number(dt.test.order$riskScore)) %>% 
  dplyr::mutate(ids=as.factor(ids), group=as.factor(diseaseProgression)) %>% 
  ggplot2::ggplot(aes(x=ids, y=(riskScore-mediancutoff1[1]), fill=group)) +
  geom_col(position = 'dodge', color="black") +
  scale_fill_manual(values = c("#FFCC33","#6699FF"), name=NULL, 
                    labels=c("mild-mild","mild-severe")) +
  labs(x = "", y = "",
       title=" Test set", font.title = c(25, "bold", "black"),
       subtitle = " e) Risk score for every patient based on Model-C", font.subtitlle = c(12, "plain", "black")) + 
  theme_bw() +   
  theme(axis.text.x  = element_blank()) +
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.border = element_blank()) +  
  theme(axis.line.y = element_line(size = 0.5)) + 
  geom_hline(aes(yintercept=0), colour="black", size = 0.5) + 
  theme(legend.position = c(0.03, 1),legend.justification = c(0, 1)) +
  theme(legend.background = element_blank()) +
  scale_y_continuous(labels = c(mediancutoff1[1]-2, mediancutoff1[1]-1, mediancutoff1[1],
                                mediancutoff1[1]+1, mediancutoff1[1]+2,mediancutoff1[1]+3), 
                     breaks=c(-2,-1,0,1,2,3))


smry <- summary(Model.R.Doc)
pre <- predict(Model.R.Doc,dt.test) 
dt.test$riskScore <- pre-smry$coefficients[[1]] # minus intercept
dt.test.order <- dt.test[order(dt.test$riskScore),]  
fig.vldRS2 <- dt.test.order %>% dplyr::mutate(ids=row_number(dt.test.order$riskScore)) %>% 
  dplyr::mutate(ids=as.factor(ids), group=as.factor(diseaseProgression)) %>% 
  ggplot2::ggplot(aes(x=ids, y=(riskScore-mediancutoff1[2]), fill=group)) +
  geom_col(position = 'dodge', color="black") +
  scale_fill_manual(values = c("#FFCC33","#6699FF"), name=NULL, 
                    labels=c("mild-mild","mild-severe")) +
  labs(x = "", y = "",
       subtitle = " f) Risk score for every patient based on Model-R-Doc", font.subtitlle = c(12, "plain", "black")) + 
  theme_bw() +   
  theme(axis.text.x  = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.border = element_blank()) +  
  theme(axis.line.y = element_line(size = 0.5)) + 
  geom_hline(aes(yintercept=0), colour="black", size = 0.5) + 
  theme(legend.position = c(0.03, 1),legend.justification = c(0, 1)) +
  theme(legend.background = element_blank()) +
  scale_y_continuous(labels = c(mediancutoff1[2]-2,mediancutoff1[2], mediancutoff1[2]+2,
                                mediancutoff1[2]+4), breaks=c(-2,0,2,4))


smry <- summary(Model.R.AI)
pre <- predict(Model.R.AI,dt.test) 
dt.test$riskScore <- pre-smry$coefficients[[1]] # minus intercept
dt.test.order <- dt.test[order(dt.test$riskScore),]  
fig.vldRS3 <- dt.test.order %>% dplyr::mutate(ids=row_number(dt.test.order$riskScore)) %>% 
  dplyr::mutate(ids=as.factor(ids), group=as.factor(diseaseProgression)) %>% 
  ggplot2::ggplot(aes(x=ids, y=(riskScore-mediancutoff1[3]), fill=group)) +
  geom_col(position = 'dodge', color="black") +
  scale_fill_manual(values = c("#FFCC33","#6699FF"), name=NULL, 
                    labels=c("mild-mild","mild-severe")) +
  labs(x = "", y = "",
       subtitle = " g) Risk score for every patient based on Model-AI-Mimic-Doc", font.subtitlle = c(12, "plain", "black")) + 
  theme_bw() +   
  theme(axis.text.x  = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.border = element_blank()) +  
  theme(axis.line.y = element_line(size = 0.5)) + 
  geom_hline(aes(yintercept=0), colour="black", size = 0.5) + 
  theme(legend.position = c(0.03, 1),legend.justification = c(0, 1)) +
  theme(legend.background = element_blank()) +
  scale_y_continuous(labels = c(mediancutoff1[3]-2,mediancutoff1[3]-1, mediancutoff1[3],
                                mediancutoff1[3]+1, mediancutoff1[3]+2), breaks=c(-2,-1,0,1,2))


smry <- summary(Model.AI.Doc)
pre <- predict(Model.AI.Doc,dt.test) 
dt.test$riskScore <- pre-smry$coefficients[[1]] # minus intercept
dt.test.order <- dt.test[order(dt.test$riskScore),] 
fig.vldRS4 <- dt.test.order %>% dplyr::mutate(ids=row_number(dt.test.order$riskScore)) %>% 
  dplyr::mutate(ids=as.factor(ids), group=as.factor(diseaseProgression)) %>% 
  ggplot2::ggplot(aes(x=ids, y=(riskScore-mediancutoff1[4]), fill=group)) +
  geom_col(position = 'dodge', color="black") +
  scale_fill_manual(values = c("#FFCC33","#6699FF"), name=NULL, 
                    labels=c("mild-mild","mild-severe")) +
  labs(x = "", y = "",
       subtitle = " h) Risk score for every patient based on Model-R-AI", font.subtitlle = c(12, "plain", "black")) + 
  theme_bw() +   
  theme(axis.text.x  = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.border = element_blank()) +  
  theme(axis.line.y = element_line(size = 0.5)) + 
  geom_hline(aes(yintercept=0), colour="black", size = 0.5) +
  theme(legend.position = c(0.03, 1),legend.justification = c(0, 1)) +
  theme(legend.background = element_blank()) +
  scale_y_continuous(labels = c(mediancutoff1[4]-2,mediancutoff1[4], mediancutoff1[4]+2,
                                mediancutoff1[4]+4), breaks=c(-2,0,2,4))

# to display all figures in one layout
lay1 <- customLayout::lay_new(mat = matrix(1:4, ncol = 4), widths = c(1,1,1,1)) 
lay1 <-customLayout::lay_bind_row(lay1,lay1,heights = c(6, 6), addmax = TRUE) 
customLayout::lay_grid(list(fig.trnRS1,fig.trnRS2,fig.trnRS3,fig.trnRS4,
                            fig.vldRS1,fig.vldRS2,fig.vldRS3,fig.vldRS4), lay1)  


#### 5. AUC (k-fold Cross Validation) ####
library(pROC)
library(lattice)
library(ggplot2)
library(plotROC)

# Number of iterations
k <- 5
# Accuracy
acc <- NULL

pre0 <- as.numeric()
ori0 <- as.numeric()
pre1 <- as.numeric()
ori1 <- as.numeric()
pre2 <- as.numeric()
ori2 <- as.numeric()
pre3 <- as.numeric()
ori3 <- as.numeric()

train_pre0 <- as.numeric()
train_pre1 <- as.numeric()
train_pre2 <- as.numeric()
train_pre3 <- as.numeric()
train_ori0 <- as.numeric()
train_ori1 <- as.numeric()
train_ori2 <- as.numeric()
train_ori3 <- as.numeric()

auc0<-as.numeric()
auc1<-as.numeric()
auc2<-as.numeric()
auc3<-as.numeric()

train_auc0 <- as.numeric()
train_auc1 <- as.numeric()
train_auc2 <- as.numeric()
train_auc3 <- as.numeric()
train_auc4 <- as.numeric()

set.seed(2019876)

for(i in 1:k)
{
  # Train-test splitting
  # 80% of samples -> fitting
  # 20% of samples -> testing
  smp_size <- floor(0.80 * nrow(dt.train))
  index <- sample(seq_len(nrow(dt.train)),size=smp_size)
  train <- dt.train[index, ]
  test <- dt.train[-index, ]
  
  # Fitting glm
  model0 <- glm(diseaseProgression ~ albumin + urea, family = binomial(link = "logit"), data=train)
  model1 <- glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL, 
                family = binomial(link = "logit"), data = train)
  model2 <- glm(diseaseProgression ~  albumin + urea + PCTLLn200T600.new,
                family = binomial(link = "logit"), data=train)
  model3 <-  glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL.new,
                 family = binomial(link = "logit"), data = train)
  
  # Predict results in dataset of "test"
  results_prob0 <- predict(model0, newdata=test, type='response')
  results_prob1 <- predict(model1, newdata=test, type='response')
  results_prob2 <- predict(model2, newdata=test, type='response')
  results_prob3 <- predict(model3, newdata=test, type='response')
  
  pre0 <- append(pre0,as.numeric(results_prob0))
  ori0 <- append(ori0,as.numeric(test$diseaseProgression))
  
  pre1 <- append(pre1,as.numeric(results_prob1))
  ori1 <- append(ori1,as.numeric(test$diseaseProgression))
  
  pre2 <- append(pre2,as.numeric(results_prob2))
  ori2 <- append(ori2,as.numeric(test$diseaseProgression))  
  
  pre3 <- append(pre3,as.numeric(results_prob3))
  ori3 <- append(ori3,as.numeric(test$diseaseProgression))
  
  # Predict results in dataset of "train"  
  train_prob0 <- predict(model0, type='response')
  train_prob1 <- predict(model1, type='response')
  train_prob2 <- predict(model2, type='response')
  train_prob3 <- predict(model3, type='response')
  
  train_pre0 <- append(train_pre0,as.numeric(train_prob0))
  train_pre1 <- append(train_pre1,as.numeric(train_prob1))
  train_pre2 <- append(train_pre2,as.numeric(train_prob2))
  train_pre3 <- append(train_pre3,as.numeric(train_prob3))
  
  train_ori0 <- append(train_ori0,as.numeric(train$diseaseProgression))
  train_ori1 <- append(train_ori1,as.numeric(train$diseaseProgression)) 
  train_ori2 <- append(train_ori2,as.numeric(train$diseaseProgression))
  train_ori3 <- append(train_ori3,as.numeric(train$diseaseProgression)) 
  
  # AUC
  auc0 <- append(auc0,as.numeric(auc(as.numeric(test$diseaseProgression),results_prob0)))
  auc1 <- append(auc1,as.numeric(auc(as.numeric(test$diseaseProgression),results_prob1)))
  auc2 <- append(auc2,as.numeric(auc(as.numeric(test$diseaseProgression),results_prob2)))
  auc3 <- append(auc3,as.numeric(auc(as.numeric(test$diseaseProgression),results_prob3)))
  
  train_auc0 <- append(train_auc0,as.numeric(auc(as.numeric(train$diseaseProgression),train_prob0)))
  train_auc1 <- append(train_auc1,as.numeric(auc(as.numeric(train$diseaseProgression),train_prob1)))
  train_auc2 <- append(train_auc2,as.numeric(auc(as.numeric(train$diseaseProgression),train_prob2)))
  train_auc3 <- append(train_auc3,as.numeric(auc(as.numeric(train$diseaseProgression),train_prob3)))
}

# AUC
auc.mdl1.train <- mean(train_auc0)
auc.mdl2.train <- mean(train_auc1)
auc.mdl3.train <- mean(train_auc2)
auc.mdl4.train <- mean(train_auc3)

auc.mdl1.test <- mean(auc0)
auc.mdl2.test <- mean(auc1)
auc.mdl3.test <- mean(auc2)
auc.mdl4.test <- mean(auc3)


# plot ROC for CV training set

ROCplot <-data.frame(c=1:length(train_ori0), train_ori0)
ROCplot0 <-data.frame(c=1:length(train_ori0), train_pre0)
ROCplot1 <-data.frame(c=1:length(train_ori0), train_pre1)
ROCplot2 <-data.frame(c=1:length(train_ori0), train_pre2)
ROCplot3 <-data.frame(c=1:length(train_ori0), train_pre3)

ROCplot <- merge(ROCplot, ROCplot0, by="c")
ROCplot <- merge(ROCplot, ROCplot1, by="c")
ROCplot <- merge(ROCplot, ROCplot2, by="c")
ROCplot <- merge(ROCplot, ROCplot3, by="c")

longtest <- melt_roc(ROCplot, "train_ori0", c("train_pre0", "train_pre1", "train_pre2", "train_pre3")) 
head(longtest)
longtest$name <- ifelse(longtest$name=="train_pre0", "Model-C", 
                        ifelse(longtest$name=="train_pre1","Model-R-Doc", 
                               ifelse(longtest$name=="train_pre2","Model-AI-Mimic-Doc", "Model-R-AI")))
colnames(longtest) <- c("D", "M", "Model")

f1 <- ggplot(longtest, aes(d = D, m = M, color = Model)) + 
  geom_roc(n.cuts = 0) +
  scale_color_manual(values=c("#FFCC33","#3366FF", "#9933FF","#FF3366"), name=NULL,
                     labels=c(paste("Model-C：AUC =",sprintf("%.3f",auc.mdl1.train)),
                              paste("Model-R-Doc：AUC =",sprintf("%.3f",auc.mdl2.train)),
                              paste("Model-AI-Mimic-Doc：AUC =",sprintf("%.3f",auc.mdl3.train)),
                              paste("Model-R-AI：AUC =",sprintf("%.3f",auc.mdl4.train)))) +
  style_roc( xlab = "1 - Specificity", ylab = "Sensitivity")  + 
  annotate("text", x = .16, y = 0.98, label = paste("(a) 5-fold CV training set")) +
  theme(legend.background = element_blank()) +
  theme(legend.justification=c(0.5,0.5), legend.position=c(0.75,0.15))


# plot ROC for CV validation set
length(ori0)
ROCplot <-data.frame(c=1:length(ori0), ori0)
ROCplot0 <-data.frame(c=1:length(ori0), pre0)
ROCplot1 <-data.frame(c=1:length(ori0), pre1)
ROCplot2 <-data.frame(c=1:length(ori0), pre2)
ROCplot3 <- data.frame(c=1:length(ori0), pre3)

ROCplot <- merge(ROCplot, ROCplot0, by="c")
ROCplot <- merge(ROCplot, ROCplot1, by="c")
ROCplot <- merge(ROCplot, ROCplot2, by="c")
ROCplot <- merge(ROCplot, ROCplot3, by="c")


longtest <- melt_roc(ROCplot, "ori0", c("pre0", "pre1", "pre2", "pre3")) #函数melt_roc()可以将多个变量列变为长格式
head(longtest)
longtest$name <- ifelse(longtest$name=="pre0", "Model 1",
                        ifelse(longtest$name=="pre1","Model 2", 
                               ifelse(longtest$name=="pre2","Model 3", "Model 4")))
colnames(longtest) <- c("D", "M", "Model")

f2 <- ggplot(longtest, aes(d = D, m = M, color = Model)) + 
  geom_roc(n.cuts = 0) +
  scale_color_manual(values=c("#FFCC33","#3366FF", "#9933FF","#FF3366"), name=NULL,
                     labels=c(paste("Model-C：AUC =",sprintf("%.3f",auc.mdl1.test)),
                              paste("Model-R-Doc：AUC =",sprintf("%.3f",auc.mdl2.test)),
                              paste("Model-AI-Mimic-Doc：AUC =",sprintf("%.3f",auc.mdl3.test)),
                              paste("Model-R-AI：AUC =",sprintf("%.3f",auc.mdl4.test)))) +
  style_roc( xlab = "1 - Specificity", ylab = "Sensitivity") + 
  annotate("text", x = .18, y = 0.98, label = paste("(b) 5-fold CV validation set")) + 
  theme(legend.background = element_blank()) +
  theme(legend.justification=c(0.5,0.5), legend.position=c(0.75,0.15))

#  plot ROC for test set
fmla0 <- glm(diseaseProgression ~ albumin + urea)
fmla1 <- glm(diseaseProgression ~ albumin + urea)
fmla2 <- glm(diseaseProgression ~ albumin + urea + PCTLLn200T600.new)
fmla4 <- glm(diseaseProgression ~ albumin + urea + LSinBSofRLLL.new)

model0 <- glm(fmla0, family = binomial(link = "logit"), data = dt.train)
model1 <- glm(fmla1, family = binomial(link = "logit"), data = dt.train)
model2 <- glm(fmla2, family = binomial(link = "logit"), data = dt.train)
model3 <- glm(fmla3, family = binomial(link = "logit"), data = dt.train)

pre0 <- predict(model0,dt.test)
pre1 <- predict(model1,dt.test)
pre2 <- predict(model2,dt.test)
pre3 <- predict(model3,dt.test)
ori0 <- dt.test$diseaseProgression

modelroc0 <- roc(dt.test$diseaseProgression,pre0)
modelroc1 <- roc(dt.test$diseaseProgression,pre1)
modelroc2 <- roc(dt.test$diseaseProgression,pre2)
modelroc3 <- roc(dt.test$diseaseProgression,pre3)

auc.mdl0.test <-auc(modelroc0) 
auc.mdl1.test <-auc(modelroc1) 
auc.mdl2.test <-auc(modelroc2) 
auc.mdl3.test <-auc(modelroc3) 

length(ori0)
ROCplot <-data.frame(c=1:length(ori0), ori0)
ROCplot0 <-data.frame(c=1:length(ori0), pre0)
ROCplot1 <-data.frame(c=1:length(ori0), pre1)
ROCplot2 <-data.frame(c=1:length(ori0), pre2)
ROCplot3 <- data.frame(c=1:length(ori0), pre3)

ROCplot <- merge(ROCplot, ROCplot0, by="c")
ROCplot <- merge(ROCplot, ROCplot1, by="c")
ROCplot <- merge(ROCplot, ROCplot2, by="c")
ROCplot <- merge(ROCplot, ROCplot3, by="c")


longtest <- plotROC::melt_roc(ROCplot, "ori0", c("pre0", "pre1", "pre2", "pre3")) 
longtest$name <- ifelse(longtest$name=="pre0", "Model 1",
                        ifelse(longtest$name=="pre1","Model 2", 
                               ifelse(longtest$name=="pre2","Model 3", "Model 4")))
colnames(longtest) <- c("D", "M", "Model")
library(ggplot2)
library(plotROC)
f3 <- ggplot(longtest, aes(d = D, m = M, color = Model)) + 
  geom_roc(n.cuts = 0) + # require "plotROC"
  scale_color_manual(values=c("#FFCC33","#3366FF", "#9933FF","#FF3366"), name=NULL,
                     labels=c(paste("Model-C：AUC =",sprintf("%.3f",auc.mdl0.test)),
                              paste("Model-R-Doc：AUC =",sprintf("%.3f",auc.mdl1.test)),
                              paste("Model-AI-Mimic-Doc：AUC =",sprintf("%.3f",auc.mdl2.test)),
                              paste("Model-R-AI：AUC =",sprintf("%.3f",auc.mdl3.test)))) +
  style_roc( xlab = "1 - Specificity", ylab = "Sensitivity") + 
  annotate("text", x = .09, y = 0.98, label = paste("(c) Test set")) + 
  theme(legend.background = element_blank()) +
  theme(legend.justification=c(0.5,0.5), legend.position=c(0.75,0.15))

# layout
lay1 <- customLayout::lay_new(mat = matrix(1:3, ncol = 3), widths = c(1,1,1)) 
customLayout::lay_show(lay1) 
customLayout::lay_grid(list(f1,f2,f3), lay1) 




#### 6. Appendix results for correlation of age/comorbidity with urea/albumin ####

attach(dt.train)
cor(age,urea)
boxplot(urea~comorbidity)
dt.train$age.ctg <- factor(ifelse(age>60, ">60", "<=60"), levels = c("<=60",">60"))
boxplot(urea~age.ctg)
boxplot(albumin~comorbidity)
boxplot(albumin~age.ctg)
detach(dt.train)
library(corrplot)
cor1 <- cor.test(x=as.numeric(comorbidity), y= urea, method = "kendall")
cor2 <- cor.test(x=as.numeric(age.ctg), y= urea, method = "kendall")
cor3 <- cor.test(x=as.numeric(comorbidity), y= albumin, method = "kendall")
cor4 <- cor.test(x=as.numeric(age.ctg), y= albumin, method = "kendall")


## boxplt
Bxplt1 <- dt.train %>% dplyr::mutate( x = factor(comorbidity,labels=c("No","Yes"))) %>% 
  ggplot( aes(x = x, y=urea, color=x)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values=c("#FFCC33","#6699FF")) +
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 35, 5)) + 
  theme(legend.position="none") + 
  geom_jitter(width = 0.2) + 
  labs(x="Comorbidity", y="Urea") +
  annotate("text", x = 1.5, y = 32,label = paste( "Kendall's Tau", " = ", sprintf("%.3f", cor1$estimate), sep=""), size = 3) +
  annotate("text", x = 1.5, y = 29,label = paste( "P = ", sprintf("%.3f", cor1$p.value), sep=""), size = 3)
Bxplt2 <- dt.train %>% 
  ggplot( aes(x = age.ctg, y=urea, color=age.ctg)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values=c("#FFCC33","#6699FF")) +
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 35, 5)) + 
  theme(legend.position="none") + 
  geom_jitter(width = 0.2) + 
  labs(x="Age", y="Urea") +
  annotate("text", x = 1.5, y = 32,label = paste( "Kendall's Tau", " = ", sprintf("%.3f", cor2$estimate), sep="" ), size = 3) +
  annotate("text", x = 1.5, y = 29,label = paste( "P = ", sprintf("%.3f", cor2$p.value), sep=""), size = 3)
Bxplt3 <- dt.train %>% dplyr::mutate( x = factor(comorbidity,labels=c("No","Yes"))) %>% 
  ggplot( aes(x = x, y=albumin, color=x)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values=c("#FFCC33","#6699FF")) +
  theme_bw() +
  scale_y_continuous(breaks=seq(30, 50, 5)) + 
  theme(legend.position="none") + 
  geom_jitter(width = 0.2) + 
  labs(x="Comorbidity", y="Albumin")  +
  annotate("text", x = 1.5, y = 47,label = paste( "Kendall's Tau", " = ", sprintf("%.3f", cor3$estimate), sep="" ), size = 3) +
  annotate("text", x = 1.5, y = 45,label = paste( "P = ", sprintf("%.3f", cor3$p.value), sep=""), size = 3) 
Bxplt4 <- dt.train %>% 
  ggplot( aes(x = age.ctg, y=albumin, color=age.ctg)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values=c("#FFCC33","#6699FF")) +
  theme_bw() +
  scale_y_continuous(breaks=seq(30, 50, 5)) + 
  theme(legend.position="none") + 
  geom_jitter(width = 0.2) + 
  labs(x="Age", y="Albumin") +
  annotate("text", x = 1.5, y = 47,label = paste( "Kendall's Tau", " = ", sprintf("%.3f", cor4$estimate), sep="" ), size = 3) +
  annotate("text", x = 1.5, y = 45,label = paste( "P = ", sprintf("%.3f", cor4$p.value), sep=""), size = 3) 

library(customLayout)
lay1 <- lay_new(matrix(1:2, ncol = 2),widths = c(1,1))
lay1 <-lay_bind_row(lay1,lay1,heights = c(6, 6), addmax = TRUE)
lay_grid(list(Bxplt1,Bxplt2,Bxplt3,Bxplt4), lay1)


