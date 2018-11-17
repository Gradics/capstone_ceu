#clear environment 
cat("\f")
rm(list=ls())

# Set the working directory
setwd("c:/Users/radics.gabor/Documents/ceu/Capstone/v03_Magan_Keresesek_1user/Initial_Model")

#require("RPostgreSQL")
library('bigQueryR')
library('data.table')
library('devtools')
library('woe')
library(xlsx)
library(DT)          # For Data Tables
library(lattice)     # The lattice add-on of Trellis graphics for R
library(knitr)       # For Dynamic Report Generation in R 
library(gplots)      # Various R Programming Tools for Plotting Data
library(ggplot2)     # An Implementation of the Grammar of Graphics 
library(ClustOfVar)  # Clustering of variables 
library(ape)         # Analyses of Phylogenetics and Evolution (as.phylo) 
library(Information) # Data Exploration with Information Theory (Weight-of-Evidence and Information Value)
library(ROCR)        # Model Performance and ROC curve
library(caret)       # Classification and Regression Training -  for any machine learning algorithms
library(rpart)       # Recursive partitioning for classification, regression and survival trees
library(rpart.utils) # Tools for parsing and manipulating rpart objects, including generating machine readable rules
library(rpart.plot)  # Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'
library(randomForest)# Leo Breiman and Cutler's Random Forests for Classification and Regression 
library(party)       # A computational toolbox for recursive partitioning - Conditional inference Trees
library(bnlearn)     # Bayesian Network Structure Learning, Parameter Learning and Inference
library(DAAG)        # Data Analysis and Graphics Data and Functions
library(vcd)         # Visualizing Categorical Data
library(kernlab)     # Support Vector Machine

library(bigrquery)

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

# # import data
# project <- "ga-bq-export" # put your project ID here
# sql <- "
# SELECT icomid as id, NR_Bankracio_All, K_nr_diff_RE, K_nr_diff_Altipus, K_nr_diff_City, MT_totals_hits, MT_Honnan_Hungary, K_Mit_tegla, K_nr_diff_RE_BP, K_Mit_panel, K_min_Alapterulet, MT_totals_pageviews, K_min_DimHirdetesar, K_Mit_csaladi_haz, MT_NR_TelszamFelfedes, K_Mit_sorhaz, MT_Budapestrol_Budapestre, K_nr_diff_Megye, K_Mit_ikerhaz, K_Mit_hazresz, K_Mit_csuszozsalus, K_max_Alapterulet
# FROM [Gabor_EU.Jelzalog_Lead_Test_id]"
# Jelzalog_Lead_Test_id <- query_exec(sql, project = project, useLegacySql = FALSE, max_pages = Inf, use_legacy_sql = TRUE)
# Jelzalog_Lead_Test_id <- data.table(Jelzalog_Lead_Test_id)
# save(Jelzalog_Lead_Test_id, file="c:/Users/radics.gabor/Documents/ceu/Capstone/v03_Magan_Keresesek_1user/Initial_Model/Jelzalog_Lead_Test_id.rda")

# load data
load("c:/Users/radics.gabor/Documents/ceu/Capstone/v03_Magan_Keresesek_1user/Initial_Model/Jelzalog_Lead_Test_id.rda")


# data manipulation
JZ <- data.table(Jelzalog_Lead_Test_id)
  #JZ <- data.table(merge(Jelzalog_Lead_Test_id, business_partner_my_hir, by = 'id', all.x = TRUE, all.y = FALSE))
  #JZ <- merge(JZ, ad_my_hir, by = 'id', all.x = TRUE, all.y = FALSE)


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

# development/validation sample

set.seed(100)
JZ$RandomNr <- runif(nrow(JZ), min=0, max=1)
JZd <- data.frame(JZ[RandomNr<=0.5,,])
JZv <- data.frame(JZ[RandomNr>0.5,,])

# IV development - to run only at the time of development !!!
IVd <- iv.mult(JZd,"NR_Bankracio_All",TRUE)
#write.xlsx(IVd, "IVd.xlsx")

# Recoding Variables
JZd_Variables <- JZd[, c('NR_Bankracio_All', 'K_nr_diff_RE', 'K_nr_diff_Altipus', 'K_nr_diff_City', 'MT_totals_hits', 'MT_Honnan_Hungary', 'K_Mit_tegla', 'K_nr_diff_RE_BP', 'K_Mit_panel', 'K_min_Alapterulet', 'MT_totals_pageviews', 'K_min_DimHirdetesar', 'K_Mit_csaladi_haz', 'MT_NR_TelszamFelfedes', 'K_Mit_sorhaz', 'MT_Budapestrol_Budapestre', 'K_nr_diff_Megye', 'K_Mit_ikerhaz', 'K_Mit_hazresz', 'K_Mit_csuszozsalus', 'K_max_Alapterulet')]
JZd_Mult <- iv.mult(JZd_Variables,"NR_Bankracio_All",summary=FALSE)

# based on coding results of JZd_Mult
JZd_Variables$VAR_NR_Bankracio_All <-as.factor(JZd_Variables$NR_Bankracio_All)

JZd_Variables$VAR_K_nr_diff_RE <-as.factor(
  ifelse(JZd_Variables$K_nr_diff_RE < 17.5, '00-17',
         ifelse(JZd_Variables$K_nr_diff_RE < 30.5,'18-30',
                ifelse(JZd_Variables$K_nr_diff_RE < 117.5,'31-117','117+'))))

JZd_Variables$VAR_K_nr_diff_Altipus <-as.factor(
  ifelse(JZd_Variables$K_nr_diff_Altipus < 1.5, '0-1',
         ifelse(JZd_Variables$K_nr_diff_Altipus < 2.5,'2-2',
                ifelse(JZd_Variables$K_nr_diff_Altipus < 5.5,'3-5','5+'))))

JZd_Variables$VAR_K_nr_diff_City <-as.factor(
  ifelse(JZd_Variables$K_nr_diff_City < 1.5,'0-1',
         ifelse(JZd_Variables$K_nr_diff_City < 5.5,'2-5',
                ifelse(JZd_Variables$K_nr_diff_City < 13.5,'6-13','13+'))))

JZd_Variables$VAR_MT_totals_hits <-as.factor(
  ifelse(JZd_Variables$MT_totals_hits < 1368,'00000-1368',
         ifelse(JZd_Variables$MT_totals_hits < 22930,'01369-22930',
                ifelse(JZd_Variables$MT_totals_hits < 166400,'22931-166400','166400+'))))

JZd_Variables$VAR_MT_Honnan_Hungary <-as.factor(
  ifelse(JZd_Variables$MT_Honnan_Hungary < 226.5, '000-226',
                ifelse(JZd_Variables$MT_Honnan_Hungary < 544.5,'227-544','544+')))

JZd_Variables$VAR_K_Mit_tegla <-as.factor(
  ifelse(JZd_Variables$K_Mit_tegla < 1.5, '00-1',
         ifelse(JZd_Variables$K_Mit_tegla < 21.5,'02-21','21+')))

JZd_Variables$VAR_K_nr_diff_RE_BP <-as.factor(
  ifelse(JZd_Variables$K_nr_diff_RE_BP < 17.5,'00-17',
         ifelse(JZd_Variables$K_nr_diff_RE_BP < 38.5,'18-38','38+')))

JZd_Variables$VAR_K_Mit_panel <-as.factor(
  ifelse(JZd_Variables$K_Mit_panel < 0.5,'00-00',
         ifelse(JZd_Variables$K_Mit_panel < 10.5,'01-10','10+')))

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
## Final_Model 

set.seed(100)
Final_Model <- glm(VAR_NR_Bankracio_All ~  VAR_K_nr_diff_RE + VAR_K_nr_diff_Altipus + VAR_K_nr_diff_City + VAR_MT_totals_hits + VAR_MT_Honnan_Hungary + VAR_K_Mit_tegla + VAR_K_nr_diff_RE_BP + VAR_K_Mit_panel, data=JZd_Variables, family=binomial())
Final_Model <- step(Final_Model)
summary(Final_Model)

#write.xlsx(coef(Final_Model), "coefficients.xlsx")

# List of significant variables and features with p-value <0.01
significant.variables <- summary(Final_Model)$coeff[-1,4] < 0.01
names(significant.variables)[significant.variables == TRUE]

prob <- predict(Final_Model, type = "response")
res <- residuals(Final_Model, type = "deviance")

#Plot Residuals
plot(predict(Final_Model), res,
     xlab="Fitted values", ylab = "Residuals",
     ylim = max(abs(res)) * c(-1,1))

## CIs using profiled log-likelihood
#confint(Final_Model)

#
JZd_Variables$score <- predict(Final_Model, type='response', JZd_Variables)
JZd_Variables$score_glm <- predict.glm(Final_Model, type='response', JZd_Variables) # gives the same result

m1_pred <- prediction(JZd_Variables$score, JZd_Variables$NR_Bankracio_All)
m1_perf <- performance(m1_pred,"tpr","fpr")

#ROC
plot(m1_perf, lwd=2, colorize=TRUE, main="ROC m1: Logistic Regression Performance")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# Plot precision/recall curve
m1_perf_precision <- performance(m1_pred, measure = "prec", x.measure = "rec")
plot(m1_perf_precision, main="m1 Logistic:Precision/recall curve")

# Plot accuracy as function of threshold
m1_perf_acc <- performance(m1_pred, measure = "acc")
plot(m1_perf_acc, main="m1 Logistic:Accuracy as function of threshold - DEVELOPMENT")

#KS, Gini & AUC m1
m1_KS <- max(attr(m1_perf,'y.values')[[1]]-attr(m1_perf,'x.values')[[1]]) * 100
m1_AUROC <- performance(m1_pred, measure = "auc")@y.values[[1]] * 100
m1_Gini <- (2 * m1_AUROC - 100)
cat("AUROC: ", m1_AUROC,"\tKS: ", m1_KS, "\tGini:", m1_Gini, "\n")

JZd_Variables <- data.table(JZd_Variables)
KSd <- JZd_Variables[, .(
  .N,
  noBR = sum(NR_Bankracio_All == 0),
  BR = sum(NR_Bankracio_All == 1))
  , by = score][order(score)]
#write.xlsx(KSd, "KSd.xlsx")


################################################################################################################
## VALIDATION #### VALIDATION #### VALIDATION #### VALIDATION #### VALIDATION #### VALIDATION #### VALIDATION ##
################################################################################################################

# Recoding Variables
JZv$VAR_NR_Bankracio_All <-as.factor(JZv$NR_Bankracio_All)

JZv$VAR_NR_Bankracio_All <-as.factor(JZv$NR_Bankracio_All)

JZv$VAR_K_nr_diff_RE <-as.factor(
  ifelse(JZv$K_nr_diff_RE < 17.5, '00-17',
         ifelse(JZv$K_nr_diff_RE < 30.5,'18-30',
                ifelse(JZv$K_nr_diff_RE < 117.5,'31-117','117+'))))

JZv$VAR_K_nr_diff_Altipus <-as.factor(
  ifelse(JZv$K_nr_diff_Altipus < 1.5, '0-1',
         ifelse(JZv$K_nr_diff_Altipus < 2.5,'2-2',
                ifelse(JZv$K_nr_diff_Altipus < 5.5,'3-5','5+'))))

JZv$VAR_K_nr_diff_City <-as.factor(
  ifelse(JZv$K_nr_diff_City < 1.5,'0-1',
         ifelse(JZv$K_nr_diff_City < 5.5,'2-5',
                ifelse(JZv$K_nr_diff_City < 13.5,'6-13','13+'))))

JZv$VAR_MT_totals_hits <-as.factor(
  ifelse(JZv$MT_totals_hits < 1368,'00000-1368',
         ifelse(JZv$MT_totals_hits < 22930,'01369-22930',
                ifelse(JZv$MT_totals_hits < 166400,'22931-166400','166400+'))))

JZv$VAR_MT_Honnan_Hungary <-as.factor(
  ifelse(JZv$MT_Honnan_Hungary < 226.5, '000-226',
         ifelse(JZv$MT_Honnan_Hungary < 544.5,'227-544','544+')))

JZv$VAR_K_Mit_tegla <-as.factor(
  ifelse(JZv$K_Mit_tegla < 1.5, '00-1',
         ifelse(JZv$K_Mit_tegla < 21.5,'02-21','21+')))

JZv$VAR_K_nr_diff_RE_BP <-as.factor(
  ifelse(JZv$K_nr_diff_RE_BP < 17.5,'00-17',
         ifelse(JZv$K_nr_diff_RE_BP < 38.5,'18-38','38+')))

JZv$VAR_K_Mit_panel <-as.factor(
  ifelse(JZv$K_Mit_panel < 0.5,'00-00',
         ifelse(JZv$K_Mit_panel < 10.5,'01-10','10+')))


JZv$score <- predict(Final_Model, type='response', JZv)
m1_pred_val <- prediction(JZv$score, JZv$NR_Bankracio_All)
m1_perf_val <- performance(m1_pred_val,"tpr","fpr")

# Plot accuracy as function of threshold
m1_perf_acc_val <- performance(m1_pred_val, measure = "acc")
plot(m1_perf_acc_val, main="m1 Logistic:Accuracy as function of threshold - VALIDATION")

#KS, Gini & AUC m1
m1_KS_val <- max(attr(m1_perf_val,'y.values')[[1]]-attr(m1_perf_val,'x.values')[[1]]) * 100
m1_AUROC_val <- performance(m1_pred_val, measure = "auc")@y.values[[1]] * 100
m1_Gini_val <- (2 * m1_AUROC_val - 100)
cat("AUROC: ", m1_AUROC_val,"\tKS: ", m1_KS_val, "\tGini:", m1_Gini_val, "\n")


JZv <- data.table(JZv)
KSv <- JZv[, .(
  .N,
  noBR = sum(NR_Bankracio_All == 0),
  BR = sum(NR_Bankracio_All == 1))
  , by = score][order(score)]
#write.xlsx(KSv, "KSv.xlsx")

################################################################################################################
#### OOT #### OOT #### OOT #### OOT #### OOT #### OOT #### OOT #### OOT #### OOT #### OOT #### OOT #### OOT ####
################################################################################################################

# # import data
# project <- "ga-bq-export" # put your project ID here
# sql <- "
# SELECT icomid as id, NR_Bankracio_All, K_nr_diff_RE, K_nr_diff_Altipus, K_nr_diff_City, MT_totals_hits, MT_Honnan_Hungary, K_Mit_tegla, K_nr_diff_RE_BP, K_Mit_panel, K_min_Alapterulet, MT_totals_pageviews, K_min_DimHirdetesar, K_Mit_csaladi_haz, MT_NR_TelszamFelfedes, K_Mit_sorhaz, MT_Budapestrol_Budapestre, K_nr_diff_Megye, K_Mit_ikerhaz, K_Mit_hazresz, K_Mit_csuszozsalus, K_max_Alapterulet
# FROM [Gabor_EU.Jelzalog_Lead_Test_id_OOT]"
# JZoot <- query_exec(sql, project = project, useLegacySql = FALSE, max_pages = Inf, use_legacy_sql = TRUE)
# JZoot <- data.table(JZoot)
# 
# save(JZoot, file="c:/Users/radics.gabor/Documents/ceu/Capstone/v03_Magan_Keresesek_1user/Initial_Model/JZoot.rda")

# load data
load("c:/Users/radics.gabor/Documents/ceu/Capstone/v03_Magan_Keresesek_1user/Initial_Model/JZoot.rda")

# VARIABLES
JZoot$VAR_NR_Bankracio_All <-as.factor(JZoot$NR_Bankracio_All)

JZoot$VAR_K_nr_diff_RE <-as.factor(
  ifelse(JZoot$K_nr_diff_RE < 17.5, '00-17',
         ifelse(JZoot$K_nr_diff_RE < 30.5,'18-30',
                ifelse(JZoot$K_nr_diff_RE < 117.5,'31-117','117+'))))

JZoot$VAR_K_nr_diff_Altipus <-as.factor(
  ifelse(JZoot$K_nr_diff_Altipus < 1.5, '0-1',
         ifelse(JZoot$K_nr_diff_Altipus < 2.5,'2-2',
                ifelse(JZoot$K_nr_diff_Altipus < 5.5,'3-5','5+'))))

JZoot$VAR_K_nr_diff_City <-as.factor(
  ifelse(JZoot$K_nr_diff_City < 1.5,'0-1',
         ifelse(JZoot$K_nr_diff_City < 5.5,'2-5',
                ifelse(JZoot$K_nr_diff_City < 13.5,'6-13','13+'))))

JZoot$VAR_MT_totals_hits <-as.factor(
  ifelse(JZoot$MT_totals_hits < 1368,'00000-1368',
         ifelse(JZoot$MT_totals_hits < 22930,'01369-22930',
                ifelse(JZoot$MT_totals_hits < 166400,'22931-166400','166400+'))))

JZoot$VAR_MT_Honnan_Hungary <-as.factor(
  ifelse(JZoot$MT_Honnan_Hungary < 226.5, '000-226',
         ifelse(JZoot$MT_Honnan_Hungary < 544.5,'227-544','544+')))

JZoot$VAR_K_Mit_tegla <-as.factor(
  ifelse(JZoot$K_Mit_tegla < 1.5, '00-1',
         ifelse(JZoot$K_Mit_tegla < 21.5,'02-21','21+')))

JZoot$VAR_K_nr_diff_RE_BP <-as.factor(
  ifelse(JZoot$K_nr_diff_RE_BP < 17.5,'00-17',
         ifelse(JZoot$K_nr_diff_RE_BP < 38.5,'18-38','38+')))

JZoot$VAR_K_Mit_panel <-as.factor(
  ifelse(JZoot$K_Mit_panel < 0.5,'00-00',
         ifelse(JZoot$K_Mit_panel < 10.5,'01-10','10+')))



JZoot$score <- predict(Final_Model, type='response', JZoot)
m1_pred_oot <- prediction(JZoot$score, JZoot$NR_Bankracio_All)
m1_perf_oot <- performance(m1_pred_oot,"tpr","fpr")


#KS, Gini & AUC m1
m1_KS_oot <- max(attr(m1_perf_oot,'y.values')[[1]]-attr(m1_perf_oot,'x.values')[[1]]) * 100
m1_AUROC_oot <- performance(m1_pred_oot, measure = "auc")@y.values[[1]] * 100
m1_Gini_oot <- (2 * m1_AUROC_oot - 100)
cat("AUROC: ", m1_AUROC_oot,"\tKS: ", m1_KS_oot, "\tGini:", m1_Gini_oot, "\n")

KSoot <-JZoot[, .(
  .N,
  noBR = sum(NR_Bankracio_All == 0),
  BR = sum(NR_Bankracio_All == 1))
  , by = score][order(score)]
#write.xlsx(KSoot, "KSoot.xlsx")


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# summary of the results

nrow(JZd)
nrow(JZoot)

rbind(
c('AUC - Train', round(m1_AUROC_val / 100, 2)),
c('AUC - OOT', round(m1_AUROC_oot / 100, 2))
)




