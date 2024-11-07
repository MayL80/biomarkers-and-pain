# merge data sets by Patient ID number. Patient ID number column created for biomarker file manually in excel prior to import
df<-merge(data.frame(biomarkers), data.frame(covariates), by="PatientID")
df<-df[order(df$"VAS.at.inclusion"),]

# install packages to create library and group data from a column by specifications
install.packages("dplyr")
library(dplyr)

# create low VAS data frame and pull values of a biomarker corresponding to low VAS. Same process for high VAS data frame
low_VAS_IL6<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(IL.6) %>%
  pull(IL.6)

high_VAS_IL6<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(IL.6) %>%
  pull(IL.6)

# perform t test
alpha <- 0.05
result_IL6<- t.test(low_VAS_IL6,high_VAS_IL6)
print(result_IL6)

# evaluate if p-value significant

if (result_IL6$p.value < alpha) 
{cat("IL6: signigicant, reject null")} else 
{cat("IL6: not significant, fail to reject null")}

# repeat for VEGF-A.low and high VAS data frames created, corresponding values pulled and t tests performed
low_VAS_VEGFA<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(VEGF.A) %>%
  pull(VEGF.A)

high_VAS_VEGFA<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(VEGF.A) %>%
  pull(VEGF.A)

result_VEGFA<- t.test(low_VAS_VEGFA,high_VAS_VEGFA)
print(result_VEGFA)

# evaluate if p-value significant for VEGFA

if (result_VEGFA$p.value < alpha) 
{cat("VEGF-A: significant, reject null")} else 
{cat("VEGF-A: not significant, fail to reject null")}

# repeat for OPG
low_VAS_OPG<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(OPG) %>%
  pull(OPG)

high_VAS_OPG<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(OPG) %>%
  pull(OPG)

result_OPG<- t.test(low_VAS_OPG,high_VAS_OPG)
print(result_OPG)

# evaluate if p-value significant

if (result_OPG$p.value < alpha) 
{cat("OPG: significant, reject null")} else 
{cat("OPG: not significant, fail to reject null")}

# repeat for TFG beta 1
low_VAS_TGF<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(TGF.beta.1) %>%
  pull(TGF.beta.1)

high_VAS_TGF<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(TGF.beta.1) %>%
  pull(TGF.beta.1)

result_TGF<- t.test(low_VAS_TGF,high_VAS_TGF)
print(result_TGF)

# evaluate if p-value significant

if (result_TGF$p.value < alpha) 
{cat("TGF beta 1: significant, reject null")} else 
{cat("TGF beta 1: not significant, fail to reject null")}

# repeat for IL8
low_VAS_IL8<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(IL.8) %>%
  pull(IL.8)

high_VAS_IL8<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(IL.8) %>%
  pull(IL.8)

result_IL8<- t.test(low_VAS_IL8,high_VAS_IL8)
print(result_IL8)

# evaluate if p-value significant

if (result_IL8$p.value < alpha) 
{cat("IL8: significant, reject null")} else 
{cat("IL8: not significant, fail to reject null")}

# repeat for CXCL9
low_VAS_CXCL9<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(CXCL9) %>%
  pull(CXCL9)

high_VAS_CXCL9<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(CXCL9) %>%
  pull(CXCL9)

result_CXCL9<- t.test(low_VAS_CXCL9,high_VAS_CXCL9)
print(result_CXCL9)

# evaluate if p-value significant

if (result_CXCL9$p.value < alpha) 
{cat("CXCL9: significant, reject null")} else 
{cat("CXCL9: not significant, fail to reject null")}

# repeat for CXCL1
low_VAS_CXCL1<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(CXCL1) %>%
  pull(CXCL1)

high_VAS_CXCL1<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(CXCL1) %>%
  pull(CXCL1)

result_CXCL1<- t.test(low_VAS_CXCL1,high_VAS_CXCL1)
print(result_CXCL1)

# evaluate if p-value significant

if (result_CXCL1$p.value < alpha) 
{cat("CXCL1: significant, reject null")} else 
{cat("CXCL1: not significant, fail to reject null")}

# repeat for IL18
low_VAS_IL18<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(IL.18) %>%
  pull(IL.18)

high_VAS_IL18<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(IL.18) %>%
  pull(IL.18)

result_IL18<- t.test(low_VAS_IL18,high_VAS_IL18)
print(result_IL18)

# evaluate if p-value significant

if (result_IL18$p.value < alpha) 
{cat("IL18: significant, reject null")} else 
{cat("IL18: not significant, fail to reject null")}

# repeat for CSF1
low_VAS_CSF1<-df %>%
  filter(VAS.at.inclusion<=5.0) %>%
  arrange(CSF.1) %>%
  pull(CSF.1)

high_VAS_CSF1<-df %>%
  filter(VAS.at.inclusion>5.0) %>%
  arrange(CSF.1) %>%
  pull(CSF.1)

result_CSF1<- t.test(low_VAS_CSF1,high_VAS_CSF1)
print(result_CSF1)

# evaluate if p-value significant

if (result_CSF1$p.value < alpha) 
{cat("CSF-1: significant, reject null")} else 
{cat("CSF-1: not significant, fail to reject null")}

# create boxplots, split into multiple so information displayed is readable
# adjust label font sizes
par(cex.label=1.5)
par(cex.axis=1.5)
par(cex.main=2)
# generate boxplots
boxplot(low_VAS_IL6, high_VAS_IL6, low_VAS_VEGFA, high_VAS_VEGFA, low_VAS_OPG, high_VAS_OPG, names=c("VAS<=5 IL-6", "VAS>5 IL-6", "VAS<=5 VEGF-A", "VAS>5 VEGF-A", "VAS<=5 OPG", "VAS>5 OPG"), main="Biomarker levels by low and high VAS")
boxplot(low_VAS_TGF, high_VAS_TGF,low_VAS_IL8, high_VAS_IL8, low_VAS_CXCL9, high_VAS_CXCL9, names=c("VAS<=5 TGF-β-1", "VAS>5 TGF-β-1", "VAS<=5 IL-8", "VAS>5 IL-8", "VAS<=5 CXCL9", "VAS>5 CXCL9"), main="Biomarker levels by low and high VAS")
boxplot(low_VAS_CXCL1, high_VAS_CXCL1, low_VAS_IL18, high_VAS_IL18, low_VAS_CSF1, high_VAS_CSF1, names=c("VAS<=5 CXCL1", "VAS>5 CXCL1", "VAS<=5 IL-18", "VAS>5 IL-18", "VAS<=5 CSF-1", "VAS>5 CSF-1"), main="Biomarker levels by low and high VAS")

# probability of type I error
alpha<-0.05
n<-9
prob_type_I<-1-(1-alpha)^n
print(prob_type_I)

# applying bonferroni correction. run same tests based on a new alpha level
alpha<-0.0055555556

# evaluate if p-values significant based on new alpha level
# IL6
if (result_IL6$p.value < alpha) 
{cat("IL6: significant, reject null")} else 
{cat("IL6: not significant, fail to reject null")}
# VEGF-A
if (result_VEGFA$p.value < alpha) 
{cat("VEGF-A: significant, reject null")} else 
{cat("VEGF-A: not significant, fail to reject null")}
# OPG
if (result_OPG$p.value < alpha) 
{cat("OPG: significant, reject null")} else 
{cat("OPG: not significant, fail to reject null")}
# TGF beta 1
if (result_TGF$p.value < alpha) 
{cat("TGF beta 1: significant, reject null")} else 
{cat("TGF beta 1: not significant, fail to reject null")}
# IL-8
if (result_IL8$p.value < alpha) 
{cat("IL8: significant, reject null")} else 
{cat("IL8: not significant, fail to reject null")}
# CXCL9
if (result_CXCL9$p.value < alpha) 
{cat("CXCL9: significant, reject null")} else 
{cat("CXCL9: not significant, fail to reject null")}
# CXCL1
if (result_CXCL1$p.value < alpha) 
{cat("CXCL1: significant, reject null")} else 
{cat("CXCL1: not significant, fail to reject null")}
# IL-18
if (result_IL18$p.value < alpha) 
{cat("IL18: significant, reject null")} else 
{cat("IL18: not significant, fail to reject null")}
# CSF-1
if (result_CSF1$p.value < alpha) 
{cat("CSF-1: significant, reject null")} else 
{cat("CSF-1: not significant, fail to reject null")}

# regression model with 12mo VAS
# resp var = VAS at 12mo
# expl var = biomarkers and covariates
# pull corresponding values to biomarker at 0 weeks, repeat for each biomarker
res<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(Vas.12months)
bio_IL6<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(IL.6)
bio_VEGFA<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(VEGF.A)
bio_OPG<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(OPG)
bio_TGF<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(TGF.beta.1)
bio_IL8<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(IL.8)
bio_CXCL9<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(CXCL9)
bio_CXCL1<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(CXCL1)
bio_IL18<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(IL.8)
bio_CSF1<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(CSF.1)
# repeat pulling corresponding values for covariates
cov_age<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(Age)
cov_sex<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(Sex..1.male..2.female.)
cov_smoke<-df %>%
  filter(Biomarker.at.time.period=="0weeks") %>%
  pull(Smoker..1.yes..2.no.)
# multi linear regression
m1<-lm(res~bio_IL6+bio_VEGFA+bio_OPG+bio_TGF+bio_IL8+bio_CXCL9+bio_CXCL1+bio_IL18+bio_CSF1+cov_age+cov_sex+cov_smoke)
summary(m1)
plot(m1) #diagnostic plots
# fit with 80% data
set.seed(123) # for reproducability
train_size<-floor(0.8*nrow(df)) # set it to 80% data
train_data<-df[1:train_size,] # training data
test_data<-df[(train_size+1):nrow(df), ]
#train model by using a regression
x_training<-train_data[, -ncol(train_data)]
y_training<-train_data[, ncol(train_data)]
model<-lm(y_training~.,data=train_data)
summary(model)

# predict next 20%
prediction<-predict(model, newdata=test_data)
print(prediction)
# calculate prediction accuracy
interpolate_pred <-approx(seq_along(prediction), prediction, xout=seq_along(res))$y #ensure prediction has same number of values as real data
threshold<-0.1 # set threshold for difference between values that is allowed to be considered accurate
accuracy<-mean(abs(res-interpolate_pred) < threshold, na.rm = TRUE) # calc accuracy
print(accuracy)

# setting up github for export of code to a repo
install.packages("usethis")
library(usethis)
use_git_config(user.name="MayL80", user.email="S2688834@ed.ac.uk")

