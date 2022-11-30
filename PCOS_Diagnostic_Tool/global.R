library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

############################Load Data Frame####################################
 
File = "C:\\Users\\sarah\\OneDrive\\Documents\\NYCDS_Bootcamp\\00_Project 2\\PCOS_Diagnostic_Tool\\PCOS_data_without_infertility.xlsx"
PCOS_df = read_xlsx(File, sheet = "Full_new")

############################Clean data frame####################################

#Drop columns not used in the data frame. 
PCOS_df = select(PCOS_df, -c("Sl. No", "Patient File No.", "Cycle(R/I)", "...45"))

#Convert categorical data into factors for modeling. 
PCOS_df$`PCOS (Y/N)` = as.factor(PCOS_df$`PCOS (Y/N)`)
PCOS_df$`Pregnant(Y/N)` = as.factor(PCOS_df$`Pregnant(Y/N)`)
PCOS_df$`Weight gain(Y/N)` = as.factor(PCOS_df$`Weight gain(Y/N)`)
PCOS_df$`hair growth(Y/N)` = as.factor(PCOS_df$`hair growth(Y/N)`)
PCOS_df$`Skin darkening (Y/N)` = as.factor(PCOS_df$`Skin darkening (Y/N)`)
PCOS_df$`Hair loss(Y/N)` = as.factor(PCOS_df$`Hair loss(Y/N)`)
PCOS_df$`Pimples(Y/N)` = as.factor(PCOS_df$`Pimples(Y/N)`)
PCOS_df$`Fast food (Y/N)` = as.factor(PCOS_df$`Fast food (Y/N)`)
PCOS_df$`Reg.Exercise(Y/N)` = as.factor(PCOS_df$`Reg.Exercise(Y/N)`)

#Convert character features to numerical. 
PCOS_df$`II    beta-HCG(mIU/mL)` = as.integer(PCOS_df$`II    beta-HCG(mIU/mL)`)
PCOS_df$`AMH(ng/mL)` = as.integer(PCOS_df$`AMH(ng/mL)`)

#Drop rows that have a null value.
PCOS_df = PCOS_df[!is.na(PCOS_df$`Marraige Status (Yrs)`) & 
                    !is.na(PCOS_df$`II    beta-HCG(mIU/mL)`) & 
                    !is.na(PCOS_df$`AMH(ng/mL)`) & 
                    !is.na(PCOS_df$`Fast food (Y/N)`), ]

#Impute outliers with the median. 
PCOS_df[453, 16] = median(PCOS_df$`LH(mIU/mL)`)
PCOS_df[295, 6] = median(PCOS_df$`Pulse rate(bpm)`)
PCOS_df[222, 6] = median(PCOS_df$`Pulse rate(bpm)`)
PCOS_df[327, 17] = median(PCOS_df$`FSH/LH`)
PCOS_df[112,13] = median(PCOS_df$`I   beta-HCG(mIU/mL)`)
PCOS_df[213, 13] = median(PCOS_df$`II    beta-HCG(mIU/mL)`)
PCOS_df[251, 13] = median(PCOS_df$`II    beta-HCG(mIU/mL)`)

TSH_boxplot = boxplot(PCOS_df$`TSH (mIU/L)`, plot = FALSE)
TSH_outliers = TSH_boxplot$out
PCOS_df$`TSH (mIU/L)`[which(PCOS_df$`TSH (mIU/L)` %in% c(TSH_outliers))] = median(PCOS_df$`TSH (mIU/L)`)
AMH_boxplot = boxplot(PCOS_df$`AMH(ng/mL)`, plot = FALSE)
AMH_outliers = AMH_boxplot$out
PCOS_df$`AMH(ng/mL)`[which(PCOS_df$`AMH(ng/mL)` %in% c(AMH_outliers))] = median(PCOS_df$`AMH(ng/mL)`)
PCOS_df[190, 24] = median(PCOS_df$`Vit D3 (ng/mL)`)
PCOS_df[194, 24] = median(PCOS_df$`Vit D3 (ng/mL)`)

#Convert Blood Group to character and create dummy variables. 
PCOS_df$`Blood Group` = as.character(PCOS_df$`Blood Group`)
PCOS_df["Blood Group"][PCOS_df["Blood Group"] == 11] = "A+"
PCOS_df["Blood Group"][PCOS_df["Blood Group"] == 12] = "A-"
PCOS_df["Blood Group"][PCOS_df["Blood Group"] == 13] = "B+"
PCOS_df["Blood Group"][PCOS_df["Blood Group"] == 14] = "B-"
PCOS_df["Blood Group"][PCOS_df["Blood Group"] == 15] = "O+"
PCOS_df["Blood Group"][PCOS_df["Blood Group"] == 16] = "O-"
PCOS_df["Blood Group"][PCOS_df["Blood Group"] == 17] = "AB+"
PCOS_df["Blood Group"][PCOS_df["Blood Group"] == 18] = "AB-"
PCOS_df = fastDummies::dummy_cols(PCOS_df, select_columns = "Blood Group", 
                                  remove_first_dummy = TRUE, remove_selected_columns = TRUE)


################################Create Model####################################

#Split data frame into test and train data sets for model training and testing. 
sample = sample(c(TRUE, FALSE), nrow(PCOS_df), replace = TRUE, prob = c(0.7, 0.3))
train = PCOS_df[sample, ]
test = PCOS_df[!sample, ]

#Logistic regression model. 
sig_feat_model2 = glm(`PCOS (Y/N)` ~ `Cycle length(days)` + `LH(mIU/mL)` + 
                        `Weight gain(Y/N)` + `hair growth(Y/N)` + `Skin darkening (Y/N)` + 
                        `Follicle No. (L)` + `Follicle No. (R)`, family ="binomial", data = train)

#Create data needed to plot the sigmoid function. 
predicted_data = data.frame(probability_of_PCOS= sig_feat_model2$fitted.values,
                            PCOS = train$`PCOS (Y/N)`)
predicted_data = predicted_data[order(predicted_data$probability_of_PCOS, decreasing=FALSE), ]
predicted_data$rank = 1:nrow(predicted_data)
