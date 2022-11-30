library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)


############################Load Data Frame####################################

File = "PCOS_data_without_infertility.xlsx"
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
LH_boxplot = boxplot(PCOS_df$`LH(mIU/mL)`, plot = FALSE)
LH_outliers = LH_boxplot$out
PCOS_df$`LH(mIU/mL)`[which(PCOS_df$`LH(mIU/mL)` %in% c(LH_outliers))] = median(PCOS_df$`LH(mIU/mL)`)


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

