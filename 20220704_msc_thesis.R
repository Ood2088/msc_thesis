rm(list = ls())
pacman::p_load(tidyverse, lavaan, psych, ltm, semTools, kableExtra, apaTables,
               report, semTable, Hmisc, semPlot, lavaan)
# install.packages("devtools")
# devtools::install_github("rempsyc/rempsyc")
options(scipen = 999)

#===============================================================================
# loading data
#===============================================================================
setwd("/Volumes/Elements/Google Drive/#Psicología/#LMU/04 Semester/Master Thesis/03 Data")
df1 <- read.csv("FOEDISES, Daten.csv",stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
df10 <- haven::read_sav("20220310_Ortho_not_cleaned.sav")
ingredients <- readxl::read_excel("ingredients.xlsx")
forCFA <- read.csv("forCFA.csv", sep = ";")
lex_info <- readxl::read_excel("BMBF Hauptstudie Daten_Ziem.xlsx",
                               sheet = 4) %>% filter(grepl("item", Typ))

#===============================================================================
# cleaning data
#===============================================================================
# *ask Xenia
df1 <- left_join(df1, df10[, c("record_id","anamn_23_exclusion")])

# filtering dataset (reproducing spss syntax)
df2 <- df1 %>% # 4542
  filter(personal_info_complete == 2) %>% # 4506
  filter(wrt4_complete == 2 | wrt3_complete == 2) %>% # 3904
  filter(wllpr_complete == 2) %>% # 3797
  filter(vsl_complete == 2) %>% # 3781
  # filter(rechtschreibtest_phonologie_complete == 2) %>% 
  filter(rechtschreibtest_lexikal_complete == 2) %>% # 3090
  filter(rechtschreibtest_sublexikal_complete == 2) %>% # 2949
  filter(rechtschreibtest_morphologie_complete == 2) %>% # 2944
  filter(cft20r_complete == 2) %>% # 2714
  filter(cft_iq_own_kl > 70) %>% # 2687
  filter(anamn_23_exclusion == 0) # # 2624

# WRT: new unified variables (rs & t) for both grades
df2 <- df2 %>% 
  mutate(wrt_rs = ifelse(!is.na(wrt3_rs), wrt3_rs, wrt4_rs),
         wrt_t = ifelse(!is.na(wrt3_t), wrt3_t, wrt4_t),
         wrt_z = scale(wrt_rs, center = TRUE, scale = TRUE)
         )

# combined reading-spelling disorder, cRSD
df2 <- df2 %>% 
  mutate(crsd_wrt = ifelse(wrt_t <= 40, 1, 0), #iSD
         crsd_wllp = ifelse(wllp_t <= 40, 1, 0), #iRD
         crsd = ifelse(crsd_wrt == 1 & crsd_wllp == 1, 1, 0)) #cRSD

# lex_info: new column
a <- which(colnames(df2)=="lex_1")
b <- which(colnames(df2)=="lex_60")
lex_data <- df2[c(a:b)]
lex_data_names <- colnames(lex_data)
lex_info <- lex_info %>% mutate(lex_items = lex_data_names)

# sublex: legal & illegal items
sublex_legal <- forCFA %>% 
  filter(dimension == 1) %>% dplyr::select(rhs) %>% pull()
sublex_illegal <- forCFA %>% 
  filter(dimension == 0) %>% dplyr::select(rhs) %>% pull()

# lex: legal & illegal items
lex_legal <- lex_info %>% 
  filter(`richtig/falsch` == 1) %>% dplyr::select(lex_items) %>% pull()
lex_illegal <- lex_info %>% 
  filter(`richtig/falsch` == 0) %>% dplyr::select(lex_items) %>% pull()

# sublex: computing sub-scales' raw scores
df2 <- df2 %>% mutate(
  sublex_legal_rs = rowSums(across(all_of(sublex_legal))), 
  sublex_illegal_rs = rowSums(across(all_of(sublex_illegal))))

# lex: computing sub-scales' raw scores
df2 <- df2 %>% mutate(
  lex_legal_rs = rowSums(across(all_of(lex_legal))), 
  lex_illegal_rs = rowSums(across(all_of(lex_illegal))))

# morph: computing total raw score
mb_dims <- c("mb_plur_rs", "mb_flex_rs", "mb_ablei_rs")
df2 <- df2 %>% mutate(mb_rs = rowSums(across(all_of(mb_dims))))

# new ablei variable (removing items after CFA)
a <- which(colnames(df2)=="mb_ablei_1")
b <- which(colnames(df2)=="mb_ablei_8")
mb_ablei_data_new <- df2[c(a:b)]
mb_ablei_data_names_new <- colnames(mb_ablei_data_new)
mb_ablei_data_names_new <- mb_ablei_data_names_new[c(1,3:4,6:8)]
df2 <- df2 %>% mutate(
  mb_ablei_rs_2 = rowSums(across(all_of(mb_ablei_data_names_new))))

# new variable names for SEM
df2$WLLP_R <- df2$wllp_rs
df2$VSL <- df2$vsl_rs
df2$WRT <- df2$wrt_t
df2$WOK <- df2$lex_illegal_rs
df2$Flex <- df2$mb_flex_rs
df2$Der <- df2$mb_ablei_rs_2
df2$GOK <- df2$sublex_illegal_rs
df2$Age <- df2$age

#===============================================================================
# training and testing sets
#===============================================================================
# seed
set.seed(123)

# new variable
df2$training <- sample(c(0,1), nrow(df2), replace = TRUE, prob = c(.8, .2))

# new datasets
df2_train <- df2[df2$training == 0,]
df2_test <- df2[df2$training == 1,]

#===============================================================================
# descriptives
#===============================================================================
# selecting relevant variables
rel_vars <- c("age", "wllp_rs", "vsl_rs", "wrt_t",
              "mb_rs", "mb_plur_rs","mb_flex_rs","mb_ablei_rs",
              "lex_rs", "lex_legal_rs", "lex_illegal_rs",
              "sublex_rs", "sublex_legal_rs", "sublex_illegal_rs")

# describing total, 3th & 4th
desc_total <- psych::describe(df2 %>% dplyr::select(dplyr::all_of(rel_vars))) 
desc_3th <- psych::describe(df2 %>% dplyr::filter(grade == 3) %>% 
                              dplyr::select(dplyr::all_of(rel_vars))) # 3th
desc_4th <- psych::describe(df2 %>% dplyr::filter(grade == 4) %>% 
                              dplyr::select(dplyr::all_of(rel_vars))) # 4th

desc_total <- desc_total %>% as.data.frame() %>% dplyr::select(mean, sd) %>% 
  rename(M_total = mean, SD_total = sd)
desc_3th <- desc_3th %>% as.data.frame() %>% dplyr::select(mean, sd) %>% 
  rename(M_3th = mean, SD_3rd = sd)
desc_4th <- desc_4th %>% as.data.frame() %>% dplyr::select(mean, sd) %>% 
  rename(M_4th = mean, SD_4th = sd)

rel_vars_text <- c("1. Age (months)", "2. RF (WLLP-R)", 
                   "3. RF (VSL)", "4. Spelling (WRT)", 
                   "5. MA", "6. Plural", "7. Inflection", "8. Derivation",
                   "9. WOK", "10. Words", "11. Pseudohom.", 
                   "12. GOK", "13. Legal pseudo.", 
                   "14. Illegal pseudo..")

tabla_desc <- desc_3th %>% bind_cols(desc_4th) %>% bind_cols(desc_total) 
rownames(tabla_desc) <- rel_vars_text
tabla_desc %>% kbl(booktabs = T, digits = 2, linesep = "",
                   caption = "Descriptive statistics (M and SD) for grades 3 and 4 and the total sample.") %>% 
  kable_styling(latex_options = "striped", stripe_index = c(6:8, 10:11, 13:14))
# write.csv(tabla_desc, "tabla_desc.csv")

#===============================================================================
# correlations
#===============================================================================
apa.cor.table(
  df2 %>% dplyr::select(dplyr::all_of(rel_vars)),
  filename = "correlations.doc",
  table.number = 1,
  show.conf.interval = FALSE,
  show.sig.stars = TRUE,
  landscape = TRUE
)

cors <- rcorr(as.matrix(df2[rel_vars]), type = "pearson")
# write.csv(data.frame(cors$r), "cors_r.csv")
# write.csv(data.frame(cors$P), "cors_P.csv")

#===============================================================================
# psychometric analysis
#===============================================================================
#-------------------------------------------------------------------------------
# WOK
#-------------------------------------------------------------------------------
# total
alpha(df2 %>% dplyr::select(all_of(lex_data_names)))

# illegal
alpha(df2 %>% dplyr::select(all_of(lex_illegal)))

# legal
alpha(df2 %>% dplyr::select(all_of(lex_legal)))

#-------------------------------------------------------------------------------
# GOK
#-------------------------------------------------------------------------------
# total
alpha(df2 %>% dplyr::select(all_of((c(sublex_illegal, sublex_legal)))))

# illegal
alpha(df2 %>% dplyr::select(all_of(sublex_illegal)))

# legal
alpha(df2 %>% dplyr::select(all_of(sublex_legal)))

#-------------------------------------------------------------------------------
# MA
#-------------------------------------------------------------------------------
# a. data
#-------------------------------------------------------------------------------
# ablei = derivation
# plur = plural formation
# flex = inflection

# morph awareness
a <- which(colnames(df2)=="mb_plur_1")
b <- which(colnames(df2)=="mb_ablei_8")
mb_data1D <- df2[c(a:b)]
mb_data1D <- mb_data1D[c(1:8,29:36,60:67)]
mb_data_names1D <- colnames(mb_data1D)

# plural formation
a <- which(colnames(df2)=="mb_plur_1")
b <- which(colnames(df2)=="mb_plur_8")
mb_plur_data <- df2[c(a:b)]
mb_plur_data_names <- colnames(mb_plur_data)

# inflection
a <- which(colnames(df2)=="mb_flex_1")
b <- which(colnames(df2)=="mb_flex_8")
mb_flex_data <- df2[c(a:b)]
mb_flex_data_names <- colnames(mb_flex_data)

# derivation
a <- which(colnames(df2)=="mb_ablei_1")
b <- which(colnames(df2)=="mb_ablei_8")
mb_ablei_data <- df2[c(a:b)]
mb_ablei_data_names <- colnames(mb_ablei_data)

mb_data <- cbind(mb_plur_data, mb_flex_data, mb_ablei_data)
mb_data_names <- colnames(mb_data)

# 1a. CFA - 1D
#-------------------------------------------------------------------------------
# specification
names_lv <- paste0("mb", " =~")
names_ov <- paste(mb_data_names1D, collapse=" + ")
sem_formula <- paste(names_lv, names_ov)

# estimation
fit1 <- sem(sem_formula, data=mb_data1D, ordered=mb_data_names1D, 
            estimator="WLSMV")
summary(fit1, standardized = TRUE, fit.measures = TRUE)
round(fitMeasures(fit1)[c("chisq.scaled","df.scaled","pvalue.scaled",
                          "cfi.scaled","rmsea.scaled","srmr")],3)

# reliability
semTools::reliability(fit1, return.total = TRUE) # McD-O = omega3

# b1. CFA - 3D
#-------------------------------------------------------------------------------
# specification
names_lv_plur <- paste0("mb_plur", " =~")
names_ov_plur <- paste(mb_plur_data_names, collapse=" + ")
sem_formula_plur <- paste(names_lv_plur, names_ov_plur)

names_lv_flex <- paste0("mb_flex", " =~")
names_ov_flex <- paste(mb_flex_data_names, collapse=" + ")
sem_formula_flex <- paste(names_lv_flex, names_ov_flex)

names_lv_ablei <- paste0("mb_ablei", " =~")
names_ov_ablei <- paste(mb_ablei_data_names, collapse=" + ")
sem_formula_ablei <- paste(names_lv_ablei, names_ov_ablei)

sem_formula <- paste(sem_formula_plur, sem_formula_flex, sem_formula_ablei, 
                     sep="\n")

# estimation
fit2 <- sem(sem_formula, data=mb_data, ordered=mb_data_names, estimator="WLSMV")
summary(fit2, standardized = TRUE, fit.measures = TRUE)
round(fitMeasures(fit2)[c("chisq.scaled","df.scaled","pvalue.scaled",
                          "cfi.scaled","rmsea.scaled","srmr")],3)

# reliability
semTools::reliability(fit2, return.total = TRUE)

# model comparison
anova(fit1, fit2)

# b2. CFA - 2D (eliminated: mb_plur)
#-------------------------------------------------------------------------------
# specification
sem_formula <- paste(sem_formula_flex, sem_formula_ablei, sep="\n")

# estimation
fit3 <- sem(sem_formula, data=mb_data, ordered=mb_data_names, estimator="WLSMV")
summary(fit3, standardized = TRUE, fit.measures = TRUE)
round(fitMeasures(fit3)[c("chisq.scaled","df.scaled","pvalue.scaled",
                          "cfi.scaled","rmsea.scaled","srmr")],3)

# reliability
semTools::reliability(fit3, return.total = TRUE)

# model comparison

# b3. CFA - 2D (eliminated: mb_plur, mb_ablei_2, mb_ablei_5)
#-------------------------------------------------------------------------------
# mb_ablei: removing items
names_lv_ablei_new <- paste0("mb_ablei", " =~")
names_ov_ablei_new <- paste(mb_ablei_data_names[c(1,3:4,6:8)], collapse=" + ")
sem_formula_ablei_new <- paste(names_lv_ablei_new, names_ov_ablei_new)

# specification
sem_formula <- paste(sem_formula_flex, sem_formula_ablei_new, sep="\n")

# estimation
fit4 <- sem(sem_formula, data=mb_data, ordered=mb_data_names, estimator="WLSMV")
summary(fit4, standardized = TRUE, fit.measures = TRUE)
round(fitMeasures(fit4)[c("chisq.scaled","df.scaled","pvalue.scaled",
                          "cfi.scaled","rmsea.scaled","srmr")],3)

# reliability
semTools::reliability(fit4, return.total = TRUE)

#===============================================================================
# SEM
#===============================================================================
# new ablei variable
#-------------------------------------------------------------------------------
# models
#-------------------------------------------------------------------------------
# model #1: no correlation between SL tasks
model1 <- '
### measurement model
Read =~ a*WLLP_R + b*VSL
Lex =~ c*WRT + d*WOK
Morph =~ e*Flex + f*Der
### regression model
Read ~ Lex + Morph + Age + GOK
Age ~~ GOK'

# model #2: correlation between SL tasks, Morph ~~ sublex
model2 <- '
### measurement model
Read =~ a*WLLP_R + b*VSL
Lex =~ c*WRT + d*WOK
Morph =~ e*Flex + f*Der
### regression model
Read ~ Lex + Morph + Age + GOK
### SL parameter
Morph ~~ GOK
Age ~~ GOK'

# training set
#--------------------------------------------------------------------------------
# model #1: no correlation between SL tasks
fit_model1a <- sem(model1, data = df2_train, estimator = "MLM")
summary(fit_model1a, standardized = TRUE, fit.measures = TRUE)
round(fitMeasures(fit_model1a)[c("chisq.scaled", "df.scaled","pvalue.scaled", 
                                "cfi.robust","rmsea.robust","srmr_bentler",
                                "aic","bic")],3)
semPaths(object = fit_model1a, whatLabels = 'std', edge.label.cex = 1, 
         what='mod', fade = FALSE, optimizeLatRes = TRUE, rotation = 1, 
         layout = "tree2", nCharNodes = 6)

# model #2: correlation between SL tasks, Morph ~~ sublex
fit_model2a <- sem(model2, data = df2_train, estimator = "MLM", fixed.x = FALSE)
summary(fit_model2a, standardized = TRUE, fit.measures = TRUE)
round(fitMeasures(fit_model2a)[c("chisq.scaled", "df.scaled","pvalue.scaled", 
                                 "cfi.robust","rmsea.robust","srmr_bentler",
                                 "aic","bic")],3)
semPaths(object = fit_model2a, whatLabels = 'std', edge.label.cex = 1, 
         what='mod', fade = FALSE, optimizeLatRes = TRUE, rotation = 1, 
         layout = "tree2", nCharNodes = 6)

# model comparison
anova(fit_model1a, fit_model2a)

# testing set
#-------------------------------------------------------------------------------
# model #1: no correlation between SL tasks
fit_model1b <- sem(model1, data = df2_test, estimator = "MLM")
summary(fit_model1b, standardized = TRUE, fit.measures = TRUE)
round(fitMeasures(fit_model1b)[c("chisq.scaled", "df.scaled","pvalue.scaled", 
                                "cfi.robust","rmsea.robust","srmr_bentler",
                                "aic","bic")],3)
semPaths(object = fit_model1b, whatLabels = 'std', edge.label.cex = 1, 
         what='mod', fade = FALSE, optimizeLatRes = TRUE, rotation = 1, 
         layout = "tree2", nCharNodes = 6)

# model #2: correlation between SL tasks, Morph ~~ sublex
fit_model2b <- sem(model2, data = df2_test, estimator = "MLM", fixed.x = FALSE)
summary(fit_model2b, standardized = TRUE, fit.measures = TRUE)
round(fitMeasures(fit_model2b)[c("chisq.scaled", "df.scaled","pvalue.scaled", 
                                 "cfi.robust","rmsea.robust","srmr_bentler",
                                 "aic","bic")],3)
semPaths(object = fit_model2b, whatLabels = 'std', edge.label.cex = 1, 
         what='mod', fade = FALSE, optimizeLatRes = TRUE, rotation = 1, 
         layout = "tree2", nCharNodes = 6)

# model comparisons
anova(fit_model1b, fit_model2b)
