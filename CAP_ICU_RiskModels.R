##########################################################################################################################################

if (!require(openxlsx)) {install.packages("openxlsx");library(openxlsx)}
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if (!require(gridExtra)) {install.packages("gridExtra");library(gridExtra)}
if (!require(grid)) {install.packages("grid");library(grid)}
if (!require(broom)) {install.packages("broom");library(broom)}
if (!require(readxl)) {install.packages("readxl"); library(readxl)}
if (!require(writexl)) {install.packages("writexl");library(writexl)}
if (!require(gtsummary)) {install.packages("gtsummary"); library(gtsummary)}
if (!require(gt)) {install.packages("gt");library(gt)}
if (!require(broom)) {install.packages("lubridate");library(lubridate)}
if (!require(survival)) {install.packages("survival"); library(survival)}
if (!require(survminer)) {install.packages("survminer"); library(survminer)}
if (!require(tidyverse)) {install.packages("tidyverse");library(tidyverse)}
if (!require(WeightIt)) {install.packages("WeightIt");library(WeightIt)}
if (!require(coxme)) {install.packages("coxme");library(coxme)}
if (!require(broom)) {install.packages("broom");library(broom)}
if (!require("MLmetrics")) {install.packages("MLmetrics");library(MLmetrics)}
if (!require("pROC")) {install.packages("pROC");library(pROC)}
if (!require("givitiR")) {install.packages("givitiR");library(givitiR)}

##########################################################################################################################################

# Data Input
load(file = "df_geral_com_criterios.RData")

##########################################################################################################################################

# Defining the Database for Multi/Univariate Analyses
df_model <- df_geral_com_criterios %>%
  droplevels() %>% 
  mutate(
    period = factor(Ano_Agrupado),
    #Saps3Q = cut_number(Saps3Points, n = 4), # SAPS3 quartiles
  ) %>% 
  select(
    period,
    Gender,
    SofaScore,
    Saps3Points,
    Beds,
    Age,
    Idade_Agrupada,
    Idade_Agrupada2,
    MFI_Agregado, 
    AdmissionSourceName,
    ChronicHealthStatusName,
    hypertension,
    diabetes,
    obesity,
    IsImmunossupression,
    IsSteroidsUse,
    IsSevereCopd,
    IsAsthma,
    tobacco,
    IsChfNyha,
    cancer,
    ResourceIsHFNC,
    ResourceIsRenalReplacementTherapy,
    IsRenalReplacementTherapy,
    ResourceIsVasopressors,
    IsVasopressors,
    ResourceIsMechanicalVentilation,
    ResourceIsNonInvasiveVentilation,
    Vent_24h,
    Vent_Resource,
    HospitalCode,
    UnitCode,
    HospitalDischargeCode_trunc,
    HospitalDischargeCode_trunc_bin,
    AnoAgrupado_HospitalDischarge_trunc,
    UnitDischargeCode_trunc, 
    HospitalLengthStay,
    HospitalLengthStay_trunc,
    UnitLengthStay
  ) %>% 
  mutate_at(
    c("hypertension",
      "diabetes",
      "obesity",
      "IsImmunossupression",
      "IsSteroidsUse",
      "IsSevereCopd",
      "IsAsthma",
      "tobacco",
      "IsChfNyha",
      "cancer",
      "ResourceIsHFNC",
      "ResourceIsRenalReplacementTherapy",
      "IsRenalReplacementTherapy",
      "ResourceIsVasopressors",
      "IsVasopressors",
      "ResourceIsMechanicalVentilation",
      "ResourceIsNonInvasiveVentilation"),
    function(x) { return(as.factor(if_else(x == 1, "yes", "no"))) }
  ) 

# First 24h IMV subgroup
subgroup_vm_24 <- df_model %>%
  filter(Vent_24h == "VM")

# Filtering the subgroup to find the p-value
df_filtered <- df_model %>%
  filter(AnoAgrupado_HospitalDischarge_trunc %in% c("2018-2019_D", "2022-2023_D"))

# Filtering the subgroup to find the p-value (IMV subgroup)
df_filtered_VMI <- subgroup_vm_24 %>%
  filter(AnoAgrupado_HospitalDischarge_trunc %in% c("2018-2019_D", "2022-2023_D"))

##########################################################################################################################################

save(df_model, file = "df_model.RData")
save(subgroup_vm_24, file = "subgroup_vm_24.RData")

setwd('D:/PUC Gabriel/IC Gabriel/Base 2018-2023/')
load(file = "df_model.RData")
load(file = "subgroup_vm_24.RData")

##########################################################################################################################################

#Descriptive Analysis
descritiva =
  tbl_summary(data = subgroup_vm_24, #base
              missing = "ifany",
              missing_text = "Not informed",
              percent = "column",
              include = c("ResourceIsHFNC"),
              #include = c("Gender","Age","Idade_Agrupada","Idade_Agrupada2","MFI_Agregado","Saps3Points","SofaScore",
              #            "AdmissionSourceName","ChronicHealthStatusName","hypertension","diabetes","obesity",
              #            "IsImmunossupression","IsSteroidsUse","IsSevereCopd","IsAsthma","tobacco","IsChfNyha","cancer",
              #            "ResourceIsRenalReplacementTherapy", "IsRenalReplacementTherapy", "ResourceIsVasopressors", "IsVasopressors",
              #           "Vent_Resource", "Vent_24h", "HospitalDischargeCode_trunc","UnitDischargeCode_trunc","HospitalLengthStay","UnitLengthStay"),
              by = period #separar a descritiva em colunas referentes a variável
              #by = HospitalDischargeCode_trunc
              #by = AnoAgrupado_HospitalDischarge_trunc
  )%>%
  add_overall()%>% #add overall column
  add_p()%>%
  modify_header(label = "**Feature**")%>%
  as_gt() %>%
  tab_header(title = "Descriptive Analysis",
             #subtitle = ""
  )
descritiva

#Excel with Descriptive Data
df_descritiva <- as.data.frame(descritiva)
write.xlsx(df_descritiva, file = "name.xlsx")

##########################################################################################################################################

# Univariate Analysis (Survival Curves)

# First plot
survXa <- survfit(
  Surv(HospitalLengthStay_trunc, , HospitalDischargeCode_trunc_bin) ~ ResourceIsVasopressors, #change the variable according to the analysis
  data = df_model)
survXa
plot_surv_agregadoXa <- (ggsurvplot(survXa, conf.int = TRUE,
                                    ggtheme = theme_bw(), 
                                    censor = FALSE,
                                    legend.title = "",
                                    risk.table = TRUE, pval.method = TRUE,
                                    legend.labs = c("No","Yes"), #match according to the defined variable's categories (quantity and text)
                                    xlab = "Time (days)",
                                    title = " (A) Vasopressors", 
                                    pval = TRUE, pval.size = 4,
                                    surv.scale = "percent",
                                    palette = c("navy","red") #match according to the defined variable's categories (quantity and text)
)
)

plot_surv_agregadoXa

# Second plot
survXb <- survfit(
  Surv(HospitalLengthStay_trunc, , HospitalDischargeCode_trunc_bin) ~ Vent_Resource,  #change the variable according to the analysis
  data = df_model)

plot_surv_agregadoXb <- (ggsurvplot(survXb, conf.int = TRUE,
                                    ggtheme = theme_bw(), 
                                    censor = FALSE,
                                    legend.title = "",
                                    risk.table = TRUE, pval.method = TRUE,
                                    legend.labs = c("NIV","IMV"), #match according to the defined variable's categories (quantity and text)
                                    xlab = "Time (days)",
                                    title = " (B) Respiratory Support", 
                                    pval = TRUE, pval.size = 4,
                                    surv.scale = "percent",
                                    palette = c("navy","red") #match according to the defined variable's categories (quantity and text)
)
)

plot_surv_agregadoXb

# Combining both plots and generating figure 1
grobXa_plot <- ggplotGrob(plot_surv_agregadoXa$plot)
grobXa_table <- ggplotGrob(plot_surv_agregadoXa$table)
grobXb_plot <- ggplotGrob(plot_surv_agregadoXb$plot)
grobXb_table <- ggplotGrob(plot_surv_agregadoXb$table)

# Combining the plots with their respective risk.tables
combined_grobXa <- arrangeGrob(grobXa_plot, grobXa_table, heights = c(4, 1))
combined_grobXb <- arrangeGrob(grobXb_plot, grobXb_table, heights = c(4, 1))

# Combining the final grobs for the final figure
FiguraX <- arrangeGrob(combined_grobXa, combined_grobXb, ncol = 2)

# Result
grid.draw(FiguraX)

# Save
setwd('D:/PUC Gabriel/IC Gabriel/Base 2018-2023/Figuras Finais Artigo/Base Geral')
setwd('D:/PUC Gabriel/IC Gabriel/Base 2018-2023/Figuras Finais Artigo/Subgrupo VMI 24h')
ggsave("Figura 6 Sobrevivência.jpeg", FiguraX, width = 16.5, height = 10.5, dpi = 200)
ggsave("Figura X Sobrevivência VM.jpeg", FiguraX, width = 16.5, height = 10.5, dpi = 200)


##########################################################################################################################################


# Multivariate analysis (cox model)

model_no_var_PF <-
  Vent_Resource ~
  period +
  Idade_Agrupada2 +
  # AdmissionSourceName + 
  ChronicHealthStatusName +
  obesity + 
  IsImmunossupression + 
  IsSteroidsUse +
  IsSevereCopd +
  IsChfNyha +
  cancer + 
  ResourceIsRenalReplacementTherapy +
  ResourceIsVasopressors 
# ResourceIsNonInvasiveVentilation +
# ResourceIsMechanicalVentilation 


# Estimating Propensity scores and weights - "ATE" estimand
ps_values_PF <- 
  weightit(
    model_no_var_PF,
    family = "binomial",
    estimand = "ATE",
    data = df_model,
    method = "ps"
  )


# Obtaining propensity scores and IPT weights for each patients
df_cox_model_ps_PF <-
  df_model %>%
  bind_cols(
    ps_ate = ps_values_PF$ps,
    ps_w_ate = ps_values_PF$weights
  ) 


cox_model_full_PF <- 
  Surv(HospitalLengthStay_trunc, HospitalDischargeCode_trunc_bin) ~
  period +
  Idade_Agrupada2 +
  # AdmissionSourceName + 
  ChronicHealthStatusName +
  obesity + 
  IsImmunossupression + 
  IsSteroidsUse +
  IsSevereCopd +
  IsChfNyha +
  cancer + 
  ResourceIsRenalReplacementTherapy +
  ResourceIsVasopressors +
  # ResourceIsNonInvasiveVentilation +
  # ResourceIsMechanicalVentilation +
  Vent_Resource +
  (1 | HospitalCode)


coxme_full_PF <-
  coxme(
    cox_model_full_PF,
    weights = ps_w_ate,
    data = df_cox_model_ps_PF
  )
summary(ps_values_PF$weights)
summary(df_cox_model_ps_PF)


# Summary cox
summary_coxme_full_PF <- summary(coxme_full_PF)
# Adjusting column names according to the summary output
df_cox_model_full_PF_results <- data.frame(
  Term = rownames(summary_coxme_full_PF$coef),
  Coef = summary_coxme_full_PF$coef[, "coef"],
  HR = summary_coxme_full_PF$coef[, "exp(coef)"],
  StdError = summary_coxme_full_PF$coef[, "se(coef)"],
  Zvalue = summary_coxme_full_PF$coef[, "z"],
  Pvalue = summary_coxme_full_PF$coef[, "p"])


# Calculating CI limits for HR
df_cox_model_full_PF_results$CI_lower = exp(log(df_cox_model_full_PF_results$HR) - 1.96 * df_cox_model_full_PF_results$StdError)
df_cox_model_full_PF_results$CI_upper = exp(log(df_cox_model_full_PF_results$HR) + 1.96 * df_cox_model_full_PF_results$StdError)


# Adding HR column and adjusting p-values
df_cox_model_full_PF_results$HR = paste0(round(df_cox_model_full_PF_results$HR, 3))
df_cox_model_full_PF_results$p_adj = ifelse(df_cox_model_full_PF_results$Pvalue < 0.001, "<0.001", round(df_cox_model_full_PF_results$Pvalue, 3))


# Selecting and renaming columns for the final output
df_cox_model_full_PF_results_final <- df_cox_model_full_PF_results %>%
  select(Term, HR, p_adj, CI_lower, CI_upper)


# Result
print(df_cox_model_full_PF_results_final)


# Manually adding reference categories
ref_categories <- data.frame(
  Term = c("Period 2018-2019 (Ref)", 
           "Age <65 (Ref)", 
           #"Admission from Emergency (Ref)",
           "Performance Status: Ambulant / independent (Ref)",
           "NIV (Ref)"
  ),
  HR = 1,  # HR = 1 for reference category
  p_adj = 1,  # Adjusted p-value (not significant as it is the reference)
  CI_lower = 1,  # CI Lower
  CI_upper = 1   # CI upper
)
# Convert HR to numeric
ref_categories$HR <- as.numeric(ref_categories$HR)


# Combine with model results
df_cox_model_full_PF_results_final <- rbind(df_cox_model_full_PF_results_final, ref_categories)
# Reorder based on term if necessary
df_cox_model_full_PF_results_final$Term <- factor(df_cox_model_full_PF_results_final$Term, levels = unique(df_cox_model_full_PF_results_final$Term))


# Convert $Term to numeric for plotting the graph
df_cox_model_full_PF_results_final$HR <- as.numeric(df_cox_model_full_PF_results_final$HR)


# Renaming the term column
df_cox_model_full_PF_results_final <- df_cox_model_full_PF_results_final %>%
  mutate(Term = recode(Term,
                       "period2022-2023" = "Period 2022-2023",
                       "Idade_Agrupada265-79" = "Age 65 - 79",
                       "Idade_Agrupada2>=80" = "Age 80+",
                       # "AdmissionSourceNameOperating Room / Other" = "Admission from Operating Room / Other",
                       # "AdmissionSourceNameOther unit at your hospital" = "Admission from Other unit at your hospital",
                       # "AdmissionSourceNameWard/Floor/Step down Unit" = "Admission from Ward/Floor/Step down Unit",
                       "ChronicHealthStatusNameMajor assistance / bedridden" = "Performance Status: Major assistance / bedridden",
                       "ChronicHealthStatusNameMinor assistance" = "Performance Status: Minor assistance",
                       "obesityyes" = "Obesity",
                       "IsImmunossupressionyes" = "Immunossupression",
                       "IsSteroidsUseyes" = "Steroids",
                       "IsSevereCopdyes" = "Severe COPD",
                       "IsChfNyhayes" = "Chronic Heart Failure",
                       "canceryes" = "Cancer",
                       "ResourceIsRenalReplacementTherapyyes" = "RRT",
                       "ResourceIsVasopressorsyes" = "Vasopressors",
                       # "ResourceIsNonInvasiveVentilationyes" = "NIV",
                       # "ResourceIsMechanicalVentilationyes" = "IMV"
                       "Vent_ResourceVM" = "IMV"
                       
  ))

# Rearranging the order of the df for correct plotting
current_order <- c("Period 2022-2023","Age 65 - 79","Age 80+",
                   #"Admission from Operating Room / Other","Admission from Other unit at your hospital","Admission from Ward/Floor/Step down Unit",
                   "Performance Status: Major assistance / bedridden","Performance Status: Minor assistance",
                   "Obesity","Immunossupression","Steroids","Severe COPD","Chronic Heart Failure",
                   "Cancer","RRT","Vasopressors","IMV","Period 2018-2019 (Ref)", "Age <65 (Ref)", 
                   #"Admission from Emergency (Ref)",
                   "Performance Status: Ambulant / independent (Ref)","NIV (Ref)")
desired_order <- c("Period 2018-2019 (Ref)","Period 2022-2023","Age <65 (Ref)","Age 65 - 79","Age 80+",
                   #"Admission from Emergency (Ref)","Admission from Operating Room / Other","Admission from Other unit at your hospital","Admission from Ward/Floor/Step down Unit",
                   "Performance Status: Ambulant / independent (Ref)","Performance Status: Minor assistance",
                   "Performance Status: Major assistance / bedridden","Obesity","Immunossupression","Steroids",
                   "Severe COPD","Chronic Heart Failure","Cancer","RRT","Vasopressors","NIV (Ref)","IMV")
desired_order <- rev(desired_order) #in the plot it places them from bottom to top


# Finding indices for reordering
reorder_indices <- match(desired_order, current_order)


# Reordering df
df_cox_model_full_PF_results_final <- df_cox_model_full_PF_results_final[reorder_indices, ]


# Ensure terms are in the desired order using the factor function
df_cox_model_full_PF_results_final$Term <- factor(df_cox_model_full_PF_results_final$Term, levels = desired_order)


# Forest Plot
Cox_plot <- ggplot(df_cox_model_full_PF_results_final, aes(y = Term, x = HR, xmin = CI_lower, xmax = CI_upper)) +
  geom_point(color = "black", size = 2.5) +
  geom_errorbarh(height = .50, size = .65, color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  xlab("Hazard Ratio (95% CI)") +
  ylab("") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10.5))
ggsave("Cox Model.jpeg", Cox_plot, width = 13.5, height = 8.65, dpi = 200)

Cox_plot

# Excel with HR and CI table
#df_cox <- as.data.frame(df_cox_model_full_PF_results_final)
#write.xlsx(df_cox_model_full_PF_results_final, file = "Tabela HR e CI cox.xlsx")


##############################################################################################################


# Define Cox model for patients with Vent_Resource == "VM" (IMV)

cox_vm <- 
  coxph(
    Surv(HospitalLengthStay_trunc, HospitalDischargeCode_trunc_bin) ~
      period +
      Idade_Agrupada2 +
      ChronicHealthStatusName +
      obesity + 
      IsImmunossupression + 
      IsSteroidsUse +
      IsSevereCopd +
      IsChfNyha +
      cancer + 
      ResourceIsRenalReplacementTherapy +
      ResourceIsVasopressors,
    data = subgroup_vm_24
)


# Obtain the summary of the coxme model
summary_cox_vm <- summary(cox_vm)
# Adjust column names according to the summary output
df_cox_vm <- data.frame(
  Term = rownames(summary_cox_vm$coef),
  Coef = summary_cox_vm$coef[, "coef"],
  HR = summary_cox_vm$coef[, "exp(coef)"],
  StdError = summary_cox_vm$coef[, "se(coef)"],
  Zvalue = summary_cox_vm$coef[, "z"],
  Pvalue = summary_cox_vm$coef[, "Pr(>|z|)"])


# Calculating confidence interval limits for the HR
df_cox_vm$CI_lower = exp(log(df_cox_vm$HR) - 1.96 * df_cox_vm$StdError)
df_cox_vm$CI_upper = exp(log(df_cox_vm$HR) + 1.96 * df_cox_vm$StdError)


# Adding the HR column and adjusting p-values
df_cox_vm$HR = paste0(round(df_cox_vm$HR, 3))
df_cox_vm$p_adj = ifelse(df_cox_vm$Pvalue < 0.001, "<0.001", round(df_cox_vm$Pvalue, 3))


# Selecting and renaming columns for the final output
df_cox_vm_final <- df_cox_vm %>%
  select(Term, HR, p_adj, CI_lower, CI_upper)


# Displaying final results so far 
print(df_cox_vm_final)


# Manually adding reference categories
ref_categories_vm <- data.frame(
  Term = c("Period 2018-2019 (Ref)", 
           "Age <65 (Ref)", 
           #"Admission from Emergency (Ref)",
           "Performance Status: Ambulant / independent (Ref)"
  ),
  HR = 1,  # HR = 1 for reference category
  p_adj = 1,  # adjusted p-value (not significant as it’s the reference)
  CI_lower = 1,  # CI Lower
  CI_upper = 1   # CI Upper
)
# Convert HR to numeric
ref_categories_vm$HR <- as.numeric(ref_categories_vm$HR)


# Combine with model results
df_cox_vm_final <- rbind(df_cox_vm_final, ref_categories_vm)
# Reorder based on Term if necessary
df_cox_vm_final$Term <- factor(df_cox_vm_final$Term, levels = unique(df_cox_vm_final$Term))


# Convert $Term to numeric for plotting
df_cox_vm_final$HR <- as.numeric(df_cox_vm_final$HR)


# Renaming the Term column in df
df_cox_vm_final <- df_cox_vm_final %>%
  mutate(Term = recode(Term,
                       "period2022-2023" = "Period 2022-2023",
                       "Idade_Agrupada265-79" = "Age 65 - 79",
                       "Idade_Agrupada2>=80" = "Age 80+",
                       # "AdmissionSourceNameOperating Room / Other" = "Admission from Operating Room / Other",
                       # "AdmissionSourceNameOther unit at your hospital" = "Admission from Other unit at your hospital",
                       # "AdmissionSourceNameWard/Floor/Step down Unit" = "Admission from Ward/Floor/Step down Unit",
                       "ChronicHealthStatusNameMajor assistance / bedridden" = "Performance Status: Major assistance / bedridden",
                       "ChronicHealthStatusNameMinor assistance" = "Performance Status: Minor assistance",
                       "obesityyes" = "Obesity",
                       "IsImmunossupressionyes" = "Immunossupression",
                       "IsSteroidsUseyes" = "Steroids",
                       "IsSevereCopdyes" = "Severe COPD",
                       "IsChfNyhayes" = "Chronic Heart Failure",
                       "canceryes" = "Cancer",
                       "ResourceIsRenalReplacementTherapyyes" = "RRT",
                       "ResourceIsVasopressorsyes" = "Vasopressors",
  ))

# Changing the order of the df for correct plotting
current_order_vm <- c("Period 2022-2023","Age 65 - 79","Age 80+",
                      #"Admission from Operating Room / Other","Admission from Other unit at your hospital","Admission from Ward/Floor/Step down Unit",
                      "Performance Status: Major assistance / bedridden","Performance Status: Minor assistance",
                      "Obesity","Immunossupression","Steroids","Severe COPD","Chronic Heart Failure",
                      "Cancer","RRT","Vasopressors","Period 2018-2019 (Ref)", "Age <65 (Ref)", 
                      #"Admission from Emergency (Ref)",
                      "Performance Status: Ambulant / independent (Ref)")
desired_order_vm <- c("Period 2018-2019 (Ref)","Period 2022-2023","Age <65 (Ref)","Age 65 - 79","Age 80+",
                      #"Admission from Emergency (Ref)","Admission from Operating Room / Other","Admission from Other unit at your hospital","Admission from Ward/Floor/Step down Unit",
                      "Performance Status: Ambulant / independent (Ref)","Performance Status: Minor assistance",
                      "Performance Status: Major assistance / bedridden","Obesity","Immunossupression","Steroids",
                      "Severe COPD","Chronic Heart Failure","Cancer","RRT","Vasopressors")
desired_order_vm <- rev(desired_order_vm) # Reversed for the plot as it displays bottom to top


# Finding indices to reorder
reorder_indices <- match(desired_order_vm, current_order_vm)


# Reordering the dataframe
df_cox_vm_final <- df_cox_vm_final[reorder_indices, ]


# Ensuring terms are in the desired order using the factor function
df_cox_vm_final$Term <- factor(df_cox_vm_final$Term, levels = desired_order_vm)


# Forest Plot
Cox_plot_vm <- ggplot(df_cox_vm_final, aes(y = Term, x = HR, xmin = CI_lower, xmax = CI_upper)) +
  geom_point(color = "black", size = 2.5) +
  geom_errorbarh(height = .50, size = .65, color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  xlab("Hazard Ratio (95% CI)") +
  ylab("") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10.5))
ggsave("Cox Model VM.jpeg", Cox_plot, width = 13.5, height = 8.65, dpi = 200)

Cox_plot_vm

# Excel with HR and CI table
# df_cox_vm <- as.data.frame(df_cox_vm_final)
# write.xlsx(df_cox_vm_final, file = "Tabela HR e CI cox VM.xlsx")


##############################################################################################################


# Logistic Regression

regressao <- glm(HospitalDischargeCode_trunc_bin ~
                   period +
                   Idade_Agrupada2 +
                   ChronicHealthStatusName +
                   obesity +
                   IsImmunossupression +
                   IsSteroidsUse +
                   IsSevereCopd +
                   IsChfNyha +
                   cancer +
                   ResourceIsRenalReplacementTherapy +
                   ResourceIsVasopressors +
                   Vent_Resource,
                             data = df_model,
                             family = binomial(link = "logit"))

# Extract coefficients, CIs, and p-values
summary_tbl <- broom::tidy(regressao, exponentiate = TRUE, conf.int = TRUE)

# Add columns std.error and statistic with values NA
ref_data <- data.frame(
  term = c("Period 2018-2019 (ref)", "Age <65 (ref)", "Performance Status: Ambulant / independent (ref)", "NIV (ref)"),
  estimate = rep(1, 4),       # OR = 1 for reference classes
  std.error = NA,             # SD is NA for reference classes
  statistic = NA,             # Statistic is NA for reference classes
  conf.low = rep(1, 4),       # Lower CI = 1 (no uncertainty)
  conf.high = rep(1, 4),      # Upper CI = 1 (no uncertainty)
  p.value = NA                # Not applicable, as these are reference classes
)

# Combine summary_tbl with references
summary_tbl <- rbind(ref_data, summary_tbl)

# View
df_regressao_multi_bruto <- as.data.frame(summary_tbl)

# Set the new order of terms as desired
new_order <- c("(Intercept)",
               "Vent_ResourceVM",
               "NIV (ref)",
               "ResourceIsVasopressorsyes",
               "ResourceIsRenalReplacementTherapyyes",
               "canceryes",
               "IsChfNyhayes",
               "IsSevereCopdyes",
               "IsSteroidsUseyes",
               "IsImmunossupressionyes",
               "obesityyes",
               "ChronicHealthStatusNameMajor assistance / bedridden",
               "ChronicHealthStatusNameMinor assistance",
               "Performance Status: Ambulant / independent (ref)",
               "Idade_Agrupada2>=80",
               "Idade_Agrupada265-79",
               "Age <65 (ref)",
               "period2022-2023",
               "Period 2018-2019 (ref)")

# Reorder the table according to the new order
summary_tbl <- summary_tbl %>%
  mutate(term = factor(term, levels = new_order)) %>%
  arrange(term)

# Assign labels for all categories and subcategories
labels <- c("(Intercept)","IMV","NIV (ref)","Vasopressors","RRT","Cancer",
            "Chronic Heart Failure","Severe COPD","Steroids",
            "Immunosuppression","Obesity","Performance Status: Major assistance / bedridden",
            "Performance Status: Minor assistance","Performance Status: Ambulant / independent (ref)",
            "Age 80+","Age 65 - 79","Age <65 (ref)","Period 2022-2023","Period 2018-2019 (ref)")

# Update term labels to cover all categories and subcategories
summary_tbl$term <- factor(summary_tbl$term, levels = summary_tbl$term, labels = labels)

# View organized df
df_regressao_multi_ordenado <- as.data.frame(summary_tbl)

# Create the Forest Plot without p-value
regression_plot <- ggplot(summary_tbl, aes(x = estimate, y = term)) +
  geom_point(shape = 17, color = "black", size = 2.83) +  # Point only, without p-value
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.5, size = .65, color = "black") +  # Horizontal error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Dashed line at Odds Ratio of 1
  theme_minimal() +
  labs(x = "Odds Ratio (95% CI)", y = "")
    #title = "Forest Plot: Multivariate Logistic Regression") +
    theme(panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10),  # Adjust y-axis text size
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and enlarge title
      plot.subtitle = element_text(hjust = 0.5, size = 12),  # Center and adjust subtitle
      axis.text.x = element_text(size = 10))  # Adjust x-axis text size
ggsave("Logistic Regression.jpeg", regression_plot, width = 13.5, height = 8.65, dpi = 200)

regression_plot

# Excel with OR and CI table
# write.xlsx(df_regressao_multi_ordenado, file = "Tabela OR e CI regressão multi.xlsx")

########################################################################################################################

# LOGISTIC REGRESSION VM

regressao_vm <- glm(HospitalDischargeCode_trunc_bin ~
                      period +
                      Idade_Agrupada2 +
                      ChronicHealthStatusName +
                      obesity +
                      IsImmunossupression +
                      IsSteroidsUse +
                      IsSevereCopd +
                      IsChfNyha +
                      cancer +
                      ResourceIsRenalReplacementTherapy +
                      ResourceIsVasopressors, 
                    data = subgroup_vm_24,
                    family = binomial(link = "logit"))

# Extract coefficients, CIs, and p-values
summary_tbl_vm <- broom::tidy(regressao_vm, exponentiate = TRUE, conf.int = TRUE)

# Add columns std.error and statistic with NA values
ref_data_vm <- data.frame(
  term = c("Period 2018-2019 (ref)", "Age <65 (ref)", "Performance Status: Ambulant / independent (ref)"),
  estimate = rep(1, 3),       # OR = 1 for reference classes
  std.error = NA,             # SD is NA for reference classes
  statistic = NA,             # Statistic is NA for reference classes
  conf.low = rep(1, 3),       # Lower CI = 1 (no uncertainty)
  conf.high = rep(1, 3),      # Upper CI = 1 (no uncertainty)
  p.value = NA                # Not applicable, as these are reference classes
)

# Combine summary_tbl with references
summary_tbl_vm <- rbind(ref_data_vm, summary_tbl_vm)

# View
df_regressao_multi_bruto_vm <- as.data.frame(summary_tbl_vm)

# Set the new order of terms as desired
new_order_vm <- c("(Intercept)",
                  "ResourceIsVasopressorsyes",
                  "ResourceIsRenalReplacementTherapyyes",
                  "canceryes",
                  "IsChfNyhayes",
                  "IsSevereCopdyes",
                  "IsSteroidsUseyes",
                  "IsImmunossupressionyes",
                  "obesityyes",
                  "ChronicHealthStatusNameMajor assistance / bedridden",
                  "ChronicHealthStatusNameMinor assistance",
                  "Performance Status: Ambulant / independent (ref)",
                  "Idade_Agrupada2>=80",
                  "Idade_Agrupada265-79",
                  "Age <65 (ref)",
                  "period2022-2023",
                  "Period 2018-2019 (ref)")

# Reorder the table according to the new order
summary_tbl_vm <- summary_tbl_vm %>%
  mutate(term = factor(term, levels = new_order_vm)) %>%
  arrange(term)

# Assign labels for all categories and subcategories
labels_vm <- c("(Intercept)","Vasopressors","RRT","Cancer",
               "Chronic Heart Failure","Severe COPD","Steroids",
               "Immunosuppression","Obesity","Performance Status: Major assistance / bedridden",
               "Performance Status: Minor assistance","Performance Status: Ambulant / independent (ref)",
               "Age 80+","Age 65 - 79","Age <65 (ref)","Period 2022-2023","Period 2018-2019 (ref)")

# Update term labels to cover all categories and subcategories
summary_tbl_vm$term <- factor(summary_tbl_vm$term, levels = summary_tbl_vm$term, labels = labels_vm)

# View organized df
df_regressao_multi_ordenado_vm <- as.data.frame(summary_tbl_vm)

# Create the Forest Plot without p-value
regression_plot_vm <- ggplot(summary_tbl_vm, aes(x = estimate, y = term)) +
  geom_point(shape = 17, color = "black", size = 2.83, fill = "black") +  # Point only, without p-value
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.5, size = .65, color = "black") +  # Horizontal error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Dashed line at Odds Ratio of 1
  theme_minimal() +
  labs(x = "Odds Ratio (95% CI)", y = "")
    #title = "Forest Plot: Multivariate Logistic Regression",
    #subtitle = "Subgroup VM") +
    theme(panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10),  # Adjust y-axis text size
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and enlarge title
      plot.subtitle = element_text(hjust = 0.5, size = 12),  # Center and adjust subtitle
      axis.text.x = element_text(size = 10))  # Adjust x-axis text size
ggsave("Logistic Regression VM.jpeg", regression_plot_vm, width = 13.5, height = 8.65, dpi = 200)
regression_plot_vm

# Excel with OR and CI table
# write.xlsx(df_regressao_multi_ordenado_vm, file = "Tabela OR e CI regressão multi VM.xlsx")


########################################################################################################################


# ROC Curve and AUC (Cox and Logistic Regression)


# Define the response variable (true outcome) for both models
true_discharge <- df_cox_model_ps_PF$HospitalDischargeCode_trunc_bin  # For the Cox model
true_discharge_reg <- df_model$HospitalDischargeCode_trunc_bin  # For logistic regression

# Generate risk scores (predictions) for both models
cox_risk_scores <- predict(coxme_full_PF, type = "risk")
regression_risk_scores <- predict(regressao, type = "response")

# Calculate ROC curves for both models
roc_curve_cox <- roc(true_discharge, cox_risk_scores)
roc_curve_reg <- roc(true_discharge_reg, regression_risk_scores)

# Calculate AUCs for both models
auc_mlmetrics_cox <- AUC(y_pred = cox_risk_scores, y_true = true_discharge)
auc_mlmetrics_reg <- AUC(y_pred = regression_risk_scores, y_true = true_discharge_reg)

# Plot both ROC curves on the same graph with adjusted axis labels
plot(roc_curve_cox, col = "blue", lwd = 3, main = "ROC Curves - Cox vs Logistic Regression", legacy.axes = TRUE, xlab = "Specificity", ylab = "Sensitivity")
lines(roc_curve_reg, col = "green", lwd = 3)  # Adds the logistic regression curve

# Fill the area under the curves (optional)
polygon(c(roc_curve_cox$specificities, 0), c(roc_curve_cox$sensitivities, 0), col = rgb(0.1, 0.4, 0.8, 0.3), border = NA)
polygon(c(roc_curve_reg$specificities, 0), c(roc_curve_reg$sensitivities, 0), col = rgb(0.3, 0.7, 0.3, 0.3), border = NA)

# Add a smaller legend
legend("bottomright", 
       legend = c(paste("ROC Cox Model / AUC =", round(auc_mlmetrics_cox, 3)), 
                  paste("ROC Logistic Regression / AUC =", round(auc_mlmetrics_reg, 3))),
       col = c("blue", "green"), lwd = 3, cex = 0.7)  # Adjusting legend size to 0.7



# Calculate confidence interval for the AUC of the Cox model
ci_auc_cox <- ci.auc(roc_curve_cox)
print(ci_auc_cox)  # Display confidence interval for Cox model AUC

# Calculate confidence interval for the AUC of the logistic regression model
ci_auc_reg <- ci.auc(roc_curve_reg)
print(ci_auc_reg)  # Display confidence interval for Logistic Regression AUC


# IMV subgroup


# Define the response variable (true outcome) for both models
true_discharge_vm <- subgroup_vm_24$HospitalDischargeCode_trunc_bin  # For the Cox model
true_discharge_reg_vm <- subgroup_vm_24$HospitalDischargeCode_trunc_bin  # For logistic regression

# Generate risk scores (predictions) for both models
cox_risk_scores_vm <- predict(cox_vm, type = "risk")
regression_risk_scores_vm <- predict(regressao_vm, type = "response")

# Calculate ROC curves for both models
roc_curve_cox_vm <- roc(true_discharge_vm, cox_risk_scores_vm)
roc_curve_reg_vm <- roc(true_discharge_reg_vm, regression_risk_scores_vm)

# Calculate AUCs for both models
auc_mlmetrics_cox_vm <- AUC(y_pred = cox_risk_scores_vm, y_true = true_discharge_vm)
auc_mlmetrics_reg_vm <- AUC(y_pred = regression_risk_scores_vm, y_true = true_discharge_reg_vm)

# Plot both ROC curves on the same plot with adjusted axis labels
plot(roc_curve_cox_vm, col = "blue", lwd = 3, main = "Subgroup IMV - ROC Curves - Cox vs Logistic Regression", legacy.axes = TRUE, xlab = "Specificity", ylab = "Sensitivity")
lines(roc_curve_reg_vm, col = "green", lwd = 3)  # Adds logistic regression curve

# Fill the area under the curves (optional)
polygon(c(roc_curve_cox_vm$specificities, 0), c(roc_curve_cox_vm$sensitivities, 0), col = rgb(0.1, 0.4, 0.8, 0.3), border = NA)
polygon(c(roc_curve_reg_vm$specificities, 0), c(roc_curve_reg_vm$sensitivities, 0), col = rgb(0.3, 0.7, 0.3, 0.3), border = NA)

# Add a smaller legend
legend("bottomright", 
       legend = c(paste("ROC Cox Model / AUC =", round(auc_mlmetrics_cox_vm, 3)), 
                  paste("ROC Logistic Regression / AUC =", round(auc_mlmetrics_reg_vm, 3))),
       col = c("blue", "green"), lwd = 3, cex = 0.7)  # Adjust legend size to 0.7


# Calculate confidence interval for the AUC of the Cox model
ci_auc_cox_vm <- ci.auc(roc_curve_cox_vm)
print(ci_auc_cox_vm)  # Display confidence interval for Cox model AUC

# Calculate confidence interval for the AUC of the logistic regression model
ci_auc_reg_vm <- ci.auc(roc_curve_reg_vm)
print(ci_auc_reg_vm)  # Display confidence interval for Logistic Regression AUC


########################################################################################################################


# CALIBRATION BELT


# Cox Model

cox_predito_lp_PF <- predict(coxme_full_PF, type = "lp")

# Convert predictions to probabilities in the (0,1) range using the logistic function
cox_predito_prob_PF <- plogis(cox_predito_lp_PF)

observado_cox_PF <- df_cox_model_ps_PF$HospitalDischargeCode_trunc_bin

cb_cox_PF <- givitiCalibrationBelt(observado_cox_PF, cox_predito_prob_PF, devel = "external")

# Plot
plot(cb_cox_PF, main="Calibration Belt - Cox Model (PF)",
     xlab="Predicted Probability", ylab="Observed Event")


# Logistic Regression

predito_logit <- regressao$fitted.values  # Model predictions
observado_logit <- df_model$HospitalDischargeCode_trunc_bin  # Actual observed values

cb_logit <- givitiCalibrationBelt(observado_logit, predito_logit, devel = "external")

# Plot
plot(cb_logit, main="Calibration Belt - Logistic Regression",
     xlab="Predicted Mortality", ylab="Observed Mortality")




