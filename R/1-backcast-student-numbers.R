

ofs_raw = ofs_raw %>%
  clean_names()


#Filter OfS data

ofs_raw = ofs_raw %>%
  filter(study_characteristic=="Subject of study (detailed)",
         domicile== "UK",
         level!= "All",
         student_sub_characteristic!="Total",
         student_characteristic %in% c("Sex*",
                                       "Ethnicity (15 groups)*",
                                       "Index of Multiple Deprivation (IMD)",
                                       "Religion or belief*"))




#Clean some columns

ofs = ofs_raw 

ofs = ofs %>%
  mutate(year = substr(year, 1, 4),
         year = as.numeric(year) +1)

ofs =ofs %>%
  mutate(number = ifelse(number_of_students %in% c("[DPH]",
                                                   "[DP]",
                                                   "[DPL]"),
                         NA,
                         number_of_students),
         number = as.numeric(number))



#Create subject variable
ofs  = ofs %>%
  mutate(subject = study_sub_characteristic)

#Drop levels which are sums of other rows
ofs = ofs %>% 
  filter(!grepl("All", level))


# Create predictor variables
ofs = ofs %>%
  mutate(var = paste0(student_characteristic, " - ",
                      student_sub_characteristic))

#Select level and subject and year and pop as ID variables

ofs = ofs %>%
  select(level, subject, year, population,
         var, number)

#Pivot wider

ofs = ofs %>%
  pivot_wider(id_cols = c("level",
                          "subject",
                          "year",
                          "population"),
              names_from = "var",
              values_from = "number")

#Clean colnames

ofs = ofs %>%
  clean_names()


#Drop non-predictor columns
ofs = ofs %>%
  select(-c(contains("religion")&!contains("muslim")))

#Impute missing data as censored - set to zero
#Except for religion data which is genuinely missing pre 2018

ofs = ofs %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(religion_or_belief_muslim = ifelse(year<2018,
                                            NA,
                                            religion_or_belief_muslim))

#Create total number

ofs = ofs %>%
  mutate(number_total = sex_male +
           sex_female +
           sex_other_sex +
           sex_no_response)


#Divide all by the total_number

ofs <- ofs %>%
  mutate(across(where(is.numeric) & !all_of(c("year", "number_total")),
                ~ . / number_total))

#Create weighted IMD variable
ofs = ofs %>%
  mutate(weighted_imd =
           1 * index_of_multiple_deprivation_imd_quintile_1_most_deprived +
           0.8 * index_of_multiple_deprivation_imd_quintile_2 +
           0.6 * index_of_multiple_deprivation_imd_quintile_3 +
           0.4 * index_of_multiple_deprivation_imd_quintile_4 +
           0.2 * index_of_multiple_deprivation_imd_quintile_5_least_deprived)

# #Create log odds muslim proportion
#Set 0 to missing to prevent NaN - impute later
ofs = ofs %>%
  mutate(log_odds_muslim = ifelse(religion_or_belief_muslim==0,
                                  NA,
                                  religion_or_belief_muslim),
         log_odds_muslim = log(log_odds_muslim/ (1- log_odds_muslim)))

# Create useful ethnicity groups
ofs = ofs %>%
  mutate(pakistani = ethnicity_15_groups_asian_or_asian_british_pakistani,
         indian = ethnicity_15_groups_asian_or_asian_british_indian,
         bangladeshi = ethnicity_15_groups_asian_or_asian_british_bangladeshi,
         non_white_other = ethnicity_15_groups_mixed_white_and_black_african+
           ethnicity_15_groups_asian_or_asian_british_other +
           ethnicity_15_groups_black_or_black_british_african +
           ethnicity_15_groups_other_ethnic_group+
           ethnicity_15_groups_mixed_white_and_asian+
           ethnicity_15_groups_mixed_white_and_black_african)

#Select only relevent vars
ofs = ofs %>%
  select(level, year, population, subject, #ID variables
         religion_or_belief_muslim, #share of muslims in each group
         log_odds_muslim, #log odds of muslim proportion
         number_total, #Number in each group
         sex_male, #All other predictors are shares
         weighted_imd,
         pakistani,
         indian,
         bangladeshi,
         non_white_other)

#Drop groups where there is not enough data (mostly PGCE, UG with PG components)
ofs = ofs %>%
  filter(number_total> 10,
         level!= "PGCE")

#Preprocess data with relevant predictor variables
ofs = ofs %>%
  mutate(subject_level_pop = interaction(subject, level, population)) %>%
  mutate(across(subject_level_pop, as.factor)) %>%
  bind_cols(., as.data.frame(model.matrix(~ subject_level_pop - 1, data = .))) %>%
  select(-subject_level_pop) %>% 
  mutate(across(starts_with("subject_level_pop"),
                ~ . * (year - 2010), 
                .names = "{.col}_trend")) 



# Filter data for training and create interactions
ofs_new <- ofs %>%
  filter(year >= 2018)

#Impute missing log odds muslim using MICE

#Characters to be factors to be used in the prediction imputation algo
imputed_data<- ofs_new %>%
  select(-contains("subject_level")) %>% 
  mutate(across(where(is.character), as.factor))

#store variables to be imputed
impute_vars = "log_odds_muslim"
exclude_vars = c("subject", 
                 "level", 
                 "population")

#Store which variables are used in prediction
pred_matrix = quickpred(imputed_data,
                        exclude = c(impute_vars, 
                                    exclude_vars))

#Set which variables are predicted and how (PMM)
meth_vector = make.method(imputed_data)
meth_vector[] = ""
meth_vector[impute_vars] = "pmm"

#Run multiple imputation
mids = mice(imputed_data,
            method = meth_vector,
            maxit = 10,
            m = 5,
            pred = pred_matrix,
            seed = 500)

#Merge imputed data into one and merge back with raw
imputed_data = merge_imputations(imputed_data, mids, imputed_data)

ofs_new = imputed_data %>% 
  select(log_odds_muslim_imp, 
         level, subject, year, population) %>% 
  right_join(ofs_new) %>% 
  select(-log_odds_muslim) %>% 
  rename(log_odds_muslim = log_odds_muslim_imp)

# Create cross-validation folds
folds <- vfold_cv(ofs_new, v = 10)

# Define penalized regression model
lr_mod <- linear_reg(penalty = tune(),
                     mixture = tune()) %>%
  set_engine("glmnet") 


# Create preprocessing recipe with interaction terms
lr_recipe <- recipe(log_odds_muslim ~ . , data = ofs_new) %>%
  step_rm(religion_or_belief_muslim) %>% 
  step_rm(number_total) %>% 
  update_role(level, new_role = "ID") %>% 
  update_role(subject, new_role = "ID") %>% 
  update_role(population, new_role = "ID") %>% 
  #step_normalize(all_numeric_predictors()) %>%  # Normalize numeric predictors
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%   # Convert categorical to dummies
  step_zv(all_predictors())   # Remove zero-variance columns


# Define workflow
lr_workflow <- workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(lr_recipe)

# Define grid of tuning parameters
lr_reg_grid <- grid_regular(
  penalty(),
  mixture(),
  levels = 30
)

# Train and tune the models
lr_res <- lr_workflow %>%
  tune_grid(
    resamples = folds,
    grid = lr_reg_grid,
    control = control_grid(save_pred = TRUE)
  )

# Select the best model based on performance
lr_best <- lr_res %>%
  select_best()

# Finalize workflow with the best parameters
lr_fit <- lr_workflow %>%
  finalize_workflow(lr_best) %>%
  fit(data = ofs_new)

# Evaluate model performance
metrics <- lr_fit %>%
  fit_resamples(folds) %>%
  collect_metrics()

# Print metrics
print(metrics)

#Back-cast pre-2018 data
ofs_imputed= augment(lr_fit, ofs) %>%
  select(level, subject, population, year, .pred, religion_or_belief_muslim, number_total) %>% 
  mutate(predicted_muslim = number_total*(exp(.pred)/(1 + exp(.pred))), 
         predicted_muslim = round(predicted_muslim, 0),
         number_muslim= ifelse(year>=2018, 
                               religion_or_belief_muslim*number_total, 
                               predicted_muslim),
         number_non_muslim = number_total- number_muslim) %>% 
  select(-.pred, -religion_or_belief_muslim)


# merge subject mapping
ofs_imputed = ofs_imputed %>% 
  left_join(hesa_subject_mapping)

