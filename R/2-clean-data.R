

###### Create progression dataset ######
progression_data = ofs_imputed %>%
  filter(!grepl("All", population)) %>%
  pivot_longer(cols = c("number_muslim",
                        "number_non_muslim"),
               names_to = "muslim",
               values_to = "number") %>%
  mutate(muslim = ifelse(muslim=="number_muslim",
                         "Muslim",
                         "Non-Muslim")) %>%
  pivot_wider(id_cols = c("year",
                          "level",
                          "subject",
                          "subject_broad",
                          "subject_stem",
                          "muslim"),
              names_from = "population",
              values_from = "number") %>%
  pivot_wider(id_cols = c("year",
                          "subject",
                          "subject_broad",
                          "subject_stem",
                          "muslim"),
              names_from = "level",
              values_from = c("Entrants", "Qualifiers")) %>% 
  clean_names() %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(q_masters =  qualifiers_first_degree +qualifiers_other_undergraduate + qualifiers_undergraduate_with_postgraduate_components, 
         q_phd = qualifiers_undergraduate_with_postgraduate_components + qualifiers_postgraduate_taught_masters, 
         e_masters = entrants_postgraduate_taught_masters, 
         e_phd = entrants_postgraduate_research) %>% 
  pivot_longer(cols = starts_with("q_")|starts_with("e_"), 
               names_to = c(".value", "level"), 
               names_sep = "_") %>% 
  mutate(level = recode(level, "masters" = "UG to Masters", "phd" = "Masters to PhD")) %>% 
  select(year, level, muslim, subject_broad, q, e, everything())







