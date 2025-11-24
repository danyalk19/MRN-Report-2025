


#### Progression over time - baseline #####

progression_data %>%
  
  plot_progression_over_time(method = "linear",
                             max_delay = group_X,
                             switching = subject_transitions,
                             plot_name = "Baseline v2")



# ##### Progression across subjects - baseline #####

progression_data %>%
  
  plot_progression_across_subjects(method = "linear",
                                   max_delay = group_X,
                                   switching = subject_transitions,
                                   plot_name = "Baseline v2")


#####  Regression results ######


run_regression(data = progression_data, 
               method = "linear",
               max_delay = group_X,
               switching = subject_transitions,
               confidence = 0.9) %>% 
  mutate(muslim = as.factor(muslim) %>% relevel(ref = "Non-Muslim")) %>% 
  mutate(dir = ifelse(diff>0, "Over-progression", "Under-progression")) %>% 
  mutate(level = fct_rev(as.factor(level))) %>% 
  mutate(sample= "All subjects") %>% 
  
  ggplot(aes(x = estimate*100,
             y = sample))+
  geom_line(aes(group = level, 
                colour  = dir), 
            show.legend = F, 
            linewidth = 1.2)+
  facet_wrap(~level, scales = "free_x")+
  ggh4x::facetted_pos_scales(
    x = list(
      "UG to Masters" = scale_x_continuous(limits = c(40, 50), breaks= seq(40, 50, 5)),
      "Masters to PhD" = scale_x_continuous(limits = c(10, 20), breaks= seq(10, 20, 5)))) +
  
  labs(x = "Progression rate (%)",
       y = "", 
       colour = "")+
  scale_colour_manual(values = c("Under-progression" = "#ff5a46",
                                 "Over-progression"  = "#14aa28"))+
  # Introduce a new color scale for the next layer
  new_scale_color() +
  scale_color_manual(values = mrn_pal_blue) +
  labs(x = "Progression rate (%)",
       y = "", 
       colour = "")+
  geom_point(aes(colour = muslim), size = 4) +
  guides(color = guide_legend(ncol = 1))+
  set_mrn_theme(base_size = 30)




##### over time -1 year progression rates #####

progression_data %>%
  
  plot_progression_over_time(method = "constant",
                             max_delay = year_1max,
                             switching = no_switching_mat,
                             plot_name = "1 year pgr- v2")



##### Over time 3- year progression #####

progression_data %>%
  
  plot_progression_over_time(method = "constant",
                             max_delay = year_3max,
                             switching = no_switching_mat,
                             plot_name = "3 year pgr- v2")


###### Comparison of weights ######


# Data for linearly declining weights
data_declining <- expand_grid(
  lag = -5:0,
  X = c(2, 3, 4)
) %>%
  mutate(j = -lag) %>%
  # For lags 1 to X, assign unnormalized weight (declining linearly); else, weight 0
  mutate(weight = if_else(j >= 1 & j <= X, (X + 1 - j), NA)) %>%
  group_by(X) %>%
  mutate(weight = weight / sum(weight, na.rm=T)) %>%  # normalize so weights sum to 1 within each X
  ungroup() %>%
  mutate(type = "Declining probability")

# Data for constant (equal) weights
data_constant <- expand_grid(
  lag = -5:0,
  X = c(2, 3, 4)
) %>%
  mutate(j = -lag) %>%
  # For lags 1 to X, assign weight = 1 (equal weight); else, weight = 0
  mutate(weight = if_else(j >= 1 & j <= X, 1, NA)) %>%
  group_by(X) %>%
  mutate(weight = weight / sum(weight, na.rm=T)) %>%  # normalize to sum to 1 within each X
  ungroup() %>%
  mutate(type = "Constant probability")

# Combine the two data sets
bind_rows(data_declining, data_constant) %>% 
  filter(lag<0& lag>-5) %>% 
  filter(X ==4) %>%
  
  
  ggplot( 
    aes(x = lag +0.1*(type=="Declining probability") ,
        y = weight, 
        color = type)) +
  geom_segment(aes(yend = 0, 
                   xend = lag +0.1*(type=="Declining probability"))) +
  geom_point() +
  labs(
    x = "Cohort of qualifiers",
    y = "Probability",
    color = "",
    type =""
  ) + 
  scale_color_manual(values = mrn_pal_blue) +
  set_mrn_theme(base_size = 20)






### Progression across subjects - no subject switching ####

progression_data %>%
  
  plot_progression_across_subjects(method = "linear",
                                   max_delay = group_X,
                                   switching = no_switching_mat,
                                   plot_name = "Across subjects - no switching")



### Progression across subjects - full subject switching #####

progression_data %>%
  
  plot_progression_across_subjects(method = "linear",
                                   max_delay = group_X,
                                   switching = max_flex_mat,
                                   plot_name = "Across subjects - max switching")




## ##### Subject switching matrix ######

subject_transitions %>% 
  group_by(subject_from) %>% 
  mutate(weight = count/sum(count)) %>% 
  ungroup() %>% 
  ggplot(aes(x = subject_to, 
             y= subject_from, 
             fill = weight))+
  geom_tile(show.legend = F)+
  geom_text(aes(label = paste0(round(weight*100, 1), "%")))+
  scale_fill_gradient(low = "white", high = "#009bd7") +
  labs(x = "To Subject", y = "From Subject", fill = "Weight") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  set_mrn_theme(base_size = 20)


#####  Examine predictions from backcasting #####

#Qualifiers
ofs_imputed %>%
  filter(population=="Qualifiers", 
         level %in% c("First degree", 
                      "Other undergraduate", 
                      "Undergraduate with postgraduate components")) %>%
  group_by(subject_broad, year) %>% 
  summarise(predicted_muslim = sum(predicted_muslim, na.rm=T), 
            number_muslim= sum(number_muslim, na.rm=T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("predicted_muslim", 
                        "number_muslim"), 
               names_to = "var", 
               values_to = "val") %>% 
  mutate(var = ifelse(grepl("number", var), 
                      "Actual", 
                      "Predicted")) %>%
  mutate(val = ifelse(year<2018 & var=="Actual", 
                      NA, 
                      val)) %>% 
  ggplot()+
  geom_line(aes(x = year,
                y = val, 
                colour = var))+
  geom_vline(xintercept = 2018)+
  facet_wrap(~subject_broad, scales = "free", ncol = 2)+
  labs(colour = "", x = "", 
       y = "Number of Muslim students")+
        scale_color_manual(values = mrn_pal_blue) +
       set_mrn_theme(base_size = 20)


##Entrants
ofs_imputed %>%
  filter(population=="Entrants", 
         level %in% c("Other postgraduate", 
                      "Postgraduate taught masters",
                      "Postgraduate research")
  ) %>%
  group_by(subject_broad, year) %>% 
  summarise(predicted_muslim = sum(predicted_muslim, na.rm=T), 
            number_muslim= sum(number_muslim, na.rm=T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("predicted_muslim", 
                        "number_muslim"), 
               names_to = "var", 
               values_to = "val") %>% 
  mutate(var = ifelse(grepl("number", var), 
                      "Actual", 
                      "Predicted")) %>%
  mutate(val = ifelse(year<2018 & var=="Actual", 
                      NA, 
                      val)) %>% 
  ggplot()+
  geom_line(aes(x = year,
                y = val, 
                colour = var))+
  geom_vline(xintercept = 2018)+
  facet_wrap(~subject_broad, scales = "free", ncol = 2)+
  labs(colour = "", x = "", 
       y = "Number of Muslim students")+
          scale_color_manual(values = mrn_pal_blue) +
       set_mrn_theme(base_size = 20)



#### Sensitivity of muslim gap to delay parameters #####


#Function to compute muslim gap
compute_gap <- function(max_delay){
  
  run_regression(max_delay = max_delay) %>% 
    filter(muslim=="Muslim") %>% 
    select(level, muslim, diff) %>% 
    left_join(max_delay, by  = c("level", "muslim"))
  
}

grid = expand_grid(
  level = c("UG to Masters", 
            "Masters to PhD"), 
  muslim = c("Muslim", "Non-Muslim"))


grid = expand_grid(!!!rep(list(1:5), nrow(grid))) %>%
  set_names(paste0("X", seq_len(nrow(grid)))) %>%
  mutate(id = row_number()) %>%
  pivot_longer(starts_with("X"), names_to="rownr", values_to="X") %>%
  mutate(rownr = as.integer(str_remove(rownr, "X"))) %>%
  right_join(grid %>% mutate(rownr = row_number()), by="rownr") %>%
  select(id, level, muslim, X)

parameters = grid %>% 
  pivot_wider(names_from = c(muslim, level), values_from = X) %>%
  filter(
    `Muslim_UG to Masters`   >= `Non-Muslim_UG to Masters`,
    `Muslim_Masters to PhD`  >= `Non-Muslim_Masters to PhD`,
    `Muslim_UG to Masters`   <= `Muslim_Masters to PhD`,
    `Non-Muslim_UG to Masters` <= `Non-Muslim_Masters to PhD`
  ) %>% 
  pivot_longer(cols = -c("id"),
               names_to = c("muslim", "level"),
               names_sep = "_",
               values_to = "X"
  ) %>%
  group_by(id) %>%
  nest(.key = "df") %>%
  pull(df)


gaps <- map_dfr(parameters, ~ compute_gap(.x), .id = "ID" )


# PLot dentity plots

opt_gaps <- compute_gap(group_X) %>%
  filter(muslim=="Muslim") %>% 
  select(level, med = diff)

dens_df <- gaps %>%
  filter(muslim == "Muslim") %>%
  rename(gap = diff) %>% 
  
  group_by(level) %>%
  do({
    .dat <- .
    lo <- quantile(.dat$gap, 0.05)
    hi <- quantile(.dat$gap, 0.95)
    d <- density(.dat$gap)
    tibble(
      x     = d$x,
      y     = d$y,
      lower = lo,
      upper = hi
    )
  }) %>%
  ungroup() %>%
  
  mutate(region = if_else(x >= lower & x <= upper, "Inside", "Outside"))

ggplot() +
  # full density, lightly shaded
  geom_area(
    data    = dens_df,
    aes(x = x, y = y, fill = level, group = level),
    alpha   = 0.2,
    colour  = NA
  ) +
  # 90% central ribbon, darker
  geom_area(
    data    = filter(dens_df, region == "Inside"),
    aes(x = x, y = y, fill = level, group = level),
    alpha   = 0.5,
    colour  = NA
  ) +
  # vertical zero line
  geom_vline(
    xintercept = 0,
    colour     = "#919191" ,
    size       = 0.3
  ) +
  # dashed lines to show gaps at the chosen parameters (group_X)
  geom_vline(
    data      = opt_gaps,
    aes(xintercept = med),
    linetype  = "dashed",
    size      = 1, 
    show.legend = F, 
    colour = "#919191"
  ) +
  labs(x = "Muslim Gap", y = "Density", fill = NULL) +
  scale_x_continuous(breaks = seq(-0.12, 0.12, 0.03)) +
  facet_wrap(~level, ncol = 1)+
  set_mrn_theme(base_size = 20)+
  scale_fill_manual(values = mrn_pal_blue)


#### Sensitivity of muslim gap to switching matrices #####


#Function to compute muslim gap

set.seed(10)

compute_gap_switching <- function(switching = subject_transitions, 
                                  noise =10){
  
  switching = switching %>% 
    mutate(noise = sample(0:noise, 65, replace = T), 
           count = count+noise)
  
  run_regression(switching = switching) %>% 
    filter(muslim=="Muslim") %>% 
    pull(diff)
  
}



gaps <- replicate(100, compute_gap_switching(noise = 10), simplify = T) %>% 
  t() %>% data.frame() %>% 
  rename("Masters to PhD" =X1, 
         "UG to Masters" = X2) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = -c("id"), 
               names_to = "level", 
               values_to = "gap")


# PLot dentity plots

opt_gaps <- compute_gap_switching(subject_transitions, 
                                  noise = 0) %>%
  data.frame(level = c("Masters to PhD", "UG to Masters"), 
             med = .)

dens_df <- gaps %>%
  group_by(level) %>%
  do({
    .dat <- .
    lo <- quantile(.dat$gap, 0.05)
    hi <- quantile(.dat$gap, 0.95)
    d <- density(.dat$gap)
    tibble(
      x     = d$x,
      y     = d$y,
      lower = lo,
      upper = hi
    )
  }) %>%
  ungroup() %>%
  
  mutate(region = if_else(x >= lower & x <= upper, "Inside", "Outside"))

ggplot() +
  # full density, lightly shaded
  geom_area(
    data    = dens_df,
    aes(x = x, y = y, fill = level, group = level),
    alpha   = 0.2,
    colour  = NA
  ) +
  # 90% central ribbon, darker
  geom_area(
    data    = filter(dens_df, region == "Inside"),
    aes(x = x, y = y, fill = level, group = level),
    alpha   = 0.5,
    colour  = NA
  ) +
  # vertical zero line
  geom_vline(
    xintercept = 0,
    colour     = "#919191" ,
    size       = 0.3
  ) +
  # dashed medians
  geom_vline(
    data      = opt_gaps,
    aes(xintercept = med),
    linetype  = "dashed",
    size      = 1, 
    show.legend = F,
    colour = "#919191"
  ) +
  labs(x = "Muslim Gap", y = "Density", fill = NULL) +
  scale_x_continuous(breaks = seq(-0.12, 0.12, 0.03)) +
  facet_wrap(~level, ncol = 1)+
  set_mrn_theme(base_size = 20)+
  scale_fill_manual(values = mrn_pal_blue)
