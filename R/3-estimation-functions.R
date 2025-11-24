
###### Function to estimate potential entrants in a given subject/year ####

compute_weighted_q_by_group <- function(i, X, q, method) {
  # Retrieve the X for the current group (should be unique)
  current_X <- unique(X)
  
  # Require exactly current_X past observations; if not available, return NA
  if ((i - 1) < current_X) {
    return(NA)
  } else {
    # Use exactly the last current_X previous values (i.e. from year i - current_X to i - 1)
    vals <- q[(i - current_X):(i - 1)]
    
    if(method=="linear"){
      
      # Compute linear weights that decrease linearly:
      # For j = 1,...,current_X, weight = ((current_X + 1) - j) / current_X
      weights <- (((current_X + 1) - seq_len(current_X)) / current_X)
      
    }
    
    if(method=="constant"){
      
      weights<- rep(1, length(vals))
      
    }
    
    
    weights <- weights / sum(weights)
    return(sum(vals * weights))
  }
}

###### Function to estimate potential entrants for all years/subjects #####

compute_weighted_q<- function(data = progression_data, 
                              method = "linear",
                              max_delay,
                              switching){
  
  
  #Calculate average over previous years qualifiers
  data = data %>% 
    group_by(muslim, level, subject_broad, year) %>% 
    summarise(q = sum(q, na.rm = TRUE), 
              e = sum(e, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(max_delay, by = c("muslim", "level")) %>% 
    group_by(muslim, level, subject_broad) %>% 
    arrange(year) %>% 
    mutate(weighted_q = map_dbl(row_number(), 
                                ~ compute_weighted_q_by_group(.x,
                                                              X, 
                                                              q = q, 
                                                              method = method))) %>% 
    ungroup() %>% 
    filter(year>max(year[is.na(weighted_q)]))
  
  
  #Calculate average over other subjects qualifiers
  data =data %>% 
    rename(subject_from = subject_broad) %>% 
    left_join(switching, by = "subject_from" , relationship = "many-to-many") %>%
    # Here, rows where the transition is not observed will have NA in count; replace with 0.
    mutate(count = coalesce(count, 0)) %>%
    # We now want to compute the weighted average of weighted_q for each destination subject (subject_to).
    group_by(year, muslim, level, subject_to) %>% 
    mutate(weight = count/sum(count, na.rm = TRUE)) %>%
    summarise(weighted_subject_q = sum(weighted_q * weight, na.rm = TRUE)) %>% 
    left_join(data, by = c("muslim", "level", "subject_to" = "subject_broad", "year")) %>% 
    ungroup()
  
  data
  
}

### MRN Style theme

library(ggplot2)

mrn_pal_blue <- c("#053228", "#009bd7", "#6edcff", "#dbfaff")

set_mrn_theme <- function(base_size = 20) {
  
  # --- 1. Define Key Colors (Blue Highlight + Grey structural) ---
  ox_blue  <- "#009bd7" 
  ox_grey  <- "#919191" 
  ox_black <- "#053228"
  caption_size <- base_size
  axis_line_size <- 0.25
  base_family <- "Silka"
  chart_element_colour <- ox_grey
  caption_colour <- ox_black


  # --- 2. Create the Theme ---
  # Start with theme_classic and replace specific elements
    ggplot2::theme_classic(
    base_size = base_size,
    base_family = base_family) %+replace%
    theme(
      axis.title = element_text(colour = chart_element_colour, size = base_size),
      axis.text = element_text(colour = chart_element_colour, size = base_size),
      axis.ticks.x = element_line(colour = chart_element_colour),
      axis.ticks.y = element_line(colour = chart_element_colour),
      axis.line = element_line(colour = chart_element_colour, size = axis_line_size),
      legend.background = element_blank(),
      legend.margin =  margin(0),
      #legend.spacing = Default,
      legend.spacing.x = unit(base_size/4, "pt"),
      legend.spacing.y = unit(base_size/4, "pt"),
      #legend.key = Default,
      #legend.key.size = Default,
      legend.text = element_text(colour = ox_black, margin = margin(r = base_size)),
      #legend.text.align = Default,
      legend.title = element_text(colour = chart_element_colour, size = base_size),
      legend.title.align = 0,
      legend.title.position = "top",
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.byrow = TRUE,
      legend.box = "vertical",
      legend.box.just = "left",
      legend.box.margin = margin(l = 0),
      legend.box.background = element_blank(),
      #legend.box.spacing = Default,
      #panel.background = Default,
      #panel.border = Default,
      #panel.spacing = Default,
      panel.grid.major = element_line(colour = chart_element_colour, size = axis_line_size/2),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = chart_element_colour, size = axis_line_size/2),
      plot.caption = element_text(hjust = 0, colour = caption_colour),
      plot.caption.position = "plot",
      plot.title = element_text(
        size = caption_size,
        hjust = 0,
        margin = margin(t = base_size, r = base_size, l = base_size, b = 1.5*base_size),
        colour = caption_colour
      ),
      plot.title.position = "plot",
      plot.subtitle = element_blank(),
      plot.margin = margin(t = base_size, r = base_size, b = base_size, l = base_size),
      #strip.background = Default,
      #strip.placement = Default,
      strip.text = element_text(colour = caption_colour,
                                margin = margin(t = base_size/4,
                                                r = base_size/4,
                                                l = base_size/4,
                                                b = base_size/3)),
      #strip.switch.pad.grid = Default,
      complete = TRUE
    )

}



#### Function to plot average progression across subjects ####

plot_progression_across_subjects<- function(data = progression_data,
                                            method = "linear",
                                            max_delay,
                                            switching,
                                            plot_name) {
  
  
  compute_weighted_q(data, 
                     method,
                     max_delay, 
                     switching) %>% 
    
    #Aggregate across subjects
    group_by(muslim, level, subject_to) %>% 
    summarise(e = sum(e, na.rm=T), 
              q = sum(weighted_subject_q, na.rm=T)) %>% 
    ungroup() %>% 
    
    
    #Calculate weighted average progression rates
    mutate(prog =e/q) %>% 
    group_by(subject_to, level) %>% 
    mutate(gap = prog[muslim=="Muslim"] - prog[muslim!="Muslim"]) %>% 
    ungroup() %>% 
    mutate(dir = ifelse(gap>0, "Over-progression", "Under-progression")) %>% 
    mutate(muslim = as.factor(muslim) %>% relevel(ref = "Non-Muslim")) %>% 
    mutate(level = fct_rev(as.factor(level))) %>% 
    
    
    #Plot 
    ggplot(aes(x = prog*100,
               y = subject_to))+
    geom_line(aes(group = subject_to, 
                  colour  = dir), 
              show.legend = F, 
              linewidth = 1.2)+
    facet_wrap(~level)+
    
    labs(x = "Progression (%)",
         y = "", 
         colour = "")+
    scale_colour_manual(values = c("Under-progression" = "#ff5a46",
                                   "Over-progression"  = "#14aa28"))+
    # Introduce a new color scale for the next layer
    new_scale_color() +
    scale_color_manual(values = mrn_pal_blue) +
    geom_point(aes(colour = muslim), size = 4) +
    labs(x = "Progression (%)",
         y = "",
         colour = "") +
    guides(color = guide_legend(ncol = 1))+
    set_mrn_theme()
  
  
}


#### Function to plot average progression rates over time ####


plot_progression_over_time<- function(data = progression_data,
                                      method = "linear",
                                      max_delay,
                                      switching,
                                      plot_name) {
  
  
  compute_weighted_q(data, 
                     method,
                     max_delay, 
                     switching) %>% 
    
    
    #Aggregate across years
    group_by(muslim, level, year) %>% 
    summarise(e = sum(e, na.rm=T), 
              q = sum(weighted_subject_q, na.rm=T)) %>% 
    ungroup() %>% 
    
    #Calculate weighted average progression rates
    mutate(prog =e/q) %>% 
    mutate(muslim = as.factor(muslim) %>% relevel(ref = "Non-Muslim")) %>% 
    mutate(level = fct_rev(as.factor(level))) %>% 
    
    #Plot
    ggplot(aes(y = prog*100,
               x = year))+
    geom_line(aes(group = muslim, 
                  colour  = muslim), 
                  linewidth = 1.2)+
    
    facet_wrap(~level)+
    scale_color_manual(values = mrn_pal_blue) +
    labs(y = "Progression rate (%)",
         x = "Year", 
         colour = "")+
    scale_x_continuous(breaks = seq(2012, 2023, by = 2))+
    guides(color = guide_legend(ncol = 1))+
    set_mrn_theme()
  
  
}



##### Function to estimate regression ####

run_regression <- function(data = progression_data,
                           method = "linear",
                           max_delay = group_X,
                           switching =subject_transitions , 
                           confidence = 0.9){
  
  compute_weighted_q(data, 
                     method = method,
                     max_delay = max_delay, 
                     switching = switching) %>% 
    group_by(muslim, level) %>%
    nest() %>%
    mutate(model = map(data, ~ lm(e ~ weighted_subject_q +as.factor(year), data = .)),
           coef = map(model, ~broom::tidy(.x, conf.int = T, conf.level = 0.9))) %>%
    unnest(coef) %>%
    select(muslim, level, term, estimate, std.error, p.value, conf.low, conf.high) %>% 
    filter(term=="weighted_subject_q") %>% 
    group_by(level) %>% 
    mutate(mid = mean(estimate), 
           diff = estimate[muslim=="Muslim"] - estimate[muslim=="Non-Muslim"]) %>% 
    ungroup()
  
}

