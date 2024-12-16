did <- function(df) {
  d = df %>% 
    group_by(updated_cohort, after) %>% 
    summarise(
      cut = sum(inflow, na.rm = T)
      , trips = sum(completed_trips, na.rm = T)
    ) %>% 
    mutate(
      diff_cut = cut - lag(cut)
      , diff_trips = trips - lag(trips)
    ) %>% 
    filter(!is.na(diff_cut)) %>% 
    ungroup() %>% 
    mutate(did_cut = diff_cut - lag(diff_cut),
           did_trips = diff_trips - lag(diff_trips)) %>% 
    select(did_cut, did_trips) %>% 
    filter(!is.na(did_cut))
  return(d)
}

ci <- function(df) {
  ci_trip = rep(NA, 1000)
  ci_cut = rep(NA, 1000)
  for(i in 1:1000) {
    sample_data = sample_frac(df, size = 1, replace = T)
    ci_cut[i] <- as.numeric(did(sample_data)[1])
    ci_trip[i] <- as.numeric(did(sample_data)[2])
  }
  
  cut_ci = quantile(ci_cut, probs = c(.05, .1, .5, .95, 1))
  trips_ci = quantile(ci_trip, probs = c(.05, .1, .5, .95, 1))
  return(rbind(cut_ci, trips_ci))
}


