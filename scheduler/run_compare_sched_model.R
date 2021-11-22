setwd("/Users/delvin/Documents/OneDrive - SickKids/scheduler/Scheduler_Input_Data/")

library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(tictoc)

source("../scheduler/run_scheduler_funs.R")
source("../scheduler/service_funs.R")
source("../scheduler/plotting_funs.R")
source("../scheduler/post_hoc_funs.R")
source("../scheduler/constraints.R")
source("../scheduler/test_funs.R")

# --- static params ----

{
  options <- c("all")
  # phase_dates_xlsx <- "OPTIMISE_09-27-2021/phase_dates_10-2021.xlsx"
  phase_dates_xlsx <- "PhaseDates_v2_20201210.xlsx"
  services <- list(
    "General", "Otolaryngology",
    "Ophthalmology",
    # "Gynaecology",
    "Cardiovascular",
    "Orthopaedics", "Plastics", "Urology", "Dentistry"
  )
  
  unit_targets <- c(
    "5A" = 8, "5B" = 3, "5C" = 3, "8C" = 5,
    "5A Step Down" = 3, "5B Step Down" = 2,
    "5C Step Down" = 3, "8C Step Down" = 2,
    "ICU" = 3
  )
  pacu_target <- 18
  
  wtis_in <- "OPTIMISE_09-27-2021/waitlist_09272021_subset.xlsx"
  turnover_buffer <- 25
}


# ------- run the scheduler -------
set.seed(2021)

options <- list(rep("OPTIMISE_09-27-2021/PhaseX_Oct2021_baseline.xlsx", 6),
                rep("OPTIMISE_09-27-2021/PhaseX_Oct2021.xlsx", 6),
                rep("OPTIMISE_09-27-2021/PhaseX_Jan2022.xlsx", 6) )


sched_compare <- 1:length(options) %>%
  map(function(idx) {
    print(options[[idx]])
    
    if (str_detect(options[[idx]][[1]], 'Oct')){
      start_date = '2021-10-04'
    } else{
      start_date = '2022-01-03'
    }
    start_date = as.Date(start_date)

    tic()
    res <- run_scheduler(
      wtis_in = wtis_in,
      sub_n = NULL,
      case_ids = NULL,
      phase_dates_xlsx = phase_dates_xlsx, 
      phase_list_xlsx = options[[idx]],
      services = services,
      start_date = start_date, 
      verbose_run = TRUE,
      turnover_buffer = turnover_buffer,
      time = "surgeon",
      rotating_services = c("Opthalmology", "Orthopaedics", "Urology", "Otolaryngology"),
      add_cases = FALSE,
      high_pri_prop = 0.5,
      home_only = FALSE,
      max_time = (10.5*60)-turnover_buffer, # max block time in hours accounting for turn over buffer
      surg_opts = "first",
      post_op_opts = "all", # whether to schedule service by service, or consider all at once'service' or 'all'
      unit_opts = unit_targets, # ensure this named vector contains all post op destinations found in the waitlist
      pacu_opts = pacu_target,
      surg_sched = NULL,
    )
    toc()
    return(res)
  })

sched_compare %>% map(~ plot_burndown_per_service(in_sched_raw = .x,phase_dates_xlsx = phase_dates_xlsx, show_lines = FALSE ))

spreadsheets <- sched_compare %>%
  map(~ get_spreadsheet_per_service_sched(
    in_sched_raw = .x,
    per_block = TRUE,
    per_case = TRUE,
    per_day = TRUE,
    write_files = FALSE,
    orig_data_only = TRUE
  ))

# save down output for Lauren
spreadsheets[[1]]$day_df %>% as_tibble() %>% readr::write_csv('../oct_2021_baseline_day_df_no_sched.csv')
spreadsheets[[2]]$day_df %>% as_tibble() %>% readr::write_csv('../oct_2021_added_blocks_day_df_no_sched.csv')
spreadsheets[[3]]$day_df %>% as_tibble()  %>% readr::write_csv('../jan_2022_added_blocks_day_df_no_sched.csv')


spreadsheets[[1]]$day_df %>% as_tibble() %>% plot_post_op_dest() + ggtitle('Oct 2021 Baseline')  + theme(plot.title = element_text(hjust = 0.5))
spreadsheets[[2]]$day_df %>% as_tibble()  %>% plot_post_op_dest() + ggtitle('Oct 2021 Ramp-Up ')  + theme(plot.title = element_text(hjust = 0.5))
spreadsheets[[3]]$day_df %>% as_tibble()  %>% plot_post_op_dest() + ggtitle('Jan 2022 Ramp-Up') + theme(plot.title = element_text(hjust = 0.5))

spreadsheets %>% map(function(this_spreadsheet){
  
  plot_post_op_dest(this_spreadsheet$day_df)
})
plot_post_op_dest 

