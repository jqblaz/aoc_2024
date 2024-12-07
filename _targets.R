# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse","here","janitor","conflicted","testthat") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
# tar_source("R/day01.R")
tar_source(
  files = "R"
)
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = file,
    command = here("inputs", "day01_01"),
    format = "file",
    description = "Data file name" 
  ),
  tar_target(
    name = data_01,
    command = get_data(file),
    description = "Data object" # requires development targets >= 1.5.0.9001: remotes::install_github("ropensci/targets")
  ),
  tar_target(
    name = diffs,
    command = total_distance(data_01),
    description = "Total distance"
  ),
  tar_target(
    name = similarity,
    command = similarity_score(data_01),
    description = "Total similarity score"
  ),
  # day 2
  tar_target(
    name = file02,
    command = here("inputs", "day02_01"),
    format = "file",
    description = "Day 02 Data file name"
  ),
  tar_target(
    name = data_02_list,
    command = get_list(file02),
    description = "Day 02 data lists"
  ),
  tar_target(
    name = data_02_rpts,
    command = get_reports(data_02_list),
    description = "Day 02 data set"
  ),
  tar_target(
    name = safe_rpts,
    command = safe_reports(data_02_rpts),
    description = "Day 02: total safe reports"
  ),
  tar_target(
    name = safe_rpts_dampened,
    command = safe_reports_dampened(data_02_rpts),
    description = "Day 02: total safe reports, dampened"
  ),
  # day 3
  tar_target(
    name = file03,
    command = here("inputs", "day03_01"),
    format = "file",
    description = "Day 03 Data file name"
  ),
  tar_target(
    name = data_03,
    command = get_day03_inputs(file03),
    description = "Day 03 data"
  ),
  tar_target(
    name = sum_prods,
    command = sum_of_products(data_03),
    description = "Day 03: sum of products"
  ),
  tar_target(
    name = sum_enabled_prods,
    command = sum_of_enabled_products(data_03),
    description = "Day 03: sum of enabled products"
  )
)
