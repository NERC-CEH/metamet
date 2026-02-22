# Run daily in /home/plevy/shunt/wget_jasmin_to_p.sh with
# R CMD BATCH --no-restore --no-save /home/plevy/shunt/join_new_level1_to_old_level2.R /home/plevy/shunt/join_new_level1_to_old_level2.Rout &

library(metamet)
# Reading in Level 1 data, updated daily
system.time(
  mm1 <- readRDS(
    file = "/prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/mm_amo.rds"
  )
)
# Read in the previously validated data
system.time(
  mm2 <- readRDS(
    file = "/prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev2/mm_amo_since_2025_06.rds"
  )
)

# Here we join the existing Level 2 data with new Level 1 data.
# Where records already exist in the Level 2 data, these are preserved
# and only new Level 1 data are added to the resulting data tables in mm2.

dim(mm1$dt)
dim(mm2$dt)

mm2 <- join(mm1, mm2)
dim(mm2$dt)

# Write the combined data to the Level 2 file
system.time(
  saveRDS(
    mm2,
    file = "/prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev2/mm_amo.rds"
  )
)


mm_s <- subset_by_date(
  mm2,
  start_date = "2025-06-01 00:30:00",
  end_date = "2030-01-01 00:00:00"
)

# Write the subsetted data to a file
system.time(
  saveRDS(
    mm_s,
    file = "/prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev2/mm_amo_since_2025_06.rds"
  )
)

quit(save = "no")
