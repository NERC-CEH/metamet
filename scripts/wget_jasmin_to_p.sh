SHELL=/bin/bash
wget -N -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/UK-AMO_BM_dt_2024.csv
wget -N -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/UK-AMO_BM_qc_2024.csv
wget -N -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/UK-AMO_BM_dt_2025.csv
wget -N -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/UK-AMO_BM_qc_2025.csv
wget -N -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/UK-AMO_BM_dt_2026.csv
wget -N -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/UK-AMO_BM_qc_2026.csv
wget -N -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/UK-AMO_BM.rds
wget -N -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/mm1_amo.rds
# join old data to new
R CMD BATCH --no-restore --no-save /home/plevy/shunt/join_new_level1_to_old_level2.R /home/plevy/shunt/join_new_level1_to_old_level2.Rout

# attempt to do whole directory not successful
# wget -N -A "*.csv" --no-parent -r -l1 -nH -R -P /prj/NECXXXX_Auchencorth/thoth_PL/UK-AMO/lev1/ -o wget_logfile.txt https://gws-access.jasmin.ac.uk/public/eddystore/UK-AMO/lev1/UK-AMO_BM.rds

