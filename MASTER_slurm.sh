#!/bin/sh
# Need to first make the script executable with
# chmod +x MASTER.sh 
# and then
# ./MASTER.sh
# if run as bash MASTER.sh then will have a problem with the module command (since both are run interactively)

cd /vol/milkunB/vbarbarossa/connectfish/


# Run main calculations
job01=$(sbatch --parsable batch/calculate_CI.sh)
job02=$(sbatch --parsable batch/calculate_CI_min10k.sh)

# Run scripts for Figures
job11=$(sbatch --parsable batch/Figure_1_initial_richness_HYBAS_ID.sh)
job12=$(sbatch --parsable batch/Figure_1_initial_richness_MAIN_BAS.sh)
job13=$(sbatch --dependency=afterok:$job01:$job02 --parsable batch/Figure_2_CI_map_MAIN_BAS.sh)

# Run scripts for Figure_2
mkdir /vol/milkunB/vbarbarossa/connectfish/batch_out/Figure_2/
job141=$(sbatch --dependency=afterok:$job01:$job02 --parsable batch/Figure_2/prepare_data.sh)
job142=$(sbatch --dependency=afterok:$job141 --parsable batch/Figure_2/tabulate_per_HYBAS_ID.sh)
job143=$(sbatch --dependency=afterok:$job142 --parsable batch/Figure_2/plot_data.sh)

