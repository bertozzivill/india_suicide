####get population from IHME drive, save to local drive for India

import pandas as pd

in_dir = "/home/j/WORK/02_mortality/03_models/1_population/results/population_gbd2015.dta"
out_dir = "/homes/abertozz/india_pop.csv"


pop = pd.read_stata(in_dir)
pop = pop[pop["ihme_loc_id"].str.contains("IND")]

pop.to_csv(out_dir, index=False)