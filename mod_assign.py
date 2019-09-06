### Script to remove columns from population assignments ###

import pandas as pd
import glob
import os

dirs = os.listdir('.')
dirs.remove('.DS_Store')
dirs.remove('mod_assign.py')
dirs.remove('.Rhistory')
dirs.remove('arl_run.ars')
dirs.remove('arlecoremac_64bit')

for dir in dirs:

	os.chdir(dir)
	
	file_name = str(glob.glob('*.csv')[0])

	pop_assign = pd.read_csv(file_name)

	pop_assign2 = pd.concat([pop_assign[['X']].reset_index(drop = True), pop_assign[['atl_gulf_coast']].reset_index(drop = True)], axis = 1, sort = False)

	pop_assign2.to_csv(file_name)
	
	os.chdir('../')
