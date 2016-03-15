##format .csv files from 2012 and 2013, which are of the same format

import pandas as pd, re, os, ast
main_dir ="C:/Users/abertozz/Documents/work/repos/india_suicide/data/data_2011_2014"
in_dir = "{main_dir}/raw_csv/".format(main_dir=main_dir)
out_dir = "{main_dir}/prepped/".format(main_dir=main_dir)

#function to clean data
def clean_data(in_dir, info):
	print info.year
	table_type = info.table_name.split("_")[0]

	all_csvs = os.listdir(in_dir)
	pattern = re.compile("table_{table}_{year}".format(table=info.table, year=info.year))
	table_list = filter(pattern.match, all_csvs)

	subtable_dict = {}

	for subtable_fname in table_list:
		print subtable_fname
		subtable_index = int(re.match(".*_([1-9]*).csv", subtable_fname).groups()[0])
		in_file = "{in_dir}/{subtable_fname}".format(in_dir=in_dir, subtable_fname=subtable_fname)
		subtable = [line.rstrip("\n") for line in open(in_file)]

		try:
			find_first = [x[0:5]=='"1.0"' for x in subtable].index(True)
			list_format = [x.split(",") for x in subtable[find_first:]]
		except ValueError: #sometimes the list is even more scrambled than usual

			try:
				find_first = [x[0:5]=='"1   ' for x in subtable].index(True) #table 2.14, subtable 9
				list_format = [x.split(",") for x in subtable[find_first:]]

			except ValueError:
				find_first = [x[3:8]=='"1.0"' for x in subtable].index(True)
				list_format = [x.split(",") for x in subtable[find_first:]]

				#move rows 0-29 up one space
				for idx in range(0, 30):
					list_format[idx].pop(0)
				#remove second space in rows 30-39
				for idx in range(30, 39):
					list_format[idx].pop(1)

		#remove unwanted rows
		for idx in sorted(info.remove_rows, reverse=True):
			try:
				list_format.pop(idx)
			except IndexError:
				pass

		#adjust for unsplit name/value pairs
		if list_format[0][0]=='"1     ANDHRA PRADESH"':
			for idx in range(len(list_format)):
				pattern = '("[0-9]{1,2})\s+([A-Z]+.*")'
				split = re.match(pattern, list_format[idx][0]).groups()

				list_format[idx][0] = split[0] + '.0"'
				list_format[idx].insert(1, '"'+split[1])

		#remove any trailing unwanted rows
		info.length=int(info.length)
		if len(list_format) > info.length:
			list_format = list_format[0:info.length]

		#remove whitepace elements
		list_format = [[x for x in row if x!='""'] for row in list_format]
		
		#some tables have scrambling in some lines ,fix this 
		if table_type=="state" and list_format[34][0]!='"35.0"':
			list_format[34][0] = '"35.0"'
			list_format[34].insert(1,'"PUDUCHERRY"')

		if table_type=="state" and list_format[28][0]!='"29.0"':
			list_format[28].insert(0, '"29.0"')
			list_format[28][1] = '"A & N ISLANDS"'

		if table_type=="profession":
			for idx in [2,3,4,8,9,10,11]:
				list_format[idx].insert(0, "'99'")

		subtable = pd.DataFrame(list_format)
		#remove column 0
		subtable.drop(0, axis=1, inplace=True)

		

		#convert strings to integers, rename identifying column
		subtable = subtable.applymap(ast.literal_eval)
		subtable.rename(columns={1:table_type}, inplace=True)
		subtable.set_index(table_type, inplace=True)
		subtable = subtable.replace("NR", "NaN").applymap(float)

		
		subtable_dict[subtable_index] = subtable

	return subtable_dict



tables = pd.read_csv("{main_dir}/table_names.csv".format(main_dir=main_dir))
tables.remove_rows = tables.remove_rows.apply(ast.literal_eval)
tables.table = tables.table.apply(ast.literal_eval)
tables = tables[tables.year!=2014]

for row_idx in range(len(tables)):

	row = tables.iloc[row_idx]

	subtables = clean_data(in_dir, row)

	#read in column names
	colnames = dict(pd.read_csv("{in_dir}../column_names.csv".format(in_dir=in_dir)))
	colnames = {col: value.dropna() for col, value in colnames.items()}
	colnames['age'] = colnames['age'].apply(int)

	###format columns

	#merge all elements of subtable into one large table (requires adjusting column names)
	print "merging subtables"
	index = 0
	for key in subtables.keys():
		print key
		subtable = subtables[key]
		col_count = len(subtable.columns)
		subtable.columns = range(index, index+col_count)

		if index==0:
			table = subtable.copy()
		else:
			table = table.merge(subtable, left_index=True, right_index=True)

		index+=col_count

	print "formatting columns"
	col_types = row.col_type.split("_")

	#if col_types includes "percent", remove every fourth AND third row. 
	# if it just includes "total", only remove every third.
	if "perc" in col_types:
		for col in range(3, len(table.columns), 4):
			table.drop(col, axis=1, inplace=True)

		table.columns = range(0, len(table.columns))

	for col in range(2, len(table.columns), 3):
		table.drop(col, axis=1, inplace=True)
	table.columns = range(0, len(table.columns))

	if row.contains_total: #drop last two columns, of "total" values
		table.drop(table.columns[-2:], axis=1, inplace=True)

	#now, name columns according to the values in col_types
	these_cols = ["{one}_{two}".format(one=one, two=two) for two in colnames[col_types[1]] for one in colnames[col_types[0]]]

	#if the number of columns in table is greater than the length of these_cols +2, there's
	# a problem. otherwise, those last two cols are just totals, and we can ignore them
	if len(table.columns)!= (len(these_cols)):
		raise IndexError("Table and proposed columns are not of the same length!")
	else:
		table.columns = these_cols

	#save
	table.to_csv("{out_dir}/{table}_{year}.csv".format(out_dir=out_dir, table=row.table_name, year=row.year, index=False)

	


