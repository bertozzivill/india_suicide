##format csv's for 2014, which should be MUCH simpler than 2011-2013
import pandas as pd, re, os, ast
main_dir ="C:/Users/abertozz/Documents/work/repos/india_suicide/data/data_2011_2014"
in_dir = "{main_dir}/raw_csv/".format(main_dir=main_dir)
out_dir = "{main_dir}/prepped/".format(main_dir=main_dir)

tables = pd.read_csv("{main_dir}/table_names_2014.csv".format(main_dir=main_dir))
tables.table = tables.table.apply(ast.literal_eval)
tables.sub_tables = tables.sub_tables.apply(ast.literal_eval)

colnames = dict(pd.read_csv("{in_dir}../column_names_2014.csv".format(in_dir=in_dir)))
colnames = {col: value.dropna() for col, value in colnames.items()}

def replace_state(string):
				pattern = "[0-9]*\s*(.*)"
				replace = re.sub(pattern, r"\1", string)
				return replace

for row_idx in range(len(tables)):
#for row_idx in [0]:
	info = tables.iloc[row_idx]
	print info.table

	column_index = 0
	
	if info.append_table==1:
		print "appending puducherry in this table format"
		for idx in range(0, len(info.sub_tables), 2):
			subtable_index= info.sub_tables[idx]
			print subtable_index
			next_subtable_index=subtable_index+1

			subtable = pd.read_csv("{in_dir}/table_{table}_{year}_{this_idx}.csv".format(in_dir=in_dir, table=info.table, year=info.year, this_idx=subtable_index))
			if "Unnamed: 0" in subtable.columns:
				print "dropping numeric first column"
				subtable.drop("Unnamed: 0", axis=1, inplace=True)
			subtable.rename(columns={"STATES":"state"}, inplace=True)
			subtable['state'] = subtable['state'].apply(replace_state)
			subtable.set_index("state", inplace=True)
			subtable.columns=range(column_index, column_index+len(subtable.columns))

			puducherry = pd.read_csv("{in_dir}/table_{table}_{year}_{this_idx}.csv".format(in_dir=in_dir, table=info.table, year=info.year, this_idx=next_subtable_index), header=None)
			puducherry.rename(columns={0:"state"}, inplace=True)
			puducherry['state'] = puducherry['state'].apply(replace_state)
			puducherry.set_index("state", inplace=True)
			puducherry.columns=range(column_index, column_index+len(puducherry.columns))
			subtable = subtable.append(puducherry)

			#string formatting here

			if column_index==0:
				table = subtable.copy()
			else:
				table = table.merge(subtable, left_index=True, right_index=True)

			column_index = column_index + len(subtable.columns)

	else:
		print "whole tables only"
		for subtable_index in info.sub_tables:
			print subtable_index
			subtable = pd.read_csv("{in_dir}/table_{table}_{year}_{this_idx}.csv".format(in_dir=in_dir, table=info.table, year=info.year, this_idx=subtable_index))
			subtable.rename(columns={"STATES":"state"}, inplace=True)
			subtable['state'] = subtable['state'].apply(replace_state)
			subtable.set_index("state", inplace=True)
			subtable.columns=range(column_index, column_index+len(subtable.columns))

			if column_index==0:
				table = subtable.copy()
			else:
				table = table.merge(subtable, left_index=True, right_index=True)

			column_index = column_index + len(subtable.columns)

	print "table accumulated!"

	print "formatting columns"
	col_types = info.col_type.split("_")

	#remove every 4th row (totals)
	for col in range(3, len(table.columns), 4):
		table.drop(col, axis=1, inplace=True)
	table.columns = range(0, len(table.columns))

	if info.contains_total: #drop last three columns, of "total" values
		table.drop(table.columns[-3:], axis=1, inplace=True)

	#now, name columns according to the values in col_types
	these_cols = ["{one}_{two}".format(one=one, two=two) for two in colnames[col_types[1]] for one in colnames[col_types[0]]]

	if len(these_cols)!=len(table.columns):
		raise IndexError("Table and proposed columns are not of the same length!")
	else:
		table.columns=these_cols

	#remove total and "ut" rows
	unwanted = ["TOTAL (STATES)", "UNION TERRITORIES", "TOTAL (UTs)", "TOTAL (ALL INDIA)"]
	table.drop(unwanted, inplace=True)

	#save
	table.to_csv("{out_dir}/{table}_{year}.csv".format(out_dir=out_dir, table=info.table_name, year=info.year), index=False)


		
