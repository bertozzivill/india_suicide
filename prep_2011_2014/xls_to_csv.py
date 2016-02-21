###convert python files to csv
import xlrd, csv, unicodedata, ast, pandas as pd

def csv_from_excel(in_file, out_file, tables):

    wb = xlrd.open_workbook(in_file)

    for table in tables:
        sh = wb.sheet_by_name('Table '+ table)
        your_csv_file = open(out_file+"_"+table+".csv", 'wb')
        wr = csv.writer(your_csv_file, quoting=csv.QUOTE_ALL)

        for rownum in xrange(sh.nrows):
            rowval = sh.row_values(rownum)
            rowval = [unicodedata.normalize("NFKD", item).encode("ascii", "ignore") if type(item)==unicode else item for item in rowval]
            wr.writerow(rowval)

        your_csv_file.close()

#actually convert everything
main_dir ="C:/Users/abertozz/Documents/work/repos/india_suicide/data/data_2011_2014/"
table_settings = pd.read_csv(main_dir+"raw/table_settings.csv", index_col="table_name")
table_settings.sub_tables = table_settings.sub_tables.apply(ast.literal_eval)

for table_name in table_settings.index:
    print table_name
    this_setting = table_settings.xs(table_name)

    table_name =table_name[1:-1]

    for year in this_setting.year:
        print year
        in_file = "{main_dir}raw/table-{table_name}_{year}.xlsx".format(main_dir=main_dir, table_name=table_name, year=year)
        out_file = "{main_dir}raw_csv/table_{table_name}_{year}".format(main_dir=main_dir, table_name=table_name, year=year)
        tables = map(str, this_setting[this_setting.year==year]["sub_tables"][0])
        print tables

        csv_from_excel(in_file, out_file, tables)
