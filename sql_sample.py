import pymssql

conn = pymssql.connect(server='localhost', user='pyuser' , password='test1234' , database='mytest')

cursor = conn.cursor()

cursor.execute('select Itemno,Category,FoodName ,Company ,Price From supermarket(nolock);')
row = cursor.fetchone()
while row:
	print (str(row[0]) + " " + str(row[1]) + " " + str(row[2]) + " "  + str(row[3]) + " " + str(row[4]))
	row = cursor.fetchone()

conn.close()
