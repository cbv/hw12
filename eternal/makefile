MC=mysql_config

LO=`echo "-L/usr0/src/mysql-standard-5.0.4-beta-pc-linux-gnu-i686/lib -lmysqlclient_r -lz -lpthread -lcrypt -lnsl -lm -lpthread -lc -lnss_files -lnss_dns -lresolv -lc -lnss_files -lnss_dns -lresolv" | sed -e 's/ / -link-opt /g'`

test : *.sml mysql.o co.mlb
	echo ${LO}
	mlton -const 'Exn.keepHistory true' -link-opt ${LO} co.mlb mysql.o

mysql.o : ../sml-lib/mlton/mysql/mysql.c
	gcc -o $@ -c -O `${MC} --cflags` `${MC} --include` $<