#! /bin/bash
# Joel Ås
# 2017-01-23

# Run webcrawler to make sure the DB exits
./python/system_crawler.py

# Run R
cd Rcode
R --save -f main.R

echo "Use \"present_wine_tbl( )\" and search function to find wines"

R
