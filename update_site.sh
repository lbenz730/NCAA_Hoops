date_path=$(date +%Y-%m-%d)
/usr/local/bin/Rscript ncaa_hoops_scraper.R
/usr/local/bin/Rscript Model_3.0.R
/usr/local/bin/Rscript 3.0_Files/other/sbunfurled.R
git add *
git commit -m "System Update ${date_path}" 
git push -u origin master
