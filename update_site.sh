date_path=$(date +%Y-%m-%d)
/usr/local/bin/Rscript ncaa_hoops_scraper.R
/usr/local/bin/Rscript Model_3.0.R
git add *
git commit -m "System Update ${date_path}" 
git push
