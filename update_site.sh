#!/bin/bash
date_path=$(date +%y-%m-%d)
#Rscript ncaa_hoops_scraper.R
#Rscript Model_3.0.R
git add *
git commit -m "System Update ${date_path}" 
git push