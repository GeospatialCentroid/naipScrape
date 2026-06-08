# naipScrape
A support repo to the agroforestry sampling methods. Generalize methods for downloading and processing materail using the mircosoft planetary computer 

created 2026-02-19

update 2026-06-08 
# primary scripts and purposes 


1. `src/bulk_download.R` 
- furrr based parallelization aim at grabing a high volume (thousands) of images and storing on network storage 

2. `src/naip_download_pipeline0515.R` : 
- sequential donwload of a target set of images, more aggresive checking and iterative calls to help full missing data gaps 

3. `src/produce_groundTruthSites.R` : 
- used to develop training data for the ground truthing process, more flexability in site selection, produce SNIC cluster features

