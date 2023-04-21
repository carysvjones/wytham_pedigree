<b>WYTHAM GREAT TITS PEDIGREE</b>

<span style="font-size:24px;">This text will be larger</span>
Get wytham great tits pedigree and run basic animal models

To use this repository:
Navigate to the folder where you want to install the repository. Then type git clone https://github.com/jonesc176/wytham_pedigree.git
Or just download as .zip file

Open the wytham_pedigree.Rproj file. 

First open and run analysis/pre_prep_data.R - will output clean data files that are needed to run the next script
you only have to run this once to create the datasets you need to make the pedigree (I just included it here for transparency so you can see what I did to the data)
Uses functions from R/clean_data.R, is messy cleaning up of data, mostly removing or correcting individual breeding attempts etc, feel free to look if you want to know whats going on, apologies in advance for messiness!

Then once you've got clean data open and run analysis/1_Prep_data.Rmd - works through how I got the pedigree, and then I also adapt the breeding data to work for my analysis, if you work through that you can then run the example models in analysis/2_Run_models.Rmd

Examples of animal models in Asreml-R are in 2_Run_models.Rmd, and looking quickly at results, likelihood ratio tests, fixed effects etc 

