<b>WYTHAM GREAT TITS PEDIGREE</b>
==============================

Get a pedigree of the wytham great tits! (and run some basic animal models)


<b>To use this repository:</b>
- In the terminal navigate to the folder where you want to install the repository. Then type `git clone https://github.com/carysvjones/wytham_pedigree.git`
- Or just download as .zip file, using the drop down in `<> Code`

<br>

1. Open the `wytham_pedigree.Rproj` file. 
2. First run `renv::restore()` to make sure all the neccessary packages are installed, then open and run `analysis/0_pre_prep_data.R` - will output clean data files that are needed to run the next script,
you only have to run this once to create the datasets you need to make the pedigree (I just included it here for transparency so you can see what I did to the data).
Uses functions from `R/clean_data.R`, for cleaning up the data, mostly removing or correcting individual breeding attempts etc, feel free to look if you want to know whats going on, apologies in advance for messiness!

3. Then once you've got clean data open and run `analysis/1_Prep_data.Rmd` - it works through how I got the pedigree, and then I also adapt the breeding data to work for my analysis, if you work through that you can then run the example models in `analysis/2_Run_models.Rmd` using that data and pedigree.

4. Examples of animal models in Asreml-R are in `analysis/2_Run_models.Rmd`, and short code looking quickly at results, likelihood ratio tests, fixed effects etc 

