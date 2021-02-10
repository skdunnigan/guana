# Guana Water Quality Project
*water quality and nutrient data collected in Guana system from July 2017-June 2020*

# Steps for JL (2021-02, from SKD):

- **Download** whole project folder as .zip
- **Extract** all files and open up 'guana.Rproj' file
- **Navigate** to `R` folder and run all `00_` scripts. Read comments for explanations
- **Open** `0.1.1_guana_load_wrangle_tidy.R` and work through code, some explanations for each section are provided below:

  - 01a LOAD read in data files - *this will bring in your nutrient data file AND the 'data_dictionary.csv' file. Also inspect your data.* 
  
  - 01b CLEAN and MERGE: important/relevant columns - *this does some tidying and joins the two dataframes together.*
  
  - 01c CHECK for spelling and duplication errors in data - *includes code like `unique()` and `janitor::get_dupes()` to help in QAQC of your data. Depending on the outcome of this code, you can go back into your Excel file, make the appropriate changes, and then start back at step 01a with the new file.*
  
  - 02a WRANGLE & TIDY - *this does a lot of tidying to redo class types of data.*
  
  - 02b CHECK your data again - *similar to step 01c, this is a way to QAQC your data again*
  
  - 03 timeseries-all sites function - *created a function to quickly plot out whatever parameter of interest. There are two variables to the function: `param` - use component_short parameter name in quotes, and `axis_title` - use axis title value from `00_vis_custom.R` with no quotes.*
  
  - 03a EXAMPLES of how to further customize - *provides further examples of the `all_sites()` function from previous section*
  
  - 04 - **THIS SECTION IS A WORK-IN-PROGRESS**
