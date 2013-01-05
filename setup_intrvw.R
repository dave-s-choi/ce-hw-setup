# setup_intrvw.R
# this is a template for creating little files from the interview portion of the CE microdata, 
# suitable for short assignments

# assumes microdata has already been downloaded and converted to .rda (r data) format
# https://github.com/ajdamico/usgsd/tree/master/Consumer%20Expenditure%20Survey
# parts of this script have been lifted from2011_fmly_analysis_example.R by anthony damico

# this would be much, much, harder without efforts of A. Damico.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
###################################################################################################################
# prior to running this replication script, all ces 2011 public use microdata files must be loaded as R data      #
# files (.rda) on the local machine. running the "2010-2011 ces - download.R" script will create these files.     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://github.com/ajdamico/usgsd/blob/master/Consumer%20Expenditure%20Survey/2010-2011%20ces%20-%20download.R  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# that script will save a number of .rda files in C:/My Directory/CES/2011/ (or the working directory was chosen) #
###################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# turn off scientific notation in most output

options( scipen = 20 )

# remove the # in order to run this install.packages line only once
# install.packages( c( "RSQLite" , "mitools" , "stringr" , "plyr" , "survey" , "RCurl" ) )

require(RSQLite)   # load RSQLite package (creates database files in R)
require(mitools)  # allows analysis of multiply-imputed survey data
require(stringr) 	# load stringr package (manipulates character strings easily)
require(plyr)		# contains the rbind.fill() function, which stacks two data frames even if they don't contain the same columns.  the rbind() function does not do this
require(survey)		# load survey package (analyzes complex design surveys)
require(RCurl)		# load RCurl package (downloads files from the web)


#######################################################
# function to download scripts directly from github.com
# http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
source_https <- function(url, ...) {
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
#######################################################
# function: load.intrvw ( yr, prefix )
# loads and rbinds the files designed by prefix for quarters 1x, 2, 3, 4
# prefix: "fmli", "mtbi", "itbi"
# yr: 10 or 11 (haven't tried with 10 yet)
load.intrvw = function( yr, prefix ) {  
  # add the ucc tables to R so that it can automatically look them up??
  load( paste0( "./intrvw/", prefix, yr , "1x.rda" ) )
  load( paste0( "./intrvw/", prefix, yr , "2.rda" ) )
  load( paste0( "./intrvw/", prefix, yr , "3.rda" ) )
  load( paste0( "./intrvw/", prefix, yr , "4.rda" ) )
  load( paste0( "./intrvw/", prefix, as.numeric( yr ) + 1 , "1.rda" ) )
  
  # save the first quarter's data frame into a new data frame called 'fmly'
  dfm <- get( paste0( prefix , yr , "1x" ) )
  
  # and create a new column called 'qtr' with all ones
  dfm$qtr <- 1
  
  # loop through the second, third, and fourth fmli data frames
  for ( i in 2:4 ){
    
    # copy each quarter into a new data frame called 'x'
    x <- get( paste0( prefix , yr , i ) )
    
    # add a quarter variable (2, 3, then 4)
    x$qtr <- i
    
    # stack 'x' below what's already in the fmly data table
    # ..this stacks quarters 2, 3, and 4 below quarter 1
    dfm <- rbind.fill( dfm , x )
  }
  
  # repeat the steps above on the fifth quarter (which uses the following year's first quarter of data)
  x <- get( paste0( prefix , as.numeric( yr ) + 1 , "1" ) )
  x$qtr <- 5
  
  # final stacking of the fifth quarter
  dfm <- rbind.fill( dfm , x )
  # now the 'fmly' data table contains everything needed for analyses
  
  # delete the temporary data frame from memory
  rm( x )
  
  # also delete the data frames loaded by the five load() function calls above
  rm( 
    list = 
      c( 
        paste0( prefix , yr , "1x" ) , 
        paste0( prefix , yr , 2:4 ) ,
        paste0( prefix , as.numeric( yr ) + 1 , "1" )
      )
  )
  # clear up RAM
  gc()
  return( dfm )  
}
#######################################################


# set this number to the year you would like to analyze..
year <- 2011

# choose a database name to be saved in the year-specific working directory.  this defaults to 
# "ces.fmly.####.db" but can be changed by replacing the paste() function with any character string ending in '.db'
# db.name <- paste( "ces.fmly" , year , "db" , sep = "." )

# r will now take the year you've selected and re-assign the current working directory
# to the year-specific folder based on what you'd set above
# so if you'd set C:/My Directory/CES/ above, it's now been changed to C:/My Directory/CES/2011/
setwd( paste( getwd() , year , sep = "/" ) )

# pull the last two digits of the year variable into a separate string
yr <- substr( year , 3 , 4 )

# read in the five quarters of family data files (fmli)
fmly = load.intrvw( yr = yr, prefix = 'fmli' )

# create a character vector containing 45 variable names (wtrep01, wtrep02, ... wtrep44 and finlwt21)
wtrep <- c( paste0( "wtrep" , str_pad( 1:44 , 2 , pad = "0" ) ) , "finlwt21" )

# immediately loop through each weight column (stored in the wtrep vector)
# and overwrite all missing values (NA) with zeroes
for ( i in wtrep ) fmly[ is.na( fmly[ , i ] ) , i ] <- 0

# create a new variable in the fmly data table called 'totalexp'
# that contains the sum of the total expenditure from the current and previous quarters
fmly$totalexp <- rowSums( fmly[ , c( "totexppq" , "totexpcq" ) ] , na.rm = TRUE )

# immediately convert missing values (NA) to zeroes
fmly[ is.na( fmly$totalexp ) , "totalexp" ] <- 0

# annualize the total expenditure by multiplying the total expenditure by four,
# creating a new variable 'annexp' in the fmly data table
fmly <- transform( fmly , annexp = totalexp * 4 )

# the "CE macros.sas" file creates estimates that match the mse = TRUE option set here.
# in order to match the sas software provided by the bureau of labor statistics, keep this set to TRUE

# if this option is set to TRUE
# R will exactly match SUDAAN results and Stata with the MSE option results
options( survey.replicates.mse = TRUE )
# otherwise if it is commented out or set to FALSE
# R will exactly match Stata without the MSE option results

# Stata svyset command notes can be found here: http://www.stata.com/help.cgi?svyset

# add a column called 'one' to the fmly data table containing 1s throughout
fmly$one <- 1

# create the survey design as a balanced repeated replication survey object, 
# with 44 replicate weights
fmly.design <- 
  svrepdesign( 
    repweights = "wtrep[0-9]+" , 
    weights = ~finlwt21 , 
    data = fmly 
  )

# extract the sampling weights from fmly.design
w = weights(fmly.design, 'sampling')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# basically everything above is taken from scripts by a. damico
# here we start dividing up the data into smaller pieces for future HW assignments

# CU identifiers
cu.id = c('newid', 'qintrvmo', 'qintrvyr')

# CU characteristics
cu.char1 = c('as_comp1', 'as_comp2', 'as_comp3', 'as_comp4', 'as_comp5', 'bls_urbn', 'cutenure','earncomp',  'fam_size', 'fam_type', 'inc_rank', 'no_earnr', 'num_auto', 'region', 'pov_cy', 'pov_py', 'childage', 'inclass')

# characteristics of reference person and spouse
cu.char2 = c('age_ref', 'marital1', 'ref_race', 'race2', 'sex_ref', 'sex2', 'high_edu')

# work experience of reference person and spouse
cu.workexp = c('inc_hrs1', 'inc_hrs2', 'incnonw1', 'incnonw2', 'incomey1', 'incomey2', 'incweek1', 'incweek2', 'occucod1', 'occucod2')

# income and other sources of money
# note: variable ends in 'x': raw survey data
#       variable ends in 'm': the mean value of 5 imputations. I think we'll skip these.
cu.income = c('compensx', 'ffrmincx', 'fincatax', 'fincbtax', 'finincx', 'fnonfrmx', 'frretirx', 'fsalaryx', 'fssix', 'inclossa', 'inclossb', 'intearnx', 'othrincx', 'pensionx', 'unemplx', 'welfarex', 'chdothx', 'aliothx', 'foodsmpx', 'colplanx', 'insrfndx', 'lumpsumx', 'nonincmx', 'ptaxrfdx', 'saleincx', 'ssoverpx', 'chdlmpx')

# taxes
cu.taxes = c('famtfedx', 'fedrfndx', 'fedtaxx', 'fsltaxx', 'misctaxx', 'othrfndx', 'sloctaxx', 'slrfundx', 'taxpropx', 'tottxpdx')

# retirement and pension deductions
cu.retpen = c('fgovretx', 'findretx', 'fjssdedx', 'fpripenx', 'frrdedx')

# financial information
cu.invest = c('bsinvstx', 'ckbkactx', 'compbnd', 'compbndx', 'compckg', 'compckgx', 'compowd', 'compowdx', 'compsav', 'compsavx', 'compsec', 'compsecx', 'monyowdx', 'purssecx', 'savacctx', 'secestx', 'sellsecx', 'setlinsx', 'usbndx', 'wdbsastx', 'wdbsgdsx')

# housing structure
cu.home = c('diracc', 'bathrmq', 'bedroomq', 'building', 'govtcost', 'hlfbathq', 'publhous', 'renteqvx', 'roomsq', 'st_hous', 'heatfuel', 'swimpool', 'waterht', 'aptment', 'ofstpark', 'windowac', 'cntralac', 'cooking', 'porch', 'unistrq', 'built')

# expenditure summaries 
# note: indicies may need to change for 2010!
cu.expn = names(fmly)[261:422]

# trip expenditures (not including vacation homes)
cu.trips = names(fmly)[465:504]

# total outlays
cu.outlays = names(fmly)[545:584]

# to do: sample from the weights, and link to the MTBI and ITBI files
w.cumsum = c(0,cumsum(w))/sum(w)
n = 1000 # will have duplicates if n > 6000 or so
u = seq(from = 0, to = 1-1/n, by = 1/n)
ind = cut(u, breaks=w.cumsum, labels=F, include.lowest=T)

# make some data frames

fmly.id = fmly[ ind, c( cu.id, cu.char1, cu.char2, cu.home ) ]
fmly.work = fmly[ ind, c( cu.id, cu.workexp, cu.income, cu.taxes, cu.retpen ) ]
col1 = seq( from = 1, by = 2, to = length( cu.expn ) )
col2 = seq( from = 2, by = 2, to = length( cu.expn ) )
fmly.expn = fmly[ ind, cu.expn[col1] ] + fmly[ ind, cu.expn[col2] ] 
str = names( fmly.expn )
names(fmly.expn) = substr( str,  start = 1, stop = nchar(str)-2)
fmly.expn = cbind( fmly[ ind, cu.id ], fmly.expn )
###########################################################
# load the 5 mtbi files
mtbi = load.intrvw( yr = yr, prefix = 'mtbi' )
mtbi.samp = subset( mtbi, subset = ( newid %in% fmly.id$newid ) )





