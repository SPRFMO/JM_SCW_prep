Jack mackerel assessment 
=================================
=================================
## 2020 SC08 Model runs
### Notes
Data files with annotations found in Excel spreadsheet in 
[`SPRFMO Teams/sharepoint`](https://southpacificrfmo.sharepoint.com/:f:/s/SPRFMOSC8/EojhefvAJq9KhLVw4AqPUPYBXrW_eBaOTt9-tc3fGwtN_w?e=tek4yE) 
folder. R file to run the models and plot diagnostics can be found in `SC07.R` in 
[`jjm/assessment/R`](https://github.com/SPRFMO/jjm/tree/master/assessment/R).

### Naming convention
Model      | Description
-----------|--------------
**Models 0.x**| **Data introductions**
mod0.00     | 	Exact 2019 (single stock Ho) model and data set through 2019 (mod??? from SC07)
mod0.01     | 	as mod0.00 but updated to 2020 and catches

**Models 1.x**	|	**Configuration sensitivities**
mod1.00			|	As mod0.xx? data file but model (i.e., selectivity changes) updated to 2020

mod1.00.ll	 |	As mod1.00 but **l**ow steepness and **l**ong recruitment time series (1970-2015)
mod1.00.ls	 |	As mod1.00 but **l**ow steepness and **s**hort recruitment time series (2000-2015)
mod1.00.hl	 |	As mod1.00 (i.e., **h**igh steepness and **l**ong recruitment time series (1970-2015))
mod1.00.hs	 |	As mod1.00 but **h**igh steepness and **s**hort recruitment time series (2000-2015)

## 2019 SC07 Model runs
### Notes
Data files with annotations found in Excel spreadsheet in [`jjm/data`](https://www.github.com/SPRFMO/jjm/data/) folder. R file to run the models and plot diagnostics can be found in `SC07.R` in [`jjm/R`](https://www.github.com/SPRFMO/jjm/R/).
### Naming convention
Model      | Description
-----------|--------------
**Models 0.x**| **Data introductions**
mod0.00     | 	Exact 2018 model and data set through 2018 (mod1.4 from SC06)
mod0.01     | 	Data file as 0.0 with revised catches through 2018; 2018 model 
mod0.02		|	As 0.01 but with updated fishery age compositions and weight-at-age for N_Chile, SC_Chile, and Offshore_Trawl to 2018
mod0.03		|	As 0.02 but with updated fishery length compositions for FarNorth to 2018
mod0.04		|	As 0.03 but with updated FarNorth (Peru) CPUE index to 2018
mod0.05		|	As 0.04 but with updated Offshore CPUE index to 2018; removed 2006-2007; downweighted Chinese_CPUE (CV=10; now combined into Offshore CPUE)
mod0.06		|	As 0.05 but with updated age comps and weight-at-age for Chile_AcousN to 2018
mod0.07		|	As 0.06 but with projected 2019 catch estimates
mod0.08		|	As 0.07 but with updated fishery age composition and weight-at-age for N_Chile and SC_Chile to 2019
mod0.09		|	As 0.08 but with updated FarNorth CPUE to 2019
mod0.10		|	As 0.09 but with updated Acous_N survey index to 2019
mod0.11		|	As 0.10 but with updated age composition data from Acous_N to 2019
mod0.12		|	As 0.11 but with updated Chile CPUE (cpue_abs)
mod0.13		|	As 0.12 but with updated Chile CPUE weight-at-age
mod0.14		|	As 0.13 but with updated Peru CPUE weight-at-age
mod0.15		|	As 0.14 but with updated Offshore CPUE weight-at-age
mod0.16		|	As 0.15 but with updated FarNorth length composition data (to 2019)
mod0.17		|	As 0.16 but with averaged weight-at-age data for 2015 SC_Chile (avg(2014,2016)), and 2015 (avg(2014,2016)) and 2018/2018 (avg(2014,2016,2017)) Offshore
mod0.18		|	As 0.17 but with downweighted (/10) age composition data for 2015 SC_Chile, and 2015+2018 Offshore
**Models 1.x**	|	**Configuration sensitivities**
mod1.00			|	As mod0.17 data file but model (i.e., selectivity changes) updated to 2019
mod1.00.ll	 |	As mod1.00 but **l**ow steepness and **l**ong recruitment time series (1970-2015)
mod1.00.ls	 |	As mod1.00 but **l**ow steepness and **s**hort recruitment time series (2000-2015)
mod1.00.hl	 |	As mod1.00 (i.e., **h**igh steepness and **l**ong recruitment time series (1970-2015))
mod1.00.hs	 |	As mod1.00 but **h**igh steepness and **s**hort recruitment time series (2000-2015)


=================================
## 2018 SC06 Model runs
### Notes
Data files with annotations found in Excel spreadsheet in [`jjm/data`](https://www.github.com/SPRFMO/jjm/data/) folder. R file to run the models and plot diagnostics can be found in `SC06.R` in [`jjm/R`](https://www.github.com/SPRFMO/jjm/R/).
### Naming convention
Model      | Description
-----------|--------------
**Models 0.x**| **Data introductions**
mod0.00     | 	Exact 2018w model and data set through 2018w (mod1.13 from SCW6)
mod0.01     | 	Data file as 0.0 with revised catches through 2017; 2018w model used through mod0.11
mod0.02		|	As 0.01 but with updated Chile AcousN index
mod0.03		|	As 0.02 but with updated Chile CPUE index
mod0.04		|	As 0.03 but with updated Peru CPUE index
mod0.05		|	As 0.04 but with updated Chinese CPUE index
mod0.06		|	As 0.05 but with updated Offshore CPUE index
mod0.07		|	As 0.06 but with updated age comps for Chile_AcousN
mod0.08		|	As 0.07 but with updated Iwtatage for Chile_AcousN
mod0.09		|	As 0.08 but with updated Fwtatage for N_Chile, SC_Chile, and Offshore_Trawl
mod0.10		|	As 0.09 but with updated age comps for N_Chile, SC_Chile, and Offshore_Trawl
mod0.11		|	As 0.10 but with updated length comps from FarNorth
mod0.12		| 	Remove Russian index from model and data
**Models 1.x**	|	**Configuration sensitivities**
mod1.0			|	As mod0.11 data file but model updated to 2018
mod1.1			|	As mod1.0 with Chinese CPUE downweighted
mod1.2			|	As mod1.0 with old Peruvian data
mod1.3			|	As mod1.0 with alternate growth assumptions (as mod1.14 from May 2018 BM Workshop)
mod1.4			|	As mod1.0 with Francis weights (one iteration, based on sample sizes from May 2018 BM Workshop 2018)
**mod1.5**			|	As mod 1.4 but with low steepness and short recruitment time series (2000-2015) **FINAL MODEL**
**mod1.4.x** 		|	**Projection Configuration** to reflect regime and uncertainty in stock productivity (previously models 2.x)
mod1.4.ll	 |	As mod1.4 but **l**ow steepness and **l**ong recruitment time series (1970-2015)
mod1.4.ls	 |	As mod1.4 but **l**ow steepness and **s**hort recruitment time series (2000-2015)
mod1.4.hl	 |	As mod1.4 (i.e., **h**igh steepness and **l**ong recruitment time series (1970-2015))
mod1.4.hs	 |	As mod1.4 but **h**igh steepness and **s**hort recruitment time series (2000-2015)

## 2018 SCW6 Model runs, May 2018 workshop
### Notes
Issue re nominal CPUE for "Offshore" fleet to start in 2006, extend to 2017 for Sept SC.
Francis weights, likelihoods, to be examined at each model / sele change...

Model      | Description
-----------|--------------
1.0        | As 0.7 in 2017, selectivity change in Chile N acoustic in 2015 and 2016 
1.1        | As 1.0, but constant selectivity for Chile N Acoustic all years
1.2        | As 1.1, but constant selectivity for S-C Chile acoustic all years
1.3        | As 1.2, but constant selectivity for DEPM all years (was change in 2003)
1.4        | As 0.7 in 2017, selectivity change in Chile N acoustic in 2012 and 2016 
1.5        | As 1.4, but replace offshore CPUE w/ new version CV=0.2...dropping Russian nominal
1.5_r      | As 1.5, retros to evaluate if Francis weights are stable
1.6        | As 1.5, but without Chinese CPUE?
1.7        | As 1.5, but without Offshore (and Russian) CPUE
1.8        | As 1.5, but only Offshore CPUE included (all other indices downweighted)
1.9        | As 1.5, but only Chinese CPUE included (all other indices downweighted)
1.10       | As 1.5, but without the Acoustic N data
1.11       | As 1.5, but allow fishery selectivity to change up until age 11 and reduce penalty on domedness
1.12       | As 1.5, but constant average weight at age w/in fleets and surveys (no time varying)
**1.13**   | As 1.5 but rescale sample size using Francis T1.8 method (one iteration)
1.14       | As 1.12, but use ageing error consistent growth relationship (w/ aL^b for wt-age) and supply Maturity at length to get conversion
1.15       | As 1.5, but change catchability in 2012 for Chilean CPUE
1.16       | As 1.14, but look at alternative values for M, = 0.28?
1.1x       | As 1.13?, but estimate M with prior? Do MCMC?

## 2017 SC05 Model runs
### Naming convention
Model      | Description
-----------|--------------
**Models 0.x**| **Data introductions**
mod0.0     | Exact 2016 model and data set through 2016
mod0.1     | Data file extended to 2017...with revised catches through 2016 and provisional 2017 catch estimates; 2016 model used throughout.
mod0.2     | As 0.1 but with new Chinese CPUE index
mod0.3     | As 0.2 but with updated Chilean CPUE index
mod0.4     | As 0.3 but with updated Offshore nominal CPUE index
mod0.5     | As 0.4 but with Russian nominal CPUE add
mod0.6     | As 0.5 but with Acoustic survey updates added
mod0.7     | As 0.6 but with age composition from all updated
**Models 1.x**	| **Configuration sensitivities**
mod1.0     	| As 0.7  ctl file updated to 2017 (selectivities and stock recruit years)


# 2016 SC04 Model runs
### Naming convention
Model      | Description
-----------|--------------
**Models 0.x**| **Data introductions**
mod0.0     | Exact 2015 model and data set through 2015
mod0.1     | Extended to 2016...with revised catches through 2015 and provisional 2016 catch estimates
mod0.2     | As 0.1 but with new Chinese CPUE index
mod0.3     | As 0.2 but with new Peruvian CPUE index
mod0.4     | As 0.3 but with updated Chilean CPUE index
mod0.5     | As 0.4 but with 2012 q changed to 2000 on Chilean CPUE index
mod0.6     | As 0.5 but with alternative Chilean CPUE index
mod0.7     | As 0.5 but with new Offshore nominal CPUE index
mod0.8     | As 0.7 but with age composition from all updated
mod0.9     | As 0.8 but with selectivity in acoustic N 
mod0.10    | As 0.9 but with age-error turned off 
mod0.11    | As 0.10 but with EU only LF for 2015 
mod0.12    | As 0.10 but echo-abundance in Far North as an alternative, uses backscatter directly
mod0.13    | As 0.12 but Updated Acoustic survey data in N Chile including 2016 biomass estimate
**Models 1.x**| **Configuration sensitivities**
mod1.0     | As 0.13 
mod1.1     | As 1.0  nominal CPUE removed
mod1.2     | As 1.0  discontinued surveys dropped
mod1.3     | As 1.0  Use CV according to data workshop
mod1.4     | As 1.0  CV according to posteriors
mod1.5     | As 1.0  Selectivity in time blocks as Cristian paper 
mod1.6     | As 1.0  Selectivity in time blocks as in SC02 
mod1.7     | As 1.0  Downweight catch-age 
mod1.8     | As 1.0  Rescale sample size using Francis T1.8 method 
mod1.9     | As 1.13  Profiles over M 
mod1.10    | As 1.0  M following Lorenzen age-specific 
**mod1.11**    | As 1.0  selectivity change in Chile N acoustic in 2015 and 2016 
mod1.12    | As 1.11 and 1.5
mod1.13    | As 1.12 and 1.7
**mod1.14**    | As 1.11 and 1.3
mod1.15    | As 1.11 but selectivity change in Chile N acoustics in 2014, 2015, and 2016
mod1.16    | As 1.11 but with rescaled Lorenzen curve to have mean of 0.23
mod1.17    | As 1.11 but provisional age-error matrix included
**mod1.18**  | As 1.11 but with time-varying selectivity incremented by one year in the fisheries
mod1.19    | As 1.18 but provisional age-error matrix included         
**Models 2.x**| **Projection Configuration** to reflect regime and uncertainty in stock productivity
mod2.0     | As 1.18, steepness=**0.80**, recruitment from **1970-2013**
mod2.1     | As 1.18, steepness=**0.80**, recruitment from **2000-2013**
mod2.2     | As 1.18, steepness=**0.65**, recruitment from **1970-2013**
mod2.3     | As 1.18, steepness=**0.65**, recruitment from **2000-2013**           





## 2015 SC03 Model runs
### Naming convention

Model         | Description
------------- | -------------
mod0.0.dat (ctl)   | Exact 2014 model and data set through 2014
mod0.1.dat (ctl)   | Exact 2014 model but with data set with revised catches through 2015
mod0.2.dat (ctl)   | As 0.1 but extended to 2015 with provisional 2014 and 2015 catch estimates
mod0.3.dat (ctl)   | As 0.2 but with all indices added
---                | ---
mod1.0.dat (ctl)   | Identical to 0.3    
---                | --- 
mod2.0.dat (ctl)   | As 1.0 (new basecase), h=0.8, 1970-2012 recruitment in fitting SRR
mod2.1.dat (ctl)   | As 1.0 (new basecase), h=0.8, 2000-2012 recruitment in fitting SRR
mod2.2.dat (ctl)   | As 1.0 (new basecase), h=0.65, 1970-2012 recruitment in fitting SRR
mod2.3.dat (ctl)   | As 1.0 (new basecase), h=0.65, 2000-2012 recruitment in fitting SRR

***

## 2014 SC02 Model runs
### Naming convention

Model         | Description
------------- | -------------
mod0.0.dat (ctl)   | Only updated catch to 2014 (no other new data)
mod0.1.dat (ctl)   | As 0.0 but with all new accepted indices updated
mod0.2.dat (ctl)   | As 0.1 but with all new size and age composition data
---                | ---
mod1.0.dat (ctl)   | Identical to 0.2    
mod1.1.dat (ctl)   | As 1.0 but with new Echo index for peruvian acoustic
mod1.2.dat (ctl)   | As 1.0 but downweight fishery CPUE indices
mod1.3.dat (ctl)   | As 1.0 but down weight offshore age compositions
mod1.4.dat (ctl)   | As 1.0 but down weight catch biomass (from 0.05 to 0.15) in CV terms for last 5 years
mod1.5.ctl         | As 1.0 but set M = 0.3
mod1.9.dat (ctl)   | As 1.0 but downweight 2012-2014 offshore fishery age compositions
mod1.10.dat (ctl)  | Features of 1.9, 1.4, and 1.2
mod1.11.dat (ctl)  | As 1.0 but downweight 2014 age composition for Chilean and Offshore fisheries
---                | ---
mod2.0.dat (ctl)   | As 1.11 (new basecase), h=0.8, 1970-2012 recruitment in fitting SRR
mod2.1.dat (ctl)   | As 1.11 (new basecase), h=0.8, 2000-2012 recruitment in fitting SRR
mod2.2.dat (ctl)   | As 1.11 (new basecase), h=0.65, 1970-2012 recruitment in fitting SRR
mod2.3.dat (ctl)   | As 1.11 (new basecase), h=0.65, 2000-2012 recruitment in fitting SRR


## 2012 and earlier runs
Updated from 2012 (with changes tracked here) to allow    
  1. age-specific natural mortality (option)  
  2. length composition data for indices  
  3. other refinements  


Note that datafiles from last year need to be modified slightly to this format to run 

### Places where datafiles are changed

1. length bins specified (uses number of bins, bin values)
2. indices now accept length frequency data

