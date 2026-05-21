
> [!todo] Requests
> - [ ] Paragraph on criteria for why we're choosing to start Chilean CPUE in 1998. Coverage? How representative is the index for the years prior?
> 	- [ ] Look at historical trend in catches and make a decision then
> 	- [ ] Run the model starting in 1998
> 	- [ ] Right now we'll start the model with the full time series truncated to 1998
> 	- [x] Plot looking at retrospective, what was historically estimated
> 	- [ ] Text on what will be allowed in the future / what we will allow. E.g., no changes until the next benchmark? Need to decide how much flexibility we want to have now
> - Chilean Acoustic North survey
> 	- [ ] Need to acknowledge the work and contribution of the spatiotemporal modelling done by INPESCA
> 	- [ ] Comment on model-based indicators for CPUE but design-based indicators for surveys (Niels)
> - Peruvian Acoustic survey
> 	- [ ] Get length frequencies from the survey as soon as possible



## Chilean CPUE
- Trends look relatively similar between Vasquez and Paya indices
- Coverage in early years was poor so might not be very accurate anyway
- Jim's personal pref is to choose the one with higher uncertainty
- Agreeing to use INLA approach for the benchmark. When the stock assessment is updated later in the year, we'll change the variables from month to quarter and add chlorophyll-a
- If we pick one, we'll need text in our report on what will be allowed in the future / what we will allow. E.g., no changes until the next benchmark? Need to decide how much flexibility we want to have now #request 
	- Big changes in how we treat these indices might have an impact on how people perceive results of the MSE
- Need to put more effort in collecting data, less on the modelling approach
	- Updated version of the models aren't using the same database (plot of GLMM indices from Ignacio) ![[Pasted image 20260520093836.png|638]]
- If we go with the INLA approach, it might run much slower. Will we allow a software change, will we have a public workflow that can be evaluated
	- Modelling approach can be made public, data might be tougher
	- sdmTMB might be more flexible and open in general terms, but INLA has also been improving on useability
- Proposal: To use the INLA approach but beginning in 2000 #request for paragraph to explain exactly why
- CPUE based on set will always be better than index based on trip. In recent years we have much more data by set than by trips. Early period is not great. Recommendation from Niels to include early data but downweight them
- Data used in INLA approach and in sdTMB approach are the same in early years, but diverged after 2023.
- Karolina proposed cut-off in 1998 because that's when coverage is >5%.
	- Number of sets might not be so different, but in 1996-98 the catches were in the millions which is why the coverage in so low.
	- Number of sets not very informative, but interesting to see catch per set being low in years with low catch- If we just ran the model beginning in 2000, how much would we expect the trend to change?
	- Behaviour of the fleet and the amount of data collected changed very much after 1997. But also it's when the big ENSO event happened.
- For purposes of MSE, how would we project forward with env variables?
	- We don't really use environmental variables to keep it simple
	- Can test for env variables in the future but currently we don't use it
- We have to update this index. In the stock assessment, we'll update the data until June. That model could include chlorophyll-a, but for practical reasons we won't use environmental variables.
- For benchmark we need to have the data now, not in June
- Proposal: Apply different CVs for different periods or we start time series from those years
- We are going to get 2026 acoustic survey data, not 2026 CPUE data
- Aquiles going to give a presentation about 2026 fishing later

#### Aquiles presentation
- Fish are in good condition very close to the minimum size limit. INPESCA might advise to wait for another month or two depending.
- El Niño might develop from the south as an indicator?
- How have fuel prices impacted fishing? 
	- Probably will expect an increase in the price of jurel
	- Would more people be fishing if fuel prices were lower?
		- Strong decrease in the amount of fishing, might compensate in the next month
- Dynamics of oceanographic conditions very interesting. Warm coming south from Peru, cold coming from south, negative anomalies in central-south, positive in the north
- Is the cold in the south due to upwelling?
	- Not a normal upwelling season (spr/summer), so probably due to atmospheric conditions
- 80% of the fishing boats at the port—is that real? Sounds like it. Ouch.
	- Concern about that not being reflected in the CPUE index
	- Proposal to include an effort vector
		- Pushback where effort vector will then become the F vector
	- This situation has happened in the past, where fishing stops in the middle of the year when the fish move offshore so fishers decide to dock instead of steaming out further.

7 options
1) 2000 set-based INLA
2) **1998+ set-based INLA**l
	- This is selected as the option
	- Weight CV by the sampling coverage
	- Annual standard errors
3) Trip based
4) Trip based til 1994 + INLA past 1994
5) Nominal set based
6) Nominal trip based
7) Full INLA + weight by coverage percentage

Nominal CPUE is very different from the standardised version, so Jim is very concerned

## Peruvian CPUE
M. Geronimo
- 1 lat/lon smoother
- Issue is if distribution of species changes, the CPUE might get inflated
- Tried an annual spatial smoother, didn't converge
- Offshore uses lat/lon | year because interpreting data got tricky
- Can include catchability break in 2018 to reflect change in distribution but might cause both time series to be too short
- Peru more comfortable with the new CPUE, but concerned about the really high values for 2015-2017

## Effort Creep
##### Chile
- Novel study but 40% step-increase is unrealistic
- Change in q rather than the 40% step-change?
- q change + 1% effort increase (Niels)
- Or gradual increase in effort between 2005
- Chile prefers 2005 q change without effort creep
	- 2005 first year of new change
	- Niels thinks this will be unrealistic to not assume a continuous effort creep
- Proposal: 1% creep until 2005, then switch to BS P10
	- Rate will not increase after 2005
	- #request for table of proposed correction factor for every year
- We don't know what the correction factor would be prior to 2005. We have a study now estimating the change in 2005
##### Offshore
- 2.5% already included, no real reason to change it
	- e.g., AI maps in last two years to help fishers find fish
##### Peru
- Short time series so might be able to leave out for now?

## Biology
### Peruvian Acoustic Survey
E. Diaz
- Extent of the survey changed over the years
- Survey doesn't always cover all the fishing grounds
- These surveys aren't set up to detect jack mackerel in their transect design and their acoustic detection
- Some years with zeroes in the survey but they're still getting catch
- #request Peru make recommendation on whether we should use this information in the assessment
- Jim thinks it would be useful to have pre 1997 (El Niño) data in the assessment
- #todo Potential solutions:
	- "Super year" to take the pre-period that's the average expected biomass, and set a post-period for something else?
	- Have an availability coefficient that varies by year?
		- Uncertainty introduced, we don't know what fraction are in the zone
		- Index of availability as the fraction of overlap with fishing area?
	- Set q to be relatively low
- Chile sees similar issues in their acoustic surveys as well. For example, sometimes the jack mackerel get too close to the shore so the survey boats can't follow because it's too shallow. Sometimes in those cases, they have to switch boats to artisanal ones.
- Acoustic CS removed years because of changes in survey design
- #future work can look at integrating fishery and survey data to expand coverage, because the survey covers nearshore whereas the fishery can sometimes operate offshore
- #request get length frequencies from the survey as soon as possible
- #todo run assessment models to include these data

## Peruvian Biological Data
C. Lujan

- #request sample sizes for composition data can inform the sample size for the assessment
	- Can also get number of fish. Right now we have number of sampling events (e.g., # boxes, # trips, etc.)
- In SPRFMO the max length is 50. Do we want to shift it?
	- Peru uses 5-90 length bins
	- Should SPRFMO do 5-90 bins? Maybe

# Decision Point
### Chile Acoustic North Survey
S. Vasquez
- Biomass is occupancy and density
	- Concern about why biomass datapoint for 2025 is so high but occupancy only show small increase whereas density drops
		- Likely because in some stations there are both high occupancy and very high density
- Why would we want to incorporate spatiotemporal modeling for an acoustic survey, unclear about why we would need to add environmental variables
	- Can use the non-env one for assessment, env for research purposes
- Incorporating spatio-temporal covariates can help account for distribution shifts in the JM population
	- Also survey extent has changed over time
- Niels thinks the model aspect esp the w/ environmental variables is useful for research, not so much for assessment. He proposes we use raw acoustic biomass for the assessment  instead.
- Conversion of NASK to biomass considers the length and weight structure already
- Length frequency compositions come from certain strata, not from the whole area. Those composition data are raised by strata to the entire survey area.
- Decision: Use raw acoustic biomass for the acoustic North, keep at the spatiotemporal modelling for research.
- Need to acknowledge the work and contribution of the spatiotemporal modelling done by INPESCA #request 
- #request on model-based indicators for CPUE but design-based indicators for surveys.


## Peru Acoustic Surveys #todo
- Check that average month was correctly done for the runs
	- Season 1 is <= 6mo, season 2 is > 6mo
- Fits to the zeros?
- New Peruvian CPUE index
- Throw out old acoustic index

## Chile CPUE
- Proposal to keep old CPUE pre 1998 to show some contrasting data
- #todo run this as a sensitivity