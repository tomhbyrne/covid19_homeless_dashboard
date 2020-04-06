# Overview
 
This dashboard is intended to provide information to inform planning and response efforts to address the COVID-19 pandemic among persons experiencing homelessness in the United States.  The dashboard is intended to provide the following information at both the national and Continuum of Care (CoC) level:

1. Estimated size of the single adult homeless population, to provide a baseline understanding of scope of potential COVID-19 related impact and needs
2. Potential impact of COVID-19 on the single homeless population, including number of infections, number of hospitalizations, number of Intensive Care Unit (ICU) admissions and number of fatalities
3. Capacity needed to provide emergency accommodation to the single adult homeless population

The dashboard is based on [this report](https://works.bepress.com/dennis_culhane/237/) of the impact of COVID-19 on the homeless population and was created by the report's authors: [Dennis P. Culhane](https://www.sp2.upenn.edu/people/view/dennis-culhane/), [Dan Treglia](https://www.sp2.upenn.edu/people/view/dan-treglia/), & [Ken Steif](https://www.design.upenn.edu/city-regional-planning/phd/people/kenneth-steif) from the University of Pennsylvania, [Tom Byrne](https://www.bu.edu/ssw/profile/thomas-byrne/) from the Boston University School of Social Work and [Randall Kuhn](https://ph.ucla.edu/faculty/kuhn) from UCLA.

The dashboard will be updated regularly as new data become available and to add new information. Update announcements will be made [here on Twitter](https://twitter.com/TomHByrne).

Code for dashboard is available [here](https://github.com/tomhbyrne/covid19_homeless_dashboard)

Please direct any comments, suggestions, questions or information about errors to Tom Byrne at [tbyrne@bu.edu](tbyrne@bu.edu)

# How to use the dashboard
The tabs at the top of the page provide the following information: 

1. Estimated size of the single adult homeless population, used as the baseline to inform estimates of the impact of COVID-19 and capacity needed to mitigate this impact

2. Estimated number of COVID-19 infections, hospitalizations ICU admissions and fatalities among persons experiencing homelessness

3. Estimated capacity needed to provide emergency accommodation to the entire single adult homeless population.  This page provides information about the overall number of emergency accommodation units needed and, in drawing on [a screening protocol developed by the California Department of Public Health](https://www.cdph.ca.gov/Programs/CID/DCDC/CDPH%20Document%20Library/COVID-19/flowchart-COVID19-homelessness.pdf) stratifies the homeless population into four groups based on whether they are COVID-19 positive (or assumed to be positive and awaiting test results) or COVID-19 negative and based on whether they are in a group at high-risk of medical complications and poor outcomes if infected with COVID-19 (e.g. older adults, pre-existing medical chronic medical conditions) or in a group at low-risk of medical complications.  The assumption in stratifying the population into these four groups is that each group may need a slightly different type of emergency accommodation (e.g. emergency shelter with appropriate social distance, private accommodation in hotel/motel, etc.) The capacity estimates are intended to provided emergency accomodation needs at any given point in time (i.e. on any given night)--not on an annual basis. In other words, it is assumed that the units needed to meet capacity on a given night will turnover and thus can serve more people over an extended period.  Little data is available as of yet about whether such emergency accomodation units will turnover more (or less) quickly than shelter beds, and this information will be incorporated as it becomes available.


The information in each of these tabs will change depending on the selected values of the input parameters on the left hand side of the page.  The values of these parameters can be modified to reflect local variations in key pieces of information that will affect the likely impact of COVID-19 and capacity needed. Where applicable, the default values of these parameters reflect those used to develop estimates in [this report](https://works.bepress.com/dennis_culhane/237/)

The modifiable parameters are as follows:

1. __% undercount of the unsheltered population__: Point-in-time counts from the U.S. Department of Housing and Urban Development (HUD) of the unsheltered population may underestimate the true number of persons experiencing homelessness.  The extent of this undercount may vary from community to community.

2. __Infection rate in the homeless population__:  The peak rate of infection of COVID-19 in the homeless population may vary and will impact the type of emergency accommodation units needed.

3. __% of homeless population with high risk of medical complications__: Certain groups (e.g. older adults, those with underlying chronic medical conditions) may be at higher risk of medical complications and poor outcomes if infected by COVID-19.  Such high-risk individuals may require different emergency accommodation arrangements, and thus the proportion of the homeless population in a high risk group is an important element for informing responses to COVID-19.

4. __Annual turnover rate in sheltered homeless population__:  The number of persons experiencing homelessness over the course of a year may be several times greater than the number of persons experiencing homelessness on a given night.  In turn, the estimated impact of COVID-19 on the homeless population in terms of infections, hospitalizations and deaths over an extended period of time will vary depending on the number of people who experience homelesnsess over an extended period of time.  However, as noted above, estimated need for emergency accomodation is not sensitive to turnover. 

5. __Groups used to estimate capacity__: As noted above, the estimates of capacity needed stratify the homeless population into 4 different groups based on their COVID-19 status and their level of risk of medical complications.  Communities may want to target emergency accommodations at some subset of these 4 different groups and thus can model capacity needed to provide emergency accommodation to any combination of these 4 groups. 
