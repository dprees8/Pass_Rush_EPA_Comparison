# Pass_Rush_EPA_Comparison
Comparison of the projected EPA for Pass &amp; Rush plays given different scenarios

# Summary
This program is intended to use the EP_Model establish by NFLScrapr to predict the EPA for rush vs pass plays in given scenarios.
For each scenario, a weighted EPA is calculated for the "possible outcomes" for a rush play and for a pass play.

For a rush play, the "possible outcomes" include:
  - Rush with success level 1
  - Rush with success level 2
  - Rush with success level 3
  - Fumble lost
  
For a pass play, the "possible outcomes" include:
  - Completed pass with success level 1
  - Completed pass with success level 2
  - Completed pass with success level 3
  - Incomplete pass
  - Interception
  - Sack
  - QB Scramble
  - Penalty

The function can be run for the full league, by team, by quarterback, or by team & quarterback.  The 'by quarterback' functionality is not great so I would focus on the full league & team.

# Details

The yardage for the success levels were based on first taking the 33rd and 67th quantile.
Then, taking the average yards gained for the first third, second third, and last third.
Since the count isn't exactly thirds, the weighting is based on actual counts.
Unforunately, by using thirds, there is a jump in the chart at the breakpoints, but this was a balance between results and functionality.

For Penalty impact, the change is assumed to be an automatic first down and a ten yard penalty.

For fumbles and interceptions, there is no change in field position assumed.

There is no change in clock accounted for.

The formula does NOT work for fourth down due to the need to add in functionality around change in possession.
The formula does NOT account for plays that would result in scores, eg LOS on 10 yard line and play for more than 10 yards.

I would suggest using the MAPPLY functionality to run multiple scenarios:
eg First<-mapply(RunPassComparison, yardline_100_input=50, down_input = 1, yards_to_go_input = 5:20)

If you already have the EP_MODEL or the PBP file saved, you can skip a lot of the coding, but I included just in case.

# Qualifier
I am new to R so any if you see any errors or inefficiencies, I would really appreciate any feedback!
I also don't know how to graph very well so any suggestions would be awesome!
I was thinking it would be cool to do a heatmap showing the relative EPA_Pass - EPA_Rush based on down & distance

