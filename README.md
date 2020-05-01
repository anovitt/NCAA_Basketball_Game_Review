###NCAA March Madness Analytics

#Introduction
The NCCA men basketball tournament, more commonly known as March Madness is, until this year, a classic annual event that captures the attention of basketball fans and non-basketball fans.  Office pools, Warren Buffet’s multimillion-dollar bracket, rooting for your alma mater or favorite team, and Kaggle’s Machine Learning Mania make it a fun time of year.  The tournament consists of 68 teams that have either won their respective conference championship or were selected by a committee of experts.   The committee seeds and assigns each team to one of four regions. When play begins it is either win or go home.   At the end of tournament, one team goes home undefeated.
Data
Data used for the study was provided by Kaggle’s Google Cloud & NCAA March Madness Analytics competition.  
•	Play by play data for the 2015 through the 2019 season.
•	Regular season detailed results, subset to the play by play seasons
•	Tournament seeds, subset to the play by play seasons.
•	Teams names data set.
•	Team conferences data set.
The games being reviewed are selected from two sources, a sporting news article by Mike Decourcy [1].  Ranking of the best games from the NCAA Tournament and one of my personal.   
Analysis Method
For the selected games, 
•	Regression analysis is used to compare each team’s regular season expected points for and points against vs time against the tournament game.  
•	The Poisson distribution is used to calculate the probability for rates of blocks, steals, turnovers, and points for a given time period in each game.
•	Plot the score difference between the two teams along with the cumulative sums of steals, blocks, turnovers vs. game time to visually locate the changes in rates, that indicate time spans to focus on.
•	Games that the play by play data has x and z coordinates shot charts to assess team strategy.
The Games.
“Heart Break Comeback”: 2016, Northern Iowa vs. Texas A&M.  A&M goes on a 12-point run to send the game into overtime in the last 44 seconds of regulation.
“Cinderella Upset”:  2018, Virginia’s loss to UMBC.  This was the first time ever that a number one seed lost to a number 16 seed.
“Teams Always Defeat Individuals”:  2019, Michigan State vs. Duke, Zion Williamson and Duke were expected roll.

To run the programs
1) NCAA_Madness is the master file.
2) Down load the data from Kaggle march-madness-anaalytics-2020.
3) Court_plot.R and NCAA_functions.R and helper file.
4) In NCAA_Madness, set path and path_play to the data file as and the play by play data from Kaggle
