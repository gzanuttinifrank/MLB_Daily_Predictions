## MLB_Daily_Predictions

### MLB_standings_analysis.R

Scrapes each team's data from [ESPN](http://proxy.espn.com/mlb/standings?type=expanded&group=9) and [Baseball-Reference](baseball-reference.com) for upcoming matchups:
* **Team data:** runs per game for and against, active winning or losing streak, and records in different situations (1-run games, home vs. away games, v. LHP and RHP, etc.)
* **Pitching data:** bullpen and scheduled starting pitcher statistics

Pulls odds for each game from [VegasInsider.com](http://www.vegasinsider.com/mlb/odds/las-vegas/) and predictions from [FiveThirtyEight](https://projects.fivethirtyeight.com/2017-mlb-predictions/games/) to act as benchmarks.

Updates the historical dataset with results from the previous day's games.


### Model.R

Under the heading "HomeWin" is the model for predicting win probability directly, followed by a function that performs five-fold cross validation to test the model's historical performance.

Under the heading "Rundiff" is the model I have been using to make predictions, which predicts run differential using random forests and then converts this value to a win probability based on the historical distribution of results. The "Check today's predictions" section trains a model on the historical data and predicts the current day's games. It then filters out the games it is most confident in, based on the margin of difference between the model's predictions and Vegas' odds.


### Historical Data

This folder contains a 3-day sample of the datasets for each day:
* **HistoricalTable.csv** is a table of each recorded matchup, with the starting pitchers, betting odds, and all of the statistics detailed above up to the day that the game was played.
* **HistTable_wToday.csv** is the same as HistoricalTable.csv but has today's games appended to it with the results marked as NA.
* **FiveThirtyEight.csv** contains FiveThirtyEight's predicted win probabilities, which are only used for future comparison.
