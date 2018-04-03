# Masters  

## ELO Calculations
ELO calculations are located in the `elo_ratings_masters.csv` file

## Masters Results Files

- `masters_results_mastersdotcom.csv` - Results copied from <http://www.masters.com/en_US/tournament/past_winners.html>. Includes all years (1934-2017), plus those who missed the cut. It does not include tournament earnings or FedExCup points.
- `masters_results_pgatourdotcom.csv` - Results copied from <https://www.pgatour.com/tournaments/masters-tournament/past-results.html>. Includes the years 1970-2017. Includes cut information, tournament earnings, and FedExCup points.
- `masters_results_augustadotcom.csv` - Results from <http://www.augusta.com>. Scraped using the file `masters_results_augustadotcom.py`. Includes all years (1934-2017) and tournament earnings. It does not include cut information or FedExCup points.

## Scraping/Data Cleaning/Analysis Files

- `masters_results_augustadotcom.py` - Scrapes and cleans Masters results from <http://www.augusta.com>
- `merge_leaderboards.py` - Creates one leaderboard from the different Masters Results Files (work in progress)

## Calculating ELO

- `preparing_for_calculating_elo.R` - R file to clean `leaderboard_mastersdotcom.csv` and prepare for ELO calculations
- `analysis.R` - R file with functions to calculate ELO

### Functions in `analysis.R` to Calculate ELO

- `first_year_elo()` - Calculates the first year of ELO in 1934.
- `get_elo_init()` - Part of `first_year_elo()`. It grabs the first year (1934) and first round (r1) results from `leaderboard_with_z_score_for_elo_calculating.csv` and players in that round from `player_list_initial_year.csv`
- `calculate_elo()` - Calculates every year of ELO after 1934
- `get_next_year_of_data()` - Gets a player's most recent ELO rating. Part of `calculate_elo()`.

#### Set up data frame
```
df <- data.frame(
  player = character(),
  year = integer(),
  round = character(),
  elo = numeric(),
  elo_new = numeric()
  )
 ```

#### Run `first_year_elo`
`first_year_elo(1934, 'r1')`

#### Run calculate_elo() for each individual year 1934-1942, 1946-2017

Each time `calculate_elo()` is run, it binds to `df`.

```
calculate_elo('1935')
calculate_elo('1936')
calculate_elo('1937')
...
```  

#### Export `df` after you run `calculate_elo()` through 2017
```
write_csv(df, "elo_ratings_masters.csv")
```

## Notes
- Cuts start at the Masters in 1957
- The Masters wasn't played from 1943-1945 because of World War II
