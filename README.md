# Forecasting_Challenge
Weekly Forecasts of wind speed and temparature in Berlin-Tempelhof and DAX Returns as part of the PTSFC.

## Code Structure
All R-Code can be found inside of the folder _src_.
Models are implemented inside of the files with names structured like ```model_<variable>.R```.
The file ```model_enhancement.R``` could be seen as a gigantic notebook containing all exploration and analyses i have done and thus is a bit messy. 
Everything worth of a getting into an own function can be found in elaborated manner inside of the mentioned ```model_<variable>.R```-files and further files like ```visual_checks.R``` for visualization and so on. The names should make the contents obvious.

## Reproduction of Forecasts
The forecasts are produced with the script ```make_forecasts.R```. 
Necessary packages are imported in the beginning of the file.
Before generating the forecasts, the date on which they sould be made has to be put into ```date```.

This file can be seen as something like a main.R-file and could be executed using the console (after changing the value of ```date``` according to the desired date of forecast initialization), but I opted for line-by-line execution (via STR+ENTER) to be able to check the results and the visual checks that are generated before saving them.

Note that the weather data has to be downloaded and put into the following folder structure before execution manually, since the functions depend on that:
```
root > data > weather_daily > Berlin
root > data > weather_historical > Berlin
```
The DAX-functions were programmed to download the data automatically.

I didn't include the raw data into this repository for the sake of keeping the repo slim, but I could add them if it was desired.

## Submitted Forecasts
All submitted forecasts can be found inside of the folder _forecasts_. They were created using the script ```make_forecasts.R``` mentioned above.

## Additional Data
Interesting data, plots and tables generated over the course of the challenge were saved inside of the folder _graphics_and_tables_for_elaboration_.
Some of them may appear in my elaboration.
