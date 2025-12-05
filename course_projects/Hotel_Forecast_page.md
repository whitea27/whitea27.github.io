## Hotel Occupancy Forecasting
### ISA 444 Final Project

**Project Purpose:** The goal of this project is to predict hotel occupancy. We are testing multiple types of models on a holdout of the data to see which one performs the best based on MAE. The models used to predict are Naive, Seasonal Naive, ETS, ARIMA, Lightgbm, NBeats, NHits, and TimeGPT.

**Data Description:** The sample of hotel data is for 18 different hotels. The data set includes the unique id for each hotel, date stamp, day, month, year, and percent occupied as a percentage. There are some hotels that when full occupancy is reached, convert conference areas into additional hotel rooms to house more guests, which causes the percent occupancy to surpass 1.0.

### Access Code Here:
[Forecasting Hotel Occupancy Code - Colab](https://colab.research.google.com/drive/1qkT8U0xFX7gUwOJr5dBeO068GXnkloZX#scrollTo=TPkiRkj-SnoO)

[Testing and Evaluation Output CSV](/course_projects/model_summary.csv)

**Evaluation:** Based on an evaluation metric of MAE we found that the best models to predict hotel occupancy were surprisingly the Naive and AutoETS models. Both models won based on MAE for 4 hotels each. We would recommend using AutoETS to predict how many rooms would be booked since this is a more complex model than Naive and with further tuning would work better in practice. 


