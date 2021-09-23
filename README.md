# Web Mining Term Paper: Corporate insider trading during COVID-19: A performance analysis


## Repository content/structure

- `term_paper.pdf`: final paper
- `code.R`: R script for data collection, cleaning, analysis and visualization

## About the collected data
The data for this project was collected:
- from http://www.openinsider.com/: used for all data related to the SEC Form 4 (stock sales)
- via tidyquant from https://finance.yahoo.com/: used for all data related to daily stock prices and market indices 
- via API from https://www.eventstudytools.com/:  used for the event study methodology

Data files:
- API input files for the event study: 01_requestFile.csv, 02_firmDataPrice.csv, 03_marketDataPrice.csv   
- Event study results: analysis_report.csv, ar_results.csv, aar_results.csv, car_results.csv, caar_results.csv
- Russell 2000 daily market prices: ^RUT.csv: 

Structure R Code:
- Part 1: Data collection I
- Part 2: Data collection II
- Part 3: Event Study
- Part 4: Plots
- Part 5: Sensitivity analysis

## Summary
The study examined whether stock sales by insiders in the month prior to the COVID-19 crisis performed worse during the crisis. For this purpose, data was collected on 312 stock sales by corporate insiders with a total volume of 2,349 mil. USD. Using an event study, the period from 10 days before the event date on February 20, 2020 to 30 days afterwards was analyzed in more detail. It was demonstrated that the proportion of transactions with significant negative abnormal returns increased sharply after the event day. The paper has also shown that a negative CAAR of -3,69% was found in an event window of -10 to +30 days using the market model. The subsequent cross-sectional t-test confirmed that the CAAR is significant at the 5% level. It can be concluded that this paper delivers a detailed look at the characteristics and performance of insider trades during the COVID-19 crisis.
