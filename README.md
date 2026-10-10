# Crime Data Analysis and Visualization Using R

MCA - R Programming Mini Project
**Author:** Sutharssan (717825Z155) | MCA Year II, Semester III, Section A
School of Computer Applications, Karpagam College of Engineering, Coimbatore

## About the project
This project cleans a crime dataset, explores it with statistics and charts, and builds machine learning
models that predict whether a case will be **Solved** or **Unsolved**. It is written in R and run in RStudio.

## Project modules
1. Dataset import
2. Data cleaning (duplicates, missing values, text errors, impossible ages)
3. Exploratory data analysis
4. Crime trend analysis (year, month, hour)
5. State / city-wise analysis
6. Victim demographic analysis
7. Solved vs unsolved analysis
8. Data visualization (20 charts)
9. Model building: Logistic Regression, Decision Tree, Random Forest
10. Key findings

## Folder structure
```
crime_analysis.R          main R script (all stages)
data/crime_data.csv       raw dataset (1,015 records, 11 columns, simulated)
charts/                   20 PNG charts produced by the script
output/                   cleaned CSV, model comparison table, console output
report/                   detailed project report (Word)
presentation/             project presentation (PowerPoint)
```

## How to run
1. Install R and RStudio.
2. Open `crime_analysis.R` in RStudio.
3. Set the working directory: *Session > Set Working Directory > To Source File Location*.
4. Click **Source**. Missing packages are installed automatically
   (ggplot2, dplyr, tidyr, scales, corrplot, caret, ranger, rpart).

## Results
| Model | Accuracy | Precision | Recall | F1 |
|---|---|---|---|---|
| Logistic Regression | 0.6884 | 0.7273 | 0.6275 | 0.6737 |
| Decision Tree | 0.6131 | 0.6543 | 0.5196 | 0.5792 |
| Random Forest | 0.6181 | 0.6204 | 0.6569 | 0.6381 |

Baseline accuracy (always predict the majority class): 0.5126.

## Key findings
- Crime fell to 108 cases in 2020 and rose every year to 185 in 2025.
- Theft (187) and Cyber Crime (147) are the most common crimes.
- Delhi, Uttar Pradesh and Maharashtra report the most cases.
- 59% of crimes happen in the evening and at night, peaking at 6-8 PM.
- Only 51.2% of cases are solved; Burglary and Cyber Crime are hardest to solve.
- Crime type is the strongest predictor of whether a case is solved.

*Note: the dataset is simulated for academic practice.*
