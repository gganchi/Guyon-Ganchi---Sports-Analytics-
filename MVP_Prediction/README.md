# Description of the Files:
- MVP_Data.xlsx: This excel file that includes MVP data to be uploaded into the MVP_Prediction.R file to allow the model to run. The data is for the players that received votes from the 2014-15 season to the 2024-25 season (excluding pandemic affected seasons).
- MVP_Prediction.R: This R file is where the XGBoost model can be found. Use the MVP_Data.xlsx file to load the input.
- predictions.csv: This is the output file after running the R file that provides the probability of players winning the MVP as of 11/28/25 from 0 to 1.
- MVP Model Summary.pptx: A slide show providing a brief overview of this project as a whole showing what and where the data was collected to the model results and predictions.
