# COVID-19 Wastewater Analysis Report for Miami-Dade County

This repository contains the COVID-19 Wastewater Analysis Report for Miami-Dade County, utilizing data up to May 3rd, 2023. The report analyzes SARS-CoV-2 virus concentrations in wastewater samples and provides insights into the spread and intensity of COVID-19 within the community.

## Overview

The report includes a comprehensive analysis of wastewater data to track the presence and concentration of COVID-19. It employs a range of statistical and visualization techniques to present a clear picture of the virus's impact over time. The analysis is based on data collected by Biobot Analytics and processed using R with various packages such as `tidyverse`, `lubridate`, and `ggplot2`.

## Repository Structure

- **aes/**: Contains aesthetic resources including color palettes (`color_palettes.R`) and the report's LaTeX preamble (`preamble.tex`).
- **data/**: The raw data file `wastewater_by_county_0508.csv` used for analysis.
- **doh_functions.R**: Helper functions to streamline the analysis.
- **README.md**: This file, providing an overview of the project and instructions for navigating the repository.

## Report Highlights

- Time series analysis of normalized wastewater concentration from March 25th, 2020, until May 3rd, 2023.
- Descriptive statistics and visualizations of the data, including 3-day rolling averages to minimize noise.
- Methodology section detailing the data aggregation and analysis process.

## Data Source

The data for this analysis was provided by the **Nationwide Wastewater Monitoring Network**. More information can be found at their [GitHub repository](https://github.com/biobotanalytics/covid19-wastewater-data).

## How to Use

To replicate this analysis or modify it for further research:

1. Clone this repository to your local machine.
2. Ensure you have R installed, along with the required libraries listed in the `libraries` section of the report.
3. Run the R Markdown file to generate the report. Adjust paths as necessary to match your directory structure.

## Contributing

Contributions to this project are welcome. Please refer to the issues tab for pending enhancements or bug fixes. For major changes, please open an issue first to discuss what you would like to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)
