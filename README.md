![Immigration Data Explorer Screenshot](https://github.com/appelson/ICE_data/blob/ebb9906081b3b58b796ec3b575716909223ca005/immigration_data_explorer.png)

## Project Overview  
The **Immigration Data Explorer** is an interactive dashboard designed to increase transparency and accessibility around immigration enforcement activities in the United States. Leveraging data from the [Deportation Data Project](https://deportationdata.org/), this tool provides a comprehensive view of arrest and detention records spanning from **September 2023 through July 2025**.

## Key Features  
- **Interactive Exploration:** Dynamically filter immigration arrest and detention records by date and case status.
- **Custom Table Creation:** Build data tables to focus on specific subsets of information relevant to your needs.
- **Export Functionality:** Download generated tables in common formats (e.g., CSV) for further analysis or reporting.  

## Intended Audience  
This project is valuable for a broad range of users, including:  
- **Journalists:** To investigate and report on immigration enforcement patterns with empirical backing.  
- **Immigration Attorneys:** To gain insights into detention and arrest data that may support casework or policy advocacy.  
- **General Public:** To promote awareness and understanding of immigration enforcement activities in the U.S.  

## Data Source  
The dataset used in this project is sourced from the [Deportation Data Project](https://deportationdata.org/), an initiative dedicated to compiling and publishing detailed immigration enforcement records.

## Replicating
### 1. Clone
Clone the repository to your local machine:
```
git clone https://github.com/appelson/ICE_dashboard
```

### 2. Data
Download arrest data from the [Deportation Data Project](https://deportationdata.org/).

### 3. App
Open the `arrest.Rproj` and `detention.Rproj` projects and, within each, app.R. This should create the app.

## Project Structure  
The repository is organized as follows to separate concerns and facilitate development:

```
├── README.md
├── arrest
│   ├── app.R
│   ├── arrest.Rproj
│   ├── arrest.parquet
│   └── rsconnect
│       └── shinyapps.io
│           └── appelson
│               ├── arrest.dcf
│               └── immigration.dcf
├── cleaning.R
├── detention
│   ├── app.R
│   └── rsconnect
│       └── shinyapps.io
│           └── appelson
│               └── detention.dcf
└── home
    ├── app.R
    └── home.Rproj
```
