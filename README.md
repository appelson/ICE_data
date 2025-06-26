# Immigration Data Explorer

## Project
This interactive dashboard is built to enhance transparency around immigration enforcement activities in the United States. This tool allows the user to create and export tables from data released by the (Deportation Data Project)[https://deportationdata.org/] and covers arrests and detention records from September 2023 through July 2025.

## Audience
Anyone interested in immigration data. This may be especially useful to journalists or immigration attorneys.

## File Structure

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
