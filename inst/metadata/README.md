## Metadata

These csv files contain metadata describing what plots the front end should display and the colours scales to use for plotting.

`metadata.csv` - describes what plot types to use for each data type and describes how to locate that data within the file.

| Column           | Description                                                                                  |
| ---------------- | -------------------------------------------------------------------------------------------- |
| data_type        | The type of the uploaded or output data one of survey, progamme, anc or output |
| plot_type        | The type of plot the metadata is relevant to, at the moment only choropleth |
| indicator        | The machine readable identifier for the indicator one of art_coverage, current_art, prevalence, vls, recent, plhiv, incidence |
| value_column     | The column containing the value to be plotted |
| error_low_column | The column containing the lower value for the error bars |
| error_high_column| The column containing the upper value for the error bars |
| indicator_column | If the file contains data for multiple indicators this is the column which should be used to differentiate between different indicators |
| indicator_value  | The value within the indicator_column to filter the data frame on |
| name             | Human readable name for the indicator |


`colour_scales.csv` - defines a colour scheme and scale for each type of indicator for each country.

| Column       | Description                                                                                    |
| ------------ | ---------------------------------------------------------------------------------------------- |
| country      | The country the colour scheme is for |
| indicator    | The indicator the colour scheme is for one of art_coverage, current_art, prevalence, vls, recent, plhiv, incidence |
| colour       | Colour scheme function name, must match one of the [d3 scale colour schemes](https://github.com/d3/d3-scale-chromatic#api-reference) |
| min          | The minimum value to scale the scheme to |
| max          | The maximum value to scale the scheme to |
| invert_scale | Whether to invert the colour scale or not TRUE or FALSE |

## FAQ

### How do I add a plot for a new indicator?
Add a new row to `metadata.csv` which describes what input data type or output data the idicator should be plotted for, the type of plot to use for the indicator and how to locate the relevant data within the dataset.

### How can I change the colour scheme for an indicator for a country?
Edit the `colour_scales.csv` file making sure that the colour column value matches the name of one of the d3-scale-chromatic schemes.

### How can I add a new plot type?
Add a new row to `metadata.csv` with the correct data type and the new plot type wanted. Then need to tell a developer on the front end that that has been added so they can add front end code to support the new kind of plot.
