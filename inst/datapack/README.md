This directory contains metadata to construct outputs for import into PEPFAR data pack.


## `datapack_psnu_area_id_map.csv` 

This file contains the ID mapping between the Naomi model dataset `area_id` and the PSNU UID in Datim. 

If either the area hierarchy changes or the Datim UID changes, this needs to be updated.

Datim PSNU UIDs are sourced from: https://www.datim.org/api/sqlViews/gsaaxFM8ZN0/data.html+css

## Data pack indicators

Below are the indicators expected by Data Pack 2021.  The indicators `VMMC_CIRC_SUBNAT.T_1` and
`VMMC_TOTALCIRC_SUBNAT.T_1` do not come from Naomi.

| indicator_code                     | data_element  |  calendar_quarter |  value_type  |
|:-----------------------------------|:--------------|:------------------|:-------------| 
| POP_EST.T_1                        | KssDaTsGWnS   |  CY2021Q3         |  integer     |
| HIV_PREV.T_1                       | lJtpR5byqps   |  CY2021Q3         |  percentage  |
| PLHIV.T_1                          | iwSejvD8cXl   |  CY2021Q3         |  integer     |
| TX_CURR_SUBNAT.T_1                 | xghQXueYJxu   |  CY2021Q3         |  integer     |
| TX_CURR_SUBNAT.R                   | MktYDp33kd6   |  CY2020Q4         |  integer     |
| DIAGNOSED_SUBNAT.T_1               | nF19GOjcnoD   |  CY2021Q3         |  integer     |
| PMTCT_STAT_SUBNAT.D.T_1            | RM8gRoxtsNw   |  CY2021Q3         |  integer     |
| PMTCT_ART_SUBNAT.D.T_1             | eJaChfuqUTs   |  CY2021Q3         |  integer     |
| PMTCT_ART_SUBNAT.N.Already.T_1     | HVBf6Sgi6Jk   |  CY2021Q3         |  integer     |
| PMTCT_ART_SUBNAT.N.New.T_1         | HVBf6Sgi6Jk   |  CY2021Q3         |  integer     |
| PMTCT_STAT_SUBNAT.N.Known.Pos.T_1  | tAE7ZD7p9zu   |  CY2021Q3         |  integer     |
| PMTCT_STAT_SUBNAT.N.New.Pos.T_1    | tAE7ZD7p9zu   |  CY2021Q3         |  integer     |
| PMTCT_STAT_SUBNAT.N.New.Neg.T_1    | tAE7ZD7p9zu   |  CY2021Q3         |  integer     |
| VMMC_CIRC_SUBNAT.T_1               | SSun4i7nHlV   |  CY2021Q3         |  integer     |
| VMMC_TOTALCIRC_SUBNAT.T_1          | ZayJeEa6pCa   |  CY2021Q3         |  integer     |
