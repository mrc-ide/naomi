These files are alternative testing and demonstration files representing subnational 
Spectrum files. Three Spectrum PJNZ files werecreated based on the Malawi 2019 PJNZ file.

_Update 22 August 2024:_ Updated files were created with the Malawi 2024 PJNZ file
using Sepctrum 6.36.

Region codes in .PJN files manually changed as follows:

* Northern Region: code 10
* Central Region: code 11
* Southern Region: code 12

Datasets are also created for zone-level programme data inputs for efficient model testing.

## Area hierarchy

The `spectrum_region_code` field was updated from `0` to the values `10`, `11`, `12`.

## Spectrum files

### Base population

The base population and net migration were scaled in each file based on the proportions:

| Region   | Proportion | 
|: --------|------------|
| Northern | 13%        |
| Central  | 43%        |
| Southern | 44%        |

No other changes were made to demographic inputs. Fertility and non-HIV mortality rates were retained at national values.


### Number on ART

The number on ART was scaled by the proportions:

| Region   | Proportion | 
|: --------|------------|
| Northern | 10%        |
| Central  | 29%        |
| Southern | 61%        |

The same proportions were used for adults on ART, children on ART, and children receiving cotrimoxazole

### PMTCT and ANC clients

PMTCT and ANC clients were scaled 


| Region   | Clients    | Known Positive | ANC tested | Tested positive | PMTCT |
|: --------|------------|----------------|------------|-----------------|-------|
| Northern | 13%        | 8%             | 12%        | 8%              | 8%    |
| Central  | 43%        | 26%            | 42%		  | 28%				| 27%   |
| Southern | 44%        | 66%            | 46%		  | 64%				| 65%   |

### EPP fitting

EPP was refit single region in each file.

_For the 2024 Spectrum file:_ EPP by region was re-imported from the 2022 PJNZ files

Analysis was done using the Spectrum v5.87 and based from the 2019 Malawi Spectrum file.
