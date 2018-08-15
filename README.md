# bdtools
Tools and reports addressing different data quality issues

### About bdtools

Freely available high quality, data on species occurrence and associated variables are needed in order to track changes in biodiversity. One of the main issues surrounding the provision of such data is that sources vary in quality, scope, and accuracy. Addressing the challenge of providing high quality primary biodiversity data potentially can serve the needs of national and international biodiversity initiatives. Effective strategies for addressing biodiversity loss, and progressing biodiversity science, require that biodiversity-relevant information is made available in a useful form. The range of existing available data must be extended by strategically filling biodiversity knowledge gaps.

Bdtools is part of the [bd-verse](https://github.com/bd-R) which contains various packages to manage and analyse biodiversity data.

#### Functionalities Available in bdtools:

Currently bdtools contains taxonomic workflows for filtering, validating and resolving biodiversity data.

#### 1) Taxonomic-Synonym: [bd_synonym](https://github.com/bd-R/bdtools/blob/master/R/bd_synonyms.R)

This function helps in finding the synonym of the given scientific name from various data sources like "nbn", "itis", "col", "worms", "tropicos". User has the option of selecting the data base from which synonym is required.

#### 2) Taxonomic-Validation: [bd_taxonomic_validation](https://github.com/bd-R/bdtools/blob/master/R/bd_taxonomic_validation.R)

This function is used for validating the biodiversity data based on the taxonomic-rank and taxonomic-level of the sepcies. User is given the choice to select the taxonomic-level; "kingdom", "family", "phylum", "class", "order", "genus". Then user will have enter the value of the selected taxonomic-level. Based on the enetered choices data will be filtered and validated.

#### 3) Taxonomic-Resolving: [bd_taxonomic_resolver](https://github.com/bd-R/bdtools/blob/master/R/bd_taxonomic_resolver.R)

This function is used to resolve the the missing taxonomic ranks of the biodiversity data. User will have the choice to select the taxonomic rank and then missing taxonomic ranks will be resolved using some taxonomic functions. Finally data will be enriched.


#### ToDo

#### 1) Outlier analysis-Add certain functions to do outlier analysis
#### 2) Add more taxonomic workflows to analyse the data.
#### 3) Improve the existing dashboards to visualize data.


