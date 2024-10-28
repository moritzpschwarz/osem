#  Naming Convention
Official Name of the Model: Open-Source Empirical Macro Model

- Abbrev. in code: osem
- Abbrev. in Text: OSEM
- Abbrev. for full Name: OSEM Model

# Objects

We generally use `osem` objects. This could be `osem.model` or `osem.forecast` but generally we want to only use `osem` etc. 

# S3 Methods

We are working towards creating more S3 methods: 
- `forecast.osem()`


# Dictionaries

Dictionaries should be specific to each individual database.
They should be named as follows: `dict_{databasename}` for example `dict_imf` or `dict_eurostat`.
Each dictionatry mus have the following columns: model_varname, full_name, database, freq, geo
