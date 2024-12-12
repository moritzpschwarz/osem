# Naming Convention

Official Name of the Model: Open-Source Empirical Macro Model

-   Abbrev. in code: osem
-   Abbrev. in Text: OSEM
-   Abbrev. for full Name: OSEM Model

# Objects

We generally use `osem` objects. This could be `osem.model` or `osem.forecast` but generally we want to only use `osem` etc.

In those objects, we have the following columns as applicable:
- `time` in a date format
- `values` for a long-format dataset


# Variable names within functions

Within a function, we want to use the following names as a convention: 
- A dictionary in use is `dictionary` - `dict` is only the default one that is in the package

# S3 Methods

We are working towards creating more S3 methods: - `forecast.osem()` - `plot.osem()`

# Dictionaries

Dictionaries should be specific to each individual database.

They should be named as follows (always lowercase): `dict_{databasename}` for example `dict_imf` or `dict_eurostat`.

Each dictionary must have at least the following columns: model_varname, full_name, database, dataset_id, freq, geo.
