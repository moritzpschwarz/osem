# Internal function to create the dictionaries This script contains a separate function called generate_dictionary_script. This can be used to read in a modified dictionary and will then give out the code to insert into this function.

Internal function to create the dictionaries This script contains a
separate function called generate_dictionary_script. This can be used to
read in a modified dictionary and will then give out the code to insert
into this function.

## Usage

``` r
create_dictionaries(single_df = FALSE)
```

## Arguments

- single_df:

  Logical. If TRUE, the function will return a single data frame with
  all dictionaries. If FALSE, the function will return a list of data
  frames.

## Value

Either a list of dictionaries or a data.frame with a single dictionary.
