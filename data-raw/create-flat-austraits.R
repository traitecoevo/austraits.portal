library(austraits)
library(arrow)
library(dplyr)

# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, needs row_id added to Zenodo upload
## For now will use the R package and store in Github Releases
austraits <- load_austraits(version = "6.0.0", path = "inst/extdata/austraits", update = FALSE)

# Wrangle data
## Flatten the database
flatten_austraits <- austraits |> flatten_database()  |> 
    dplyr::mutate(row_id = dplyr::row_number())  |> 
    dplyr::mutate(measurement_remarks = iconv(measurement_remarks,
                    from = "",      # Let R guess the current encoding
                    to = "UTF-8",   # Convert to UTF-8
                    sub = ""        # Remove any bytes that can't be converted
    ))


## Save this as a parquet
arrow::write_parquet(flatten_austraits, "inst/extdata/austraits/austraits-6.0.0-flatten.parquet")

## Save lite version as a parquet
arrow::write_parquet(austraits:::austraits_5.0.0_lite |> flatten_database(), "inst/extdata/austraits/austraits-lite.parquet")

## Save the bib file
RefManageR::WriteBib(austraits:::austraits_5.0.0_lite$sources, "inst/extdata/austraits/sources.bib")
