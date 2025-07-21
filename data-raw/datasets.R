## code to prepare datasets goes here

dataset_ccy <- currencies()

dataset_crypto <- cryptocurrencies()[, c("code", "name", "address_regex")]


usethis::use_data(dataset_ccy, dataset_crypto, internal = TRUE, overwrite = TRUE)
