#' To reuse the same CUIs, first query the local library, which is a combination of an existing lookup found in data/ and the keys that are being generated in the working directory
#' @param ... phrases used to query the local library
#' @return dataframe of CUI and STR combinations from the lookup table and existing keys in the working directory
#' @import dplyr
#' @import readr
#' @import crayon
#' @export

query_local_library <-
        function(...) {
                note <- crayon::yellow $ bold
                Args <- list(...)
                key_fns <- list.files(pattern = "KEY")
                keys <- lapply(key_fns, readr::read_csv, col_types = cols(.default = "c"))
                names(keys) <- basename(key_fns)

                LOOKUP <- readr::read_csv("data/LOOKUP.csv", col_types = cols(.default = "c"))

                MASTER_KEY <- dplyr::bind_rows(keys, .id = "SOURCE_KEY")
                MASTER_KEY <- dplyr::bind_rows(LOOKUP, MASTER_KEY)


                output <- MASTER_KEY %>%
                                dplyr::filter_at(vars(dplyr::contains("STR")), any_vars(grepl(Args[[1]], ., ignore.case = TRUE) == TRUE))

                if (length(Args) > 1) {
                        for (i in 2:length(Args)) {
                                        output <-
                                                output %>%
                                                dplyr::filter_at(vars(dplyr::contains("STR")), any_vars(grepl(Args[[i]], ., ignore.case = TRUE) == TRUE))

                        }
                }

                return(output %>%
                               dplyr::select(CUI, STR, CUI_STR))
        }
