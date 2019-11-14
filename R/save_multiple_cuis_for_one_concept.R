#' Saves multiple cuis to one concept into a KEY for pre-coordinated concepts
#' @param concept_field can only be "VARIABLE_FIELD_NAME" or "PERMISSIBLE_VALUE_LABEL"
#' @param ... Multiple CUI-STRING combined in a vector. For example: c("C13431", "Breast cancer"), c("C39134", "Testicular cancer")
#' @return dataframe written to a csv file prefixed with "Odysseus_KEY_REDCAP_TO_UMLS_COORDINATES_"
#' @import crayon
#' @import readr
#' @import dplyr
#' @export

save_multiple_cuis_for_one_concept <-
        function(manifest_id,
                 concept_field,
                 concept_name,
                 ...) {

                Args <- list(...)

                path_to_key <- paste0("Odysseus_KEY_REDCAP_TO_UMLS_COORDINATES_", Sys.Date(), ".csv")

                error <- crayon::red $ bold

                if (!(concept_field %in% c("VARIABLE_FIELD_NAME", "PERMISSIBLE_VALUE_LABEL"))) {
                        cat(error("Error: invalid concept_field argument\n"))
                } else {
                        if (!file.exists(path_to_key)) {
                                x <- data.frame(KEY_TIMESTAMP= '',
                                                MANIFEST_ID= '',
                                                KEY_FIELD= '',
                                                KEY_CONCEPT_NAME= '',
                                                KEY_WORD= '',
                                                CUI_STR= '',
                                                CUI_COMMENT= '',
                                                CUI_FLAG= '',
                                                DENOVO_REASON= '',
                                                DENOVO_FLAG= '',
                                                FOLLOWUP_FLAG= '',
                                                FOLLOWUP_COMMENT= '',
                                                OTHER_COMMENTS= ''
                                )
                                x <- x[-1,]
                                readr::write_csv(x, path_to_key)

                                for (i in 1:length(Args)) {
                                        current_key <- readr::read_csv(path_to_key, col_types = cols(.default = "c")) %>%
                                                                dplyr::mutate_all(as.character) %>%
                                                                dplyr::mutate_all(trimws, "both")

                                        CUI_STR <- Args[[i]]

                                        add_to_key <-
                                                dplyr::tibble(KEY_TIMESTAMP = as.character(Sys.time()),
                                                              MANIFEST_ID   = manifest_id,
                                                              KEY_FIELD     = concept_field,
                                                              KEY_CONCEPT_NAME = concept_name,
                                                              CUI_STR        = CUI_STR
                                                              ) %>%
                                                dplyr::mutate_all(as.character) %>%
                                                dplyr::mutate_all(trimws, "both")

                                        new_key <- dplyr::bind_rows(current_key,
                                                                    add_to_key)

                                        readr::write_csv(new_key, path_to_key)

                                }
                        } else {
                                for (i in 1:length(Args)) {
                                        current_key <- readr::read_csv(path_to_key, col_types = cols(.default = "c")) %>%
                                                dplyr::mutate_all(as.character) %>%
                                                dplyr::mutate_all(trimws, "both")

                                        CUI_STR <- Args[[i]]

                                        add_to_key <-
                                                dplyr::tibble(KEY_TIMESTAMP = as.character(Sys.time()),
                                                              MANIFEST_ID   = manifest_id,
                                                              KEY_FIELD     = concept_field,
                                                              KEY_CONCEPT_NAME = concept_name,
                                                              CUI_STR        = CUI_STR
                                                ) %>%
                                                dplyr::mutate_all(as.character) %>%
                                                dplyr::mutate_all(trimws, "both")

                                        new_key <- dplyr::bind_rows(current_key,
                                                                    add_to_key)

                                        readr::write_csv(new_key, path_to_key)

                                }
                        }
                }
        }
