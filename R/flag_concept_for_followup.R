#' Flags a concept for requiring further inquiry
#' @param manifest_id identifier for concept
#' @param concept_field can only be "VARIABLE_FIELD_NAME" or "PERMISSIBLE_VALUE_LABEL"
#' @param concept_name concept as a character string of length 1
#' @param comment your question about that concept that you need answered
#' @import crayon
#' @import dplyr
#' @import readr
#' @export

flag_concept_for_followup <-
        function(manifest_id,
                 concept_field,
                 concept_name,
                 comment = "") {

                path_to_key <- paste0("Odysseus_KEY_REDCAP_TO_UMLS_", Sys.Date(), ".csv")

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
                                                CUI= '',
                                                STR= '',
                                                CUI_COMMENT= '',
                                                CUI_FLAG= '',
                                                DENOVO_REASON= '',
                                                DENOVO_FLAG= '',
                                                FOLLOWUP_FLAG= '',
                                                FOLLOWUP_COMMENT= '',
                                                OTHER_COMMENTS= '') %>%
                                        dplyr::mutate_all(as.character) %>%
                                        dplyr::mutate_all(trimws, "both")
                                x <- x[-1,]
                                readr::write_csv(x, path_to_key)

                                current_key <- readr::read_csv(path_to_key, col_types = cols(.default = "c"))

                                add_to_key <-
                                        dplyr::tibble(KEY_TIMESTAMP = as.character(Sys.time()),
                                                      MANIFEST_ID   = manifest_id,
                                                      KEY_FIELD     = concept_field,
                                                      CUI           = cui,
                                                      STR           = str
                                        ) %>%
                                        dplyr::mutate_all(as.character) %>%
                                        dplyr::mutate_all(trimws, "both")

                                new_key <- dplyr::bind_rows(current_key,
                                                            add_to_key)

                                readr::write_csv(new_key, path_to_key)

                        } else {
                                current_key <- readr::read_csv(path_to_key, col_types = cols(.default = "c"))

                                add_to_key <-
                                        dplyr::tibble(KEY_TIMESTAMP = as.character(Sys.time()),
                                                      MANIFEST_ID   = manifest_id,
                                                      KEY_FIELD     = concept_field,
                                                      KEY_CONCEPT_NAME = concept_name,
                                                      FOLLOWUP_FLAG = "x",
                                                      FOLLOWUP_COMMENT = comment
                                                      ) %>%
                                        dplyr::mutate_all(as.character) %>%
                                        dplyr::mutate_all(trimws, "both")

                                new_key <- dplyr::bind_rows(current_key,
                                                            add_to_key)

                                readr::write_csv(new_key, path_to_key)
                        }
                }
        }
