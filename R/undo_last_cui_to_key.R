#' Undos last cui to key
#' @import crayon
#' @import readr
#' @export

undo_last_cui_to_key <-
        function() {

                note <- crayon::yellow $ bold
                cat(note("Note: you are about to remove the last cui you added today.\n"))
                readline(prompt = "Press [ENTER] to continue or [ESC] to end. ")

                path_to_key <- paste0("Odysseus_KEY_REDCAP_TO_UMLS_", Sys.Date(), ".csv")
                current_key <- readr::read_csv(path_to_key, col_types = cols(.default = "c"))

                new_key <- current_key[-(nrow(current_key)),]
                readr::write_csv(new_key, path_to_key)

        }
