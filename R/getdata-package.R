#' @import rlang
#' @importFrom glue glue glue_collapse
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom sfext st_bbox_ext st_filter_ext df_to_sf is_sf as_sf
#'   relocate_sf_col rename_sf_col
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
rlang::on_load(rlang::local_use_cli())
