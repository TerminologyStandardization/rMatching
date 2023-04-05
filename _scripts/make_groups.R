#' Helper Function: Make Groups
#'
#' @param .dir_tabs Directory to store Tables
#' @param .dir_store Directory to store matching
#' @param .cols
#' A named character, with the columns names as string you want to match.\cr
#' The vector must be named wit either fuzzy (f) of exact (e).
#' @param .range Character range
#'
#' @return A table with Groups (saved in .dir)
make_groups <- function(.dir_tabs, .dir_store, .cols, .range = Inf) {

  # Assign NULL to Global Variables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  tmp <- id <- val <- group <- nc <- nct <- ncs <- ns <- nt <- NULL

  dir.create(file.path(.dir_store, "_tmp"), FALSE, TRUE)

  cols_e_ <- .cols[names(.cols) %in% c("e", "exact")]
  cols_f_ <- .cols[names(.cols) %in% c("f", "fuzzy")]

  tabs_ <- dtplyr::lazy_dt(fst::read_fst(file.path(.dir_tabs, "sdata.fst")))
  tabs_ <- tibble::as_tibble(tabs_)
  grps_ <- tabs_ %>%
    dplyr::filter(col %in% cols_e_) %>%
    dplyr::mutate(val = dplyr::if_else(is.na(val), "_none_", val)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(group = paste(val, collapse = "<>"), .groups = "drop") %>%
    dplyr::mutate(group = dplyr::if_else(grepl("_none_", group, fixed = TRUE), "_none_", group)) %>%
    tibble::as_tibble()
  tabs_ <- tabs_ %>%
    dplyr::filter(col %in% cols_f_) %>%
    dplyr::left_join(grps_, by = "id") %>%
    dplyr::group_by(group, col, val, nc) %>%
    dplyr::mutate(tmp = dplyr::cur_group_id()) %>%
    dplyr::arrange(tmp) %>%
    dplyr::select(tmp, id, group, col, val, nc) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()

  ids_  <- dplyr::select(tabs_, tmp, id)
  tabs_ <- tabs_ %>%
    dplyr::distinct(tmp, group, col, val, nc)

  fst::write_fst(ids_, file.path(.dir_store, "_tmp", "sids.fst"), 100)
  fst::write_fst(tabs_, file.path(.dir_store, "_tmp", "sdata.fst"), 100)


  s_none_ <- unique(tabs_$group)
  s_none_ <- s_none_[grepl("_none_", s_none_)]


  tabt_ <- dtplyr::lazy_dt(fst::read_fst(file.path(.dir_tabs, "tdata.fst")))
  grpt_ <- tabt_ %>%
    dplyr::filter(col %in% cols_e_) %>%
    dplyr::mutate(val = dplyr::if_else(is.na(val), "_none_", val)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(group = paste(val, collapse = "<>"), .groups = "drop") %>%
    dplyr::mutate(group = dplyr::if_else(grepl("_none_", group, fixed = TRUE), "_none_", group)) %>%
    tibble::as_tibble()
  tabt0_ <- tabt_ %>%
    dplyr::filter(col %in% cols_f_) %>%
    dplyr::left_join(grpt_, by = "id") %>%
    tibble::as_tibble()
  tabt1_ <- tabt0_ %>%
    dplyr::select(-group) %>%
    tidyr::expand_grid(group = s_none_)

  tabt_ <- dplyr::distinct(dplyr::bind_rows(tabt0_, tabt1_)) %>%
    dtplyr::lazy_dt()  %>%
    dplyr::group_by(group, col, val, nc) %>%
    dplyr::mutate(tmp = dplyr::cur_group_id()) %>%
    dplyr::arrange(tmp) %>%
    dplyr::select(tmp, id, group, col, val, nc) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()

  idt_  <- dplyr::select(tabt_, tmp, id)
  tabt_ <- tabt_ %>%
    dplyr::distinct(tmp, group, col, val, nc)

  fst::write_fst(idt_, file.path(.dir_store, "_tmp", "tids.fst"), 100)
  fst::write_fst(tabt_, file.path(.dir_store, "_tmp", "tdata.fst"), 100)


  ncs_ <- tabs_ %>%
    dplyr::select(group, col, nc) %>%
    dplyr::count(group, col, nc)
  nct_ <- tabt_ %>%
    dplyr::select(group, col, nc) %>%
    dplyr::count(group, col, nc)

  group_ <- ncs_ %>%
    # Join with Target Dataframe
    dplyr::inner_join(nct_, by = c("group", "col"), suffix = c("s", "t")) %>%
    # Filter Nchars according to range
    dplyr::filter(nct >= ncs - .range & nct <= ncs + .range) %>%
    dplyr::group_by(group, col, ncs) %>%
    dplyr::summarise(
      ncs1 = ncs[1],
      ncs2 = ncs[1],
      nct1 = min(nct),
      nct2 = max(nct),
      ns = ns[1],
      nt = sum(nt),
      size = ns * nt,
      .groups = "drop"
    )

  return(group_)
}
