# Bring inst/extdata/austraits/trait_groups_for_portal.csv in line with an
# AusTraits release. Run from the repo root at each release:
#
#   Rscript dev/update_trait_groups.R <austraits-<version>.rds> <path to APD checkout>
#
# - A trait whose APD URI now carries another name is renamed in place (it
#   keeps its group, structure, keywords and core flag).
# - A trait the release no longer defines is dropped.
# - A trait with data in the release but no row is added: its APD category
#   becomes `trait_groupings`, and `trait_group_for_portal` is the portal group
#   most rows of that category already use (else the category's label).
#   Its structure and keywords stay empty, for a curator to fill in.
# The script prints every change, for the PR.

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)
database <- readRDS(args[1])
apd <- args[2]
path <- "inst/extdata/austraits/trait_groups_for_portal.csv"

groups <- utils::read.csv(path, colClasses = "character", na.strings = "NA")
definitions <- database$definitions
uri <- vapply(definitions, function(x) if (is.null(x$entity_URI)) NA_character_ else x$entity_URI, "")

# Renamed: same URI, new name. Only a real URI that names exactly one trait in
# the release, and only to a name the list does not already hold (two rows can
# share a URI, and some carry the placeholder .../traits/NA)
uri_ok <- uri[!is.na(uri) & !grepl("/NA$", uri) & !uri %in% uri[duplicated(uri)]]
new_name <- names(uri_ok)[match(groups$Entity, uri_ok)]
renamed <- !groups$trait %in% names(definitions) & !is.na(new_name) & !new_name %in% groups$trait
for (i in which(renamed)) {
  message("renamed: ", groups$trait[i], " -> ", new_name[i])
  groups$trait[i] <- new_name[i]
}

# Dropped: no longer defined
gone <- !groups$trait %in% names(definitions)
for (t in groups$trait[gone]) message("dropped: ", t)
groups <- groups[!gone, ]

# Added: traits with data and no row
apd_traits <- yaml::read_yaml(file.path(apd, "data/APD_traits_input.yml"))
category <- vapply(apd_traits, function(x) if (is.null(x$category)) NA_character_ else x$category, "")
names(category) <- vapply(apd_traits, function(x) x$trait, "")
hierarchy <- utils::read.csv(file.path(apd, "data/APD_trait_hierarchy.csv"), colClasses = "character")

portal_group_for <- function(grouping) {
  existing <- groups$trait_group_for_portal[groups$trait_groupings == grouping & !is.na(groups$trait_group_for_portal)]
  if (length(existing) == 0) return(grouping)
  names(sort(table(existing), decreasing = TRUE))[1]
}

with_data <- sort(unique(database$traits$trait_name))
missing <- setdiff(with_data, groups$trait)
added <- do.call(rbind, lapply(missing, function(t) {
  grouping <- hierarchy$label[match(category[[t]], hierarchy$identifier)]
  if (length(grouping) == 0 || is.na(grouping)) grouping <- "other"
  data.frame(
    Entity = uri[[t]], trait = t, structure_measured = NA_character_,
    trait_groupings = grouping, trait_group_for_portal = portal_group_for(grouping),
    core_trait = NA_character_, keywords = NA_character_
  )
}))
if (!is.null(added)) {
  for (i in seq_len(nrow(added))) message("added: ", added$trait[i], " (", added$trait_group_for_portal[i], ")")
  groups <- rbind(groups, added[, names(groups)])
}

readr::write_csv(groups, path, na = "NA")
message(sprintf("%d renamed, %d dropped, %d added; %d traits, %d core",
                sum(renamed), sum(gone), length(missing), nrow(groups), sum(!is.na(groups$core_trait))))
