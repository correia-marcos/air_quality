# ============================================================================================
# IDB: Air monitoring — the paper's ggplot theme
# ============================================================================================
#' @Goal: Register the manuscript font and set the global ggplot theme, on request.
#
#' @Description: Defines one function; sourcing this file changes nothing. Figure scripts call
#   set_paper_theme() once, immediately after sourcing config_utils_plot_tables.R. It used to
#   run at the top level of that file, which meant any script sourcing it — including
#   validation scripts that draw nothing — silently acquired a global font and theme.
#
#' @Summary:
#   I. set_paper_theme — register Tex Gyre Pagella and set the ggplot default
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: set_paper_theme
#
#' @param base_size        numeric; ggplot base font size. Default 14, the manuscript's.
#
#' @return     invisible NULL. Registers the font and sets the global ggplot theme.
#
#' @Purpose  : Gives every figure in the paper one font and one theme, from one call a reader
#              can see at the top of the script rather than inheriting invisibly.
#
#' @details    showtext_auto() routes text through showtext for all later devices, which is
#              what makes the registered font appear in the PDFs. Both are global settings,
#              which is exactly why this is a call and not a side effect of sourcing.
#
#' @Written_by: Marcos Paulo
#' @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
set_paper_theme <- function(base_size = 14) {
  sysfonts::font_add(
    "Palatino",
    regular = here::here("fonts", "texgyrepagella-regular.otf")
  )
  showtext::showtext_auto()

  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = "Palatino", base_size = base_size)
  )

  invisible(NULL)
}
