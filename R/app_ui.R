#' User interface (UI) for AusTraits Data Portal

app_ui <- function() {
  # Serve the packaged static assets (AusTraits stylesheet, logo) under www/
  shiny::addResourcePath(
    "www",
    system.file("www", package = "austraits.portal")
  )

  ui <- page_sidebar(
    # AusTraits site stylesheet (matches austraits.org)
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/austraits-portal.css"),
      tags$link(rel = "icon", href = "www/austraits_hex.png", type = "image/png")
    ),
    # Custom CSS for DataTable
tags$head(
  tags$style(HTML("
    /* Set max widths for truncated columns */
    .truncated {
      max-width: 20rem !important;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    
    /* Tooltip styling */
    .cell-tooltip {
      position: absolute;
      background-color: #333;
      color: white;
      padding: 8px 12px;
      border-radius: 4px;
      font-size: 13px;
      z-index: 9999;
      max-width: 400px;
      word-wrap: break-word;
      box-shadow: 0 2px 8px rgba(0,0,0,0.3);
      pointer-events: none;
    }
  ")),
  
  # JavaScript for tooltip on truncated cells
  tags$script(HTML("
    $(document).ready(function() {
      // Create tooltip element
      if ($('.cell-tooltip').length === 0) {
        $('body').append('<div class=\"cell-tooltip\" style=\"display: none;\"></div>');
      }
      
    // Add help cursor and show tooltip ONLY for actually truncated cells
    $(document).on('mouseenter', '.truncated', function(e) {
      var cell = $(this);
      var text = cell.text().trim();
      
      // Check if text is actually truncated (content wider than container)
      if (this.offsetWidth < this.scrollWidth) {
        // Add help cursor
        cell.css('cursor', 'help');
        
        // Show tooltip
        $('.cell-tooltip')
          .text(text)
          .css({
            top: e.pageY + 15 + 'px',
            left: e.pageX + 15 + 'px',
            display: 'block'
          });
      } else {
        // Remove help cursor if not truncated
        cell.css('cursor', 'default');
      }
    });
      
      // Move tooltip with mouse
      $(document).on('mousemove', '.truncated', function(e) {
        if ($('.cell-tooltip').is(':visible')) {
          $('.cell-tooltip').css({
            top: e.pageY + 15 + 'px',
            left: e.pageX + 15 + 'px'
          });
        }
      });
      
      // Hide tooltip on mouse leave
      $(document).on('mouseleave', '.truncated', function() {
        $('.cell-tooltip').hide();
      });
    });
  "))
),

      # Loading spinner
      shinybusy::add_busy_spinner(
        spin = "fading-circle",
        position = "top-right",
        color = "#2f7d55",
        height = "60px",
        width = "60px"
      ),

    # Set the overall theme of the app to match austraits.org (simplex base
    # with the AusTraits brand palette).
    theme = bs_theme(
      preset = "simplex",
      primary = "#235c43",
      secondary = "#64726b",
      success = "#2f7d55",
      info = "#2a7f9e",
      warning = "#d9a441",
      "body-bg" = "#fdfdfc",
      "body-color" = "#2b332f",
      "link-color" = "#235c43",
      "link-hover-color" = "#2a7f9e",
      "border-radius" = "8px"
    ),

    # Title of the portal, with the AusTraits hex logo. The
    # `bslib-page-title` class matches the brand-bar rules in
    # austraits-portal.css (bslib does not add it for custom title nodes).
    title = tags$span(
      class = "bslib-page-title navbar-brand",
      tags$img(src = "www/austraits_hex.png", alt = "AusTraits", height = "30"),
      "AusTraits Data Portal",
      # The release the portal serves, linked to its DOI
      portal_version_badge(release)
    ),

    # Create a sidebar for the app
    sidebar = mod_filters_ui("filters"),

    # Data display
    navset_bar(
      id = "main_tabs",

      nav_panel(
        title = "Data Preview",
        mod_data_table_ui("data_table")
      ),
      nav_panel(
        title = "App Information",
        mod_app_info_ui("app_info")
      ),
      nav_panel(
        title = "Taxon View",
        mod_taxon_view_ui("taxon_view")
      ),
      nav_panel(
        title = "Trait View",
        mod_trait_view_ui("trait_view")
      ),
      nav_panel(
        title = "Citations",
        mod_citations_ui("citations")
      ),

      # Note: the footer belongs on navset_bar, not page_sidebar —
      # page_sidebar() has no `footer` argument, so passing one there is
      # silently swallowed as an HTML attribute and never renders.
      footer = tags$footer(
        "Powered by ",
        tags$a(href = "https://www.unsw.edu.au/science", target = "_blank",
               "UNSW Faculty of Science"),
        align = "right", style = "padding: 30px",

        div("Created by AusTraits Team",
            target)
      )
    )
  )
}
#' Badge naming the AusTraits release the portal serves
#'
#' @param release Release record (see `portal_release_info()`), or NULL.
#' @return A tag linking to the release's DOI, or NULL.
#' @noRd
portal_version_badge <- function(release) {
  if (is.null(release)) return(NULL)
  label <- sprintf("data: AusTraits %s", release$version)
  doi <- if (is.na(release$doi)) release$concept_doi else release$doi
  tags$a(
    class = "portal-data-version",
    href = paste0("https://doi.org/", doi), target = "_blank",
    title = sprintf("AusTraits %s, doi:%s", release$version, doi),
    style = "font-size: 0.6em; margin-left: 0.8em; color: inherit; opacity: 0.8; text-decoration: none; border: 1px solid currentColor; border-radius: 4px; padding: 1px 6px; vertical-align: middle;",
    label
  )
}
