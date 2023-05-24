(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("acmart" "manuscript" "review=True" "anonymous=True")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "acmart"
    "acmart10"
    "color"
    "tikz"
    "booktabs"
    "subcaption"
    "natbib")
   (TeX-add-symbols
    "BibTeX"
    "citepos"
    "citespos"
    "oldciteauthor"
    "citeauthor")
   (LaTeX-add-labels
    "sec:resource_dep"
    "sec:ecology_background"
    "sec:community_ecology"
    "tab:commensalism"
    "sec:var"
    "eq:var1"
    "sec:inferring"
    "eq:irf"
    "fig:simulation_coef"
    "sec:case.studies"
    "tab:case studies"
    "fig:seattle.coef"
    "fig:seattle.irf"
    "fig:seattle.network"
    "fig:var.seattle"
    "fig:wallpaper.coef"
    "fig:wallpaper.irf"
    "fig:wallpaper.network"
    "fig:var.wallpaper"
    "fig:design.coef"
    "fig:design.irf"
    "fig:design.network"
    "fig:var.design"
    "ap:model"
    "eq:bvar")
   (LaTeX-add-bibliographies
    "CHI_VAR")
   (LaTeX-add-xcolor-definecolors
    "c77a1d2"
    "cbf9837"
    "cc0c0c0"))
 :latex)

