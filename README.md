This R package generates Xmas-tree-plots.
These plots were invented by Jan Hattendorf in 2018 to plot 
highly dimensional data.
The plot is based on a scatter plot, but with an arrow 
in Z direction.
The arrowhead(s) can have up to four different colour codes 
to display the values of additional variables. 
The main functions are xmasTree() and doubleTree().

You can see an example plot here:

https://pmc.ncbi.nlm.nih.gov/articles/PMC6502312/figure/pntd.0007268.g005/

To install the package:

pak::pak("epi-stats/xmasTree")

or alternatively

devtools::install_github("epi-stats/xmasTree")
