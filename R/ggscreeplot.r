# 
#  ggscreeplot.r
#
#  Copyright 2011 Vincent Q. Vu.
# 
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# 

#' Screeplot for Principal Components
#'
#' @param pcobj          an object returned by prcomp() or princomp()
#' @param type           the type of scree plot.  'pev' corresponds proportion of explained variance, i.e. the eigenvalues divided by the trace. 'cev' corresponds to the cumulative proportion of explained variance, i.e. the partial sum of the first k eigenvalues divided by the trace.
#' @export
#' @examples
#'   data(wine)
#'   wine.pca <- prcomp(wine, scale. = TRUE)
#'   print(ggscreeplot(wine.pca))
#'
ggscreeplot <- function(pcobj, type = c('pev', 'cev'), line = FALSE) 
{
  type <- match.arg(type)
  d <- pcobj$sdev^2
  yvar <- switch(type, 
                 pev = d / sum(d), 
                 cev = cumsum(d) / sum(d))

  yvar.lab <- switch(type,
                     pev = 'Proportion of explained variance',
                     cev = 'Cumulative proportion of explained variance')

  df <- data.frame(PC = 1:length(d), yvar = yvar)

  g <- ggplot(data = df, aes(x = PC, y = yvar)) + 
    labs(x = 'Principal component number', y = yvar.lab) 

    if(line){
      g + geom_point() + geom_path()
    } else{
      g + geom_col()
    }
}
