% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def_theme.r
\docType{class}
\name{theme}
\alias{theme}
\title{Theme objects}
\description{
The \pkg{stoRy} package uses the \code{theme} object to store
information about themes in the package theme dataset.
}
\examples{
########################################################################
# Create a theme object for the theme "utopia" and print the result    #
########################################################################
theme_name <- "utopia"
mytheme <- theme$new(theme_name)
mytheme$print()

########################################################################
# Display the "utopia" theme and its descendent themes in tree format  #
########################################################################
print_tree(mytheme)

########################################################################
# Display the "society" theme hierarchy in tree format                 #
########################################################################
theme_name <- "society"
mytheme <- theme$new(theme_name)
print_tree(mytheme, pruneMethod = "dist", limit = 50)
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{theme$new()}}
\item \href{#method-print}{\code{theme$print()}}
\item \href{#method-clone}{\code{theme$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{theme$new(theme)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{theme}}{A string representing a theme in the dataset.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{theme$print(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{theme$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
