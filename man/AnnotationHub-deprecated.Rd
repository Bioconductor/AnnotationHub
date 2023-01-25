\name{AnnotationHub-deprecated}

\alias{display}
\alias{display,Hub-method}
\alias{AnnotationHub-deprecated}

\title{Deprecated method for Hub Objects}

\description{
    The \code{display} method for Hub objects is deprecated and now defunct.
    It is recommended to use the \code{BiocHubsShiny} package to visually
    explore either \code{AnnotationHub} or \code{ExperimentHub} resources.
}

\usage{
display(object, ...)
}

\arguments{
    \item{object}{A Hub object, either from \code{AnnotationHub} or
        \code{ExperimentHub}
    }
    \item{...}{arguments passed on to other functions, not used.}
}

\section{Related operations}{
    \describe{
        \item{}{
            \code{display(object)}:
            Open a web browser allowing for easy selection of hub records via
            interactive tabular display. Return value is the subset of hub
            records identified while navigating the display. This method is
            defunct.
        }
    }
}

\examples{
if(interactive()) {
    ## Display method involves user interaction through web interface
    ah <- AnnotationHub()
    display(ah)
}
}
