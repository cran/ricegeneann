% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ricegeneannotation.R
\name{ricegeneannotation}
\alias{ricegeneannotation}
\title{Annotation of genes of rice (Oryza Sativa)}
\usage{
ricegeneannotation(myID, type)
}
\arguments{
\item{myID}{A vector of entrez id}

\item{type}{Keytype of input id}
}
\value{
Return a vector or a datafram
}
\description{
\code{ricegeneann} Annotate the gene of rice (Oryza Sativa)
}
\examples{
convert_id <- ricegeneannotation('Os01g0100500','RAP')
convert_id <- ricegeneannotation(myID = 'Os01g0100500',
                            type = 'RAP')
}
\author{
Xiang LI <ynaulx@gmail.com> Linna MA <973649882@qq.com>
}
