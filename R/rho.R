## Wrapper for cor(method="spearman")
##
## Mark Cowley, 9 Nov 2005
##
rho <- function(x, y=NULL, use="all.obs") {
    cor(x=x, y=y, use=use, method="spearman")
}
