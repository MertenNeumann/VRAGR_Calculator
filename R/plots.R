# Presentation only: the selected plot never changes the scoring or CSV data.
vragr_normal_view <- function(percentile, limit=3.5) {
  if(length(percentile)!=1L || !is.finite(percentile) || percentile<0 || percentile>100)
    stop("Percentile must be between 0 and 100.",call.=FALSE)
  # Avoid infinite qnorm endpoints in plot geometry. The caption explains
  # that 100 is represented at the edge of the displayed normal curve.
  marker <- if(percentile==0) -limit else if(percentile==100) limit else stats::qnorm(percentile/100)
  x <- sort(unique(c(seq(-limit,limit,length.out=1001),marker)))
  curve <- data.frame(Z=x,Density=stats::dnorm(x))
  list(curve=curve,shaded=curve[curve$Z<=marker,,drop=FALSE],marker=marker)
}
vragr_result_plot <- function(score,type="empirical",language="en") {
  tx <- function(text) translate(text,language)
  if(!type %in% c("empirical","normal")) stop("Unknown plot type.",call.=FALSE)
  if(type=="empirical") {
    plot <- ggplot2::ggplot(data.frame(Score=-34:46,Percentile=vragr_percentiles),ggplot2::aes(Score,Percentile))+
      ggplot2::geom_step(colour="#60666c")+
      ggplot2::geom_vline(xintercept=score$Score,colour="#b22222",linetype=2)+
      ggplot2::geom_point(data=data.frame(Score=score$Score,Percentile=score$Percentile),colour="#337ab7",size=3)+
      ggplot2::scale_y_continuous(limits=c(0,100))+
      ggplot2::labs(x=tx("VRAG-R score"),y=tx("Percentile rank"))
  } else {
    view <- vragr_normal_view(score$Percentile)
    plot <- ggplot2::ggplot(view$curve,ggplot2::aes(Z,Density))+
      ggplot2::geom_area(data=view$shaded,fill="#d3d3d3")+
      ggplot2::geom_line(colour="#60666c",linewidth=.7)+
      ggplot2::annotate("segment",x=view$marker,xend=view$marker,y=0,yend=stats::dnorm(view$marker),colour="#b22222",linewidth=.8)+
      ggplot2::labs(x=tx("Standard normal score (z)"),y=tx("Density"),
        subtitle=paste0(tx("VRAG-R score"),": ",score$Score," · ",tx("Percentile rank"),": ",format_number(score$Percentile,language,1)))
  }
  plot+ggplot2::theme_minimal(base_size=13)
}
