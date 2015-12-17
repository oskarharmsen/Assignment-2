library(ggplot2)

plot <- rbind( c(1, 4, 5), c(1, 3, 3)
  )
plot <- t(plot)

plot <- as.data.frame(plot)

rownames(plot) <- c("A", "B", "C")
plot$names <- rownames(plot)


p <- ggplot(data = plot, aes( x = V1, y = V2)) +
      geom_point( shape = 21, color = "black", fill = "blue", size = 10) +
      geom_text(aes(label = names),hjust=0, vjust=-2) +
      ylim(0,4) +
      xlim(0,6) +
      xlab( "Policy dimension 1") +
      ylab( "Policy dimension 2") +
      theme_minimal()
p