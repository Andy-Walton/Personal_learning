library(magick)
### Functions ----

create_population <- function(grid_size){
  
  values <- rbinom(grid_size^2, size = 1, prob = 0.5)
  
  grid <- matrix(values, nrow=grid_size, ncol=grid_size)
  
}

evaluate_neighbours <- function(grid, x, y, grid_size){
  
  x_over = x+1
  x_under = x-1
  y_over = y+1
  y_under = y-1
  
  if (y == grid_size){y_over=1} 
  if (y == 0){y_under=grid_size} 
  if (x == grid_size){x_over=1} 
  if (x == 0){x_under=grid_size} 
  
  neighbours <- sum(grid[c(x_under, x, x_over), y_under], 
                    grid[c(x_under, x, x_over), y_over], 
                    grid[c(x_under, x_over), y])

}

modify_cells_conway <- function(grid, grid_size){
    
  new_grid <- grid
  
  for (x in 1:nrow(grid)) {
    for (y in 1:ncol(grid)) {
      neighbours <- evaluate_neighbours(grid, x, y, grid_size)
      new_grid[x, y] <- ifelse(grid[x, y] == 1, ifelse(neighbours < 2 | neighbours > 3, 0, 1), ifelse(neighbours == 3, 1, 0))
    }
  }
  new_grid
}

modify_cells_day_night <- function(grid, grid_size){
  
  new_grid <- grid
  
  for (x in 1:nrow(grid)) {
    for (y in 1:ncol(grid)) {
      neighbours <- evaluate_neighbours(grid, x, y, grid_size)
      new_grid[x, y] <- ifelse(
        grid[x, y] == 1,
        ifelse(neighbours %in% c(3, 4, 6, 7, 8), 1, 0),
        ifelse(neighbours %in% c(3, 6, 7, 8), 1, 0)
      )
      
    }
  }
  new_grid
}

modify_cells_maze <- function(grid, grid_size){
  
  new_grid <- grid
  
  for (x in 1:nrow(grid)) {
    for (y in 1:ncol(grid)) {
      neighbours <- evaluate_neighbours(grid, x, y, grid_size)
      new_grid[x, y] <- ifelse(
        grid[x, y] == 1,
        ifelse(neighbours %in% 1:5, 1, 0),
        ifelse(neighbours == 3, 1, 0)
      )
    }
  }
  new_grid
}

simulate_cells_gif <- function(G, grid_size, rule = NULL){
  grid <- create_population(grid_size)
  imgs <- list()
  
  if (rule == "day_night"){
    for (g in 1:G){
      png_filename <- sprintf("frame%03d.png", g)
      png(png_filename, width=400, height=400)
      par(mar=c(0,0,0,0))
      image(t(apply(grid, 2, rev)), col=c("black", "white"), axes=FALSE)
      dev.off()
      
      imgs[[g]] <- image_read(png_filename)
      grid <- modify_cells_day_night(grid, grid_size)
    }
  } else if (rule == "maze"){
    for (g in 1:G){
      png_filename <- sprintf("frame%03d.png", g)
      png(png_filename, width=400, height=400)
      par(mar=c(0,0,0,0))
      image(t(apply(grid, 2, rev)), col=c("black", "white"), axes=FALSE)
      dev.off()
      
      imgs[[g]] <- image_read(png_filename)
      grid <- modify_cells_maze(grid, grid_size)
    }
    } else {
    for (g in 1:G){
      png_filename <- sprintf("frame%03d.png", g)
      png(png_filename, width=400, height=400)
      par(mar=c(0,0,0,0))
      image(t(apply(grid, 2, rev)), col=c("black", "white"), axes=FALSE)
      dev.off()
      
      imgs[[g]] <- image_read(png_filename)
      grid <- modify_cells_conway(grid, grid_size)
    }
    }

  animation <- image_animate(image_join(imgs), fps = 10)
  image_write(animation, "automaton.gif")
}

### Simulate ----

simulate_cells_gif(1000, 100, rule="day_night")
