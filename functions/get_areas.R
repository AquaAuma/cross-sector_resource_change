grid_cells_areas <- function(var_array) {
  
  library(raster)
  
  if(length(dim(var_array))==3){
    # make grid from var_array data
    xx <- raster(t(var_array[,,1]), crs = 4326, 
                 xmn = -180, xmx = 180, ymn = -90, ymx = 90)
    area_xx <- area(xx)
  }
  if(length(dim(var_array))==4){
    # make grid from var_array data
    xx <- raster(t(var_array[,,1,1]), crs = 4326, 
                 xmn = -180, xmx = 180, ymn = -90, ymx = 90)
    area_xx <- area(xx)
  }
  
  
  return(area_xx)
}