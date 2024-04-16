get_times <- function(name.models, i){

  if (name.models$time_res[i]=="annual"){
    years <- sort(seq(from=name.models$start_year[i], to=name.models$end_year[i], by=1))
    time <- c(1:length(years))
    times <- data.frame(cbind(time, years)) %>% 
      mutate(time = as.character(time))
  } else {
    years <- sort(rep(seq(from=name.models$start_year[i], to=name.models$end_year[i], by=1), times=12))
    months <- rep(seq(from=1, to=12, by=1), times = (name.models$end_year[i]-name.models$start_year[i]+1))
    time <- c(1:length(months))
    times <- data.frame(cbind(time, years, months)) %>% 
      mutate(time = as.character(time))
  }
  
  return(times)
}


get_times_combined <- function(name.models, i, start_year = NA){
  
  if (name.models$time_res[i]=="annual"){
    years <- sort(seq(from=start_year, to=name.models$end_year[i], by=1))
    time <- c(1:length(years))
    times <- data.frame(cbind(time, years)) %>% 
      mutate(time = as.character(time))
  } else {
    years <- sort(rep(seq(from=start_year, to=name.models$end_year[i], by=1), times=12))
    months <- rep(seq(from=1, to=12, by=1), times = (name.models$end_year[i]-start_year+1))
    time <- c(1:length(months))
    times <- data.frame(cbind(time, years, months)) %>% 
      mutate(time = as.character(time))
  }
  
  return(times)
}
