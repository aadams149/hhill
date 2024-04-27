hhill <- function(names, populations, seats, min = 1, exclude = c()){
  #' Huntington-Hill method of legislative seat allocation
  #'
  #' `hhill()` implements the Huntington-Hill method of legislative
  #'  seat allocation which is currently used by the United States House of Representatives.
  #'  The Huntington-Hill method is calculated as follows: each entity represented in the chamber
  #'  is assigned a score, calculated as the population of that entity divided by the square root
  #'  of *n*(*n*+1), where *n* is the current number of seats allocated to that entity. All entities are assigned
  #'  the minimum number of seats, and then scores are calculated. The entity with the highest score receives a seat,
  #'  its *n* value increments by 1, and then scores are recalculated. The new entity with the highest score receives a seat,
  #'  increments its *n*, and the process repeats until all available seats have been allocated.
  #'
  #' @param names A vector of names of entities to be allocated seats. All values in this vector must be unique.
  #' @param populations A vector of populations to be used in calculating Huntington-Hill scores. This should be ordered to align with the
  #' vector of names. Ideally, these two arguments are drawn from the same data frame.
  #' @param seats The total number of seats to be allocated in the political body.
  #' @param min The minimum number of seats allocated to each entity. Default is 1, the minimum number of seats for each state in the
  #' House of Representatives.
  #' @param exclude A vector of entities to exclude from consideration when allocating seats.
  #' @returns A list object containing the following:
  #'
  #'
  #' * `seat_order`, data frame of length seats-nrow(names), showing the order in which seats were allocated. Includes the Huntington-Hill score,
  #' the entity which received the seat, which number seat that is for the entity, and which number seat that is for the chamber.
  #'
  #'
  #' * `min_seat`, a vector of entity names which were allocated the minimum number of seats as defined in the function call.
  #'
  #'
  #' * `final_seats`, a data frame showing the total number of seats allocated to each entity.

  if(is.numeric(populations) == FALSE){
    tryCatch(
        populations = as.numeric(populations),
        error = function(e){
          message('An error occurred:\n', e)
        })}
      if(length(names) != length(populations)){
        errorCondition('Error: argument `names` must be the same length as argument `populations`')
      }
      else{
      #Bind the names and populations into a data frame
      df = cbind.data.frame(names, populations)
      #Drop DC since it's not a state
      df = df %>% filter(!(names %in% exclude))
      #Instantiate the seat count column
      df$seatcount = min
      #Calculate each state's initial seat score
      df$seatscore = df$populations/(sqrt(df$seatcount*(df$seatcount+1)))
      df$seat_number = 1:nrow(df)
      #Figure out how many seats are left to be allocated
      seats = seats - (nrow(df)*min)
      #Copy the dataframe for processing purposes
      df1 = df
      #New dataframe
      newdf = data.frame()
      #Loop 1: calculate which state gets which seat in which order
      for (i in 1:seats){
        maxrow = df1 %>%
          dplyr::filter(seatscore == max(seatscore))
        maxrow$seat_number = i+nrow(df)
        maxrow$seatcount = maxrow$seatcount+1
        others = df1 %>%
          dplyr::filter(seatscore != max(seatscore))
        newdf = rbind.data.frame(newdf, maxrow)
        df1 = rbind.data.frame(maxrow, others)
        df1$seatscore = df1$populations/(sqrt(df1$seatcount*(df1$seatcount+1)))
      }
      newdf = rbind.data.frame(df, newdf)
      #New dataframe
      newdf1 = data.frame()
      #Loop 2: Pull out the rows with the total seats allocated to each state
      for(i in unique(newdf$names)){
        state = df1 %>%
          dplyr::filter(names == i)
        toprow = state %>%
          filter(seatcount == max(seatcount))
        newdf1 = rbind.data.frame(newdf1, toprow)
      }
      #Create a vector of all states which did not receive more than 1 seat
      min_seat = newdf1 %>% filter(seatcount == min) %>% select(names)
      #Add all items to list
      #seat_order = data frame of length seats-nrow(names), showing the order in which seats were allocated
      #one_seat = character vector of states which only get 1 seat
      #final_seats = data frame of length nrow(names), showing how many seats each state gets
      item_list = list(seat_order = newdf, min_seat = list(min_seat), final_seats = newdf1)
      item_list}
  }
