library(dplyr)
library(magrittr)
library(purrr)

hhill <- function(names,
                  populations,
                  seats,
                  min = 1,
                  exclude = c(),
                  quiet = TRUE,
                  detailed = TRUE) {
  #' Huntington-Hill method of legislative seat allocation
  #'
  #' `hhill()` implements the Huntington-Hill method of legislative
  #'  seat allocation which is currently used by the
  #'  United States House of Representatives.
  #'  The Huntington-Hill method is calculated as follows:
  #'  each entity represented in the chamber
  #'  is assigned a score, calculated as the population of that
  #'  entity divided by the square root
  #'  of  \emph{n}(\emph{n}+1), where \emph{n} is the current number of seats
  #'  allocated to that entity. All entities are assigned
  #'  the minimum number of seats, and then scores are calculated.
  #'  The entity with the highest score receives a seat,
  #'  its \emph{n} value increments by 1, and then scores are recalculated.
  #'  The new entity with the highest score receives a seat,
  #'  increments its \emph{n}, and the process repeats until all available
  #'  seats have been allocated.
  #'
  #' @param names A vector of names of entities to be allocated seats.
  #' All values in this vector must be unique.
  #' @param populations A vector of populations to be used in calculating
  #' Huntington-Hill scores. This should be ordered to align with the
  #' vector of names. Ideally, these two arguments are drawn from the same
  #' data frame.
  #' @param seats The total number of seats to be allocated in the
  #' political body.
  #' @param min The minimum number of seats allocated to each entity.
  #' Default is 1, the minimum number of seats for each state in the
  #' House of Representatives.
  #' @param exclude A vector of entities to exclude from consideration
  #' when allocating seats.
  #' @param quiet Boolean for printing a progress message. Defaults to TRUE.
  #' @param detailed Boolean for returning a list which includes a
  #' data frame of every seat in the order it was allocated, instead of
  #' just the totals. Defaults to TRUE.
  #' @returns A list object containing the following:
  #'
  #'
  #' * `seat_order`, data frame of length seats-nrow(names), showing the
  #' order in which seats were allocated.
  #' Includes the Huntington-Hill score,
  #' the entity which received the seat, which number seat that is for the
  #' entity, and which number seat that is for the chamber.
  #' (Only if `detailed` == TRUE).
  #'
  #'
  #' * `min_seat`, a vector of entity names which were allocated the minimum
  #' number of seats as defined in the function call.
  #'
  #'
  #' * `final_seats`, a data frame showing the total number of seats
  #' allocated to each entity.

  if (is.numeric(populations) == FALSE) {
    tryCatch(
             populations = as.numeric(populations),
             error = function(e) {
               message("An error occurred:\n", e)
             })
  }
  if (length(names) != length(populations)) {
    errorCondition(
                   "Error: argument `names` must be the same
                   length as argument `populations`")
  }else {
    #Bind the names and populations into a data frame
    df <- cbind.data.frame(names, populations)
    #Drop DC since it's not a state
    df <- df %>% filter(!(names %in% exclude))
    #Instantiate the seat count column
    #df$seatcount <- 1

    df <-
      tidyr::uncount(df,
                     weights = min,
                     .id = "seatcount")
    #Calculate each state's initial seat score
    df$seatscore <-
      df$populations / (sqrt(df$seatcount * (df$seatcount + 1)))

    df$seat_number <- seq_len(nrow(df))
    #Figure out how many seats are left to be allocated
    seats_left <- seats - nrow(df)
    #Copy the dataframe for processing purposes
    df1 <- df %>%
      group_by(names,
               .drop = FALSE) %>%
      filter(seatcount == max(seatcount)) %>%
      ungroup()
    #New dataframe
    newdf <- data.frame()
    #Loop 1: calculate which state gets which seat in which order
    for (i in 1:seats_left) {
      if (quiet == FALSE) {
        print(paste0("Now assigning seat ",
                     i + nrow(df),
                     " of ",
                     seats))
      }
      maxrow <- df1 %>%

        dplyr::filter(seatscore == max(seatscore))
      maxrow1 <- maxrow # Duplicate row
      maxrow1$seat_number <- max(df1$seat_number) + 1 # increment seat #
      maxrow1$seatcount <- maxrow1$seatcount + 1 # increment seat count
      others <- df1 %>%
        dplyr::anti_join(maxrow,
                         by = c(colnames(maxrow)))
      newdf <- rbind.data.frame(newdf, maxrow1)
      df1 <- rbind.data.frame(maxrow1,
                              #maxrow,
                              others)
      df1$seatscore <-
        df1$populations / (sqrt(df1$seatcount * (df1$seatcount + 1)))
    }
    newdf <- rbind.data.frame(df, newdf)
    #New dataframe
    newdf1 <- data.frame()
    #Loop 2: Pull out the rows with the total seats allocated to each state
    for (i in unique(newdf$names)) {
      state <- df1 %>%
        dplyr::filter(names == i)
      toprow <- state %>%
        filter(seatcount == max(seatcount))
      newdf1 <- rbind.data.frame(newdf1, toprow)
    }
    #Create a vector of all states which did not receive more than 1 seat
    min_seat <-
      newdf1 %>%
      filter(seatcount == min(seatcount)) %>%
      select(names)
    #Add all items to list
    #seat_order = data frame of length seats-nrow(names),
    #             showing the order in which seats were allocated
    #one_seat = character vector of states which only get 1 seat
    #final_seats = data frame of length nrow(names), showing how many
    #              seats each state gets
    item_list <- list(final_seats = newdf1,
                      min_seat = list(min_seat)
                      )

    if (detailed == TRUE) {

      item_list$seat_order <-
        newdf
    }
    item_list
  }
}
