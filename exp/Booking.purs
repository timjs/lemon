module Booking where


import Preload

import Data.Array ((..))
import Data.Task (Check, Store, Task, enter, only, select, store, view, watch, (-&&-), (<<-))

import Data.Array as Array



-- Data ------------------------------------------------------------------------


data Nationality
  = Dutch
  | British
  | German


data Passenger = Passenger
  { first_name :: String
  , last_name :: String
  , nationality :: Nationality
  , age :: Int
  }


data Flight
  = ToAmsterdam
  | ToLondon
  | ToBerlin


type Row = Int
type Chair = Char
data Seat = Seat
  { row :: Row
  , chair :: Chair
  }


data Booking = Booking
  { passengers :: Array Passenger
  , flight :: Flight
  , seats :: Array Seat
  }



-- Stores ----------------------------------------------------------------------


free_seat_store :: Store (Array Seat)
free_seat_store = store ado
  row <- 1 .. 4
  chair <- ['A', 'B', 'C', 'D']
  in Seat { row, chair }



-- Checks ----------------------------------------------------------------------


valid :: Check Passenger
valid (Passenger p) =
  p.age >= 0


adult :: Check Passenger
adult (Passenger p) =
  p.age >= 18



-- Tasks -----------------------------------------------------------------------


enter_passengers :: {} -> Task { passengers :: Array Passenger }
enter_passengers {} = do
  { value: passengers } <- enter "Passenger details" {}
  only
    { on: "Continue"
    , when: all valid passengers && any adult passengers && not (null passengers)
    , cont: pure { passengers }
    }


enter_flight :: {} -> Task { flight :: Flight }
enter_flight {} = do
  { value: flight } <- enter "Flight details" {}
  only
    { on: "Continue"
    , when: true
    , cont: pure { flight }
    }


choose_seats :: { amount :: Int } -> Task { seats :: Array Seat }
choose_seats { amount } = do
  { value: seats } <- select "Pick a Seat" [] free_seat_store {}
  only
    { on: "Continue"
    , when: length seats == amount
    , cont: do
        free_seat_store <<- Array.difference seats
        pure { seats }
    }


make_booking :: {} -> Task { booking :: Booking }
make_booking {} = do
  { flight, passengers } <-
    enter_flight {}
      -&&-
    enter_passengers {}
  { seats } <- choose_seats { amount: length passengers }
  view "Booking" { booking: Booking { passengers, flight, seats } }


run :: {} -> Task { booking :: Booking }
run {} = do
  { booking } <-
    watch "Free seats" free_seat_store {}
      -&&-
    make_booking {}
  pure { booking }



-- Boilerplate -----------------------------------------------------------------

derive instance eqSeat :: Eq Seat
