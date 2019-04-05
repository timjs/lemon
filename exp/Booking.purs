module Booking where


import Preload

import Data.Task (Check, Store, Task, done, enter, only, select, store, view, watch, (-&&-), (<<-))

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
  chair <- 'A' .. 'D'
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
  { passengers } <- enter "Passenger details" {}
  only
    { on: "Continue"
    , when: all valid passengers && any adult passengers && not (null passengers)
    , then: done { passengers }
    }


choose_seats :: { amount :: Int } -> Task { seats :: Array Seat }
-- choose_seats :: forall a b. Eq b => Semiring b
--   => { amount :: b | a } -> Task { seats :: Array Seat }
choose_seats { amount } = do
  { values: seats } <- select "Pick a Seat" [] free_seat_store {}
  only
    { on: "Continue"
    , when: length seats == amount
    , then: do
        free_seat_store <<- Array.difference seats
        done { seats }
    }


make_booking :: {} -> Task { booking :: Booking }
make_booking {} = do
  { flight, passengers } <- (do
    { flight } <- enter "Flight details" {}
    only
      { on: "Continue"
      , when: true
      , then: done { flight }
      }
    ) -&&-
    enter_passengers {}
  { seats } <- choose_seats { amount: length passengers }
  view "Booking" { booking: Booking { passengers, flight, seats } }


run :: {} -> Task { booking :: Booking }
run {} = do
  { booking } <-
    watch "Free seats" free_seat_store {}
      -&&-
    make_booking {}
  done { booking }



-- Boilerplate -----------------------------------------------------------------

derive instance eqSeat :: Eq Seat
