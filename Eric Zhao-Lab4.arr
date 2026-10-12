use context dcic2024
include csv
include data-source

#Exercise 1 --- Long Flights
#| Column Identifiers:
   rownames,dep_time,sched_dep_time,dep_delay,arr_time,sched_arr_time,arr_delay,carrier,flight,tailnum,origin,dest,air_time,distance,hour,minute,time_hour

   Example row:
   1,517,515,2,830,819,11,UA,1545,N14228,EWR,IAH,227,1400,5,15,2013-01-01T10:00:00Z
|#

#Step 1: Load the Flights Table
flights = load-table:
  rownames :: Number, 
  dep_time :: Number, 
  sched_dep_time :: Number, 
  dep_delay :: Number, 
  arr_time :: Number, 
  sched_arr_time :: Number, 
  arr_delay :: Number, 
  carrier :: String, # 8th row
  flight :: String, #9th Row
  tailnum :: String, #10th Row
  origin :: String, #11th Row
  dest :: String, #12th Row
  air_time :: Number, 
  distance :: Number, 
  hour :: Number, 
  minute :: Number, 
  time_hour :: String #Last one
  #took notes of which parts is a string
  
  #below is to load the table in
  source: csv-table-file("flights.csv", default-options) 
  sanitize rownames using num-sanitizer 
  sanitize dep_time using num-sanitizer 
  sanitize sched_dep_time using num-sanitizer 
  sanitize dep_delay using num-sanitizer 
  sanitize arr_time using num-sanitizer 
  sanitize sched_arr_time using num-sanitizer 
  sanitize arr_delay using num-sanitizer 
  sanitize carrier using string-sanitizer #String
  sanitize flight using string-sanitizer #String
  sanitize tailnum using string-sanitizer #String
  sanitize origin using string-sanitizer #String
  sanitize dest using string-sanitizer #String
  sanitize air_time using num-sanitizer
  sanitize distance using num-sanitizer
  sanitize hour using num-sanitizer
  sanitize minute using num-sanitizer
  sanitize time_hour using string-sanitizer #String
end
#Testing:
flights

#Step 2: Check if the distance is ≥ 1500
fun is_long_flight(r :: Row) -> Boolean:
  doc: "determines if the distance given exceeds or equates to 1500"
  r["distance"] >= 1500
 where:
  is_long_flight(flights.row-n(2)) is false
  is_long_flight(flights.row-n(3)) is true
end
#Step 3: Filter out all of the flights except the long flights
new_flights = filter-with(flights, is_long_flight)
#Testing
new_flights

#Step 4: Order the resulting table by "air_time"
ordered_new_flights = new_flights.order-by("air_time", false)
#Testing
ordered_new_flights

#Step 5: Extract the carrier, origin, and dest of the first row.
#Carrier:
ordered_new_flights.row-n(0)["carrier"]
#Origin:
ordered_new_flights.row-n(0)["origin"]
#Destination:
ordered_new_flights.row-n(0)["dest"]



#Exercise 2
#We want to find flights that were delayed by at least 30 minutes, scheduled to depart in the morning, and then identify the worst offender.

#Step 1: Check if dep_delay ≥ 30
fun is_delayed_departure(r :: Row) -> Boolean:
  doc: "check if the departure has been delayed by atleast 30 minutes"
  r["dep_delay"] >= 30
where:
  is_delayed_departure(flights.row-n(2)) is false
  is_delayed_departure(flights.row-n(9)) is false
  is_delayed_departure(flights.row-n(56)) is true
end

#Step 2: Check if the scheduled departure is before noon
fun is_morning_sched_dep(r :: Row) -> Boolean:
  doc: "Check if the scheduled departure is before noon"
  r["sched_dep_time"] < 1200
where: 
  is_morning_sched_dep(flights.row-n(430)) is false
  is_morning_sched_dep(flights.row-n(10)) is true
end

#Step 3: Use a lambda with filter-with to keep flights with delayed departures (≥ 30). Chain another lambda filter to keep only flights scheduled before noon.
delayed-morning = filter-with(
  filter-with(flights,lam(r): r["dep_delay"] >= 30 end),
  lam(r): r["sched_dep_time"] < 1200 end)
#Testing:
delayed-morning

#Step 4: From that subset, filter again to keep only flights where distance > 500
delayed-morning-500 = filter-with(delayed-morning, lam(r): r["distance"] > 500 end)
#Testing:
delayed-morning-500

#Step 5: Order by dep_delay descending (worst delays first).
ordered-delays = delayed-morning-500.order-by("dep_delay", false)
#Testing:
ordered-delays

#Step 6: Extract the flight number, origin, and dep_delay of the single worst delayed flight in that subset.
#Flight Number:
ordered-delays.row-n(0)["flight"]
#Origin
ordered-delays.row-n(0)["origin"]
#Departure Delay
ordered-delays.row-n(0)["dep_delay"]



#Exercise 3: 
#Practise using transform-column with a lambda, then build-column to derive a new quantitative column.'

#Step 1: Some flights have negative delays (meaning they left early). Replace those with 0.
#First, we transform departure delay
# Transform dep_delay
zero-dep = transform-column(flights, "dep_delay",
  lam(d):
    if d < 0: 0 else: d end
  end)

# Transform arr_delay (using the table from the previous step)
zero-delay = transform-column(zero-dep, "arr_delay",
  lam(a):
    if a < 0: 0 else: a end
  end)

# Testing:
zero-delay
# Step 2: Add a new column "effective_speed" in miles per hour:
# Formula: distance / (air_time / 60) (only when air_time > 0 otherwise, set it to 0)
speed-flights = build-column(zero-delay, "effective_speed",
  lam(r):
    if r["air_time"] > 0:
      r["distance"] / (r["air_time"] / 60)
    else:
      0
    end
  end)

#Step 3:Order by "effective_speed" descending
speed-flights-ordered = speed-flights.order-by("effective_speed", false)
# Testing:
speed-flights-ordered

# Step 4: Extract the carrier, origin, and dest of the fastest flight
# Carrier:
speed-flights-ordered.row-n(0)["carrier"]
# Origin:
speed-flights-ordered.row-n(0)["origin"]
# Destination:
speed-flights-ordered.row-n(0)["dest"]



# Exercise 4:
#Discount Late Arrivals + On-Time Score

# Step 1: Define a function apply-arrival-discount(t :: Table) -> Table
# that reduces arr_delay by 20% only when: 0 <= arr_delay <= 45
fun apply-arrival-discount(t :: Table) -> Table:
  doc: "Reduces arr_delay by 20% only when 0 <= arr_delay <= 45; otherwise leaves value unchanged"
  transform-column(t, "arr_delay",
    lam(d):
      if (d >= 0) and (d <= 45):
        d * 0.8
      else:
        d
      end
    end)
  where:
    # Small test table with delays: -10, 0, 30, 60
    test-input = table:
      arr_delay
  row: -10
  row: 0
  row: 30
  row: 60
end
  apply-arrival-discount(test-input).row-n(0)["arr_delay"] is -10
  apply-arrival-discount(test-input).row-n(1)["arr_delay"] is 0
  apply-arrival-discount(test-input).row-n(2)["arr_delay"] is 24
  apply-arrival-discount(test-input).row-n(3)["arr_delay"] is 60
  end

#Step 2: Apply this function to the flights table
flights-discounted = apply-arrival-discount(flights)
# Testing:
flights-discounted

# Step 3: Add a new column "on_time_score" with this scoring idea:
# score = 100 - (dep_delay - arr_delay) - (air_time / 30)
# If dep_delay or arr_delay are less than 0, set them to 0 first.
# If the score is less than 0, set it to 0.
scored-flights = build-column(flights-discounted, "on_time_score",
  lam(r):
    # Step 1: Clamp dep_delay and arr_delay to be at least 0
    safe-dep = if r["dep_delay"] < 0: 0 else: r["dep_delay"] end
    safe-arr = if r["arr_delay"] < 0: 0 else: r["arr_delay"] end
    
    # Step 2: Calculate the base score
    raw-score = 100 - (safe-dep - safe-arr) - (r["air_time"] / 30)
    
    # Step 3: Clamp the final score to be at least 0
    if raw-score < 0:
      0
    else:
      raw-score
    end
  end)
# Testing:
scored-flights

# Step 4: Order by "on_time_score" descending, then by "distance" ascending to break ties
# Note: chain order-by with secondary sort first, then primary sort
scored-ordered = scored-flights.order-by("distance", true).order-by("on_time_score", false)
# Testing:
scored-ordered

# Step 5: Extract the carrier, flight, origin, and dest of the top two flights

#Top flight (row 0) 
# Carrier:
scored-ordered.row-n(0)["carrier"]
# Flight:
scored-ordered.row-n(0)["flight"]
# Origin:
scored-ordered.row-n(0)["origin"]
# Destination:
scored-ordered.row-n(0)["dest"]

#Second flight (row 1)
# Carrier:
scored-ordered.row-n(1)["carrier"]
# Flight:
scored-ordered.row-n(1)["flight"]
# Origin:
scored-ordered.row-n(1)["origin"]
# Destination:
scored-ordered.row-n(1)["dest"]