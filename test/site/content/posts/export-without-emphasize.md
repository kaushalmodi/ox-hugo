+++
title = "Table with Org markup where the markup is ignored"
draft = false
+++

| QueryID | SQL Text                                                                                                                                                                                    | Query Time (Seconds) | Query Time Hot (Seconds) |
|---------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------|--------------------------|
|         |                                                                                                                                                                                             |                      |                          |
| Q0      | SELECT cab_type, count(\*) FROM trips_log GROUP BY cab_type;                                                                                                                                | 10.14                | 11.57                    |
| Q1      | SELECT passenger_count, avg(total_amount) FROM trips_log GROUP BY passenger_count;                                                                                                          | 12.00                | 6.27                     |
| Q2      | SELECT passenger_count, toYear(pickup_datetime) AS year, count(\*) FROM trips_log GROUP BY passenger_count, year;                                                                           | 10.45                | 7.23                     |
| Q3      | SELECT passenger_count, toYear(pickup_datetime) AS year, round(trip_distance) AS distance, count(\*) FROM trips_log GROUP BY passenger_count, year, distance ORDER BY year, count(\*) DESC; | 13.03                | 10.80                    |
