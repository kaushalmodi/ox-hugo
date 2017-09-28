+++
title = "Table with Org markup where the markup is ignored"
draft = false
+++

| QueryID | SQL Text                                                                                                                                                                                         | Query Time (Seconds) | Query Time Hot (Seconds) |
|---------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------|--------------------------|
|         |                                                                                                                                                                                                  |                      |                          |
| Q0      | SELECT cab\_type, count(\*) FROM trips\_log GROUP BY cab\_type;                                                                                                                                  | 10.14                | 11.57                    |
| Q1      | SELECT passenger\_count, avg(total\_amount) FROM trips\_log GROUP BY passenger\_count;                                                                                                           | 12.00                | 6.27                     |
| Q2      | SELECT passenger\_count, toYear(pickup\_datetime) AS year, count(\*) FROM trips\_log GROUP BY passenger\_count, year;                                                                            | 10.45                | 7.23                     |
| Q3      | SELECT passenger\_count, toYear(pickup\_datetime) AS year, round(trip\_distance) AS distance, count(\*) FROM trips\_log GROUP BY passenger\_count, year, distance ORDER BY year, count(\*) DESC; | 13.03                | 10.80                    |
