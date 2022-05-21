# Timezone Abbreviations

Timezone abbreviations are, in general[^1], not standardized.

Furthermore, [they can be ambiguous](https://stackoverflow.com/tags/timezone/info):

> For example, there are five different interpretations of CST:
>
>   * Central Standard Time (North America)
>   * Central Standard Time (Australia)
>   * Central Summer Time (Australia)
>   * China Standard Time
>   * Cuba Standard Time

For these reasons, it's advised that timezone abbreviations should not be parsed.

> In general, if an abbreviation is used, it should be for display purposes.

However, the use of timezone abbreviations in everyday conversations is undeniable.
We decided to support a subset of [the timezone abbreviations listed in wikipedia][tz-list],
duly documented in the table below for disambiguation.
More may be supported in the future.

If a user sends a message with a reference to a timezone abbreviation that we don't
support / doesn't exist, the bot notifies them of this.

| Abbreviation | Name                                     | UTC offset |
|--------------|------------------------------------------|------------|
| UTC          | UTC                                      | UTC+00:00  |
| GMT          | GMT                                      | UTC+00:00  |
| HST          | Hawaii-Aleutian Standard Time            | UTC-10:00  |
| HDT          | Hawaii-Aleutian Daylight Time            | UTC-09:00  |
| PST          | Pacific Standard Time (North America)    | UTC-08:00  |
| PDT          | Pacific Daylight Time (North America)    | UTC-07:00  |
| MST          | Mountain Standard Time (North America)   | UTC-07:00  |
| MDT          | Mountain Daylight Time (North America)   | UTC-06:00  |
| CST          | Central Standard Time (North America)    | UTC-06:00  |
| CDT          | Central Daylight Time (North America)    | UTC-05:00  |
| EST          | Eastern Standard Time (North America)    | UTC-05:00  |
| EDT          | Eastern Daylight Time (North America)    | UTC-04:00  |
| AST          | Atlantic Standard Time                   | UTC-04:00  |
| ADT          | Atlantic Daylight Time                   | UTC-03:00  |
| AMT          | Amazon Time                              | UTC-04:00  |
| AMST         | Amazon Summer Time                       | UTC-03:00  |
| CLT          | Chile Standard Time                      | UTC-04:00  |
| CLST         | Chile Summer Time                        | UTC-03:00  |
| BRT          | Brasília Time                            | UTC-03:00  |
| BRST         | Brasília Summer Time                     | UTC-02:00  |
| WET          | Western European Time                    | UTC+00:00  |
| WEST         | Western European Summer Time             | UTC+01:00  |
| BST          | British Summer Time                      | UTC+01:00  |
| CET          | Central European Time                    | UTC+01:00  |
| CEST         | Central European Summer Time             | UTC+02:00  |
| WAT          | West Africa Time                         | UTC+01:00  |
| WAST         | West Africa Summer Time                  | UTC+02:00  |
| CAT          | Central Africa Time                      | UTC+02:00  |
| SAST         | South African Standard Time              | UTC+02:00  |
| EET          | Eastern European Time                    | UTC+02:00  |
| EEST         | Eastern European Summer Time             | UTC+03:00  |
| MSK          | Moscow Time                              | UTC+03:00  |
| TRT          | Turkey Time                              | UTC+03:00  |
| GET          | Georgia Standard Time                    | UTC+04:00  |
| IST          | India Standard Time                      | UTC+05:30  |
| AWST         | Australian Western Standard Time         | UTC+08:00  |
| AWDT         | Australian Western Daylight Time         | UTC+09:00  |
| ACWST        | Australian Central Western Standard Time | UTC+08:45  |
| JST          | Japan Standard Time                      | UTC+09:00  |
| KST          | Korea Standard Time                      | UTC+09:00  |
| ACST         | Australian Central Standard Time         | UTC+09:30  |
| ACDT         | Australian Central Daylight Time         | UTC+10:30  |
| AEST         | Australian Eastern Standard Time         | UTC+10:00  |
| AEDT         | Australian Eastern Daylight Time         | UTC+11:00  |
| NZST         | New Zealand Standard Time                | UTC+12:00  |
| NZDT         | New Zealand Daylight Time                | UTC+13:00  |


 [^1]: The notable exception being the 11 timezone abbreviations listed
       in [RFC 822, section 5.1](https://datatracker.ietf.org/doc/html/rfc822#section-5.1).

 [tz-list]: https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
