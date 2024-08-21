This project uses local data on temperatures and humidity levels across the city of Charlottesville, VA to explore how afternoon surface temperatures vary across neighborhoods and census tracts, and the correlation between afternoon surface temperature and household income.

Temperature data is made available by the 2021 NIHHIS-CAPA Urban Heat Island Mapping Campaign: https://www.charlottesville.gov/1469/Urban-Heat-Island-Mapping-Campaign

Household income data is retrieved from the US Census American Community survey using the tidycensus package: https://walker-data.com/tidycensus/
You will need a Census API key in order to run the get_acs functions which can be obtained from http://api.census.gov/data/key_signup.html.

Results of this exploration include a (weak) negative correlation between median household income and afternoon temperatures. 

![Rplot01](https://github.com/user-attachments/assets/e94e9106-9fb4-4bb7-a81c-146271afbb21)


A map of temperatures by neighborhood suggest Fifeville, Ridge St, and Belmont experienced higher average surface temperatures than neighborhoods like Barracks/Rugby, Greenbrier and Fry's Spring.

![Rplot02](https://github.com/user-attachments/assets/15ae9b8c-103a-4c0d-8a17-809132be2cf0)

