# Data Visualization 

When I have some free time on my hands I like to explore some interesting topic. This project started as a spinoff of #TidyTuesday so I tried to keep the weekly updates and the tidyverse spirit.

Most of my visualizations are about economics, Brazil, and São Paulo.

# 2022

## February

- [Births and Wedding Rates](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-02-25/births_weddings.R)

- [Births and Deaths](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-02-18/nascimentos_mortes.R)

- [Real GDP per capita](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-02-11/pib_per_capita_real.R)

- [Homes, People, and Jobs in Capital Cities](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-02-04/homes_people_jobs.R)

## January

- [City GDP Distribution](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-01-27/pib_participacao_muni.R)

- [City GDP](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-01-20/pib_composicao.Rmd)

- [Recessions in Brazil](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-01-13)

- [Growth in Brazil and Latin America](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-01-06)

# 2021

## December

- [Thefts in Sao Paulo](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-12-30/full_code.R)

## November

- [Road Accidents in Sao Paulo](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-11-02)

## October

- [Unemployment in Brazil and Sao Paulo](https://github.com/viniciusoike/weekly_viz/blob/main/R/viz-10-26)

- [Skycrapers in Sao Paulo](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2020_10)

- [Global Warming](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2020_10)

## September

- [Family Income Distribution in Sao Paulo](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2020_10)


***

# Plots

## Births and Weddings

* Wedding rates have been relatively stable across the past 20 years. There has been a recent decline in weddings due to Covid-19 (data end in dec/20)
* Birth rates fall more clearly across all states.
* The average age at which women have children rose by almost 6 years across all states.

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_02/births_weddings.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_02/births_weddings_avg_age.png?raw=true)

## Births and Deaths

* As a result of Covid-19, the number of naturally caused deaths (i.e. non-violent) surpassed the number of births.
* Age of death distribution is left-skewed so the average is less than the median.

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_02/births_deaths.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_02/age_of_death.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_02/age_of_death_distribution.gif?raw=true)

## People, Houses, and Jobs in Capital Cities

* Job growth stagnates in the post 2011-2 period in most cities.
* Houses grow faster than population for all cities due to a variety of factors: aging population forming families; smaller families (i.e. more houses per person); larger demand for real-estate as an investment.

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_02/people_houses_jobs_panel.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_02/people_houses_jobs_southeast.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_02/people_houses_census_Southeast.png?raw=true)

## GDP concentration in Brazil

* All economies exhibit spatial concentration of productive activities. In Brazil, 1,2% of its wealthiest cities contribute to half of the country's GDP.
* The majority of these cities are located along Brazil's coastline.
* Even going up to 90% requires including only less than a quarter of Brazil's cities. 

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_01/mapa_concentracao_50.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_01/mapa_concentracao_90.png?raw=true)

## GDP Composition by City

* Brazil is mostly a services economy, despite its exports being focused on primary goods (commodities).
* Public sector spending has a large impact on many cities specially in the northeastern region.
* Both agriculture and services exhibit some spatial concentration; industrial cities, on the other hand, seem very disperse throught Brazil.

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_01/pib_brasil_composicao_p1.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_01/pib_brasil_composicao_p5.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_01/pib_brasil_composicao_p6.png?raw=true)

## Recessions in Brazil

* Since 1996, Brazil faced 5 recessions, including the most recent Covid-19 recession. Notably, the 2008-09 Financial Crisis had only a mild effect on the Brazilian economy: technically, the slump lasted only two quarters, and after 4 quarters GDP was already higher then pre-recession levels. The most severe recession in recent record was the 2014-16 recession. According to CODACE (Brazilian Business Cycle Dating Committee) it was the largest cumulative drop in %GDP since 1980 and it lasted 11 quarters. Even though the bust is technically over, the Brazilian economy is still **bellow** pre-2014 levels of activity.
* Note: despite current GDP being above pre-2020 levels, CODACE has yet to define when/if the Covid-19 recession ended.

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_01/recessores_brasil_cairo.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2022_01/queda_e_recuperacao_brasil_cairo.png?raw=true)

## Growth in Brazil

* Brazil exhibited a strong growth pattern in the 1960s and 1970s. The combination of the two Oil Shocks, the restrictive monetary policy led by Volcker in the USA, and the overall increase in interest rates around the world caused a massive rise in foreign debt and helped fuel the hiper-inflation of the 1980s.
* During the 1980s, extremely restrictive measures were put into place to try to alleviate the BoP deficit and reduce foreign debt. Despite this, external pressure mounted after Mexico declared insolvency in 1982 and in 1987 Brazil also declared insolvency. Several economic plans were enacted in the following years, with catastrophic results. Only in 1994 the country managed to reduce inflation but at the cost of very high interest rates, and an artificially overvalued exchange rate. As a result, growth was very modest during the 1990s.
* The 2000s were a positive decade despite the 2008-09 International Financial Crisis. The 2010s, on the other hand, were more mixed, and was overall a Lost Decade.

### GDP per capita growth in Brazil

![](https://raw.githubusercontent.com/viniciusoike/weekly_viz/main/graphics/2022_01/p5.png)

![](https://raw.githubusercontent.com/viniciusoike/weekly_viz/main/graphics/2022_01/p2.png)

### Latin America Panel

![](https://raw.githubusercontent.com/viniciusoike/weekly_viz/main/graphics/2022_01/p3.png)

![](https://raw.githubusercontent.com/viniciusoike/weekly_viz/main/graphics/2022_01/p4.png)

## Thefts in Sao Paulo

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2021_12/roubos_furtos_panel.png?raw=true)

## Car Accidents in Sao Paulo

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2021_11/map_road_accidents_2020.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2021_11/road_accidents_2.png?raw=true)


## Unemployment in Brazil and Sao Paulo

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2021_10/unemployment_1.png?raw=true)

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2021_10/unemployment_panel.png?raw=true)

## Skyscrapers in Sao Paulo

![.graphics/2020_10_sao_paulo_mapa_renda.png](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2021_10/skycraper_sp_panel.png?raw=true)

## Global Warming

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2021_10/global_warming_economist_cairo.png?raw=true)

## Family Income Distribution in Sao Paulo

![](https://github.com/viniciusoike/weekly_viz/blob/main/graphics/2021_10/sao_paulo_renda.png?raw=true)
