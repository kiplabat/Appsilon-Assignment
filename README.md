Appsilon - Dashboard for Polish Biodiversity Observation
================
Kiplabat Tarus
10/17/2022

## Abstract

This R Shiny app was developed to visualize the biodiversity that has
been observed in Poland over the years. Thanks to Appsilon for
initiating this project as part of their technical skill evaluation.

This README will begin by highlighting the requirements of the task and
the other sections will be dedicated to how the identified requirements
were met. The final section will summarize the results of the task and
give insights and recommendations.

## Inroduction

This assignment was split into two tasks. The main task and extras.

1.  Main Task: To “Build a dashboard that main purpose is to visualize
    selected species observations on the map and how often it is
    observed”. The user can search either the scientific or the common
    name and the app will display its observation on a map and as a
    timeline. On the technical side, the app should also have shiny
    modules and should be tested for use cases. The app should also be
    deployed to <http://shinyapps.io> and the solution also be shared on
    github. Room for addition of features was also allowed.

2.  Extras: Optionally, the following skills could be integrated into
    the app. UIs (using CSS and Sass), Performance optimization,
    JavaScript, and Infrastructure know-how.

## Main Task

### Data and Preprocessing

The data shared at
<https://drive.google.com/file/d/1l1ymMg-K_xLriFv1b8MgddH851d6n2sU/view?usp=sharing>
was quite large and was technically unusable considering the specs of
the available computer.

The data hosted at
<https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165>
already had one filter applied and was quite sizable as well. I applied
the country filter (Poland) and downloaded the data. A pre-processing
step was necessary right after unpacking the data. The code chunk below
was used to both clean the data and prepare it for usage in the app.

The main aim of data preprocessing code was to split the data into
smaller chunks and rename them appropriately. This would have obvious
implications on memory allocation in the server in the case of many
users. Stepwise:

1.  Data was downloaded based on observation and Poland filters. The
    data did not contain vernacular name.  
2.  The vernacular names were obtained by web scraping from the
    observations website.  
3.  Data was merged and unique pair-wise combination of scientific and
    vernacular names were extracted.  
4.  Each species data was extracted into separate RDS files.

\[Check the preprocessor code\]

### App Development

The business requirements were considered. This is the technical side.

The app UI underwent several iterations and I settled for a traditional
Shiny app look. The fluiPage design is both easy for prototyping, fast,
and is mobile friendly. I know of SemanticUI. Multiple features were
integrated.

On the UI side:  
- Simplified fluidPage UI. First row contains 3 columns that hold Logo,
SearchInput, and Text Output. The second row holds has two columns that
hold the map and a graph pf observation timeline. The third row contains
download-able map data.

-   Attempts were made at customizing the UI using CSS. The default
    performed well in mobile devices.

On the server side:  
- Contains multiple reactive expressions and observers that control the
searched/selection and map output.

-   Functions for manipulating data have also been integrated.

Testing:

The app was constantly tested for correct performance using test code
provided. In this app, we only had one input section. To test its
correctness, it was tested against the sum of records retrieved.

### Deployment

The R Shiny app is deployed at
<https://labatt.shinyapps.io/Appsilon_Assignment/>.  
The code and data are hosted privately and shared with @appsilon-hiring
on <https://github.com/kiplabat/Appsilon-Assignment>.

### Conclusions

Building the Polish Biodiversity Observation R Shiny dashboard has been
a first. I have learnt so much in such a short period of time.

Key takeaways:

-   Data manipulation, even clean data, need to be constantly optimized
    to yield better performance.

-   File formats are critical for optimizing performance.

-   Web scraping is a powerful tool and can be used to fill gaps in
    datasets.

-   Simple UIs can be quite powerful given the right situation.

-   Some of the codes from Appsilon inspired this app.

-   The size of the initial datasets can be reviewed since not many have
    powerful computing and storage.

### References (web)

1.  GBIF.org (13 October 2022) GBIF Occurrence Download
    <https://doi.org/10.15468/dl.8qs7ms?>

2.  Observation.org (17 October) Observations original data source and
    Web Scraping <https://observation.org/>
