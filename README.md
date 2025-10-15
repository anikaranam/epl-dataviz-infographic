# Home Field Advantage in the English Premier League: A Data Visualization Project

This repository contains the R code and final infographic for an in-depth analysis of home field advantage in the English Premier League, using data from 2000 to 2022. The project's goal was to move beyond simple metrics and use a variety of visualization techniques to tell a compelling, multi-faceted story about this well-known sporting phenomenon.

## The Final Infographic


## The Narrative: Deconstructing Home Advantage 🏟️

The central question was: "What does home field advantage actually look like in the data?" The infographic explores this by visualizing several key aspects:

1.  **Historical Trend:** How has home advantage changed over 22 years, and what was the impact of the "natural experiment" of the pandemic seasons with no crowds?
2.  **Performance Metrics:** Do home teams create more chances (shots, corners) and are they more clinical (goals)?
3.  **Game State Dynamics:** How does the half-time result influence the final outcome for home and away teams?
4.  **Potential Referee Bias:** Are home teams penalized less frequently for fouls compared to away teams?

## Showcasing Data Visualization & Analysis Skills

This project was an exercise in using R for end-to-end data analysis and visualization. The following skills are demonstrated through the code and the final product.

### 1. Data Wrangling and Preparation (`dplyr`, `purrr`, `tidyr`)

-   **Efficiently Imported and Merged Data:** Used the `purrr` package to iterate over 22 separate CSV files, loading and combining them into a single, tidy dataset.
-   **Feature Engineering:** Created new, meaningful columns from the raw data using `dplyr::mutate()` and `case_when()`, such as identifying the match winner, calculating goal differences, and parsing dates. This was crucial for simplifying the subsequent visualization steps.

### 2. Statistical Storytelling with `ggplot2`

The project intentionally uses a variety of chart types, choosing the best visualization for each specific question. This demonstrates an understanding of how to select the right tool for the job.

-   **Showing Trends and Anomalies (Line Chart):** The "Home Advantage Over Time" chart uses a line graph to show the long-term trend, but also creatively adds a shaded `geom_polygon` and an `annotate` layer to explicitly highlight the anomalous "pandemic period," adding crucial context.
-   **Visualizing Proportions (Stacked Bar Chart):** The "Half-Time to Full-Time" chart effectively shows how the probability of a final result is conditioned on the half-time score. In-chart percentage labels (`geom_text`) make it instantly readable.
-   **Comparing Distributions (Beeswarm Plot):** Instead of a simple boxplot, the "Shot Quality" chart uses `ggbeeswarm::geom_quasirandom` to plot every single data point (team-season). This provides a much richer view of the distribution's shape and density, revealing that while the medians are similar, the distributions themselves are quite alike.

### 3. Advanced Visualization and Composition

-   **Communicating Uncertainty (Error Bars):** The "Home vs. Away Performance" bar charts don't just show the average values; they include **95% confidence intervals** (`geom_errorbar`). This adds a layer of statistical rigor, allowing the viewer to see that the differences in goals, shots, and corners are statistically significant.
-   **Custom Layouts (`gridExtra`):** The performance statistics section was created by generating three independent `ggplot` objects and then arranging them into a single, coherent visual using `gridExtra::grid.arrange`. This technique allows for greater control over complex layouts.
-   **Data-Dense Heatmaps:** The "Referee Bias Analysis" uses a `geom_tile` heatmap to compare two different metrics across all 20 teams simultaneously. A diverging color palette (`scale_fill_gradient2`) centered at 1.0 makes it easy to spot which teams are treated more or less favorably at home.
-   **Presentation-Ready Tables (`gt`):** The Premier League table is not a simple printout. It was created using the `gt` package to produce a polished, presentation-quality table. The `data_color()` function was used to create a color scale within the points column, turning the table itself into a mini-visualization.

### 4. Design and Aesthetics

-   **Consistent Color Palette:** A consistent color scheme is used throughout the infographic (green for home/positive, red for away/negative) to make it easy to interpret.
-   **Clear Labeling and Subtitles:** Every chart has a clear title and a subtitle that explains the key takeaway, guiding the viewer through the analysis.
-   **Logical Flow:** The infographic is organized to tell a story, starting with a high-level overview and progressively drilling down into more specific areas of analysis.

## Tools and Libraries

-   **Language:** R
-   **Core Packages:** `tidyverse` (especially `ggplot2`, `dplyr`, `readr`)
-   **Specialized Visualization:** `ggbeeswarm`, `gt`, `gridExtra`
-   **Data Processing:** `purrr`, `lubridate`
