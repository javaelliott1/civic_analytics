# Spatial Equitability Analysis of NYC Public Spaces

![Intro Photo of Open Streets Program](imgs/open-streets.jpeg)


Try the website to view public spaces around the city: https://javaelliott1.github.io/civic-analytics-map/
--

## Visualizing Accessibility
The simplest viewpoint is "Where Am I Close To?", and I map this within two different modes across different travel-time thresholds.

Each census tract is represented by a centroid. Public spaces are linked to tracts if they are reachable within the modeled travel-time threshold.

Travel-time thresholds:

10 minutes
20 minutes
30 minutes

Modes:

Walking
Walking + transit
Public space types include:

Parks
Plazas
Privately owned public spaces
Schoolyards open to the public at certain times
Waterfront access areas
Miscellaneous public spaces


--

## Live Map
The app does not calculate routes live in the browser. Instead, it loads precomputed tract-space travel-time tables and filters them instantly when the user clicks a tract.

The clicked point is matched to a census tract.
The selected tract is highlighted.
Nearby public spaces are filtered by mode, threshold, and type.
Public spaces are plotted on the map.
The sidebar updates with:
total nearby spaces
selected tract ID
type distribution chart
nearest public spaces list
The map is bounded to the greater NYC area to keep the experience focused.