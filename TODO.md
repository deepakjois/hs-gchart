FEATURES
========
* Implement remaining chart types and params (see docs for Types.hs for list)
* Update documentation to match the new docs on API
  - Add Chart Gallery charts into examples.hs
* Implement new fixes including data encoding and new charts
* Include helper functions to scale datasets according to min/max values

CODE IMPROVEMENTS
=================
* A better function to show float values when they are integers
* Basic Error Handling when attempting to use items not allowed in a chart type
* Find ways to remove boilerplate
  - from ChartItem class instances
  - the `getParams` function

