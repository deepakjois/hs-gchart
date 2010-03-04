FEATURES
========
* Implement remaining chart types and params (see docs for Types.hs for list)
* Fix Data Encoding to be able to take only width parameter
* Add smart constructors for gradients and stripes
* Update documentation to match the new docs on API
  - Add Chart Gallery charts into examples.hs
* Implement new fixes including data encoding and new charts
* Include helper functions to scale datasets according to min/max values

CODE IMPROVEMENTS
=================
* Basic Error Handling when attempting to use items not allowed in a chart type
* Find ways to remove boilerplate
  - from ChartItem class instances
  - the `getParams` function

