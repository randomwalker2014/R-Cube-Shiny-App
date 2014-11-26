<font size="3">

__OLAP Terminology__

Basic definitions for a data cube, dimensions, dimensional hierarchy and a measure.

* __Data Cube__ : Multi-dimensional represention of data where the 'cube' can have more than three dimensions

* __Dimensions__ : Represents descriptive categories of data such as Product or Time

* __Dimensional Hierarchy__ : The hierarchical breakdown showing the organization of levels within a dimension. For instance, the Time dimension has  three levels of Year,Quarter and Month 

* __Measure__ : Represents a computable fact or a numerical measure such as Revenue or Profit
</p>
<hr>
<p>
__OLAP Operations Supported and User Controls__
<font color= #586949><br>All operations require that a Data Cube, at least one Dimension and a Measure are selected</br></font>

* __Cube View__ : Choose the __Retail Cube__ from the dropdown and choose at least one Dimension and a Measure of interest.

* __Slice__ : Generate a sub-cube by selecting a __single__ dimension, a Measure and setting the selected dimension to specific value(s) by using the __Filters__ dropdown.

* __Dice__ : Generate a new sub-cube by selecting __multiple__ dimensions, a Measure and setting the multiple dimensions to specific value(s) using the __Filter__ dropdowns.

* __Roll-up__ : Take the current aggregation level of Measures and do a further aggregation on one or more of the dimensions. Deselect the lower hierarchical levels while keeping the dimensions at a higher level in the dimensional hierarchy.

* __Drill-Down__ : Summarize data at a lower level of the dimensional hierarchy, thereby viewing data in a more specialized level within a dimension.
<hr>

</font>



