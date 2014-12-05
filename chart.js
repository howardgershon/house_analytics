n = {top: 20, right: 100, bottom: 30, left: 100},
    width = 720 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;
d3.json("113.json", function(dataset){
var xScale = d3.scale.linear()
    .domain([0, d3.max(dataset, function(d){ return d[" ideology"]; })])
    .range([0, width]);

var yScale = d3.scale.linear()
    .domain([0, d3.max(dataset, function(d){ return d[" leadership"]; })])
    .range([height, 0]);

var xAxis = d3.svg.axis()
    .scale(xScale)
    //.ticks([])
    .orient("bottom")
    .innerTickSize(-height)
    .outerTickSize(0)
    .tickPadding(10);

var yAxis = d3.svg.axis()
    .scale(yScale)
    .orient("left")
    .innerTickSize(-width)
    .outerTickSize(0)
    .tickPadding(10);

var colors = {" Democrat":"blue", " Republican":"red", " Independent":"green"};

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis)

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)

  var tooltip = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

  svg.selectAll(".dot")
      .data(dataset)
    .enter().append("circle")
      //.attr("class", "dot")
      .attr("r", 3.5)
      .attr("cx", function(d) { return xScale(d[" ideology"]); })
      .attr("cy", function(d) { return yScale(d[" leadership"]); })
      .style("fill", function(d) { return colors[d[" party"]];})
      .on("mouseover", function(d) {
          tooltip.transition()
               .duration(200)
               .style("opacity", .9);
          tooltip.html(d[" name"] )
               .style("left", (d3.event.pageX + 5) + "px")
               .style("top", (d3.event.pageY - 18) + "px");
      })
      .on("mouseout", function(d) {
          tooltip.transition()
               .duration(500)
               .style("opacity", 0);
      });
});
