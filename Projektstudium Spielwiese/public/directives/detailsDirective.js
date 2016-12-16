(function () {
  'use strict';

	angular.module('lisamon.directives').directive('details', [function() {

		var svg;

		return {
			restrict: "EA", // element names and attribute names can invoke directive
			link: function(scope, elem, attr) {

				var width, height;

				// TODO: add to css file
		        width = 1500,
		        height = 600,

              	// watch for data changes and re-render
		        scope.$watch('data', function(newVals, oldVals) {
		          return scope.render(newVals);
		        }, true);

		        // render statistic
              	scope.render = function(data){

              		// remove all previous items before render
            		//svg.selectAll("*").remove();

			        drawDetailLineGraph();
		        }

			    function drawDetailLineGraph() {

			    	var detailData = scope.detailsData;

					// Set the dimensions of the canvas / graph
					var margin = {top: 30, right: 20, bottom: 30, left: 50},
					    width = 1500 - margin.left - margin.right,
					    height = 600 - margin.top - margin.bottom;

					// Parse the date / time

					// Set the ranges
					var x = d3.scale.linear().range([0, width]);
					var y = d3.scale.linear().range([height, 0]);
					var help1 = d3.scale.linear().range([0, width]);

					// Define the axes
					var xAxis = d3.svg.axis().scale(x)
					    .orient("bottom").ticks(5);

					var yAxis = d3.svg.axis().scale(y)
					    .orient("left").ticks(5);
					
					var helpAxis = d3.svg.axis().scale(help1)
				    	.orient("bottom").ticks(0);

					// Define the line
					var valueline = d3.svg.line()
						.interpolate("basis")
					    .x(function(d) { return x(d.date); })
					    .y(function(d) { return y(d.value); });
					    
					// Adds the svg canvas
					var svg = d3.select('#detailsSVG')
					        .attr("width", width)
					        .attr("height", height + margin.top + margin.bottom)
					    .append("g")
					        .attr("transform", 
					              "translate(" + margin.left + "," + margin.top + ")");
						//.append("h")
							//.attr("transform", 
									//"translate(" + margin.left + "," + margin.top + ")");

				    // Scale the range of the data
				    x.domain(d3.extent(detailData, function(d) { return d.date; }));
				    y.domain([0, d3.max(detailData, function(d) { return d.value; })]);

				    // Add help Lines
				    svg.append("g")
				    	.attr("class", "helplines")
				    	.attr("transform", "translate(0," + (height-(height/5*1)) + ")")
				    	.call(helpAxis); 
				    
				    svg.append("g")
			    		.attr("class", "helplines")
			    		.attr("transform", "translate(0," + (height-(height/5*2)) + ")")
			    		.call(helpAxis);
			    		
			    	svg.append("g")
			    		.attr("class", "helplines")
			    		.attr("transform", "translate(0," + (height-(height/5*3)) + ")")
			    		.call(helpAxis);
			    		
			    	svg.append("g")
			    		.attr("class", "helplines")
			    		.attr("transform", "translate(0," + (height-(height/5*4)) + ")")
			    		.call(helpAxis);
			    	
			    	// Add the valueline path.
				    svg.append("path")
				        .attr("class", "detailLine")
				        .attr("d", valueline(detailData)); 
				    
				    // Add the X Axis
				    svg.append("g")
				        .attr("class", "detailXAxis")
				        .attr("transform", "translate(0," + height + ")")
				        .call(xAxis); ;
				        
				    //Test
			    }

			}
		};
	}]);
}());
