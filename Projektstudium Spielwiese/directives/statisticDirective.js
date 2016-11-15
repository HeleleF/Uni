(function () {
  'use strict';

	angular.module('lisamon.directives').directive('statistics', [function() {

		var svg;

		return {
			restrict: "EA", // element names and attribute names can invoke directive
			link: function(scope, elem, attr) {

				var width, height, radius;

					// TODO: add to css file
			        width = 1400,
			        height = 1200,
			        radius = 400;

				svg = d3.select('#chart')
					.attr("height", height)
					.attr("width", width);

              	// watch for data changes and re-render
		        scope.$watch('data', function(newVals, oldVals) {
		          return scope.render(newVals);
		        }, true);

		        // render statistic
              	scope.render = function(data){

              		// remove all previous items before render
            		svg.selectAll("*").remove();

			        drawDonut();
			        drawRectangles();
		        }

		        function drawDonut() {

		        	var hData = scope.actualHeartData;

		        	var xHeart = d3.scale.linear()
	                    .domain([0, d3.max(hData, function(d) { return d.values.length - 1; })])
	                    .range([0, width]);
                            
	                var yHeart = d3.scale.linear()
	                    .domain([d3.min(hData, function(d) { return d3.min(d.values); }),
	                             d3.max(hData, function(d) { return d3.max(d.values); })])
	                    .range([height, 0]);

	                var lineHeart = d3.svg.line()
	                    .interpolate("basis")
	                    .x(function(d, i) { return xHeart(i); })
	                    .y(function(d, i) { return yHeart(d); });

		        	// dataset with angles for the chart pieces
		        	var dataset = [
			          { label: 'Jan-2016', startAngle: 25, endAngle:  31.25 },
			          { label: 'Feb-2016', startAngle: 31.25, endAngle:  37.5 },
			          { label: 'Mar-2016', startAngle: 37.5, endAngle:  43.75 },
			          { label: 'Apr-2016', startAngle: 43.75, endAngle:  50 },
			          { label: 'Mai-2016', startAngle: 50, endAngle:  56.25 },
			          { label: 'Jun-2016', startAngle: 56.25, endAngle:  62.5 },
			          { label: 'Jul-2016', startAngle: 62.5, endAngle:  68.75 },
			          { label: 'Aug-2016', startAngle: 68.75, endAngle:  75 },
			          { label: 'Sep-2016', startAngle: 75, endAngle:  81.25 },
			          { label: 'Okt-2016', startAngle: 81.25, endAngle:  87.5 },
			          { label: 'Nov-2016', startAngle: 87.5, endAngle:  93.75 },
			          { label: 'Dez-2016', startAngle: 93.75, endAngle:  100 }
			        ];

			        var stat_scale = d3.scale.linear().domain([0, 100]).range([0, 2 * Math.PI]);

			        var svgOuterCircle = d3.select("#chart").append("svg");
			        var svgMiddleCircle = d3.select("#chart").append("svg");
			        var svgInnerCircle = d3.select("#chart").append("svg");

			        // create donut objects
			        var outerCircle = d3.svg.arc()
				        .padAngle(.01)
				        .outerRadius(radius)
				        .innerRadius(radius - 80)
				        .startAngle(function(d){return stat_scale(d.startAngle);})
				        .endAngle(function(d){return stat_scale(d.endAngle);});

			        var middleCircle = d3.svg.arc()
				        .padAngle(.01)
				        .outerRadius(radius - 85)
				        .innerRadius(radius - 165)
				        .startAngle(function(d){return stat_scale(d.startAngle);})
				        .endAngle(function(d){return stat_scale(d.endAngle);});

			        var innerCircle = d3.svg.arc()
				        .padAngle(.01)
				        .outerRadius(radius - 170)
				        .innerRadius(radius - 250)
				        .startAngle(function(d){return stat_scale(d.startAngle);})
				        .endAngle(function(d){return stat_scale(d.endAngle);});

			        // add dataset
			        var g = svg.selectAll(".svg_div")
                      	.data(dataset)
                    	.enter().append("g")
                      	.attr("class", "svg_div");


                    // add donut objects to svg
                    g.append("path")
					    .attr("d", outerCircle)
					    .attr("transform", "translate(420,420)");

					g.append("path")
					    .attr("d", middleCircle)
					    .attr("transform", "translate(420,420)");

					g.append("path")
					    .attr("d", innerCircle)
					    .attr("transform", "translate(420,420)");
					    
		        }

		        function drawRectangles() {

		        	var hData = scope.actualHeartData;

	            	var eData =scope.actualEDAData;

	            	var aData = scope.actualAccelerometerData;

		        	var height, width;

		        	height = 80;
		        	width = 1400;

		        	var svgHeart = d3.select("#chart").append("svg")
	                    .attr("width", width)
	                    .attr("height", height)
	                    .attr("x", "425")
	                    .attr("y", "20");

                    var svgEDA = d3.select("#chart").append("svg")
	                    .attr("width", width)
	                    .attr("height", height)
	                    .attr("x", "425")
	                    .attr("y", "105");

                    var svgAccelerometer = d3.select("#chart").append("svg")
	                    .attr("width", width)
	                    .attr("height", height)
	                    .attr("x", "425")
	                    .attr("y", "190");

		        	// create x, y objects for line objects
		        	var xHeart = d3.scale.linear()
	                    .domain([0, d3.max(hData, function(d) { return d.values.length - 1; })])
	                    .range([0, width]);
                            
	                var yHeart = d3.scale.linear()
	                    .domain([d3.min(hData, function(d) { return d3.min(d.values); }),
	                             d3.max(hData, function(d) { return d3.max(d.values); })])
	                    .range([height, 0]);

	                var xEDA = d3.scale.linear()
	                    .domain([0, d3.max(eData, function(d) { return d.values.length - 1; })])
	                    .range([0, width]);
                            
	                var yEDA = d3.scale.linear()
	                    .domain([d3.min(eData, function(d) { return d3.min(d.values); }),
	                             d3.max(eData, function(d) { return d3.max(d.values); })])
	                    .range([height, 0]);

	                var xAccelerometer = d3.scale.linear()
	                    .domain([0, d3.max(aData, function(d) { return d.values.length - 1; })])
	                    .range([0, width]);
                            
	                var yAccelerometer = d3.scale.linear()
	                    .domain([d3.min(aData, function(d) { return d3.min(d.values); }),
	                             d3.max(aData, function(d) { return d3.max(d.values); })])
	                    .range([height, 0]);

	                // create line objects
                    var lineHeart = d3.svg.line()
	                    .interpolate("basis")
	                    .x(function(d, i) { return xHeart(i); })
	                    .y(function(d, i) { return yHeart(d); });

                    var lineEDA = d3.svg.line()
	                    .interpolate("basis")
	                    .x(function(d, i) { return xEDA(i); })
	                    .y(function(d, i) { return yEDA(d); });

                    var lineAccelerometer = d3.svg.line()
	                    .interpolate("basis")
	                    .x(function(d, i) { return xAccelerometer(i); })
	                    .y(function(d, i) { return yAccelerometer(d); });

		        	// TODO: calculate positions?
		        	var rectangleHeart = svgHeart.append("rect")
                    	.attr("x", 0)
                        .attr("y", 0)
                        .attr("width", width)
                        .attr("height", height)
                        .attr("class", "svg_parts");

                    var rectangleEDA = svgEDA.append("rect")
                    	.attr("x", 0)
                        .attr("y", 0)
                        .attr("width", width)
                        .attr("height", height)
                        .attr("class", "svg_parts");

                    var rectangleAccelerometer = svgAccelerometer.append("rect")
                    	.attr("x", 0)
                        .attr("y", 0)
                        .attr("width", width)
                        .attr("height", height)
                        .attr("class", "svg_parts");

                    var heartData = svgHeart.selectAll(".heartData")
                      .data(hData)
                    .enter().append("g")
                      .attr("class", "heartData");

	                heartData.append("path")
	                    .attr("class", "line")
	                    .attr("d", function(d) { return lineHeart(d.values); });

			        var edaData = svgEDA.selectAll(".edaData")
                      .data(eData)
                    .enter().append("g")
                      .attr("class", "edaData");

	                edaData.append("path")
	                    .attr("class", "line")
	                    .attr("d", function(d) { return lineEDA(d.values); });

			        var accelerometerData = svgAccelerometer.selectAll(".accelerometerData")
                      .data(aData)
                    .enter().append("g")
                      .attr("class", "accelerometerData");

	                accelerometerData.append("path")
	                    .attr("class", "line")
	                    .attr("d", function(d) { return lineAccelerometer(d.values); });
	                
			    }
			}
		};
	}]);
}());