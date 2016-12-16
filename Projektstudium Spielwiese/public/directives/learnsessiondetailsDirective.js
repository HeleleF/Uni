(function () {
  'use strict';

	angular.module('lisamon.directives').directive('learnsessiondetails', [function() {

		var ld_svg;

		return {
			restrict: "EA", // element names and attribute names can invoke directive
			link: function(scope, elem, attr) {

				var width, height;
				var rectangleHeight, rectangleWidth;

				var brushHeart, brushEDA, brushAccelerometer;

				var xScale, yScale;

				var actualHeartData, actualEDAData, actualAccelerometerData;

				// TODO: add to css file
		        width = 5000,
		        height = 430,//460

		        rectangleHeight = 127;
		        rectangleWidth = 5000;

				ld_svg = d3.select('#learnsessionchart')
					.attr("height", height)
					.attr("width", width);

              	// watch for data changes and re-render
		        scope.$watch('data', function(newVals, oldVals) {
		          return scope.render(newVals);
		        }, true);

		        // render statistic
              	scope.render = function(data){

              		// remove all previous items before render
            		ld_svg.selectAll("*").remove();

			        drawCurrentStatistics();
		        }

		        function drawCurrentStatistics() {

		        	drawCurrentHeartStatistic();
		        	drawCurrentEDAStatistic();
	                drawCurrentAccelerometerStatistic();
	                drawXAxis();
			    }

			    function drawCurrentHeartStatistic() {

			    	actualHeartData = scope.getHeartRateDataOfLearnsession;

		        	var ld_svgHeart = d3.select("#learnsessionchart").append("svg")
	                    .attr("width", rectangleWidth)
	                    .attr("height", rectangleHeight)
	                    .attr("x", "0")
	                    .attr("y", "0");

		        	// create x, y scales and ranges
					xScale = d3.scale.linear().range([0, rectangleWidth]);
					yScale = d3.scale.linear().range([rectangleHeight, 0]);

					xScale.domain(d3.extent(actualHeartData, function(d) { return d.date; }));
    				yScale.domain([0, d3.max(actualHeartData, function(d) { return d.value; })]);

	                // create line objects
                    var lineHeart = d3.svg.line()
                    	.interpolate("basis")
	                    .x(function(d) { return xScale(d.date); })
	                    .y(function(d) { return yScale(d.value); });

		        	var rectangleHeart = ld_svgHeart.append("rect")
                    	.attr("x", 0)
                        .attr("y", 0)
                        .attr("width", rectangleWidth)
                        .attr("height", rectangleHeight)
                        .attr("class", "ld_svg_parts");

                    var graphData = ld_svgHeart.selectAll(".heartData")
                      .data(actualHeartData)
                    .enter().append("g")
                      .attr("class", "heartData");

	                graphData.append("path")
	                    .attr("class", "line")
	                    .attr("d", function(d) { return lineHeart(actualHeartData); });

	                brushHeart = d3.svg.brush().x(xScale).on("brushend", brushHeartRateDetails);

					var gBrush = ld_svgHeart.append("g")
					      .attr("class", "x brush")
					      .attr("width", rectangleWidth)
					    .attr("height", rectangleHeight)
					      .call(brushHeart)
					    .selectAll("rect")
					      .attr("height", rectangleHeight);		

			    }

			    /*
				*	Brush function and data are not mapped. Calculate the selected values.
			    */
			    function brushHeartRateDetails() {
			    	
			    	// get selected values from the brush
			    	var extent = brushHeart.extent();
					var dataStartIndex = Math.round(extent[0], -1) - 1;
					var dataEndIndex = Math.round(extent[1], - 1) - 1;
					
					var detailData = actualHeartData.slice(dataStartIndex, dataEndIndex + 1);

					console.log("heart reate detail: " + JSON.stringify(detailData));
					
					scope.setHeartRateDetails(detailData);
			    }

			    function drawCurrentEDAStatistic() {

	            	actualEDAData = scope.getEDADataOfLearnsession;

                    var ld_svgEDA = d3.select("#learnsessionchart").append("svg")
	                    .attr("width", rectangleWidth)
	                    .attr("height", rectangleHeight)
	                    .attr("x", "0")
	                    .attr("y", "132");

	                // create x, y scales and ranges
					xScale = d3.scale.linear().range([0, rectangleWidth]);
					yScale = d3.scale.linear().range([rectangleHeight, 0]);

					xScale.domain(d3.extent(actualEDAData, function(d) { return d.date; }));
    				yScale.domain([0, d3.max(actualEDAData, function(d) { return d.value; })]);

	                // create line objects
                    var lineEDA = d3.svg.line()
                    	.interpolate("basis")
	                    .x(function(d) { return xScale(d.date); })
	                    .y(function(d) { return yScale(d.value); });

                    var rectangleEDA = ld_svgEDA.append("rect")
                    	.attr("x", 0)
                        .attr("y", 0)
                        .attr("width", rectangleWidth)
                        .attr("height", rectangleHeight)
                        .attr("class", "ld_svg_parts");

			        var graphData = ld_svgEDA.selectAll(".edaData")
                      .data(actualEDAData)
                    .enter().append("g")
                      .attr("class", "edaData");

	                graphData.append("path")
	                    .attr("class", "line")
	                    .attr("d", function(d) { return lineEDA(actualEDAData); });

	              	brushEDA = d3.svg.brush().x(xScale).on("brushend", brushEDADetails);

					var gBrush = ld_svgEDA.append("g")
					      .attr("class", "x brush")
					      .attr("width", rectangleWidth)
					    .attr("height", rectangleHeight)
					      .call(brushEDA)
					    .selectAll("rect")
					      .attr("height", rectangleHeight);
			    }

			    /*
				*	Brush function and data are not mapped. Calculate the selected values.
			    */
			    function brushEDADetails() {
			    	
			    	// get selected values from the brush
			    	var extent = brushEDA.extent();
					var dataStartIndex = Math.round(extent[0], -1) - 1;
					var dataEndIndex = Math.round(extent[1], - 1) - 1;
					
					var detailData = actualEDAData.slice(dataStartIndex, dataEndIndex + 1);

					console.log(JSON.stringify(detailData));
					
					scope.setEDADetails(detailData);
			    }

			    function drawCurrentAccelerometerStatistic() {

	            	actualAccelerometerData = scope.getAccelerometerDataOfLearnsession;

                    var ld_svgAccelerometer = d3.select("#learnsessionchart").append("svg")
	                    .attr("width", rectangleWidth)
	                    .attr("height", rectangleHeight)
	                    .attr("x", "0")
	                    .attr("y", "264");

					// create x, y scales and ranges
					xScale = d3.scale.linear().range([0, rectangleWidth]);
					yScale = d3.scale.linear().range([rectangleHeight, 0]);

					xScale.domain(d3.extent(actualAccelerometerData, function(d) { return d.date; }));
    				yScale.domain([0, d3.max(actualAccelerometerData, function(d) { return d.value; })]);

	                // create line objects
                    var lineAccelerometer = d3.svg.line()
                    	.interpolate("basis")
	                    .x(function(d) { return xScale(d.date); })
	                    .y(function(d) { return yScale(d.value); });

                    var rectangleAccelerometer = ld_svgAccelerometer.append("rect")
                    	.attr("x", 0)
                        .attr("y", 0)
                        .attr("width", width)
                        .attr("height", height)
                        .attr("class", "ld_svg_parts");

			        var graphData = ld_svgAccelerometer.selectAll(".accelerometerData")
                      .data(actualAccelerometerData)
                    .enter().append("g")
                      .attr("class", "accelerometerData");
			        
	                graphData.append("path")
	                    .attr("class", "line")
	                    .attr("d", function(d) { return lineAccelerometer(actualAccelerometerData); });
		        
	                brushAccelerometer = d3.svg.brush().x(xScale).on("brushend", brushAcceleratormeterDetails);
			        
					var gBrush = ld_svgAccelerometer.append("g")
					      .attr("class", "x brush")
					      .attr("width", rectangleWidth)
					    .attr("height", rectangleHeight)
					      .call(brushAccelerometer)
					    .selectAll("rect")
					      .attr("height", rectangleHeight);
			    }

			    /*
				*	Brush function and data are not mapped. Calculate the selected values.
			    */
			    function brushAcceleratormeterDetails() {
			    	
			    	// get selected values from the brush
			    	var extent = brushAccelerometer.extent();
					var dataStartIndex = Math.round(extent[0], -1) - 1;
					var dataEndIndex = Math.round(extent[1], - 1) - 1;
					
					var detailData = actualAccelerometerData.slice(dataStartIndex, dataEndIndex + 1);
					
					scope.setAcceleratorDetails(detailData);
			    }
			    
			    function drawXAxis() {

			    	actualHeartData = scope.getHeartRateDataOfLearnsession;

		        	var ld_svgHeart = d3.select("#learnsessionchart").append("svg")
	                    .attr("width", rectangleWidth)
	                    .attr("height", rectangleHeight)
	                    .attr("x", "0")
	                    .attr("y", "396");

		        	// create x, y scales and ranges
					xScale = d3.scale.linear().range([0, rectangleWidth]);
					yScale = d3.scale.linear().range([rectangleHeight, 0]);

					xScale.domain(d3.extent(actualHeartData, function(d) { return d.date; }));
    				yScale.domain([0, d3.max(actualHeartData, function(d) { return d.value; })]);
    				
    				//x-Achse einfuegen
			        var xAxis = d3.svg.axis().scale(xScale)
			        	.orient("bottom").ticks(12);
		        
			        ld_svgHeart.append("svg:g")
			        	.attr("transform", "translate(0, 7)")
			        	.attr("class", " ld_x_axis")
			        	.call(xAxis);
			    }

			}
		};
	}]);
}());