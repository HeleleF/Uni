(function () {
  'use strict';

	angular.module('lisamon.directives').directive('statistics', [function() {

		var svg;

		return {
			restrict: "EA", // element names and attribute names can invoke directive
			link: function(scope, elem, attr) {

				var width, height, radius;
				var rectangleHeight, rectangleWidth;

				var brushHeart, brushEDA;

				var xScale, yScale;

				var actualHeartData, actualEDAData;

				var transformX = 420;
				var transformY = 270;

				// TODO: add to css file
		        width = 1500,
		        height = 1200,
		        radius = 250;

		        rectangleHeight = 80;
		        rectangleWidth = 1000;

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
			        drawCurrentStatistics();
		        }

		        function getDonutChartAngleDataset() {    

			        var dataset = [
			        	{ id: 1, startAngle: 87.5, endAngle:  100},
			        	{ id: 2, startAngle: 75, endAngle:  87.5},
			        	{ id: 3, startAngle: 62.5, endAngle:  75},
			        	{ id: 4, startAngle: 50, endAngle:  62.5},
			        	{ id: 5, startAngle: 37.5, endAngle:  50},
			        	{ id: 6, startAngle: 25, endAngle:  37.5}
			        ];

			        return dataset;
		        }

		        function getHeartStatisticElementsPositions(positionID) {
		        	var dataset = [
			        	{ id: 1, circleX: -90, circleY: -185, textX: -100, textY: -180},
			        	{ id: 2, circleX: -190, circleY: -80, textX: -200, textY: -75},
			        	{ id: 3, circleX: -190, circleY: 80, textX: -200, textY: 85},
			        	{ id: 4, circleX: -80, circleY: 190, textX: -90, textY: 195},
			        	{ id: 5, circleX: 80, circleY: 190, textX: 70, textY: 195},
			        	{ id: 6, circleX: 190, circleY: 80, textX: 180, textY:  85}
			        ];

			        var item;

			        dataset.forEach(function(d) {
			        	if (d.id == positionID) {
			        		item = d;
			        	}
			        });

			        return item;
		        }

		        function getEDAStatisticElementsPositions(positionID) {
		        	var dataset = [
			        	{ id: 1, circleX: -65, circleY: -110, textX: -75, textY:  -105},
			        	{ id: 2, circleX: -115, circleY: -45, textX: -125, textY:  -40},
			        	{ id: 3, circleX: -115, circleY: 45, textX: -125, textY:  50},
			        	{ id: 4, circleX: -50, circleY: 115, textX: -60, textY:  120},
			        	{ id: 5, circleX: 45, circleY: 115, textX: 35, textY:  120},
			        	{ id: 6, circleX: 115, circleY: 45, textX: 105, textY:  50}
			        ];

			        var item;

			        dataset.forEach(function(d) {
			        	if (d.id == positionID) {
			        		item = d;
			        	}
			        });

			        return item;
		        }

		        /*
		        * Creates and returns the scale for the donut charts
		        */
		        function getScaleForDonutChart() {
		        	var stat_scale = d3.scale.linear().domain([0, 100]).range([0, 2 * Math.PI]);

		        	return stat_scale;
		        }

		        function drawDonut() {
		        	drawHeartDonutStatistics();
					drawEDADonutStatistics();
		        }

		        function drawHeartDonutStatistics() {

		        	// dataset with angles for the chart pieces
		        	var dataset = getDonutChartAngleDataset();
			        var stat_scale = getScaleForDonutChart();

			        var svgOuterCircle = d3.select("#chart").append("svg");

			        // create donut objects
			        var outerCircle = d3.svg.arc()
				        .padAngle(.01)
				        .outerRadius(radius)
				        .innerRadius(radius - 80)
				        .startAngle(function(d){return stat_scale(d.startAngle);})
				        .endAngle(function(d){return stat_scale(d.endAngle);});

			        // add dataset
			        var group = svgOuterCircle.selectAll(".svg_div_out")
                      	.data(dataset)
                    	.enter().append("g")
	                    	.attr("class", "svg_div_out")
	                      	.attr("id", function(d){return "group_outer" + d.id;})
	                      	.attr("transform", "translate(" + transformX + "," + transformY +")");

                    var tmpID = 0;

					group.each(function d() {

						tmpID = tmpID + 1;
                    	var groupID = this.id;
                    	var tmpGroup = svgOuterCircle.select("#" + groupID);
                    	var positions = getHeartStatisticElementsPositions(tmpID);

                    	// add donut objects to svg
    					var path = tmpGroup.append("path")
					    	.attr("d", outerCircle)

					    // get the number of heart notifications for this session
					    // TODO: change tmpID with correct session id
                		var numberOfNotifications = scope.getNotificationsOfLearnsession(tmpID, "pulse");

                		if (numberOfNotifications > 0) {

							var circle = tmpGroup.append("svg:circle")
								.attr("r", 25)
								.attr("class", "statsNotificationCircle")
								.attr("cx", positions.circleX)
								.attr("cy", positions.circleY);

							var text = tmpGroup.append("text")
								.attr("class", "statsNotificationText")
								.attr("dx", positions.textX)
								.attr("dy", positions.textY)
								.text(numberOfNotifications);

						}	
		
					});
					
		        }
	
		        function drawEDADonutStatistics() {
		        	var dataset = getDonutChartAngleDataset();
		        	var stat_scale = getScaleForDonutChart();

			        var svgMiddleCircle = d3.select("#chart").append("svg");

			        // create donut objects	        
			        var middleCircle = d3.svg.arc()
				        .padAngle(.01)
				        .outerRadius(radius - 85)
				        .innerRadius(radius - 165)
				        .startAngle(function(d){return stat_scale(d.startAngle);})
				        .endAngle(function(d){return stat_scale(d.endAngle);});
			    
			        // add dataset
			        var group = svgMiddleCircle.selectAll(".svg_div_middle")
                      	.data(dataset)
                    	.enter().append("g")
	                    	.attr("class", "svg_div_middle")
	                      	.attr("id", function(d){return "group_eda_" + d.id;})
	                      	.attr("transform", "translate(" + transformX + "," + transformY +")");

					var tmpID = 0;

					group.each(function d() {

						tmpID = tmpID + 1;
                    	var groupID = this.id;
                    	var tmpGroup = svgMiddleCircle.select("#" + groupID);
                    	var positions = getEDAStatisticElementsPositions(tmpID);

                    	// add donut objects to svg
    					var path = tmpGroup.append("path")
					    	.attr("d", middleCircle)

                		// get the number of heart notifications for this session
					    // TODO: change tmpID with correct session id
                		var numberOfNotifications = scope.getNotificationsOfLearnsession(tmpID, "pulse");

                		if (numberOfNotifications > 0) {

							var circle = tmpGroup.append("svg:circle")
								.attr("r", 25)
								.attr("class", "statsNotificationCircle")
								.attr("cx", positions.circleX)
								.attr("cy", positions.circleY);	

							var text = tmpGroup.append("text")
								.attr("class", "statsNotificationText")
								.attr("dx", positions.textX)
								.attr("dy", positions.textY)
								.text(numberOfNotifications);
						}			
		
					});
		        }

		        function drawCurrentStatistics() {

		        	drawCurrentHeartStatistic();
		        	drawCurrentEDAStatistic();
			    }

			    function drawCurrentHeartStatistic() {

			    	actualHeartData = scope.getActualHeartData;

		        	var svgHeart = d3.select("#chart").append("svg")
	                    .attr("width", rectangleWidth)
	                    .attr("height", rectangleHeight)
	                    .attr("x", "425")
	                    .attr("y", "20");

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

		        	var rectangleHeart = svgHeart.append("rect")
                    	.attr("x", 0)
                        .attr("y", 0)
                        .attr("width", rectangleWidth)
                        .attr("height", rectangleHeight)
                        .attr("class", "svg_parts");

                    var graphData = svgHeart.selectAll(".heartData")
                      .data(actualHeartData)
                    .enter().append("g")
                      .attr("class", "heartData");

	                graphData.append("path")
	                    .attr("class", "line")
	                    .attr("d", function(d) { return lineHeart(actualHeartData); });

	                brushHeart = d3.svg.brush().x(xScale).on("brushend", brushHeartRateDetails);

					var gBrush = svgHeart.append("g")
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
					
					scope.setHeartRateDetails(detailData);
			    }

			    function drawCurrentEDAStatistic() {

	            	actualEDAData = scope.getActualEDAData;

                    var svgEDA = d3.select("#chart").append("svg")
	                    .attr("width", rectangleWidth)
	                    .attr("height", rectangleHeight)
	                    .attr("x", "425")
	                    .attr("y", "105");

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

                    var rectangleEDA = svgEDA.append("rect")
                    	.attr("x", 0)
                        .attr("y", 0)
                        .attr("width", rectangleWidth)
                        .attr("height", rectangleHeight)
                        .attr("class", "svg_parts");

			        var graphData = svgEDA.selectAll(".edaData")
                      .data(actualEDAData)
                    .enter().append("g")
                      .attr("class", "edaData");

	                graphData.append("path")
	                    .attr("class", "line")
	                    .attr("d", function(d) { return lineEDA(actualEDAData); });

	              	brushEDA = d3.svg.brush().x(xScale).on("brushend", brushEDADetails);

					var gBrush = svgEDA.append("g")
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
					
					scope.setEDADetails(detailData);
			    }

			}
		};
	}]);
}());