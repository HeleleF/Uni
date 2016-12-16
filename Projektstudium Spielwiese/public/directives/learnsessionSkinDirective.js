(function () {
  'use strict';

	angular.module('lisamon.directives').directive('learnsessionskin', ['$window', '$timeout', function($window, $timeout) {

		var chartHeight;
		var chartWidth;

		var limit = 60 * 2,
            duration = 150,
            now = new Date(Date.now() - duration);

        var sensorData = {
            data: d3.range(limit).map(function() {
                return 0
            })
        };

        var xScale;
        var yScale;
        var paths;
        var axis;

        var skinLine;

        var svgEDA;

        var old = 0;
        var threshold_skin = 118; // einfacher Grenzwert
        var machpause = 30000; // nur alle 30 Sekunden anzeigen
        var autoclose = 5; // Notification schließt sich nach 5 Sekunden selbst

        var skin_crits = []; // Sammlung aller kritischen Zeitpunkte
         

		return {
			restrict: "EA", // element names and attribute names can invoke directive
			link: function(scope, elem, attr) {
			
              	// watch for data changes and re-render
		        scope.$watch('sessionIsRunning', function() {

		        	if (scope.clearSkinSessionData  == true) {
			        	clearData();
		         	}
		        
		        	// just draw the graph if the session is running
		         	if (scope.sessionIsRunning) {
		         		scope.clearSkinSession(false);
		         		
			        	setChartAttributes();
						drawSkinGraph();

	          			addValuesToGraph();
		         	}
		    
		        });

		        function clearData() {
		         	d3.select('#chartEDA >*').remove();

		        	sensorData = {
			            data: d3.range(limit).map(function() {
			                return 0
			            })
			        };

			        sensorData.path = null;
		        }

		        /*
		        *	Set chart height and width depending on the current window height and width.
		        */
		        function setChartAttributes() {
		        	chartHeight = 250;
		        	chartWidth = 950;
		        }

		        function drawSkinGraph() {
		        	xScale = d3.time.scale()
			            .domain([now - (limit - 2), now - duration])
			            .range([0, chartWidth])

			        yScale = d3.scale.linear()
			            .domain([0, d3.max(sensorData.data)])
			            .range([chartHeight, 0])

			        skinLine = d3.svg.line()
			            .interpolate('basis')
			            .x(function(d, i) {
			                return xScale(now - (limit - 1 - i) * duration)
			            })
			            .y(function(d) {
			                return yScale(d)
			            })

			        svgEDA = d3.select('#chartEDA')
				        .attr("width", chartWidth)
				        .attr("height", chartHeight + 50);

				    //var g = svgEDA.append("g").attr("transform", "translate(" + 0 + "," + 0 + ")");

			        axis = svgEDA.append('g')
			            .attr('class', 'sessionAxis')
			            .attr('transform', 'translate(0,' + chartHeight + ')')
			            .call(xScale.axis = d3.svg.axis().scale(xScale).orient('bottom'))

			        paths = svgEDA.append('g')

		            sensorData.path = paths.append('path')
		                .data([sensorData.data])		                
		        }

		        function addValuesToGraph() {
		        	if (scope.sessionIsRunning) {
						now = new Date();

			            // Add new values
						var skinData = scope.getSkinLiveTestdata();

		                sensorData.data.push(skinData);
		                sensorData.path.attr('d', skinLine);

		        	    // Wenn der neue Wert größer ist als ein Threshold, ploppt was auf
		                if (skinData > threshold_skin) {
		                    console.log("Skin kritisch: ", skinData, "; Zeitpunkt: ", now)

		                    // kritischen Zeitpunkt speichern
                            // skin_crits.push(now) speichert das Datum des kritischen Werts
		                    console.log("Anzahl Skin Crits bisher: ", skin_crits.push(now));

		                    // damit man nicht vollgemüllt wird mit Notifications,
		                    // nur eine anzeigen, wenn die letzte min. 30 Sekunden her ist
		                    if ((now - old) >= machpause) {
		                        scope.showAlertSkin(autoclose * 1000);
		                        old = now
		                    }

		                    // hier müsste man den Punkt des Graphen orange machen
		                    // aber sensorData.path.attr("class", ...) macht halt alles orange

		                }

		                sensorData.path.attr("class", "sessionLine");              
		      
			            // Shift all domains
			            xScale.domain([now - (limit - 2) * duration, now - duration]);
			            yScale.domain([0, d3.max(sensorData.data)]);

			            // Slide all x-axis to the left
			            axis.transition()
			                .duration(duration)
			                .ease('linear')
			                .call(xScale.axis)

			            // Slide all paths to the left
			            paths.attr('transform', null)
			                .transition()
			                .duration(duration)
			                .ease('linear')
			                .attr('transform', 'translate(' + xScale(now - (limit - 1) * duration) + ')')
			                .each('end', addValuesToGraph)

			            // Remove oldest data point from each group
		                sensorData.data.shift();
	            	}
		        }

			}
		};
	}]);
}());
