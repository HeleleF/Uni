(function () {
  'use strict';

	angular.module('lisamon.directives').directive('learnsessionpulse', ['$window', '$timeout', function($window, $timeout) {

		var chartHeight;
		var chartWidth;
		var chartMargin;

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

        var pulseLine;

        var svgPulse;

        var old = 0;
        var threshold_pulse = 118; // einfacher Grenzwert
        var machpause = 30000; // nur alle 30 Sekunden anzeigen
        var autoclose = 5; // Notification schließt sich nach 5 Sekunden selbst

        var pulse_crits = []; // Sammlung aller kritischen Zeitpunkte
        
		return {
			restrict: "EA", // element names and attribute names can invoke directive
			link: function(scope, elem, attr) {
			
              	// watch for data changes and re-render
		        scope.$watch('sessionIsRunning', function() {

		        	if (scope.clearPulseSessionData  == true) {
			        	clearData();
		         	}
		        
		        	// just draw the graph if the session is running
		         	if (scope.sessionIsRunning) {
		         		scope.clearPulseSession(false);

			        	setChartAttributes();
						drawPulseGraph();

	          			addValuesToGraph();
		         	}
		    
		        });

		        function clearData() {
		         	d3.select('#chartPulse >*').remove();

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

		        	chartMargin = (window.innerWidth / 100) * 2;
		        }

		        function drawPulseGraph() {
		        	xScale = d3.time.scale()
			            .domain([now - (limit - 2), now - duration])
			            .range([0, chartWidth])

			        yScale = d3.scale.linear()
			            .domain([0, d3.max(sensorData.data)])
			            .range([chartHeight, 0])

			        pulseLine = d3.svg.line()
			            .interpolate('basis')
			            .x(function(d, i) {
			                return xScale(now - (limit - 1 - i) * duration)
			            })
			            .y(function(d) {
			                return yScale(d)
			            })

			        svgPulse = d3.select('#chartPulse')
				        .attr("width", chartWidth)
				        .attr("height", chartHeight)
				        	.append("g").attr("transform", "translate(" + 30 + "," + 30 + ")");
				 

			        paths = svgPulse.append('g')

		            sensorData.path = paths.append('path')
		                .data([sensorData.data])		                
		        }

		        function addValuesToGraph() {
		        	if (scope.sessionIsRunning) {
						now = new Date();

			            // Add new values
						var pulseData = scope.getPulseLiveTestdata();

						sensorData.data.push(pulseData);
						sensorData.path.attr('d', pulseLine);

		        	    // Wenn der neue Wert größer ist als ein Threshold, ploppt was auf
						if (pulseData > threshold_pulse) {
						    console.log("Pulse kritisch: ", pulseData, "; Zeitpunkt: ", now)

						    // kritischen Zeitpunkt speichern
						    // pulse_crits.push(now) speichert das Datum des kritischen Werts
						    console.log("Anzahl Pulse Crits bisher: ", pulse_crits.push(now));

						    // damit man nicht vollgemüllt wird mit Notifications,
						    // nur eine anzeigen, wenn die letzte min. 30 Sekunden her ist
						    if ((now - old) >= machpause) {
						        scope.showAlertPulse(autoclose * 1000);
						        old = now
						    }

						    // hier müsste man den Punkt des Graphen orange machen
						    // aber sensorData.path.attr("class", ...) macht halt alles orange
						}

						sensorData.path.attr("class", "sessionLine");
		                
			            // Shift all domains
			            xScale.domain([now - (limit - 2) * duration, now - duration]);
			            yScale.domain([0, d3.max(sensorData.data)]);	      

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
