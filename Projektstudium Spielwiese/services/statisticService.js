(function () {
'use strict';
	angular.module('lisamon.services').service("statisticService", function() {
		this.loadActualHeartRateStatistics = function() {
			var hData = [	
            	{values: [60,2,3,4,60,2,4,2,62,3,4,4,63,60,2,3,4,60,2,4,2,62,3,4,4,63,60,2,3,4,60,2,4,2,62,3,4,4,63,
            		60,2,3,4,60,2,4,2,62,3,4,4,63,60,2,3,4,60,2,4,2,62,3,4,4,63,60,2,3,4,60,2,4,2,62,3,4,4,63,
            		60,2,3,4,60,2,4,2,62,3,4,4,63,60,2,3,4,60,2,4,2,62,3,4,4,63]}
            ];

            return hData;
		};

		this.loadActualEDAStatistics = function() {
			var eData = [
                {values: [10,8,7,8,9,31,33,34,24,21,18,22,25,26,27,28,32,35,42,41,
                	39,38,27,25,24,19,17,18,19,20,24,25,26,28,29,31,33,28,39,40]}
            ];

            return eData;
		};

		this.loadActualAccelerometer = function() {
			var aData = [
                {values: [1,1,1,2,2,2,2,3,3,3,3,4,5,5,5,5,9,9,9]}
            ];

            return aData;
		};

	});
}());