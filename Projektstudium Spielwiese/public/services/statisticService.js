(function () {
'use strict';
	angular.module('lisamon.services').service("statisticService", function() {
		this.loadActualHeartRateStatistics = function() {
           
            var hData = [   
                {date: 1, value: 60}, {date: 2, value: 2}, {date: 3, value: 3}, {date: 4, value: 4}, {date: 5, value: 60}, {date: 6, value: 2}, {date: 7, value: 4}, 
                {date: 8, value: 2}, {date: 9, value: 62}, {date: 10, value: 3}, {date: 11, value: 4}, {date: 12, value: 4}, {date: 13, value: 63}, {date: 14, value: 60}, 
                {date: 15, value: 2}, {date: 16, value: 5}, {date: 17, value: 8}, {date: 18, value: 60}, {date: 19, value: 5}, {date: 20, value: 3}, {date: 21, value: 2}, 
                {date: 22, value: 72}, {date: 23, value: 60}, {date: 24, value: 3}, {date: 25, value: 4}, {date: 26, value: 6}, {date: 27, value: 68}, {date: 28, value: 60}, 
                {date: 29, value: 6}, {date: 30, value: 8}, {date: 31, value: 4}, {date: 32, value: 62}, {date: 33, value: 5}, {date: 34, value: 3}, {date: 35, value: 4}, 
                {date: 36, value: 72}, {date: 37, value: 64}, {date: 38, value: 3}, {date: 39, value: 6}, {date: 40, value: 8}, {date: 41, value: 64}, {date: 42, value: 60}, 
                {date: 43, value: 6}, {date: 44, value: 8}, {date: 45, value: 3}, {date: 46, value: 61}, {date: 47, value: 78}, {date: 48, value: 4}, {date: 49, value: 4}, 
                {date: 50, value: 2},
                {date: 51, value: 72}, {date: 52, value: 64}, {date: 53, value: 3}, {date: 54, value: 6}, {date: 55, value: 8}, {date: 56, value: 64}, {date: 57, value: 60}, 
                {date: 58, value: 6}, {date: 59, value: 8}, {date: 60, value: 3}, {date: 61, value: 61}, {date: 62, value: 78}, {date: 63, value: 4}, {date: 64, value: 4}
            ];

            return hData;
		};

		this.loadActualEDAStatistics = function() {

            var eData = [   
                {date: 1, value: 10}, {date: 2, value: 8}, {date: 3, value: 7}, {date: 4, value: 8}, {date: 5, value: 9}, {date: 6, value: 31}, {date: 7, value: 33}, 
                {date: 8, value: 34}, {date: 9, value: 24}, {date: 10, value: 21}, {date: 11, value: 18}, {date: 12, value: 22}, {date: 13, value: 25}, {date: 14, value: 26}, 
                {date: 15, value: 27}, {date: 16, value: 28}, {date: 17, value: 32}, {date: 18, value: 35}, {date: 19, value: 42}, {date: 20, value: 41}, {date: 21, value: 39}, 
                {date: 22, value: 38}, {date: 23, value: 27}, {date: 24, value: 25}, {date: 25, value: 24}, {date: 26, value: 19}, {date: 27, value: 17}, {date: 28, value: 18}, 
                {date: 29, value: 19}, {date: 30, value: 20}, {date: 31, value: 24}, {date: 32, value: 25}, {date: 33, value: 26}, {date: 34, value: 28}, {date: 35, value: 29}, 
                {date: 36, value: 31}, {date: 37, value: 33}, {date: 38, value: 28}, {date: 39, value: 39}, {date: 40, value: 40}
            ];

            return eData;
		};

		this.loadActualAccelerometer = function() {

            var aData = [
                {date: 1, value: 1}, {date: 2, value: 1}, {date: 3, value: 1}, {date: 4, value: 2}, {date: 5, value: 2}, {date: 6, value: 2}, {date: 7, value: 2}, 
                {date: 8, value: 3}, {date: 9, value: 3}, {date: 10, value: 3}, {date: 11, value: 3}, {date: 12, value: 4}, {date: 13, value: 5}, {date: 14, value: 5},
                {date: 15, value: 5}, {date: 16, value: 5}, {date: 17, value: 5}, {date: 18, value: 8}, {date: 19, value: 9}, {date: 20, value: 9}, {date: 21, value: 9}
            ];

            return aData;
		};

        this.loadHeartRateDataOfLearnsession = function(learnsessionID) {
            console.log("Get heart rate data for learnsession: " + learnsessionID);
        }

	});
}());