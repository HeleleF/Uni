(function () {
  'use strict';
	angular.module('lisamon.controllers').controller('statisticController', 
		['$scope', 'statisticService', 'detailsService',
			function($scope, statisticService, detailsService) {

			    $scope.getActualHeartData = statisticService.loadActualHeartRateStatistics();

			    $scope.getActualEDAData = statisticService.loadActualEDAStatistics();

			    $scope.getActualAccelerometerData = statisticService.loadActualAccelerometer();

			    /*
			    *	Transfer the selected hear rate data to the detailsService and call the Details view
			    */ 
			    $scope.setHeartRateDetails = function(hRateDetails) {
			    	detailsService.setDetailsData("assets/icons/heart_blue.svg", hRateDetails);
			    };

			    /*
			    *	Transfer the selected eda data to the detailsService and call the Details view
			    */
			    $scope.setEDADetails = function(edaDetails) {
			    	detailsService.setDetailsData("assets/icons/skin_blue.svg", edaDetails);
			    };

			    /*
			    *	Transfer the selected accelerator data to the detailsService and call the Details view
			    */ 
			    $scope.setAcceleratorDetails = function(accelerometerDetails) {
			    	detailsService.setDetailsData("assets/icons/arrow_blue.svg", accelerometerDetails);
			    }

				/*
				* Returns the number of notifications for the learnsession and graph type
				*/
				$scope.getNotificationsOfLearnsession = function() {
		            var min = 0;
		            var max = 12;
		            
		            return Math.floor(Math.random() * (max - min) + min);
		        };

	}]);
}());