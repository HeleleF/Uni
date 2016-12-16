(function () {
  'use strict';
	angular.module('lisamon.controllers').controller('learnsessiondetailsController', 
		['$scope', 'learnsessiondetailsService', 'detailsService', 'learnsessionService','$routeParams',
			function($scope, learnsessiondetailsService, detailsService, learnsessionService, $routeParams) {

				$scope.currentID = $routeParams.learnsessionID;
				
				$scope.currentDate = learnsessionService.getLearnsessionDate($routeParams.learnsessionID);

				$scope.getHeartRateDataOfLearnsession = learnsessiondetailsService.loadHeartRateDataOfLearnsession($routeParams.learnsessionID);

				$scope.getEDADataOfLearnsession = learnsessiondetailsService.loadEDADataOfLearnsession($routeParams.learnsessionID);

				$scope.getAccelerometerDataOfLearnsession = learnsessiondetailsService.loadAccelerometerDataOfLearnsession($routeParams.learnsessionID);

			    /*
			    *	Transfer the selected heart rate data to the detailsService and call the Details view
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

	}]);
}());