(function () {
  'use strict';
	angular.module('lisamon.controllers').controller('detailsController', 
		['$scope', 'detailsService',
			function($scope, detailsService) {

				$scope.detailsData = detailsService.getDetailsData();

				$scope.detailsType = detailsService.getDetailsType();

				$scope.getHeartRateTestdata = detailsService.getHeartRateTestdata();

				$scope.getEDATestdata = detailsService.getEDATestdata();

				$scope.getAccelerometerTestdata = detailsService.getAccelerometerTestdata();
			    
	}]);
}());