(function () {
  'use strict';
	angular.module('lisamon.controllers').controller('statisticController', 
		['$scope', 'statisticService',
			function($scope, statisticService) {
			    $scope.actualHeartData = statisticService.loadActualHeartRateStatistics();

			    $scope.actualEDAData = statisticService.loadActualEDAStatistics();

			    $scope.actualAccelerometerData = statisticService.loadActualAccelerometer();
	}]);
}());