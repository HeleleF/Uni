(function () {
  'use strict';

	angular.module('lisamon.controllers')
	.controller('navmenu', function($scope) {
	    $scope.hidden = true;
	    $scope.toggle = function() {
	        $scope.hidden = !($scope.hidden);
	    };
	});
}());