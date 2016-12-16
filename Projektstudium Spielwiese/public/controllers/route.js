(function () {
  'use strict';

	angular.module('lisamon.controllers').controller('routeController', function($scope, $route, $location) {
		$scope.$on('$routeChangeSuccess', function() {
			var path = $location.path();
		    $scope.navmenuVisible = true;
		    if(path === '/login') {
		       $scope.navmenuVisible = false;
		       console.log("User: t  PW: t")
		    }
		});
	});
}());