(function () {
    'use strict';

    angular.module('lisamon.controllers')
	.controller('greetController', 
		['$scope', '$rootScope', '$localStorage', 
			function ($scope, $rootScope, $localStorage) {
	    		var hm = this;
	    		hm.username = $localStorage.currentUser.username;
	    		
	    		$scope.greetStartLearnsession = function() {
                    console.log("starting learnsession");
	                $rootScope.$broadcast("CallStartLearnsession", {});
	            }
	    		
	    		$scope.greetStopLearnsession = function() {
	                $rootScope.$broadcast("CallStopLearnsession", {});
	            }
	}]);
}());
