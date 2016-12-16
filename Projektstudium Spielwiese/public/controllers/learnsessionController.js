(function () {
  'use strict';
	angular.module('lisamon.controllers').controller('learnsessionController', 
		['$scope', '$rootScope', 'learnsessionService', '$location',
			function ($scope, $rootScope, learnsessionService, $location) {

			    // lässt das HerzOverlay erscheinen
			    $scope.showAlertPulse = function (autoclose) {
			        document.getElementById("pulseOver").style.height = "100%";
			        window.setTimeout($scope.closePulse, autoclose);
			    };

			    // lässt das HautOverlay erscheinen
			    $scope.showAlertSkin = function (autoclose) {
			        document.getElementById("skinOver").style.height = "100%";
			        window.setTimeout($scope.closeSkin, autoclose);
			    };

			    // lässt das HerzOverlay wieder verschwinden
			    $scope.closePulse = function () {
			        document.getElementById("pulseOver").style.height = "0%";
			    };

			    // lässt das HautOverlay wieder verschwinden
			    $scope.closeSkin = function () {
			        document.getElementById("skinOver").style.height = "0%";
			    };

				$rootScope.sessionIsRunning;

				// This ( + clearSkinSession, clearPulseSession) seems like an unclean hack - Search for other solution!
				$rootScope.clearPulseSessionData;
				$rootScope.clearSkinSessionData;
				
				$rootScope.$on("CallStartLearnsession", function() {
					$scope.startLearnsession();
			    });
				
				$rootScope.$on("CallStopLearnsession", function(){
					$scope.stopLearnsession();
			    });

				// will be called from the directives
			    $scope.clearPulseSession = function(bool) {
			    	$rootScope.clearPulseSessionData = bool;
			    } 

			    // will be called from the directives
			    $scope.clearSkinSession = function(bool) {
			    	$rootScope.clearSkinSessionData = bool;
			    } 

				$scope.startLearnsession = function() {
					$rootScope.sessionIsRunning = true;
					
					window.location.href='#/learn';
				}

				$rootScope.stopLearnsession = function() {
					$rootScope.sessionIsRunning = false;
					$rootScope.clearPulseSessionData = true;
					$rootScope.clearSkinSessionData = true;
				}

				$scope.getPulseLiveTestdata = function() {
					var pulseData = learnsessionService.getPulseLiveTestdata();

					return pulseData;				
				}

				$scope.getSkinLiveTestdata = function() {
					var skinData = learnsessionService.getSkinLiveTestdata();
			
					return skinData;
				}

				$scope.getAccelerationLiveTestdata = function() {
					var accelerationData = learnsessionService.getAccelerationLiveTestdata();
			
					return accelerationData;				
				}

			    $scope.getLearnsessionByDateRange = function(startDate, endDate) {
			    	return learnsessionService.getLearnsessionByDateRange(startDate, endDate);
			    }
			    
			    $scope.getLearnsessionByDate = function(date) {
			    	return learnsessionService.getLearnsessionByDate(date);
			    }

			    $scope.getAllLearnsessions = function() {
			    	return learnsessionService.getAllLearnsessions();
			    }
			    
			    
			    $scope.goToLearnsessiondetails = function(learnsession_id) {
			    	$location.path('/learnsession/'+learnsession_id);
			   }
	}]);
}());
