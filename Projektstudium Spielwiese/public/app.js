(function () {
  'use strict';

  // create the angular app
  angular.module('lisamon', [
    'lisamon.controllers',
    'lisamon.directives',
    'lisamon.services',
    'ngRoute',
    'ngStorage',
    'ngMessages',
    'ngStorage',
    'ngMockE2E'
    ]).config(function($routeProvider) {
    $routeProvider
        .when("/", {
            templateUrl: "views/greet.html",
            controller: 'greetController',
            controllerAs: 'hm'
        })
        .when("/learn", {
            templateUrl: "views/learn.html"
        })
        .when("/stats", {
            templateUrl: "views/stats.html"
        })
        .when("/details", {
            templateUrl: "views/details.html"
        })
        .when("/options", {
            templateUrl: "views/options.html"
        })
        .when("/history", {
            templateUrl: "views/learnsessionHistory.html"
        })
        .when("/learnsession/:learnsessionID", {
            templateUrl: "views/learnsessionDetails.html"
        })
        .when("/login", {
            templateUrl: "views/login.html",
            controller : 'logincController',
			controllerAs : 'vm'
        });
}).run(run);
  
  function run($rootScope, $http, $location, $localStorage) {
		// keep user logged in after page refresh
		if ($localStorage.currentUser) {
			$http.defaults.headers.common.Authorization = 'Bearer '
					+ $localStorage.currentUser.token;
		}

		// redirect to login page if not logged in and trying to access a
		// restricted page
		$rootScope.$on('$locationChangeStart', function(event, next, current) {
			var publicPages = [ '/login' ];
			var restrictedPage = publicPages.indexOf($location.path()) === -1;
			if (restrictedPage && !$localStorage.currentUser) {
				$location.path('/login');
			}
		});
	}

  // setup dependency injection
  //angular.module('d3', []);
  angular.module('lisamon.services', []);
  angular.module('lisamon.controllers', []);
  angular.module('lisamon.directives', []);

}());
