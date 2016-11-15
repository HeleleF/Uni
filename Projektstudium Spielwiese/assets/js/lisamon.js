angular.module("lisamon", ["ngRoute"])
.config(function($routeProvider) {
    $routeProvider
        .when("/", {
            templateUrl: "views/greet_entry.html"
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
        });
});
