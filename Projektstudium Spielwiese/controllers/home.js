(function () {
    'use strict';

    angular.module('lisamon.controllers')
	.controller('home', function ($localStorage) {
	    var hm = this;
	    hm.username = $localStorage.currentUser.username;
	});
}());
