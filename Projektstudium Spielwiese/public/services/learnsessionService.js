(function () {
'use strict';
	angular.module('lisamon.services').service("learnsessionService", function() {

        // test data
        var learnsessions = [   
                {id: 1, start: "2016-11-11T10:15:00", end: "2016-11-11T13:30:00"},
                {id: 2, start: "2016-11-12T11:25:00", end: "2016-11-12T16:00:00"},
                {id: 3, start: "2016-11-13T09:00:00", end: "2016-11-13T13:45:00"},
                {id: 4, start: "2016-11-14T12:00:00", end: "2016-11-14T16:00:00"}
            ];

        this.getPulseTestdata = function() {
            var hData = [   
                {date: 1, value: 60}, {date: 2, value: 2}, {date: 3, value: 3}, {date: 4, value: 4}, {date: 5, value: 60}, {date: 6, value: 2}, {date: 7, value: 4}, 
                {date: 8, value: 2}, {date: 9, value: 62}, {date: 10, value: 3}, {date: 11, value: 4}, {date: 12, value: 4}, {date: 13, value: 63}, {date: 14, value: 60}, 
                {date: 15, value: 2}, {date: 16, value: 5}, {date: 17, value: 8}, {date: 18, value: 60}, {date: 19, value: 5}, {date: 20, value: 3}, {date: 21, value: 2}, 
                {date: 22, value: 72}, {date: 23, value: 60}, {date: 24, value: 3}, {date: 25, value: 4}, {date: 26, value: 6}, {date: 27, value: 68}, {date: 28, value: 60}, 
                {date: 29, value: 6}, {date: 30, value: 8}, {date: 31, value: 4}, {date: 32, value: 62}, {date: 33, value: 5}, {date: 34, value: 3}, {date: 35, value: 4}, 
                {date: 36, value: 72}, {date: 37, value: 64}, {date: 38, value: 3}, {date: 39, value: 6}, {date: 40, value: 8}, {date: 41, value: 64}, {date: 42, value: 60}, 
                {date: 43, value: 6}, {date: 44, value: 8}, {date: 45, value: 3}, {date: 46, value: 61}, {date: 47, value: 78}, {date: 48, value: 4}, {date: 49, value: 4}, 
                {date: 50, value: 2},
                {date: 51, value: 72}, {date: 52, value: 64}, {date: 53, value: 3}, {date: 54, value: 6}, {date: 55, value: 8}, {date: 56, value: 64}, {date: 57, value: 60}, 
                {date: 58, value: 6}, {date: 59, value: 8}, {date: 60, value: 3}, {date: 61, value: 61}, {date: 62, value: 78}, {date: 63, value: 4}, {date: 64, value: 4}
            ];

            return hData;
        }

        this.getPulseLiveTestdata = function() {
            var data = 20 + Math.random() * 100;

            return data;
        }

        this.getSkinLiveTestdata = function() {
            var data = 20 + Math.random() * 100;

            return data;
        }

        this.getAccelerationLiveTestdata = function() {
            var data = 20 + Math.random() * 100;

            return data;
        }

        /*
            Return all existing learn sessions
        */
		this.getAllLearnsessions = function() {
			return learnsessions;
		};

        /*
            Return all learn sessions which began and ended in the given time range
        */
        this.getLearnsessionByDateRange = function(startDate, endDate) {
            var lsessions = [];

            learnsessions.forEach(function(entry) {
                if (entry.start >= startDate && entry.end <= endDate) {
                    lsessions.push(entry);
                }
            });

            return lsessions;
        };
        
        /*
        Return all learn sessions which began at a special date
	    */
	    this.getLearnsessionByDate = function(date) {
	        var lsessions = [];
	
	        learnsessions.forEach(function(entry) {
	            if (entry.start == date) {
	                lsessions.push(entry);
	            }
	        });
	
	        return lsessions;
	    };

        /*
            Return the learn session with the given id
        */
        this.getLearnsessionData = function(learnsessionID) {

        }
        
        /*
         * Return date of a specific learnsessionID
         */
        this.getLearnsessionDate = function(learnsessionID){
            var date;

            learnsessions.forEach(function(entry) {
                if (entry.id == learnsessionID) {
                    date = entry.start;
                }
            });

            return date;
        }

	});
}());
