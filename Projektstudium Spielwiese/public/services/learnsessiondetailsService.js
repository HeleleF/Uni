(function () {
	'use strict';
	angular.module('lisamon.services').service("learnsessiondetailsService", function() {

        this.loadHeartRateDataOfLearnsession = function(learnsessionID) {
            var hData = [ 
            	{id: 1, data: [
	                {date: 1, value: 60}, {date: 2, value: 2}, {date: 3, value: 3}, {date: 4, value: 4}, {date: 5, value: 60}, {date: 6, value: 2}, {date: 7, value: 4}, 
	                {date: 8, value: 2}, {date: 9, value: 62}, {date: 10, value: 3}, {date: 11, value: 4}, {date: 12, value: 4}, {date: 13, value: 63}, {date: 14, value: 60}, 
	                {date: 15, value: 2}, {date: 16, value: 5}, {date: 17, value: 8}, {date: 18, value: 60}, {date: 19, value: 5}, {date: 20, value: 3}, {date: 21, value: 2}, 
	                {date: 22, value: 72}, {date: 23, value: 60}, {date: 24, value: 3}, {date: 25, value: 4}, {date: 26, value: 6}, {date: 27, value: 68}, {date: 28, value: 60}, 
	                {date: 29, value: 6}, {date: 30, value: 8}, {date: 31, value: 4}, {date: 32, value: 62}, {date: 33, value: 5}, {date: 34, value: 3}, {date: 35, value: 4}, 
	                {date: 36, value: 72}, {date: 37, value: 64}, {date: 38, value: 3}, {date: 39, value: 6}, {date: 40, value: 8}, {date: 41, value: 64}, {date: 42, value: 60}, 
	                {date: 43, value: 6}, {date: 44, value: 8}, {date: 45, value: 3}, {date: 46, value: 61}, {date: 47, value: 78}, {date: 48, value: 4}, {date: 49, value: 4}, 
	                {date: 50, value: 2},
	                {date: 51, value: 72}, {date: 52, value: 64}, {date: 53, value: 3}, {date: 54, value: 6}, {date: 55, value: 8}, {date: 56, value: 64}, {date: 57, value: 60}, 
	                {date: 58, value: 6}, {date: 59, value: 8}, {date: 60, value: 3}, {date: 61, value: 61}, {date: 62, value: 78}, {date: 63, value: 4}, {date: 64, value: 4}]},
            	{id: 2, data: [
	                {date: 1, value: 50}, {date: 2, value: 10}, {date: 3, value: 1}, {date: 4, value: 1}, {date: 5, value: 78}, {date: 6, value: 10}, {date: 7, value: 1}, 
	                {date: 8, value: 10}, {date: 9, value: 56}, {date: 10, value: 1}, {date: 11, value: 1}, {date: 12, value: 1}, {date: 13, value: 70}, {date: 14, value: 70}, 
	                {date: 15, value: 10}, {date: 16, value: 3}, {date: 17, value: 1}, {date: 18, value: 75}, {date: 19, value: 20}, {date: 20, value: 1}, {date: 21, value: 10}, 
	                {date: 22, value: 72}, {date: 23, value: 58}, {date: 24, value: 1}, {date: 25, value: 1}, {date: 26, value: 6}, {date: 27, value: 78}, {date: 28, value: 68}, 
	                {date: 29, value: 6}, {date: 30, value: 8}, {date: 31, value: 1}, {date: 32, value: 62}, {date: 33, value: 5}, {date: 34, value: 1}, {date: 35, value: 1}, 
	                {date: 36, value: 72}, {date: 37, value: 76}, {date: 38, value: 1}, {date: 39, value: 6}, {date: 40, value: 8}, {date: 41, value: 54}, {date: 42, value: 67}, 
	                {date: 43, value: 6}, {date: 44, value: 8}, {date: 45, value: 1}, {date: 46, value: 78}, {date: 47, value: 78}, {date: 48, value: 1}, {date: 49, value: 1}, 
	                {date: 50, value: 10},
	                {date: 51, value: 72}, {date: 52, value: 67}, {date: 53, value: 1}, {date: 54, value: 6}, {date: 55, value: 8}, {date: 56, value: 76}, {date: 57, value: 64}, 
	                {date: 58, value: 6}, {date: 59, value: 8}, {date: 60, value: 1}, {date: 61, value: 61}, {date: 62, value: 78}, {date: 63, value: 4}, {date: 64, value: 4}]},
                {id: 3, data: [
	                {date: 1, value: 60}, {date: 2, value: 2}, {date: 3, value: 3}, {date: 4, value: 4}, {date: 5, value: 60}, {date: 6, value: 2}, {date: 7, value: 4}, 
	                {date: 8, value: 2}, {date: 9, value: 62}, {date: 10, value: 3}, {date: 11, value: 4}, {date: 12, value: 4}, {date: 13, value: 63}, {date: 14, value: 60}, 
	                {date: 15, value: 2}, {date: 16, value: 5}, {date: 17, value: 8}, {date: 18, value: 60}, {date: 19, value: 5}, {date: 20, value: 3}, {date: 21, value: 2}, 
	                {date: 22, value: 72}, {date: 23, value: 60}, {date: 24, value: 3}, {date: 25, value: 4}, {date: 26, value: 6}, {date: 27, value: 68}, {date: 28, value: 60}, 
	                {date: 29, value: 6}, {date: 30, value: 8}, {date: 31, value: 4}, {date: 32, value: 62}, {date: 33, value: 5}, {date: 34, value: 3}, {date: 35, value: 4}, 
	                {date: 36, value: 72}, {date: 37, value: 64}, {date: 38, value: 3}, {date: 39, value: 6}, {date: 40, value: 8}, {date: 41, value: 64}, {date: 42, value: 60}, 
	                {date: 43, value: 6}, {date: 44, value: 8}, {date: 45, value: 3}, {date: 46, value: 61}, {date: 47, value: 78}, {date: 48, value: 4}, {date: 49, value: 4}, 
	                {date: 50, value: 2},
	                {date: 51, value: 72}, {date: 52, value: 64}, {date: 53, value: 3}, {date: 54, value: 6}, {date: 55, value: 8}, {date: 56, value: 64}, {date: 57, value: 60}, 
	                {date: 58, value: 6}, {date: 59, value: 8}, {date: 60, value: 3}, {date: 61, value: 61}, {date: 62, value: 78}, {date: 63, value: 4}, {date: 64, value: 4}]},
            	{id: 4, data: [
	                {date: 1, value: 50}, {date: 2, value: 10}, {date: 3, value: 1}, {date: 4, value: 1}, {date: 5, value: 78}, {date: 6, value: 10}, {date: 7, value: 1}, 
	                {date: 8, value: 10}, {date: 9, value: 56}, {date: 10, value: 1}, {date: 11, value: 1}, {date: 12, value: 1}, {date: 13, value: 70}, {date: 14, value: 70}, 
	                {date: 15, value: 10}, {date: 16, value: 3}, {date: 17, value: 1}, {date: 18, value: 75}, {date: 19, value: 20}, {date: 20, value: 1}, {date: 21, value: 10}, 
	                {date: 22, value: 72}, {date: 23, value: 58}, {date: 24, value: 1}, {date: 25, value: 1}, {date: 26, value: 6}, {date: 27, value: 78}, {date: 28, value: 68}, 
	                {date: 29, value: 6}, {date: 30, value: 8}, {date: 31, value: 1}, {date: 32, value: 62}, {date: 33, value: 5}, {date: 34, value: 1}, {date: 35, value: 1}, 
	                {date: 36, value: 72}, {date: 37, value: 76}, {date: 38, value: 1}, {date: 39, value: 6}, {date: 40, value: 8}, {date: 41, value: 54}, {date: 42, value: 67}, 
	                {date: 43, value: 6}, {date: 44, value: 8}, {date: 45, value: 1}, {date: 46, value: 78}, {date: 47, value: 78}, {date: 48, value: 1}, {date: 49, value: 1}, 
	                {date: 50, value: 10},
	                {date: 51, value: 72}, {date: 52, value: 67}, {date: 53, value: 1}, {date: 54, value: 6}, {date: 55, value: 8}, {date: 56, value: 76}, {date: 57, value: 64}, 
	                {date: 58, value: 6}, {date: 59, value: 8}, {date: 60, value: 1}, {date: 61, value: 61}, {date: 62, value: 78}, {date: 63, value: 4}, {date: 64, value: 4}]}
            ];
 
	        var actualHData = [];
	    	
	        hData.forEach(function(entry) {
	            if (entry.id == learnsessionID) {
	            	actualHData = entry.data;
	            }
	        });
	
	        return actualHData;            
        };
        
        this.loadEDADataOfLearnsession = function(learnsessionID) {
        	
        	var eData = [ 
        		{id: 1, data: [
	                {date: 1, value: 10}, {date: 2, value: 8}, {date: 3, value: 7}, {date: 4, value: 8}, {date: 5, value: 9}, {date: 6, value: 31}, {date: 7, value: 33}, 
	                {date: 8, value: 34}, {date: 9, value: 24}, {date: 10, value: 21}, {date: 11, value: 18}, {date: 12, value: 22}, {date: 13, value: 25}, {date: 14, value: 26}, 
	                {date: 15, value: 27}, {date: 16, value: 28}, {date: 17, value: 32}, {date: 18, value: 35}, {date: 19, value: 42}, {date: 20, value: 41}, {date: 21, value: 39}, 
	                {date: 22, value: 38}, {date: 23, value: 27}, {date: 24, value: 25}, {date: 25, value: 24}, {date: 26, value: 19}, {date: 27, value: 17}, {date: 28, value: 18}, 
	                {date: 29, value: 19}, {date: 30, value: 20}, {date: 31, value: 24}, {date: 32, value: 25}, {date: 33, value: 26}, {date: 34, value: 28}, {date: 35, value: 29}, 
	                {date: 36, value: 31}, {date: 37, value: 33}, {date: 38, value: 28}, {date: 39, value: 39}, {date: 40, value: 40}]},
        		{id: 2, data: [
                    {date: 1, value: 30}, {date: 2, value: 28}, {date: 3, value: 2}, {date: 4, value: 3}, {date: 5, value: 4}, {date: 6, value: 50}, {date: 7, value: 60}, 
                    {date: 8, value: 45}, {date: 9, value: 24}, {date: 10, value: 20}, {date: 11, value: 8}, {date: 12, value: 3}, {date: 13, value: 6}, {date: 14, value: 10}, 
                    {date: 15, value: 30}, {date: 16, value: 28}, {date: 17, value: 32}, {date: 18, value: 56}, {date: 19, value: 67}, {date: 20, value: 54}, {date: 21, value: 39}, 
                    {date: 22, value: 56}, {date: 23, value: 27}, {date: 24, value: 15}, {date: 25, value: 14}, {date: 26, value: 9}, {date: 27, value: 17}, {date: 28, value: 38}, 
                    {date: 29, value: 39}, {date: 30, value: 40}, {date: 31, value: 44}, {date: 32, value: 45}, {date: 33, value: 46}, {date: 34, value: 48}, {date: 35, value: 49}, 
                    {date: 36, value: 61}, {date: 37, value: 63}, {date: 38, value: 58}, {date: 39, value: 59}, {date: 40, value: 50}]},
        		{id: 3, data: [
	                {date: 1, value: 10}, {date: 2, value: 8}, {date: 3, value: 7}, {date: 4, value: 8}, {date: 5, value: 9}, {date: 6, value: 31}, {date: 7, value: 33}, 
	                {date: 8, value: 34}, {date: 9, value: 24}, {date: 10, value: 21}, {date: 11, value: 18}, {date: 12, value: 22}, {date: 13, value: 25}, {date: 14, value: 26}, 
	                {date: 15, value: 27}, {date: 16, value: 28}, {date: 17, value: 32}, {date: 18, value: 35}, {date: 19, value: 42}, {date: 20, value: 41}, {date: 21, value: 39}, 
	                {date: 22, value: 38}, {date: 23, value: 27}, {date: 24, value: 25}, {date: 25, value: 24}, {date: 26, value: 19}, {date: 27, value: 17}, {date: 28, value: 18}, 
	                {date: 29, value: 19}, {date: 30, value: 20}, {date: 31, value: 24}, {date: 32, value: 25}, {date: 33, value: 26}, {date: 34, value: 28}, {date: 35, value: 29}, 
	                {date: 36, value: 31}, {date: 37, value: 33}, {date: 38, value: 28}, {date: 39, value: 39}, {date: 40, value: 40}]},
        		{id: 4, data: [
                    {date: 1, value: 30}, {date: 2, value: 28}, {date: 3, value: 2}, {date: 4, value: 3}, {date: 5, value: 4}, {date: 6, value: 50}, {date: 7, value: 60}, 
                    {date: 8, value: 45}, {date: 9, value: 24}, {date: 10, value: 20}, {date: 11, value: 8}, {date: 12, value: 3}, {date: 13, value: 6}, {date: 14, value: 10}, 
                    {date: 15, value: 30}, {date: 16, value: 28}, {date: 17, value: 32}, {date: 18, value: 56}, {date: 19, value: 67}, {date: 20, value: 54}, {date: 21, value: 39}, 
                    {date: 22, value: 56}, {date: 23, value: 27}, {date: 24, value: 15}, {date: 25, value: 14}, {date: 26, value: 9}, {date: 27, value: 17}, {date: 28, value: 38}, 
                    {date: 29, value: 39}, {date: 30, value: 40}, {date: 31, value: 44}, {date: 32, value: 45}, {date: 33, value: 46}, {date: 34, value: 48}, {date: 35, value: 49}, 
                    {date: 36, value: 61}, {date: 37, value: 63}, {date: 38, value: 58}, {date: 39, value: 59}, {date: 40, value: 50}]}                    
            ];
        	
	        var actualEData = [];
	    	
	        eData.forEach(function(entry) {
	            if (entry.id == learnsessionID) {
	            	actualEData = entry.data;
	            }
	        });
	
	        return actualEData; 
        };
        
		this.loadAccelerometerDataOfLearnsession = function(learnsessionID) {

            var aData = [
            	{id: 1, data: [
	                {date: 1, value: 1}, {date: 2, value: 1}, {date: 3, value: 1}, {date: 4, value: 2}, {date: 5, value: 2}, {date: 6, value: 2}, {date: 7, value: 2}, 
	                {date: 8, value: 3}, {date: 9, value: 3}, {date: 10, value: 3}, {date: 11, value: 3}, {date: 12, value: 4}, {date: 13, value: 5}, {date: 14, value: 5},
	                {date: 15, value: 5}, {date: 16, value: 5}, {date: 17, value: 5}, {date: 18, value: 8}, {date: 19, value: 9}, {date: 20, value: 9}, {date: 21, value: 9}]},
	            {id: 2, data: [
	                {date: 1, value: 20}, {date: 2, value: 20}, {date: 3, value: 20}, {date: 4, value: 30}, {date: 5, value: 30}, {date: 6, value: 30}, {date: 7, value: 30}, 
	                {date: 8, value: 40}, {date: 9, value: 40}, {date: 10, value: 40}, {date: 11, value: 40}, {date: 12, value: 40}, {date: 13, value: 50}, {date: 14, value: 50},
	                {date: 15, value: 50}, {date: 16, value: 50}, {date: 17, value: 50}, {date: 18, value: 55}, {date: 19, value: 60}, {date: 20, value: 60}, {date: 21, value: 60}]},
                {id: 3, data: [
	                {date: 1, value: 1}, {date: 2, value: 1}, {date: 3, value: 1}, {date: 4, value: 2}, {date: 5, value: 2}, {date: 6, value: 2}, {date: 7, value: 2}, 
	                {date: 8, value: 3}, {date: 9, value: 3}, {date: 10, value: 3}, {date: 11, value: 3}, {date: 12, value: 4}, {date: 13, value: 5}, {date: 14, value: 5},
	                {date: 15, value: 5}, {date: 16, value: 5}, {date: 17, value: 5}, {date: 18, value: 8}, {date: 19, value: 9}, {date: 20, value: 9}, {date: 21, value: 9}]},
	            {id: 4, data: [
	                {date: 1, value: 20}, {date: 2, value: 20}, {date: 3, value: 20}, {date: 4, value: 30}, {date: 5, value: 30}, {date: 6, value: 30}, {date: 7, value: 30}, 
	                {date: 8, value: 40}, {date: 9, value: 40}, {date: 10, value: 40}, {date: 11, value: 40}, {date: 12, value: 40}, {date: 13, value: 50}, {date: 14, value: 50},
	                {date: 15, value: 50}, {date: 16, value: 50}, {date: 17, value: 50}, {date: 18, value: 55}, {date: 19, value: 60}, {date: 20, value: 60}, {date: 21, value: 60}]}
            ];
        	
	        var actualAData = [];
	    	
	        aData.forEach(function(entry) {
	            if (entry.id == learnsessionID) {
	            	actualAData = entry.data;
	            }
	        });
	
	        return actualAData;             
		}

	});
}());