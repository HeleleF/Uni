(function () {
'use strict';
	angular.module('lisamon.services').service("detailsService", function() {

		this.detailType;
		this.detailsData;

		this.setDetailsData = function(dType, dData) {
			this.detailType = dType;
			this.detailsData = dData;
		}

		this.getDetailsData = function() {
			if (typeof this.detailsData == 'undefined') {
				var defaultData = [ {date:0, value:0} ];
				return defaultData;
			}

			return this.detailsData;
		}

		this.getDetailsType = function() {
			if (typeof this.detailsData == 'undefined') {
				return "assets/icons/warning.svg";
			}

			return this.detailType;
		}

		this.getHeartRateTestdata = function() {
			var heartRateDetailsData = [   
                {date: 1, value: 60}, {date: 2, value: 2}, {date: 3, value: 3}, {date: 4, value: 4}, {date: 5, value: 60}, {date: 6, value: 2}, {date: 7, value: 4}, 
                {date: 8, value: 2}, {date: 9, value: 62}, {date: 10, value: 3}, {date: 11, value: 4}, {date: 12, value: 4}, {date: 13, value: 63}, {date: 14, value: 60}, 
                {date: 15, value: 2}, {date: 16, value: 5}, {date: 17, value: 8}, {date: 18, value: 60}, {date: 19, value: 5}, {date: 20, value: 3}, {date: 21, value: 2}
            ];

            return heartRateDetailsData;
		};

		this.getEDATestdata = function() {
			var edaDetailsData = [
				{date: 1, value: 10}, {date: 2, value: 8}, {date: 3, value: 7}, {date: 4, value: 8}, {date: 5, value: 9}, {date: 6, value: 31}, {date: 7, value: 34}, 
                {date: 8, value: 42}, {date: 9, value: 32}, {date: 10, value: 30}, {date: 11, value: 44}, {date: 12, value: 34}, {date: 13, value: 33}, {date: 14, value: 29}
            ];

            return edaDetailsData;
		};

		this.getAccelerometerTestdata = function() {
			var accDetailsData = [
				{date: 1, value: 1}, {date: 2, value: 1}, {date: 3, value: 1}, {date: 4, value: 2}, {date: 5, value: 2}, {date: 6, value: 2}, {date: 7, value: 2}, 
                {date: 8, value: 3}, {date: 9, value: 4}, {date: 10, value: 4}, {date: 11, value: 9}, {date: 12, value: 9}, {date: 13, value: 5}, {date: 14, value: 8}
            ];

            return accDetailsData;
		};

	});
}());