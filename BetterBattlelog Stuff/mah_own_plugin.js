/**
* Example Plugin - Show how you can use the plugin engine
* NOTE: Do NOT set global values outside of the plugin object
*    Maybe they will conflict with other addons or any in-page related variables
*    Only use the plugin cache/storage to set/get variables
*
* @author Lehne
* @version 0.1
* @url https://getbblog.com
*/

// initialize your plugin
BBLog.handle("add.plugin", {

    /**
    * The unique, lowercase id of my plugin
    * Allowed chars: 0-9, a-z, -
    */
    id : "correct-vehicle-names-plugin",

    /**
    * The name of my plugin, used to show config values in bblog options
    * Could also be translated with the translation key "plugin.name" (optional)
    *
    * @type String
    */
    name : "Show Correct Vehicle Names Plugin",

    /**
    * Some translations for this plugins
    * For every config flag must exist a corresponding EN translation
    *   otherwise the plugin will no be loaded
    *
    * @type Object
    */
    translations : {
        "en" : {
            "foo.bar" : "My Test Config Flag",
            "foo.bar.tooltip" : "The tooltip for my foo bar flag",
            "my.option" : "Config Flag 2",
            "my.btn.option" : "Edit List",
            "other.trans" : "Foo Bar"
        },
        "de" : {
            "foo.bar" : "Mein Test",
            "foo.bar.tooltip" : "Der Tooltip zum Test Key",
            "my.option" : "Config Flag 2",
            "my.btn.option" : "Liste bearbeiten",
            "other.trans" : "Foo Bar",
            "plugin.name" : "Der Name meines Plugins"
        }
    },

    /**
    * Configuration Options that appears in the BBLog Menu
    * Every option must be an object with properties as shown bellow
    * Properties available:
    *   key : The name for your config flag - The user can toggle this option
    *         and you can retreive the users choice with instance instance.storage(YOUR_KEY_NAME) (0 or 1 will be returned)
    *   init : Can be 0 or 1 - Represent the initial status of the option when the user load the plugin for the first time
    *          If you want that this option is enabled on first load (opt-out) than set it to 1, otherwise to 0 (opt-in)
    *   handler(optional): When set as a function this config entry turns into a button (like the plugins button you see in the bblog menu)
    *                       The function well be executed when the user clicks the button
    */
    configFlags : [
        {"key" : "foo.bar", "init" : 0},
        {"key" : "my.option", "init" : 1},
        {"key" : "my.btn.option", "init" : 1, "handler" : function(instance){
            instance.myOwnCustomFunc123(instance);
        }}
    ],

    /**
    * A handler that be fired immediately (only once) after the plugin is loaded into bblog
    *
    * @param object instance The instance of your plugin which is the whole plugin object
    *    Always use "instance" to access any plugin related function, not use "this" because it's not working properly
    *    For example: If you add a new function to your addon, always pass the "instance" object
    */
    init : function(instance){
        // some log to the console to show you how the things work
		
		/*
        console.log(
            "plugin."+instance.id+".init",
            instance.t("my.option"),
            instance.storage("foo.bar"),
            instance.storage("my.option"),
            instance.cache("cache.test"),
            instance.storage("permanent.test")
        );
		*/
        // testdata
        instance.cache("cache.test", Math.random());
        instance.storage("permanent.test", Math.random());
    },

    /**
    * A trigger that fires everytime when the dom is changing but at max only once each 200ms (5x per second) to prevent too much calls in a short time
    * Example Case: If 10 DOM changes happen in a period of 100ms than this function will only been called 200ms after the last of this 10 DOM changes
    * This make sure that all actions in battlelog been finished before this function been called
    * This is how BBLog track Battlelog for any change, like url, content or anything
    *
    * @param object instance The instance of your plugin which is the whole plugin object
    *    Always use "instance" to access any plugin related function, not use "this" because it's not working properly
    *    For example: If you add a new function to your addon, always pass the "instance" object
    */
    domchange : function(instance){
        // some log to the console to show you how the things work
		
		/*
        console.log(
            "plugin."+instance.id+".domchanged",
            instance.t("my.option"),
            instance.storage("foo.bar"),
            instance.storage("my.option"),
            instance.cache("cache.test"),
            instance.cache("permanent.test")
        );
		*/
		
		try {	
			if ($('[data-category="Vehicle Air Helicopter Scout"]').hasClass("active") &&
				$('[data-guid="A18AF4CF-8E25-632C-E385-4291BCB791F3"] > span')[0].innerHTML !== "Z-11W (CN)"
				){
				
				// Change "Z-11W (RU)" to "Z-11W (CN)":
				$('[data-guid="A18AF4CF-8E25-632C-E385-4291BCB791F3"] > span')[0].innerHTML = "Z-11W (CN)";
			

				// Change "Z-11W (US)" to "Z-11W (RU)":
				$('[data-guid="D780AFF6-38B7-11DE-BF1C-984D9AEE762C"] > span')[0].innerHTML = "Z-11W (RU)";
			}	
		} catch (ex) {
			console.log("[ERROR in heli]:" + ex);
		}
		
		try {	
			if ($('[data-category="Vehicle Air"]').hasClass("active") &&
				$('[data-guid="F8E367F2-8843-B33D-856A-C0F75C921370"] > span')[0].innerHTML !== "BOMBER"
				){
				
				// remove "BOMBER (US)" and "BOMBER (CN)"
				$('[data-guid="3DDB3389-B5B0-6687-B982-9FC2F7380B45"]').hide();
				$('[data-guid="8B058C6C-3813-429B-B3E7-10190C71500D"]').hide();
				
				// rename "BOMBER (RU)" to just "BOMBER"
				$('[data-guid="F8E367F2-8843-B33D-856A-C0F75C921370"] > span')[0].innerHTML = "BOMBER";
				
				// get the total kills for "Vehicle Air"			
				var total_elem = $('ul.category-vehicles-list li:last-child > strong');			
				var total = parseInt(total_elem[0].innerHTML);
				
				// get bomber kills
				var kills = parseInt($('[data-guid="F8E367F2-8843-B33D-856A-C0F75C921370"] > strong')[0].innerHTML);
				
				// subtract bomber kills 2 times from total and set as new total
				var new_total = total - 2 * kills;
				total_elem[0].innerHTML = new_total;
				
				$("tr.active > td.item-kills > strong")[0].innerHTML = new_total;
				$("tr.active > td.item-kills").attr("data-sort-value", new_total);
								
			}	
		} catch (ex) {
			console.log("[ERROR in bomber]:" + ex);
		}
		
		try {	
			if ($('[data-category="Weapon Stationary"]').hasClass("active") &&
				$('[data-guid="29EFF907-098D-4054-A3D7-93A2901D24FA"] > span')[0].innerHTML !== ".50 CAL"
				){
				
				// remove ".50 CAL (US)" and ".50 CAL (CN)"
				$('[data-guid="4D54AFDC-1053-4239-A964-615075C1FFBF"]').hide();
				$('[data-guid="FF909529-3DF7-D56D-74FE-8A45D9CA116F"]').hide();
				
				// rename ".50 CAL (RU)" to just ".50 CAL"
				$('[data-guid="29EFF907-098D-4054-A3D7-93A2901D24FA"] > span')[0].innerHTML = ".50 CAL";
				
				// get the total kills for "Weapon Stationary"			
				var total_elem = $('ul.category-vehicles-list li:last-child > strong');			
				var total = parseInt(total_elem[0].innerHTML);
				
				// get .50 Cal kills
				var kills = parseInt($('[data-guid="29EFF907-098D-4054-A3D7-93A2901D24FA"] > strong')[0].innerHTML);
				
				// subtract .50 Cal kills 2 times from total and set as new total
				var new_total = total - 2 * kills;
				total_elem[0].innerHTML = new_total;
				
				$("tr.active > td.item-kills > strong")[0].innerHTML = new_total;
				$("tr.active > td.item-kills").attr("data-sort-value", new_total);
								
			}	
		} catch (ex) {
			console.log("[ERROR in stationary]:" + ex);
		}
		
		try {	
			if ($('[data-category="Soldier Equiment"]').hasClass("active") &&
				$('[data-guid="347BF954-A53D-B675-1D40-2CDBE2CC8D9F"] > span')[0].innerHTML !== "UCAV"
				){
				
				// remove "UCAV (RU)"
				$('[data-guid="CA24B186-F4C7-C889-FDEE-2ED48731C679"]').hide();
				
				// rename "UCAV (US)" to just "UCAV"
				$('[data-guid="347BF954-A53D-B675-1D40-2CDBE2CC8D9F"] > span')[0].innerHTML = "UCAV";
				
				// get the total kills for "Soldier Equiment"			
				var total_elem = $('ul.category-vehicles-list li:last-child > strong');			
				var total = parseInt(total_elem[0].innerHTML);
				
				// get ucav kills
				var kills = parseInt($('[data-guid="347BF954-A53D-B675-1D40-2CDBE2CC8D9F"] > strong')[0].innerHTML);
				
				// subtract ucav kills 2 times from total and set as new total
				var new_total = total - 2 * kills;
				total_elem[0].innerHTML = new_total;
				
				$("tr.active > td.item-kills > strong")[0].innerHTML = new_total;
				$("tr.active > td.item-kills").attr("data-sort-value", new_total);
								
			}	
		} catch (ex) {
			console.log("[ERROR in equip]:" + ex);
		}
    },

    /**
    * This could be a function that you've implemented, it's up to you and your plugin
    * Notice the "instance" parameter, you should always pass the instance to any own function
    * See in the "my.btn.option" config flag click handler where this function is called for example
    *
    * @param object instance The instance of your plugin which is the whole plugin object
    *    Always use "instance" to access any plugin related function, not use "this" because it's not working properly
    *    For example: If you add a new function to your addon, always pass the "instance" object
    */
    myOwnCustomFunc123 : function(instance){
        alert("Hooo boy, you've clicked the button in the options. Now it's on you what you will make with this feature!");
    },

    /**
    * This function will be setted (injected) by the initializer
    * This placeholder must not be implemented in your plugin,
    *    it's added for tutorial purposes only in this example to show you how the function will look like
    * Get the translation for your plugin, depends on the current user language
    *
    * @param string key
    */
    t : function(key){},

    /**
    * This function will be setted (injected) by the initializer
    * This placeholder must not be implemented in your plugin,
    *    it's added for tutorial purposes only in this example to show you how the function will look like
    * Get/Set values in the plugin cache, cache means a temporarily cache which will be flushed after a complete page reload (not a ajax reload)
    * You don't need to add a prefix/namespace to the key, it's already namespaced and sandboxed to your plugin
    *
    * @param string key
    * @param mixed value Optional, if not set the function return the value instead of setting it
    */
    cache : function(key, value){},

    /**
    * This function will be setted (injected) by the initializer
    * This placeholder must not be implemented in your plugin,
    *    it's added for tutorial purposes only in this example to show you how the function will look like
    * Get/Set values in the permanent storage, this data will be stored forever
    * Please use this not as much because users browser storage is limited
    * You don't need to add a prefix/namespace to the key, it's already namespaced and sandboxed to your plugin
    * Also the config flag setting will be stored here, in our example "foo.bar", "my.option" and "my.btn.option" as integer values
    *
    * @param string key
    * @param mixed value Optional, if not set the function return the value instead of setting it
    */
    storage : function(key, value){}
});