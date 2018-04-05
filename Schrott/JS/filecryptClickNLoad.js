/*
 * Bookmarklet
 * javascript:+function(){try{let e=[],f="",g=new XMLHttpRequest;e=document.getElementsByClassName("cnlform")[0].getAttribute("onsubmit").slice(7,-16).replace(/'/g,"").split(","),f+="passwords=filecrypt.cc&source=filecrypt.cc&package="+e[3].trim()+"&jk=function f(){ return '"+e[1].trim()+"'};&crypted="+e[2].trim(),g.open("POST","http://127.0.0.1:9666/flash/addcrypted2",!0),g.setRequestHeader("Content-type","application/x-www-form-urlencoded"),g.send(f)}catch(e){alert("failed")}}();
 */

+function () {
    try {
        // get click'n'load form element
        var formElement = document.getElementsByClassName("cnlform")[0];

        // extract query values to list
        var submitString = formElement.getAttribute("onsubmit");
        var formattedString = submitString.slice(7, -16).replace(/'/g, "");
        var values = formattedString.split(",");

        // create query string (http://jdownloader.org/knowledge/wiki/glossary/cnl2#post-links-to-jdownloader)
        var defaultSource = "filecrypt.cc";
        var packageName = values[3].trim();
        var aesKeyFunction = "function f(){ return '" + values[1].trim() + "'};";
        var encryptedUrls = values[2].trim();

        var query = "passwords=" + defaultSource + "&source=" + defaultSource + "&package=" + packageName + "&jk=" + aesKeyFunction + "&crypted=" + encryptedUrls;

        // send POST request to JD
        // doesnt work if JD is not running
        var xhr = new XMLHttpRequest();
        xhr.open("POST", "http://127.0.0.1:9666/flash/addcrypted2", true);
        xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        xhr.send(query);

    } catch (anyError) {
        // not really needed
        alert("failed");
    }
}();