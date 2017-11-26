// get current location path
var l = document.location.href;

// we are on linkshrink, skip the wait and go to the target site (which is hopefully linx.cloud)
if (!!l.match(/https?:\/\/linkshrink.net\//)) {

    var gotoLinx = atob(/"(\S+=)"/g.exec(document.getElementsByTagName("script")[document.getElementsByTagName("script").length - 2].text)[1]);

    // same window, use "_blank" for new tab
    window.open(gotoLinx, '_self'); 

  // we are on linx.cloud, so we have to parse the relevant links
} else if (!!l.match(/https?:\/\/linx.cloud\//)) {

    // this will look for mega, openload and zippy links in this order and copy the first one that is found to the clipboard (so JD can pick it up)
    var found = document.querySelector('meta[property="og:description"]').content.match(/https?:\/\/(mega\.nz\/#!|openload\.co\/f\/|www\d{1,3}\.zippyshare\.com\/v\/)\S*/ig);

    if (found.length > 0) {

        var i = document.createElement("input");
        document.body.appendChild(i);
        i.setAttribute("id", "x");
        document.getElementById('x').value = found[0];
        i.select();
        document.execCommand("copy");
        window.close();

    } else {
        alert("no links found!");
    }
}
