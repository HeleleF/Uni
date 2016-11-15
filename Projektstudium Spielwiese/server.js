var fs = require('fs');
var url = require('url');
var http = require('http');

var PLAIN_CONTENT = {'Content-Type': 'text/html'};
var CSS_CONTENT = {'Content-Type': 'text/css'};

// create server
http.createServer(function (req, res) {

    var path = url.parse(req.url).pathname;
    console.log("Request on: " + path);

    // serve index.html on root path
    if(path == '/') {
        path = '/index.html';
    }

    fs.readFile(path.substr(1), function(err, data) {

        var types = path.split('.');

        if(err) {
            console.log(err);
            res.writeHead(404, PLAIN_CONTENT);
        } else if(types[types.length-1]==='css') {
            res.writeHead(200, CSS_CONTENT);
            res.write(data.toString());
        } else {
            res.writeHead(200, PLAIN_CONTENT);
            res.write(data.toString());
        }
        res.end();
    });
}).listen(8081);

console.log('Server running at localhost:8081');

