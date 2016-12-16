var express = require('express');
var app = express();

// serve public files
app.use(express.static('public'));

// register backend services
app.use('/data/auth', require('./data/auth.js'));

// use cmdline args
var port = process.argv[2] || 3000;

// start server 
app.listen(port, function() {
    console.log('Server running at http://localhost:'+port);
});
