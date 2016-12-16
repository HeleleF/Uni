(function() {
    'use strict';

    var express = require('express');
    var router = express.Router();

    router.post('/', function(req, res) {

        console.log(req.params);

        res.json( {token: 'fake-jwt-token'} );
    });

    module.exports = router;

}());
