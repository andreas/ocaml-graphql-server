var fs = require('fs');
var graphql = require('graphql');
var mockServer = require('graphql-tools').mockServer;

var schema = graphql.buildSchema(fs.readFileSync('/dev/stdin', 'utf-8'));
mockServer(schema).query(graphql.introspectionQuery).then(response => {
    fs.writeSync(process.stdout.fd, JSON.stringify(response));
});