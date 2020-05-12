statement = require('./statement');

const fs = require('fs');

let invoices = JSON.parse(fs.readFileSync('data/invoices.json'))[0];
let plays = JSON.parse(fs.readFileSync('data/plays.json'));

console.log(statement(invoices, plays));
