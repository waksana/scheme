const fs = require('fs');
const path = require('path');
const filename = process.argv[2];
const dir = process.cwd();

const scheme = require('./scheme');

const schemeCode = fs.readFileSync(path.join(dir, filename), 'utf8');

console.log(scheme(schemeCode));