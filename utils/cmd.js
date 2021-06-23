const fs = require('fs');
const path = require('path');
const scriptname = process.argv[2];
const filename = process.argv[3];
const dir = process.cwd();

const fn = require(path.join(dir, scriptname));

const text = fs.readFileSync(path.join(dir, filename), 'utf8');

console.log(JSON.stringify(fn(text), null, 2));
