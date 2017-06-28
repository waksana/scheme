const {fromArray} = require('./list');

const tokenize = str => str.replace(/(\(|\))/g, ' $1 ').trim().split(/\s+/);

const parse = tokens => {
  var i = 0;
  function parse_help() {
    if(tokens[i] == ')' || tokens.length <= i)
      throw new Error('fuck');
    if(tokens[i] == '(') {
      let res = [];
      while(tokens[++i] != ')')
        res.push(parse_help());
      return fromArray(res);
    }
    return tokens[i];
  }
  return parse_help();
};

module.exports = str => {
  const tokens = tokenize(`(${str})`);
  return parse(tokens);
};
