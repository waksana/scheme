const tokenizer = require('./tokenizer');
const {valueOf, exp, or, setValue, many, sat, item} = require('./parserMonad');

const match = word => sat(({type, value}) => type === "BasicToken" && word === value, word);

const isType = needType => sat(({type}) => type === needType, needType);

const openBracket = isType('OpenBracket');
const closeBracket = isType('CloseBracket');
const quoteType = isType('Quote');

const numberToken = isType('NumberToken');
const boolToken = isType('BoolToken');
const basicToken = isType('BasicToken');

const atom = or(numberToken, boolToken, basicToken);

const list = exp(function*() {
  yield openBracket
  const value = yield many(expression);
  yield closeBracket;
  return setValue({type: 'List', value});
});

const quoteExp = exp(function*() {
  yield quoteType;
  const value = yield expression;
  return setValue({type: 'List', value: [
    {type: 'BasicToken', value: 'quote'},
    value
  ]});
});

const expression = or(list, atom, quoteExp);

const quoteToken = match('quote');
const quote = or(
  exp(function* () {
    yield openBracket;
    yield quoteToken;
    const value = yield expression;
    yield closeBracket;
    return setValue({ type: 'Quote', value });
  }),
  exp(function* () {
    yield quoteType;
    const value = yield expression;
    return setValue({ type: 'Quote', value });
  })
);

const basicTokenList = exp(function*() {
    yield openBracket;
    const value = yield many(basicToken);
    yield closeBracket;
  return setValue({type: 'BasicTokenList', value});
});

const lambdaToken = match('lambda');
const lambda = exp(function* () {
  yield openBracket;
  yield lambdaToken;
  const paramNames = yield basicTokenList;
  const body = yield scheme;
  yield closeBracket;
  return setValue({ type: 'Lambda', value: { paramNames, body } });
});

const condPair = exp(function* () {
  yield openBracket;
  const cond = yield scheme;
  const branch = yield scheme;
  yield closeBracket;
  return setValue({ type: 'CondPair', value: { cond, branch } });
});

const condToken = match('cond');
const cond = exp(function* () {
  yield openBracket;
  yield condToken;
  const pairOne = yield condPair;
  const pairs = yield many(condPair);
  yield closeBracket;
  return setValue({ type: 'Cond', value: [pairOne, ...pairs] });
});

const defineToken = match('define');
const def = exp(function* () {
  yield openBracket;
  yield defineToken;
  const name = yield basicToken;
  const val = yield scheme;
  yield closeBracket;
  return setValue({ type: 'Define', value: { name, val } });
});

const application = exp(function* () {
  yield openBracket;
  const fn = yield scheme;
  const paramValues = yield many(scheme);
  yield closeBracket;
  return setValue({ type: 'Application', value: { fn, paramValues } });
});

const scheme = or(
  quote,
  lambda,
  cond,
  def,
  numberToken,
  boolToken,
  basicToken,
  application,
)

module.exports = function (text) {
  const result = many(scheme)(tokenizer(text));
  return valueOf(result, text);
}
