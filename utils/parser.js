const tokenizer = require('./tokenizer');
const {valueOf, exp, or, setValue, many, sat, item} = require('./parserMonad');

const match = word => sat(({type, value}) => type === "BasicToken" && word === value, word);

const isType = needType => sat(({type}) => type === needType, needType);

const openBracket = isType('OpenBracket');
const closeBracket = isType('CloseBracket');
const quoteBracket = isType('QuoteBracket');

const quoteToken = match('quote');
const quote = exp(function* () {
  yield openBracket;
  yield quoteToken;
  const value = yield expression;
  yield closeBracket;
  return setValue({ type: 'Quote', value });
});

const lambdaToken = matchToken('lambda');
const lambda = exp(function* () {
  yield openBracket;
  yield lambdaToken;
  const params = yield basicTokenList;
  const body = yield scheme;
  yield closeBracket;
  return setValue({ type: 'lambda', value: { params, body } });
});

const condPair = exp(function* () {
  yield openBracket;
  const cond = yield scheme;
  const branch = yield scheme;
  yield closeBracket;
  return setValue({ type: 'condPair', value: { cond, branch } });
});

const condToken = matchToken('cond');
const cond = exp(function* () {
  yield openBracket;
  yield condToken;
  const pairOne = yield condPair;
  const pairs = yield many(condPair);
  yield closeBracket;
  return setValue({ type: 'cond', value: [pairOne, ...pairs] });
});

const defineToken = matchToken('define');
const def = exp(function* () {
  yield openBracket;
  yield defineToken;
  const name = yield basicToken;
  const val = yield scheme;
  yield closeBracket;
  return setValue({ type: 'define', value: { name, val } });
});

const application = exp(function* () {
  yield openBracket;
  const fn = yield scheme;
  const params = yield many(scheme);
  yield closeBracket;
  return setValue({ type: 'application', value: { fn, params } });
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
  const result = tokenizer(text);
  return valueOf(result, text);
}