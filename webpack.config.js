const webpack = require('webpack');
const path = require('path');

const devServer = {
  contentBase: path.join(__dirname, "doc"),
  compress: true,
  port: 9000,
};

const plugins = [
  new webpack.optimize.UglifyJsPlugin({
    compress: {
      warnings: false,
      drop_console: true,
    },
  })
];
module.exports = {
  devServer,
  entry: './src/index.js',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'doc')
  }
};
