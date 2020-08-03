const webpack = require("webpack");
const path = require('path');
const UglifyJsPlugin = require('terser-webpack-plugin');

module.exports = {
  mode: 'development',
  entry: path.resolve(__dirname, 'src/index.js'),
  devtool: "source-map",
  output: {
    path: path.resolve(__dirname, '../shiny-addons/dipsaus/'),
    filename: "dipsaus-dipterix-lib.js"
  },
  optimization: {
    minimize: true,
    minimizer: [new UglifyJsPlugin({
      include: /\.min\.js$/
    })]
  }
};
