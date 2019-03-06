const path = require("path");
const rimraf = require("rimraf");
const webpack = require("webpack");
const DashboardPlugin = require("webpack-dashboard/plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const ExtractTextPlugin = require("extract-text-webpack-plugin");
const UglifyJSPlugin = require("uglifyjs-webpack-plugin");

const extractCssInstance = new ExtractTextPlugin({
    filename: "styles/[name].bundle.css",
    allChunks: true
});

const isDevServer = process.argv.find(v => v.includes("webpack-dev-server"));
const defaultOutputPath = path.resolve(__dirname + "/frontend");

module.exports = function({ outputPath, nptWebApi } = {}, argv) {
    if (isDevServer && typeof nptWebApi === "undefined") {
        throw Error("Error: Please define --env.nptWebApi param for webpack-dev-server");
    }

    if (!isDevServer && typeof outputPath === "undefined") {
        console.warn(
            "Warning: outputPath is not defined, fallback to default: " + defaultOutputPath
        );
    }

    outputPath = outputPath || defaultOutputPath;

    const config = {
        context: path.resolve("src"),

        entry: {
            main: "./index"
        },

        output: {
            path: outputPath,
            publicPath: "/",
            filename: "[name].js"
        },

        module: {
            rules: [
                // Regular js pipeline
                {
                    test: /\.js$/,
                    exclude: /node_modules/,
                    use: [
                        {
                            loader: "babel-loader",
                            options: {
                                presets: ["env"]
                            }
                        }
                    ]
                },
                // Elm app compilation pipeline
                {
                    test: /Main\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    use: Array.prototype.concat(
                        // Develompent Server specific loaders
                        isDevServer
                            ? [
                                  {
                                      loader: "elm-hot-loader"
                                  },
                                  {
                                      loader: "string-replace-loader",
                                      query: {
                                          search: /serverDomain\s*=\s*(\'|\")(\'|\")/,
                                          replace: `serverDomain = '${nptWebApi}'`
                                      }
                                  }
                              ]
                            : [],
                        // General loaders
                        [
                            {
                                loader: "elm-assets-loader",
                                options: {
                                    module: "Assets",
                                    tagger: "AssetPath"
                                }
                            },
                            {
                                loader: "elm-webpack-loader",
                                options: isDevServer
                                    ? {
                                          verbose: true,
                                          debug: true
                                          // warn: true
                                      }
                                    : {}
                            }
                        ]
                    )
                },
                // Sass proccessing pipeline
                {
                    test: /\.scss$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    use: isDevServer
                        ? [
                              "style-loader",
                              "css-loader",
                              {
                                  loader: "sass-loader",
                                  options: {
                                      sourceMap: true
                                  }
                              }
                          ]
                        : extractCssInstance.extract([
                              "css-loader",
                              /* 'postcss-loader', */ "sass-loader"
                          ])
                },
                // Css proccessing pipeline
                {
                    test: /\.css$/,
                    use: isDevServer
                        ? ["style-loader", "css-loader"]
                        : extractCssInstance.extract(["css-loader" /* 'postcss-loader', */])
                },
                // Images proccessing pipeline
                {
                    test: /\.(jpe?g|png|gif|svg)$/i,
                    include: [/images/],
                    loader: "file-loader",
                    options: {
                        name: "[path][name]-[hash].[ext]"
                    }
                },
                // Fonts proccessing pipeline
                {
                    test: /\.(svg|ttf|eot|woff|woff2)(\?[a-z0-9=&.]+)?$/,
                    include: /(node_modules)/,
                    use: "file-loader?name=assets/fonts/[name].[ext]"
                }
            ]
        },
        plugins: [
            // Generate index page
            new HtmlWebpackPlugin({
                filename: `${outputPath}/index.html`,
                title: "NPT"
            }),
            extractCssInstance
        ],
        resolve: {
            alias: {
                elm: path.resolve(__dirname, "src/elm"),
                ports: path.resolve(__dirname, "src/ports"),
                "native-components": path.resolve(__dirname, "src/native-components")
            }
        }
    };

    // Development server configuration
    if (isDevServer) {
        config.devtool = "cheap-module-eval-source-map";
        config.devServer = {
            // stats: 'errors-only',
            host: "0.0.0.0",
            port: "8081",
            contentBase: path.join(__dirname, "src/assets"),
            historyApiFallback: true
        };
        config.plugins.push(new DashboardPlugin(), new webpack.NamedModulesPlugin());
    } else {
        // Production configuration
        config.plugins.push(new UglifyJSPlugin());
    }

    return config;
};
