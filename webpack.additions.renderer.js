var path = require("path");

function resolve(filePath) {
    return path.join(__dirname, filePath);
}

const CopyPlugin = require('copy-webpack-plugin');

module.exports = {

    entry: [
        resolve("src/Renderer/Renderer.fsproj")],
    output: {
        filename: "renderer.js"
    },
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: {
                    loader: "fable-loader"
                }
            },
        ]
    }
};
