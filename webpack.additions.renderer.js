var path = require("path");

function resolve(filePath) {
  return path.join(__dirname, filePath)
};



module.exports = {
    entry: [
        resolve('website/main.scss'),
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
            }
        ]
    }
};
