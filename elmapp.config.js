module.exports = {
    homepage: "https://aorta.cigmah.org",
    configureWebpack: (config, env) => {
        config.optimization.minimizer[0].options.uglifyOptions.compress.passes = 1;
        config.optimization.minimizer[0].options.uglifyOptions.compress.unsafe_comps = false;
        config.optimization.minimizer[0].options.uglifyOptions.compress.unsafe = false;
        // Manipulate the config object and return it.
        return config;
    }
}
