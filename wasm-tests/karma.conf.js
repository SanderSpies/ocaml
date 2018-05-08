module.exports = function(config) {
  config.set({
    frameworks: ["mocha", "chai"],
    files: [
      "test/**/*.js",
      {
        pattern: "test/**/*.wasm",
        included: false
      }
    ],
    reporters: ["progress"],
    port: 9876, // karma web server port
    colors: true,
    logLevel: config.DEBUG,
    // browsers: ['Chrome'],
    autoWatch: false,
    singleRun: false, // Karma captures browsers, runs the tests and exits
    concurrency: Infinity
  });
};
