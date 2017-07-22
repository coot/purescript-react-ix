"use strict"

module.exports = config => {
  config.set({
    autoWatch: true,
    browsers: ["Chrome", "Firefox"],
    files: [
      "karma/index.js",
    ],
    reporters: ["spec"],
    singleRun: false
  })
}
