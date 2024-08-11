/** @type {import('tailwindcss').Config} */

const tailwindColors = require("tailwindcss/colors");

module.exports = {
  content: ["./index.html", "./src/**/*.res.mjs", "./src/**/*.jsx"],
  theme: {
    extend: {
      colors: {
        primary: tailwindColors["blue"],
      },
    },
  },
  plugins: [],
};
