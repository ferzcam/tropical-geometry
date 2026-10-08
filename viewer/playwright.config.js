const { defineConfig } = require('@playwright/test');
module.exports = defineConfig({
  testDir: './tests', timeout: 30000, workers: 1,
  use: {
    baseURL: process.env.VIEWER_URL || 'http://127.0.0.1:8765', headless: true, viewport: { width: 1440, height: 1000 },
    // Headless Chromium without a GPU drops WebGL contexts; software rendering
    // keeps the three.js views drawing so the 3D tests check real pixels.
    launchOptions: { args: ['--use-angle=swiftshader', '--enable-unsafe-swiftshader', '--ignore-gpu-blocklist'] }
  }
});
