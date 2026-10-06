const { defineConfig } = require('@playwright/test');
module.exports = defineConfig({
  testDir: './tests', timeout: 30000, workers: 1,
  use: { baseURL: process.env.VIEWER_URL || 'http://127.0.0.1:8765', headless: true, viewport: { width: 1440, height: 1000 } }
});
