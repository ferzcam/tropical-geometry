const { test, expect } = require('@playwright/test');
const path = require('node:path');
const fs = require('node:fs');
const cubicPath = path.join(__dirname, '../examples/genus-1-cubic.json');
const line = { terms: [{ x: 0, y: 0, coefficient: '0' }, { x: 1, y: 0, coefficient: '0' }, { x: 0, y: 1, coefficient: '0' }] };
const upload = (page, value, name = 'polynomial.json') => page.locator('#polynomial-file').setInputFiles({ name, mimeType: 'application/json', buffer: Buffer.from(typeof value === 'string' ? value : JSON.stringify(value)) });
const editor = page => page.locator('#terms input').evaluateAll(inputs => inputs.map(input => input.value));
async function open(page) {
  await page.goto('/');
  await expect(page.locator('#status')).toContainText('1 vertices · 3 edges');
}
async function downloadText(download) {
  const stream = await download.createReadStream();
  let text = ''; for await (const chunk of stream) text += chunk;
  return text;
}
async function delayFileRead(page) {
  await page.evaluate(() => {
    const original = File.prototype.text;
    File.prototype.text = async function () {
      const text = await original.call(this);
      if (this.name === 'slow.json') {
        window.fileReadStarted = true;
        await new Promise(resolve => { window.releaseFileRead = resolve; });
      }
      return text;
    };
  });
}
async function releaseFileRead(page) {
  // Wait for the actual native read and its consumer, not a guessed timeout.
  await page.evaluate(async () => {
    window.releaseFileRead();
    await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)));
  });
}

test('imported degree-three example has a connected genus-one bounded graph', async ({ page }) => {
  await open(page);
  const responsePromise = page.waitForResponse(response => response.url().endsWith('/api/curve') && response.request().method() === 'POST');
  await page.locator('#polynomial-file').setInputFiles(cubicPath);
  const response = await responsePromise;
  expect(response.ok()).toBe(true);
  const geometry = await response.json();
  await expect(page.locator('#status')).toContainText('9 vertices · 18 edges · 9 cells');
  await expect(page.locator('#file-status')).toContainText('Imported genus-1-cubic.json');
  await expect(page.locator('#terms tr')).toHaveCount(10);
  await expect(page.locator('#example')).toHaveValue('custom');
  await expect(page.locator('#export-curve')).toBeEnabled();
  const bounded = geometry.edges.filter(edge => edge.kind === 'segment');
  const vertices = new Set(geometry.vertices.map(vertex => JSON.stringify(vertex.point)));
  expect(vertices.size).toBe(9);
  expect(bounded).toHaveLength(9);
  const adjacency = new Map([...vertices].map(key => [key, []]));
  for (const edge of bounded) {
    const a = JSON.stringify(edge.start), b = JSON.stringify(edge.end);
    expect(vertices.has(a)).toBe(true); expect(vertices.has(b)).toBe(true);
    adjacency.get(a).push(b); adjacency.get(b).push(a);
  }
  const visited = new Set(), pending = [[...vertices][0]];
  while (pending.length) {
    const vertex = pending.pop();
    if (visited.has(vertex)) continue;
    visited.add(vertex); pending.push(...adjacency.get(vertex));
  }
  expect(visited.size).toBe(vertices.size);
  expect(bounded.length - vertices.size + 1).toBe(1);
});

test('polynomial export and import preserve exact fractions and current edits', async ({ page }) => {
  await open(page);
  await page.getByLabel('Term 1 c', { exact: true }).fill('1/3');
  const downloadPromise = page.waitForEvent('download');
  await page.click('#export-polynomial');
  const download = await downloadPromise;
  expect(download.suggestedFilename()).toBe('tropical-polynomial.json');
  const text = await downloadText(download), saved = JSON.parse(text);
  expect(saved).toEqual({ terms: [{ x: 0, y: 0, coefficient: '1/3' }, ...line.terms.slice(1)] });
  await page.selectOption('#example', 'square');
  await expect(page.locator('#status')).toContainText('4 edges');
  await upload(page, text, download.suggestedFilename());
  await expect(page.locator('#features')).toContainText('(1/3, 1/3)');
  await expect(page.getByLabel('Term 1 c', { exact: true })).toHaveValue('1/3');
  const againPromise = page.waitForEvent('download');
  await page.click('#export-polynomial');
  expect(JSON.parse(await downloadText(await againPromise))).toEqual(saved);
});

test('invalid files preserve the editor and the current drawing', async ({ page }) => {
  await open(page);
  const before = await editor(page), status = await page.locator('#status').textContent();
  const invalid = [
    ['malformed.json', '{'],
    ['empty.json', { terms: [] }],
    ['wrong-schema.json', { polynomial: line.terms }],
    ['boolean-exponent.json', { terms: [{ x: false, y: 0, coefficient: '0' }] }],
    ['numeric-coefficient.json', { terms: [{ x: 0, y: 0, coefficient: 0.5 }] }],
    ['zero-denominator.json', { terms: [{ x: 0, y: 0, coefficient: '1/0' }] }],
    ['too-many-terms.json', { terms: Array.from({ length: 33 }, () => line.terms[0]) }],
    ['oversized.json', ' '.repeat(32769)]
  ];
  let requests = 0;
  page.on('request', request => { if (request.url().endsWith('/api/curve')) requests++; });
  for (const [name, value] of invalid) {
    await upload(page, value, name);
    await expect(page.locator('#file-status')).toHaveClass(/error/);
    await expect(page.locator('#file-status')).toContainText('Could not import polynomial:');
    expect(await editor(page)).toEqual(before);
    await expect(page.locator('#status')).toHaveText(status);
    await expect(page.locator('.explorer')).not.toHaveClass(/stale/);
    await expect(page.locator('#export-curve')).toBeEnabled();
  }
  await expect(page.locator('#file-status')).toContainText('32768');
  expect(requests).toBe(0);
});

test('the same file can be imported again after editing', async ({ page }) => {
  await open(page);
  await page.locator('#polynomial-file').setInputFiles(cubicPath);
  await expect(page.locator('#status')).toContainText('9 vertices · 18 edges');
  await page.getByLabel('Term 1 c', { exact: true }).fill('42');
  await page.locator('#polynomial-file').setInputFiles(cubicPath);
  await expect(page.getByLabel('Term 1 c', { exact: true })).toHaveValue('0');
  await expect(page.locator('#status')).toContainText('9 vertices · 18 edges');
});

test('a slow file read cannot overwrite a newer coefficient edit', async ({ page }) => {
  await open(page); await delayFileRead(page);
  await upload(page, JSON.parse(fs.readFileSync(cubicPath, 'utf8')), 'slow.json');
  await page.waitForFunction(() => window.fileReadStarted);
  await page.getByLabel('Term 1 c', { exact: true }).fill('1/3');
  await releaseFileRead(page);
  await expect(page.getByLabel('Term 1 c', { exact: true })).toHaveValue('1/3');
  await expect(page.locator('#terms tr')).toHaveCount(3);
  await expect(page.locator('#status')).toContainText('Polynomial changed');
  await expect(page.locator('#export-curve')).toBeDisabled();
});

test('a slow file read cannot overwrite a newer import', async ({ page }) => {
  await open(page); await delayFileRead(page);
  await upload(page, JSON.parse(fs.readFileSync(cubicPath, 'utf8')), 'slow.json');
  await page.waitForFunction(() => window.fileReadStarted);
  const fractional = { terms: [{ x: 0, y: 0, coefficient: '1/3' }, ...line.terms.slice(1)] };
  await upload(page, fractional, 'newer.json');
  await expect(page.locator('#features')).toContainText('(1/3, 1/3)');
  await releaseFileRead(page);
  await expect(page.getByLabel('Term 1 c', { exact: true })).toHaveValue('1/3');
  await expect(page.locator('#terms tr')).toHaveCount(3);
  await expect(page.locator('#file-status')).toContainText('Imported newer.json');
  await expect(page.locator('#export-curve')).toBeEnabled();
});

test('the downloadable cubic example and the built-in example agree', async ({ page, request }) => {
  const response = await request.get('/examples/genus-1-cubic.json');
  expect(response.ok()).toBe(true);
  expect(await response.json()).toEqual(JSON.parse(fs.readFileSync(cubicPath, 'utf8')));
  await open(page);
  await page.selectOption('#example', 'cubic');
  await expect(page.locator('#status')).toContainText('9 vertices · 18 edges · 9 cells');
});
