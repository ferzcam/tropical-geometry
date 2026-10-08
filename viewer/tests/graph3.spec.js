const { test, expect } = require('@playwright/test');
const path = require('node:path');
const fs = require('node:fs');
const splitPath = path.join(__dirname, '../examples/split-quadratic-3d.json');
const planeStatus = '1 vertices · 4 edges (0 bounded, 4 rays) · 1 cells · 4 faces · direct solver · one-skeleton only.';

async function open(page) {
  const errors = []; page.on('pageerror', error => errors.push(error.message));
  await page.goto('/graph3.html');
  await expect(page.locator('#status')).toHaveText(planeStatus);
  return errors;
}
const sceneCounts = page => page.evaluate(() => {
  const count = view => { let meshes = 0, lines = 0; view.scene.traverse(o => { if (o.isMesh) meshes++; if (o.isLine) lines++; }); return { meshes, lines, pickables: view.pickables.length }; };
  return { graph: count(window.tropicalViews.graph), newton: count(window.tropicalViews.newton) };
});
// Render and read the drawing buffer back: the number of pixels that are not
// grey/transparent, so highlights (which change colors) are measurable.
const coloredPixels = page => page.evaluate(() => {
  const out = {};
  for (const [name, view] of Object.entries(window.tropicalViews)) {
    view.renderer.render(view.scene, view.camera);
    const gl = view.renderer.getContext(), w = gl.drawingBufferWidth, h = gl.drawingBufferHeight;
    const pixels = new Uint8Array(w * h * 4);
    gl.readPixels(0, 0, w, h, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    let colored = 0, digest = 0;
    for (let i = 0; i < pixels.length; i += 4) {
      if (pixels[i + 3] > 0 && (pixels[i] !== pixels[i + 1] || pixels[i + 1] !== pixels[i + 2])) colored++;
      digest = (digest * 31 + pixels[i] + pixels[i + 1] * 3 + pixels[i + 2] * 7) % 1000000007;
    }
    out[name] = { colored, digest, lost: gl.isContextLost() };
  }
  return out;
});

test('tropical plane loads in both 3D views with the one-skeleton contract and linked selection', async ({ page }) => {
  const errors = await open(page);
  await expect(page.locator('#contract')).toContainText('one-skeleton');
  await expect(page.locator('#contract')).toContainText('not computed or drawn');
  await expect(page.locator('#graph-view canvas')).toBeVisible();
  await expect(page.locator('#newton-view canvas')).toBeVisible();
  const drawn = await coloredPixels(page);
  expect(drawn.graph.lost).toBe(false); expect(drawn.newton.lost).toBe(false);
  expect(drawn.graph.colored).toBeGreaterThan(500); expect(drawn.newton.colored).toBeGreaterThan(500);
  const counts = await sceneCounts(page);
  expect(counts.graph.pickables).toBe(1 + 4 * 2); // one vertex sphere, four tubes with cones
  expect(counts.newton.pickables).toBe(4); // four true polygonal faces
  expect(counts.newton.lines).toBeGreaterThanOrEqual(4 + 3); // face outlines plus axes
  await expect(page.locator('.feature')).toHaveCount(5);
  const vertex = page.locator('.feature').first();
  await expect(vertex).toContainText('(0, 0, 0) ↔ cell with 4 faces');
  await vertex.click();
  await expect(vertex).toHaveAttribute('aria-pressed', 'true');
  await expect(page.locator('#selection')).toContainText('subdivision cell bounded by 4 faces');
  const faceOpacity = await page.evaluate(() => { const o = []; window.tropicalViews.newton.scene.traverse(m => { if (m.isMesh && m.material.transparent) o.push(m.material.opacity); }); return o; });
  expect(faceOpacity.filter(o => o > 0.3)).toHaveLength(4);
  const highlighted = await coloredPixels(page);
  expect(highlighted.newton.digest).not.toBe(drawn.newton.digest); // the cell's faces changed color/opacity
  expect(highlighted.graph.colored).toBeGreaterThan(drawn.graph.colored); // the vertex sphere grew
  const ray = page.locator('.feature', { hasText: 'ray 1' });
  await ray.click();
  await expect(ray).toHaveAttribute('aria-pressed', 'true');
  await expect(vertex).toHaveAttribute('aria-pressed', 'false');
  await expect(page.locator('#selection')).toContainText('face with 3 corners');
  await expect(page.locator('#selection')).toContainText('on the boundary of the subdivision');
  const activeFaces = await page.evaluate(() => { const o = []; window.tropicalViews.newton.scene.traverse(m => { if (m.isMesh && m.material.transparent) o.push(m.material.opacity); }); return o.filter(x => x > 0.3).length; });
  expect(activeFaces).toBe(1);
  await page.click('#clear-selection');
  await expect(ray).toHaveAttribute('aria-pressed', 'false');
  await expect(page.locator('#selection')).toContainText('Hover over geometry');
  expect(errors).toEqual([]);
});

test('clicking geometry in a 3D view selects its dual object', async ({ page }) => {
  await open(page);
  const box = await page.locator('#graph-view canvas').boundingBox();
  // Project the single vertex at the origin into canvas pixels and click it.
  const pixel = await page.evaluate(() => {
    const THREE_VECTOR = window.tropicalViews.graph.scene.children.find(o => o.isMesh && o.geometry.type === 'SphereGeometry').position.clone();
    const view = window.tropicalViews.graph;
    THREE_VECTOR.project(view.camera);
    const rect = view.renderer.domElement.getBoundingClientRect();
    return [(THREE_VECTOR.x + 1) / 2 * rect.width, (1 - THREE_VECTOR.y) / 2 * rect.height];
  });
  await page.mouse.click(box.x + pixel[0], box.y + pixel[1]);
  await expect(page.locator('.feature').first()).toHaveAttribute('aria-pressed', 'true');
  await expect(page.locator('#selection')).toContainText('Graph vertex (0, 0, 0)');
});

test('each named method runs as selected and unsupported input is an error, not a fallback', async ({ page }) => {
  await open(page);
  const requests = [];
  page.on('request', request => { if (request.url().endsWith('/api/graph3')) requests.push(request.postDataJSON().method); });
  await page.selectOption('#method', 'lrs');
  await expect(page.locator('#status')).toContainText('4 faces · LRS');
  await page.selectOption('#method', 'hull');
  await expect(page.locator('#status')).toContainText('4 faces · tailored hull');
  await page.selectOption('#example', 'fractional');
  await expect(page.locator('#status')).toHaveClass('error');
  await expect(page.locator('#status')).toContainText('integral coefficients');
  await expect(page.locator('.explorer')).toHaveClass(/stale/);
  await expect(page.locator('#export-graph')).toBeDisabled();
  await page.selectOption('#method', 'direct');
  await expect(page.locator('#features')).toContainText('(1/2, 2/3, 3/4)');
  await expect(page.locator('#status')).toContainText('direct solver');
  await page.selectOption('#method', 'lrs');
  await expect(page.locator('#features')).toContainText('(1/2, 2/3, 3/4)');
  expect(requests).toEqual(['lrs', 'hull', 'hull', 'direct', 'lrs']);
});

test('split quadratic shows a bounded edge dual to the face shared by two cells', async ({ page }) => {
  await open(page);
  await page.selectOption('#example', 'split');
  await expect(page.locator('#status')).toContainText('2 vertices · 7 edges (1 bounded, 6 rays) · 2 cells · 7 faces');
  const segment = page.locator('.feature', { hasText: 'segment' });
  await expect(segment).toHaveCount(1);
  await segment.click();
  await expect(page.locator('#selection')).toContainText('Segment from (-1, -2, -2) to (1, 0, 0)');
  await expect(page.locator('#selection')).toContainText('shared by two cells');
  await page.selectOption('#example', 'cubic');
  await expect(page.locator('#status')).toContainText('4 vertices · 16 edges (3 bounded, 13 rays) · 4 cells · 16 faces · direct solver');
  await page.selectOption('#method', 'lrs');
  await expect(page.locator('#status')).toContainText('4 cells · 16 faces · LRS');
  // The GLPK/Yang hull pipeline misses one lower cell here; the backend reports it instead of drawing it.
  await page.selectOption('#method', 'hull');
  await expect(page.locator('#status')).toHaveClass('error');
  await expect(page.locator('#status')).toContainText('missed a lower cell');
  await expect(page.locator('.explorer')).toHaveClass(/stale/);
});

test('PNG exports, view resets, and term editing keep exact state honest', async ({ page }) => {
  await open(page);
  for (const name of ['graph', 'newton']) {
    const downloadPromise = page.waitForEvent('download'); await page.click(`#export-${name}`);
    expect((await downloadPromise).suggestedFilename()).toBe(`tropical-${name}.png`);
  }
  const positions = () => page.evaluate(() => Object.fromEntries(Object.entries(window.tropicalViews).map(([k, v]) => [k, v.camera.position.toArray()])));
  const home = await positions();
  await page.locator('#graph-view canvas').hover();
  await page.mouse.wheel(0, -300);
  await expect.poll(async () => (await positions()).graph).not.toEqual(home.graph);
  await page.click('#reset-graph');
  const reset = await positions();
  reset.graph.forEach((x, i) => expect(x).toBeCloseTo(home.graph[i], 6));
  await page.getByLabel('Term 1 c', { exact: true }).fill('1');
  await expect(page.locator('#status')).toContainText('Polynomial changed');
  await expect(page.locator('#export-graph')).toBeDisabled();
  await page.click('#compute');
  await expect(page.locator('#features')).toContainText('(1, 1, 1)');
  await page.getByLabel('Term 1 c', { exact: true }).fill('1/0');
  await page.click('#compute');
  await expect(page.locator('#status')).toHaveClass('error');
  await expect(page.locator('.explorer')).toHaveClass(/stale/);
  await page.getByLabel('Remove term 4', { exact: true }).click();
  await page.getByLabel('Term 1 c', { exact: true }).fill('0');
  await page.click('#compute');
  await expect(page.locator('#status')).toHaveClass('error');
  await expect(page.locator('#status')).toContainText('affine rank three');
});

test('3D polynomial files import and export with z exponents and exact fractions', async ({ page }) => {
  await open(page);
  await page.getByLabel('Term 2 c', { exact: true }).fill('-1/2');
  const downloadPromise = page.waitForEvent('download');
  await page.click('#export-polynomial');
  const download = await downloadPromise;
  expect(download.suggestedFilename()).toBe('tropical-polynomial-3d.json');
  const stream = await download.createReadStream(); let text = ''; for await (const chunk of stream) text += chunk;
  expect(JSON.parse(text).terms[1]).toEqual({ x: 1, y: 0, z: 0, coefficient: '-1/2' });
  await page.locator('#polynomial-file').setInputFiles(splitPath);
  await expect(page.locator('#status')).toContainText('2 vertices · 7 edges');
  await expect(page.locator('#file-status')).toContainText('Imported split-quadratic-3d.json');
  await expect(page.locator('#example')).toHaveValue('custom');
  const before = await page.locator('#terms input').evaluateAll(inputs => inputs.map(input => input.value));
  await page.locator('#polynomial-file').setInputFiles({ name: 'two-d.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify({ terms: [{ x: 0, y: 0, coefficient: '0' }] })) });
  await expect(page.locator('#file-status')).toHaveClass(/error/);
  await expect(page.locator('#file-status')).toContainText('needs x, y, z, and coefficient');
  expect(await page.locator('#terms input').evaluateAll(inputs => inputs.map(input => input.value))).toEqual(before);
  await expect(page.locator('.explorer')).not.toHaveClass(/stale/);
  const response = await page.request.get('/examples/split-quadratic-3d.json');
  expect(await response.json()).toEqual(JSON.parse(fs.readFileSync(splitPath, 'utf8')));
});

test('navigation links the three explorers and the layout fits a narrow viewport', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await open(page);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  const sizes = await page.evaluate(() => ['graph-view', 'newton-view'].map(id => { const e = document.getElementById(id); return [e.clientWidth, e.clientHeight]; }));
  sizes.forEach(([w, h]) => { expect(w).toBeGreaterThan(200); expect(h).toBeGreaterThan(200); });
  await expect(page.locator('header nav a[href="/"]')).toBeVisible();
  await expect(page.locator('header nav a[href="/slices.html"]')).toBeVisible();
  await page.goto('/');
  await expect(page.locator('header nav a[href="/graph3.html"]')).toBeVisible();
  await expect(page.locator('header nav a[href="/slices.html"]')).toBeVisible();
  await page.goto('/slices.html');
  await expect(page.locator('a[href="/graph3.html"]')).toBeVisible();
  await expect(page.locator('#slice-status')).toContainText('z = 0 · 18 curve edges');
});
