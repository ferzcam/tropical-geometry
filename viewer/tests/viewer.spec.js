const { test, expect } = require('@playwright/test');
test('examples, exact fractions, linked selection, coefficient editing and SVG exports', async ({ page }) => {
  const errors = []; page.on('pageerror', error => errors.push(error.message));
  await page.goto('/');
  await expect(page.locator('#status')).toContainText('1 vertices · 3 edges');
  await expect(page.locator('#curve-board svg')).toBeVisible();
  await expect(page.locator('#newton-board svg')).toBeVisible();
  await page.selectOption('#example', 'fractional');
  await expect(page.locator('#features')).toContainText('(1/2, 1/2)');
  const vertex = page.locator('.feature').first(); await vertex.click();
  await expect(vertex).toHaveAttribute('aria-pressed', 'true');
  await expect(page.locator('#selection')).toContainText('subdivision cell');
  await page.click('#clear-selection'); await expect(vertex).toHaveAttribute('aria-pressed', 'false');
  for (const name of ['curve', 'newton']) {
    const downloadPromise = page.waitForEvent('download'); await page.click(`#export-${name}`);
    const download = await downloadPromise; expect(download.suggestedFilename()).toBe(`tropical-${name}.svg`);
    const stream = await download.createReadStream(); let content = ''; for await (const chunk of stream) content += chunk;
    expect(content).toContain('<svg'); expect(content).toContain('1/2'); expect(content).toContain('<title>');
  }
  for (const [name, cells, edges] of [['square', 1, 4], ['hexagon', 1, 6], ['mixed', 2, 6], ['parallel', 0, 2]]) {
    await page.selectOption('#example', name);
    await expect(page.locator('#status')).toContainText(`${edges} edges · ${cells} cells`);
  }
  await page.selectOption('#example', 'line'); await expect(page.locator('#status')).toContainText('3 edges');
  await page.getByLabel('Term 1 c', { exact: true }).fill('1/3');
  await expect(page.locator('#export-curve')).toBeDisabled();
  await page.click('#compute'); await expect(page.locator('#features')).toContainText('(1/3, 1/3)');
  await page.click('#reset-curve'); await page.click('#reset-newton'); expect(errors).toEqual([]);
});
test('invalid input cannot leave an apparently current plot or export', async ({ page }) => {
  await page.goto('/'); await expect(page.locator('#status')).toContainText('3 edges');
  await page.getByLabel('Term 1 c', { exact: true }).fill('1/0'); await page.click('#compute');
  await expect(page.locator('#status')).toHaveClass('error');
  await expect(page.locator('.explorer')).toHaveClass(/stale/); await expect(page.locator('#export-curve')).toBeDisabled();
  await page.getByLabel('Term 1 c', { exact: true }).fill('0'); await page.click('#compute');
  await expect(page.locator('#export-curve')).toBeEnabled();
});
test('editing while a response is pending prevents stale results', async ({ page }) => {
  await page.goto('/'); await expect(page.locator('#status')).toContainText('3 edges');
  let release; const gate = new Promise(resolve => { release = resolve; });
  await page.route('**/api/curve', async route => { await gate; await route.continue(); });
  await page.click('#compute'); await expect(page.locator('#status')).toContainText('Computing');
  await page.getByLabel('Term 1 c', { exact: true }).fill('2'); release();
  await expect(page.locator('#status')).toContainText('Polynomial changed');
  await expect(page.locator('#export-curve')).toBeDisabled();
});
test('zoom resets, term editing, and the empty curve work on a narrow viewport', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto('/'); await expect(page.locator('#status')).toContainText('3 edges');
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  const boundingBox = () => page.evaluate(() => Object.values(JXG.boards).find(b => b.container === 'curve-board').getBoundingBox());
  const original = await boundingBox();
  await page.locator('#curve-board').scrollIntoViewIfNeeded();
  await page.locator('#curve-board').hover(); await page.mouse.wheel(0, -400);
  await expect.poll(boundingBox).not.toEqual(original);
  await page.click('#reset-curve');
  const reset = await boundingBox(); reset.forEach((x, i) => expect(x).toBeCloseTo(original[i], 6));
  await page.getByLabel('Remove term 3', { exact: true }).click();
  await page.getByLabel('Remove term 2', { exact: true }).click();
  await page.click('#compute'); await expect(page.locator('#status')).toContainText('curve is empty');
  await page.click('#add-term'); await expect(page.getByLabel('Term 2 c', { exact: true })).toBeVisible();
});
test('redundant collinear support preserves the full weighted dual edge', async ({ page }) => {
  await page.goto('/'); await expect(page.locator('#status')).toContainText('3 edges');
  await page.selectOption('#example', 'square'); await expect(page.locator('#status')).toContainText('4 edges');
  const terms = [[0, 0], [2, 0], [2, 2], [0, 2], [1, 0]];
  await page.click('#add-term');
  for (let i = 0; i < terms.length; i++) {
    await page.getByLabel(`Term ${i + 1} a`, { exact: true }).fill(String(terms[i][0]));
    await page.getByLabel(`Term ${i + 1} b`, { exact: true }).fill(String(terms[i][1]));
  }
  await page.click('#compute'); await expect(page.locator('#status')).toContainText('4 edges');
  const lengths = await page.evaluate(() => Object.values(JXG.boards).find(b => b.container === 'newton-board').objectsList.filter(o => o.elType === 'segment' && o.getAttribute('strokeWidth') === 2.5).map(o => o.L()));
  expect(lengths).toHaveLength(4); lengths.forEach(length => expect(length).toBeCloseTo(2));
  await expect(page.locator('#features')).toContainText('weight 2');
});
test('unrepresentable floating point directions show an explicit display error', async ({ page }) => {
  await page.goto('/'); await expect(page.locator('#status')).toContainText('3 edges');
  await page.getByLabel('Term 1 c', { exact: true }).fill('999999999999999999');
  await page.click('#compute'); await expect(page.locator('#status')).toContainText('display precision');
  await expect(page.locator('#export-curve')).toBeDisabled();
});
test('a ray with only one collapsed coordinate is not drawn with a false direction', async ({ page }) => {
  await page.goto('/'); await expect(page.locator('#status')).toContainText('3 edges');
  await page.getByLabel('Term 2 c', { exact: true }).fill('-999999999999999999');
  await page.click('#compute'); await expect(page.locator('#status')).toContainText('display precision');
  await expect(page.locator('#export-curve')).toBeDisabled();
});
