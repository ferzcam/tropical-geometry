const { test, expect } = require('@playwright/test');

async function setSlider(page, value) {
  await page.locator('#slice-height-slider').evaluate((slider, next) => {
    slider.value = next;
    slider.dispatchEvent(new Event('input', { bubbles: true }));
  }, String(value));
}

const polygonsInView = page => page.evaluate(() => {
  const board = Object.values(JXG.boards).find(item => item.container === 'slice-board');
  return board.objectsList.filter(object => object.elType === 'polygon').length;
});

test('cubic slice loop collapses while the surrounding curve remains', async ({ page }) => {
  await page.goto('/slices.html');
  await expect(page.locator('#slice-status')).toContainText('z = 0 · 18 curve edges');
  const boardMetrics = await page.evaluate(() => {
    const element = document.querySelector('#slice-board');
    const board = Object.values(JXG.boards).find(item => item.container === 'slice-board');
    return { width: element.clientWidth, height: element.clientHeight, unitX: board.unitX, unitY: board.unitY };
  });
  expect(boardMetrics.width).toBeGreaterThan(0);
  expect(boardMetrics.height).toBeGreaterThan(0);
  expect(boardMetrics.unitX).toBeCloseTo(boardMetrics.unitY, 6);
  await expect(page.locator('#slice-explanation')).toContainText('hexagonal loop');
  const h0Segments = await page.evaluate(() => {
    const board = Object.values(JXG.boards).find(item => item.container === 'slice-board');
    return board.objectsList.filter(object => object.elType === 'line' && object.getAttribute('straightFirst') === false && object.getAttribute('straightLast') === false).length;
  });
  expect(h0Segments).toBeGreaterThanOrEqual(6);

  await setSlider(page, 1);
  await expect(page.locator('#slice-status')).toContainText('z = 1 · 12 curve edges');
  await expect(page.locator('#slice-explanation')).toContainText('loop has collapsed');
  await setSlider(page, 2);
  await expect(page.locator('#slice-status')).toContainText('z = 2 · 12 curve edges');
  await expect(page.locator('#slice-explanation')).toContainText('loop is gone');
  await expect(page.locator('#export-slice')).toBeEnabled();
});

test('plane and coincident-term examples show positive-area and whole-plane slices', async ({ page }) => {
  await page.goto('/slices.html');
  await expect(page.locator('#slice-status')).toContainText('z = 0');
  await page.selectOption('#slice-example', 'plane');
  await expect(page.locator('#slice-status')).toContainText('3 curve edges');
  await expect(page.locator('#slice-explanation')).toContainText('Filled regions mark');
  await expect.poll(() => polygonsInView(page)).toBeGreaterThan(0);
  const initialBox = await page.evaluate(() => Object.values(JXG.boards).find(item => item.container === 'slice-board').getBoundingBox());
  await page.locator('#slice-board').hover();
  await page.mouse.wheel(0, -300);
  await expect.poll(() => page.evaluate(() => Object.values(JXG.boards).find(item => item.container === 'slice-board').getBoundingBox())).not.toEqual(initialBox);
  await expect.poll(() => polygonsInView(page)).toBeGreaterThan(0);
  await page.click('#reset-slice');
  await expect.poll(() => page.evaluate(() => Object.values(JXG.boards).find(item => item.container === 'slice-board').getBoundingBox())).toEqual(initialBox);

  await setSlider(page, 1);
  await expect(page.locator('#slice-status')).toContainText('z = 1 · 3 curve edges');
  expect(await polygonsInView(page)).toBe(0);
  await setSlider(page, -1);
  await expect(page.locator('#slice-status')).toContainText('z = -1 · 3 curve edges');
  expect(await polygonsInView(page)).toBe(0);

  await page.selectOption('#slice-example', 'flat');
  await setSlider(page, 0);
  await expect(page.locator('#slice-explanation')).toContainText('whole plane is the slice');
  await expect.poll(() => polygonsInView(page)).toBeGreaterThan(0);
  await setSlider(page, -1);
  await expect(page.locator('#slice-explanation')).toContainText('this slice is empty');
  expect(await polygonsInView(page)).toBe(0);
});

test('rational height stays exact in the formula and SVG metadata', async ({ page }) => {
  await page.goto('/slices.html');
  await expect(page.locator('#slice-status')).toContainText('z = 0');
  await page.locator('#slice-height-input').fill('1/3');
  await page.locator('#slice-height-input').press('Tab');
  await expect(page.locator('#slice-height-value')).toHaveText('1/3');
  await expect(page.locator('#specialized-expression')).toContainText('10/3');
  const downloadPromise = page.waitForEvent('download');
  await page.click('#export-slice');
  const download = await downloadPromise;
  expect(download.suggestedFilename()).toContain('1-of-3');
  const stream = await download.createReadStream();
  let svg = '';
  for await (const chunk of stream) svg += chunk;
  expect(svg).toContain('<title>Tropical slice at z = 1/3</title>');
  expect(svg).toContain('"height":"1/3"');
  expect(svg).toContain('"sourceTerms"');
});

test('height edits immediately stale the plot, reject invalid text, and ignore an older response', async ({ page }) => {
  await page.addInitScript(() => {
    const nativeFetch = window.fetch.bind(window);
    window.fetch = (input, init = {}) => {
      const url = typeof input === 'string' ? input : input.url;
      if (url.includes('/api/slice')) {
        const unabortable = { ...init };
        delete unabortable.signal;
        return nativeFetch(input, unabortable);
      }
      return nativeFetch(input, init);
    };
  });
  let held = false;
  let releaseOld;
  const oldGate = new Promise(resolve => { releaseOld = resolve; });
  await page.route('**/api/slice', async route => {
    const { height } = route.request().postDataJSON();
    if (height === '1/4') {
      const response = await route.fetch();
      held = true;
      await oldGate;
      await route.fulfill({ response });
    } else {
      await route.continue();
    }
  });
  await page.goto('/slices.html');
  await expect(page.locator('#slice-status')).toContainText('z = 0');
  await setSlider(page, 0.25);
  await expect(page.locator('#slice-board')).toHaveClass(/stale/);
  await expect(page.locator('#export-slice')).toBeDisabled();
  await expect.poll(() => held).toBe(true);
  await setSlider(page, 0.5);
  await expect(page.locator('#slice-status')).toContainText('z = 1/2');
  await expect(page.locator('#specialized-expression')).toContainText('7/2');
  releaseOld();
  await page.waitForTimeout(100);
  await expect(page.locator('#slice-status')).toContainText('z = 1/2');
  await expect(page.locator('#specialized-expression')).toContainText('7/2');

  const previousHeight = await page.locator('#slice-height-value').textContent();
  await page.locator('#slice-height-input').fill('1/0');
  await expect(page.locator('#slice-board')).toHaveClass(/stale/);
  await expect(page.locator('#export-slice')).toBeDisabled();
  await page.locator('#slice-height-input').press('Tab');
  await expect(page.locator('#slice-status')).toHaveClass('error');
  await expect(page.locator('#slice-height-value')).toHaveText(previousHeight);
  await expect(page.locator('#slice-board')).toHaveClass(/stale/);

  await page.locator('#slice-height-input').fill('4');
  await page.locator('#slice-height-input').press('Tab');
  await expect(page.locator('#slice-height-value')).toHaveText('4');
  await expect(page.locator('#slider-range-note')).toBeVisible();
  await expect(page.locator('#slice-status')).toContainText('z = 4');
});
