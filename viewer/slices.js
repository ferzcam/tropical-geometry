'use strict';

const $ = id => document.getElementById(id);
const examples = {
  cubic: [
    [0, 0, 0, '0'], [1, 0, 0, '1'], [0, 1, 0, '1'], [2, 0, 0, '4'],
    [1, 1, 1, '3'], [0, 2, 0, '4'], [3, 0, 0, '9'], [2, 1, 0, '7'],
    [1, 2, 0, '7'], [0, 3, 0, '9']
  ],
  plane: [[0, 0, 0, '0'], [1, 0, 0, '0'], [0, 1, 0, '0'], [0, 0, 1, '0']],
  flat: [[0, 0, 0, '0'], [0, 0, 1, '0']]
};
const colors = ['#a8d2c1', '#ead2a0', '#cfc8e8', '#b9d6e8', '#e7bed0', '#d7dfad', '#b9dcd7', '#e8c6a5'];
const heightPattern = /^-?[0-9]{1,18}(?:\/[1-9][0-9]{0,17})?$/;

let terms = examples.cubic.map(([x, y, z, coefficient]) => ({ x, y, z, coefficient }));
let height = '0';
let board = null;
let baseBounds = [-7, 1, 1, -7];
let currentData = null;
let prepared = null;
let controller = null;
let revision = 0;
let debounce = null;
let regionElements = [];
let curveElements = [];
let paintingRegions = false;

function canonicalRational(value) {
  const text = String(value).trim();
  if (!heightPattern.test(text)) throw new Error('Enter an integer or exact fraction such as 1/3 (up to 18 digits per part).');
  let [n, d = '1'] = text.split('/');
  n = BigInt(n);
  d = BigInt(d);
  const gcd = (a, b) => b ? gcd(b, a % b) : a < 0n ? -a : a;
  const g = gcd(n, d);
  n /= g;
  d /= g;
  return d === 1n ? String(n) : n + '/' + d;
}

function toNumber(value) {
  const [n, d = '1'] = String(value).split('/');
  const result = Number(n) / Number(d);
  if (!Number.isFinite(result)) throw new Error('A coordinate is too large to display.');
  return result;
}

function exactDifference(a, b) {
  const [an, ad = '1'] = String(a).split('/');
  const [bn, bd = '1'] = String(b).split('/');
  const n = BigInt(an) * BigInt(bd) - BigInt(bn) * BigInt(ad);
  const d = BigInt(ad) * BigInt(bd);
  return toNumber(n + '/' + d);
}

function displayHeight(value) {
  height = canonicalRational(value);
  $('slice-height-value').textContent = height;
  $('slice-height-input').value = height;
  const numeric = toNumber(height);
  const slider = $('slice-height-slider');
  const note = $('slider-range-note');
  if (numeric >= -3 && numeric <= 3) {
    slider.value = String(numeric);
    note.hidden = true;
    slider.removeAttribute('aria-valuetext');
  } else {
    note.hidden = false;
    slider.setAttribute('aria-valuetext', height + ', outside slider range');
  }
}

function termExpression(term, includeZ) {
  const parts = [];
  if (term.coefficient !== '0') parts.push(term.coefficient);
  if (term.x) parts.push((term.x === 1 ? '' : term.x === -1 ? '-' : term.x) + 'x');
  if (term.y) parts.push((term.y === 1 ? '' : term.y === -1 ? '-' : term.y) + 'y');
  if (includeZ && term.z) parts.push((term.z === 1 ? '' : term.z === -1 ? '-' : term.z) + 'z');
  return parts.join(' + ').replace(/\+ -/g, '- ') || '0';
}

function renderSourceTerms() {
  const rows = terms.map((term, index) => {
    const row = document.createElement('tr');
    [String(index + 1), String(term.x), String(term.y), String(term.z), term.coefficient].forEach(value => {
      const cell = document.createElement('td');
      cell.textContent = value;
      row.appendChild(cell);
    });
    return row;
  });
  $('source-terms').replaceChildren(...rows);
  $('original-expression').textContent = 'min(' + terms.map(term => termExpression(term, true)).join(', ') + ')';
}

function setStatus(message, kind = '') {
  $('slice-status').textContent = message;
  $('slice-status').className = kind;
}

function updateGeometryStatus() {
  if (!currentData || $('slice-board').classList.contains('stale')) return;
  setStatus(
    'z = ' + height + ' · ' + (currentData.edges || []).length + ' curve edges · ' +
    (currentData.vertices || []).length + ' vertices · ' + regionElements.length + ' filled regions in view'
  );
}

function invalidate() {
  revision++;
  controller?.abort();
  controller = null;
  $('export-slice').disabled = true;
  $('slice-board').classList.add('stale');
  return revision;
}

function initBoard() {
  if (board) return;
  board = JXG.JSXGraph.initBoard('slice-board', {
    boundingbox: baseBounds,
    axis: true,
    grid: true,
    keepaspectratio: true,
    renderer: 'svg',
    showCopyright: false,
    showNavigation: true,
    showInfobox: false,
    pan: { enabled: true, needShift: true },
    zoom: { enabled: true, wheel: true, needShift: false },
    defaultAxes: {
      x: { strokeColor: '#a3b0a6', ticks: { strokeColor: '#ccd4cc', label: { fontSize: 10 } } },
      y: { strokeColor: '#a3b0a6', ticks: { strokeColor: '#ccd4cc', label: { fontSize: 10 } } }
    }
  });
  board.on('boundingbox', () => {
    if (!currentData || paintingRegions) return;
    drawRegions(currentData.regions || []);
    renderCurve(prepared);
    updateGeometryStatus();
  });
}

function clearElements(elements) {
  for (const element of elements) {
    try { board.removeObject(element); } catch {}
  }
  elements.length = 0;
}

function clipHalfPlane(polygon, a, b, bound) {
  if (!polygon.length) return [];
  const out = [];
  const value = point => a * point[0] + b * point[1] - bound;
  for (let i = 0; i < polygon.length; i++) {
    const p = polygon[i];
    const q = polygon[(i + 1) % polygon.length];
    const vp = value(p);
    const vq = value(q);
    const insideP = vp <= 1e-9;
    const insideQ = vq <= 1e-9;
    if (insideP) out.push(p);
    if (insideP !== insideQ) {
      const t = vp / (vp - vq);
      out.push([p[0] + t * (q[0] - p[0]), p[1] + t * (q[1] - p[1])]);
    }
  }
  return out;
}

function clippedRegion(region, box) {
  const [left, top, right, bottom] = box;
  let polygon = [[left, bottom], [right, bottom], [right, top], [left, top]];
  for (const inequality of region.inequalities || []) {
    polygon = clipHalfPlane(
      polygon,
      toNumber(inequality.a),
      toNumber(inequality.b),
      toNumber(inequality.bound)
    );
    if (polygon.length < 3) return [];
  }
  const twiceArea = polygon.reduce((sum, p, i) => {
    const q = polygon[(i + 1) % polygon.length];
    return sum + p[0] * q[1] - q[0] * p[1];
  }, 0);
  return Math.abs(twiceArea) > 1e-8 ? polygon : [];
}

function drawRegions(regions) {
  if (!board) return;
  paintingRegions = true;
  try {
    clearElements(regionElements);
    const box = board.getBoundingBox();
    regions.forEach((region, index) => {
      const polygon = clippedRegion(region, box);
      if (polygon.length < 3) return;
      const color = colors[index % colors.length];
      const shape = board.create('polygon', polygon, {
        fixed: true,
        highlight: false,
        fillColor: color,
        fillOpacity: 0.3,
        borders: { strokeColor: color, strokeOpacity: 0.35, strokeWidth: 1, highlight: false },
        vertices: { visible: false },
        hasInnerPoints: true
      });
      regionElements.push(shape);
    });
  } finally {
    paintingRegions = false;
  }
}

function point(exactPoint) {
  return exactPoint.map(toNumber);
}

function prepareData(data) {
  const vertices = (data.vertices || []).map(vertex => ({ ...vertex, displayPoint: point(vertex.point) }));
  const edges = (data.edges || []).map(edge => {
    const start = point(edge.start);
    const expected = edge.end
      ? edge.end.map((coordinate, i) => exactDifference(coordinate, edge.start[i]))
      : point(edge.direction);
    const end = edge.end ? point(edge.end) : expected.map((delta, i) => start[i] + delta);
    if (expected.some((delta, i) => Math.abs((end[i] - start[i]) - delta) > Math.abs(delta) * 1e-10)) {
      throw new Error('This geometry exceeds browser display precision. Reduce the coefficients or height.');
    }
    return { ...edge, displayStart: start, displayEnd: end };
  });
  for (const region of data.regions || []) {
    for (const inequality of region.inequalities || []) {
      toNumber(inequality.a);
      toNumber(inequality.b);
      toNumber(inequality.bound);
    }
  }
  return { vertices, edges };
}

function renderCurve(displayData) {
  clearElements(curveElements);
  const fixed = { fixed: true, highlight: false };
  for (const edge of displayData.edges) {
    curveElements.push(board.create('line', [edge.displayStart, edge.displayEnd], {
      ...fixed,
      strokeColor: '#146451',
      strokeWidth: 3,
      straightFirst: edge.kind === 'line',
      straightLast: edge.kind !== 'segment'
    }));
  }
  for (const vertex of displayData.vertices) {
    curveElements.push(board.create('point', vertex.displayPoint, {
      ...fixed,
      size: 3,
      strokeColor: '#0e5141',
      fillColor: '#fff',
      strokeWidth: 2,
      name: ''
    }));
  }
}

function draw(data, displayData, resetView) {
  initBoard();
  currentData = data;
  prepared = displayData;
  if (resetView) board.setBoundingBox(baseBounds, true);
  drawRegions(data.regions || []);
  renderCurve(displayData);
  $('slice-board').classList.remove('stale');
  updateGeometryStatus();
}

function explain(data, z) {
  const numericHeight = toNumber(z);
  const edges = data.edges || [];
  const vertices = data.vertices || [];
  const isFlatExample = terms.length === 2 && terms.every(term => term.x === 0 && term.y === 0) && terms.some(term => term.z !== 0);
  if (isFlatExample) {
    return numericHeight === 0
      ? 'At z = 0 the two terms tie everywhere, so the whole plane is the slice.'
      : 'Away from z = 0 one term is strictly smaller everywhere; this slice is empty.';
  }
  const isLiftedCubic = terms.length === 10 && terms.some(term => term.x === 1 && term.y === 1 && term.z === 1);
  if (isLiftedCubic) {
    if (numericHeight === 0) return 'At z = 0 this slice contains a hexagonal loop. As the slice rises, the lifted interior term shrinks that loop.';
    if (numericHeight === 1) return 'At z = 1 the hexagonal loop has collapsed. The remaining curve is still part of the slice.';
    if (numericHeight > 1) return 'Above z = 1 the lifted interior term no longer wins on a two-dimensional region; the hexagonal loop is gone.';
  }
  if (!edges.length && !vertices.length) {
    return 'At z = ' + z + ' one term wins everywhere. The slice does not meet the tropical surface.';
  }
  if (!edges.length) {
    return 'At z = ' + z + ' the specialized curve has no edges in this view, while filled regions still mark surface points in the slice.';
  }
  return 'At z = ' + z + ' the exact slice has ' + vertices.length + ' vertices and ' + edges.length + ' curve edges. Filled regions mark where multiple original 3D terms tie for the minimum.';
}

function sliderFraction(value) {
  const quarters = Math.round(Number(value) * 4);
  const n = BigInt(quarters);
  const d = 4n;
  const gcd = (a, b) => b ? gcd(b, a % b) : a < 0n ? -a : a;
  const g = gcd(n, d);
  return d / g === 1n ? String(n / g) : String(n / g) + '/' + String(d / g);
}

async function requestSlice(value, resetView = false) {
  const id = invalidate();
  controller = new AbortController();
  try {
    displayHeight(value);
    $('specialized-expression').textContent = 'Computing exact specialization…';
    $('slice-explanation').textContent = 'The backend is computing this slice with exact rational arithmetic.';
    setStatus('Computing exact slice…', 'pending');
    const response = await fetch('/api/slice', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ terms, height }),
      signal: controller.signal
    });
    const data = await response.json();
    if (id !== revision) return;
    if (!response.ok) throw new Error(data.error || 'Slice request failed (' + response.status + ').');
    const displayData = prepareData(data);
    const actualHeight = canonicalRational(data.height ?? height);
    displayHeight(actualHeight);
    $('specialized-expression').textContent = 'min(' + (data.terms || []).map(term => termExpression(term, false)).join(', ') + ')';
    draw(data, displayData, resetView);
    $('slice-explanation').textContent = explain(data, actualHeight);
    updateGeometryStatus();
    $('export-slice').disabled = false;
  } catch (error) {
    if (id !== revision || error.name === 'AbortError') return;
    setStatus(error.message || 'Could not compute this slice.', 'error');
  }
}

function exportSvg() {
  if (!board || !currentData || $('export-slice').disabled) return;
  const source = $('slice-board').querySelector('svg');
  if (!source) return;
  const svg = source.cloneNode(true);
  svg.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
  const boardBox = $('slice-board').getBoundingClientRect();
  svg.setAttribute('width', Math.round(boardBox.width));
  svg.setAttribute('height', Math.round(boardBox.height));
  const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
  title.textContent = 'Tropical slice at z = ' + height;
  const metadata = document.createElementNS('http://www.w3.org/2000/svg', 'desc');
  metadata.textContent = JSON.stringify({ height, sourceTerms: terms, slice: currentData });
  svg.prepend(metadata);
  svg.prepend(title);
  const url = URL.createObjectURL(new Blob([new XMLSerializer().serializeToString(svg)], { type: 'image/svg+xml' }));
  const anchor = document.createElement('a');
  anchor.href = url;
  anchor.download = 'tropical-slice-z-' + height.replace('/', '-of-') + '.svg';
  anchor.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

function chooseExample(name) {
  if (!examples[name]) return;
  clearTimeout(debounce);
  terms = examples[name].map(([x, y, z, coefficient]) => ({ x, y, z, coefficient }));
  renderSourceTerms();
  baseBounds = name === 'cubic' ? [-7, 1, 1, -7] : [-4.5, 4.5, 4.5, -4.5];
  if (board) board.setBoundingBox(baseBounds, true);
  requestSlice(height);
}

$('slice-example').addEventListener('change', event => chooseExample(event.target.value));
$('slice-height-slider').addEventListener('input', event => {
  const candidate = sliderFraction(event.target.value);
  clearTimeout(debounce);
  invalidate();
  try {
    displayHeight(candidate);
    setStatus('Height changed; recomputing the exact slice…', 'pending');
    debounce = setTimeout(() => requestSlice(candidate), 110);
  } catch (error) {
    setStatus(error.message, 'error');
  }
});
$('slice-height-input').addEventListener('input', () => {
  clearTimeout(debounce);
  invalidate();
  $('slice-height-input').removeAttribute('aria-invalid');
  setStatus('Height changed; enter a valid exact value to update the slice.', 'pending');
});
$('slice-height-input').addEventListener('change', event => {
  try {
    const candidate = canonicalRational(event.target.value);
    event.target.removeAttribute('aria-invalid');
    requestSlice(candidate);
  } catch (error) {
    event.target.setAttribute('aria-invalid', 'true');
    $('export-slice').disabled = true;
    setStatus(error.message, 'error');
  }
});
$('reset-slice').addEventListener('click', () => {
  if (board) board.setBoundingBox(baseBounds, true);
});
$('export-slice').addEventListener('click', exportSvg);

renderSourceTerms();
requestSlice('0', true);
