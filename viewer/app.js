'use strict';

// Geometry comes exclusively from the exact Haskell API. Numbers below are
// converted to floating point only at the JSXGraph rendering boundary.
const $ = id => document.getElementById(id);
const palette = ['#157768', '#bc6534', '#6558a5', '#367eb0', '#ad4776', '#7b842b'];
const examples = {
  line: [[0, 0, '0'], [1, 0, '0'], [0, 1, '0']],
  fractional: [[0, 0, '0'], [2, 0, '-1'], [0, 2, '-1']],
  square: [[0, 0, '0'], [1, 0, '0'], [1, 1, '0'], [0, 1, '0']],
  hexagon: [[0, 0, '0'], [1, 0, '0'], [2, 1, '0'], [2, 2, '0'], [1, 2, '0'], [0, 1, '0']],
  mixed: [[0, 0, '0'], [1, 0, '0'], [1, 1, '0'], [0, 1, '0'], [2, 0, '1']],
  parallel: [[0, 0, '0'], [1, 0, '-1'], [2, 0, '0']]
};
let boards = {}, bounds = {}, groups = [], selected = null, requestId = 0, controller, result;
const defaultSelection = 'Hover over geometry or select an item below. Matching colors connect the two views.';

function status(message, kind = '') { $('status').textContent = message; $('status').className = kind; }
function invalidate() {
  requestId++;
  controller?.abort();
  document.querySelector('.explorer').classList.add('stale');
  ['export-curve', 'export-newton'].forEach(id => $(id).disabled = true);
  document.querySelectorAll('.feature').forEach(button => button.disabled = true);
  status('Polynomial changed. Draw it to update both views.', 'pending');
}
function addRow(values = [0, 0, '0']) {
  const row = document.createElement('tr');
  row.appendChild(document.createElement('td'));
  ['a', 'b', 'c'].forEach((name, index) => {
    const cell = document.createElement('td'), input = document.createElement('input');
    input.type = 'text'; input.value = values[index]; input.required = true;
    input.dataset.field = name; input.autocomplete = 'off';
    if (index < 2) input.inputMode = 'numeric';
    input.addEventListener('input', invalidate);
    cell.appendChild(input); row.appendChild(cell);
  });
  const cell = document.createElement('td'), remove = document.createElement('button');
  remove.type = 'button'; remove.textContent = '×';
  remove.addEventListener('click', () => { row.remove(); renumber(); invalidate(); });
  cell.appendChild(remove); row.appendChild(cell); $('terms').appendChild(row); renumber();
}
function renumber() {
  [...$('terms').children].forEach((row, i) => {
    row.firstChild.textContent = i + 1;
    [...row.querySelectorAll('input')].forEach(input => input.setAttribute('aria-label', `Term ${i + 1} ${input.dataset.field}`));
    row.querySelector('button').setAttribute('aria-label', `Remove term ${i + 1}`);
  });
}
function readTerms() {
  const terms = [...$('terms').children].map((row, i) => {
    const values = [...row.querySelectorAll('input')].map(input => input.value.trim());
    if (!values.slice(0, 2).every(v => /^[+-]?\d+$/.test(v) && Number.isSafeInteger(Number(v)))) throw new Error(`Term ${i + 1}: exponents must be integers in the supported range.`);
    return { x: Number(values[0]), y: Number(values[1]), coefficient: values[2] };
  });
  if (!terms.length) throw new Error('Add at least one term.');
  return terms;
}
function number(value) {
  const parts = String(value).split('/'), n = Number(parts[0]) / (parts.length === 2 ? Number(parts[1]) : 1);
  if (!Number.isFinite(n)) throw new Error('A coordinate is too large to display. Try smaller coefficients.');
  return n;
}
const point = p => p.map(number);
const exactPoint = p => `(${p.join(', ')})`;
function exactDifference(a, b) {
  const [an, ad = '1'] = String(a).split('/'), [bn, bd = '1'] = String(b).split('/');
  return number(`${BigInt(an) * BigInt(bd) - BigInt(bn) * BigInt(ad)}/${BigInt(ad) * BigInt(bd)}`);
}
function edgeEndpoints(edge) {
  const start = point(edge.start);
  const expected = edge.end ? edge.end.map((v, i) => exactDifference(v, edge.start[i])) : point(edge.direction);
  const end = edge.end ? point(edge.end) : expected.map((v, i) => start[i] + v);
  if (expected.some((v, i) => Math.abs((end[i] - start[i]) - v) > Math.abs(v) * 1e-10)) throw new Error('This geometry exceeds the display precision of the browser. Try smaller coefficients; exact geometry has not been rounded.');
  return [start, end];
}
function fit(points, margin = 2) {
  if (!points.length) return [-4, 4, 4, -4];
  const xs = points.map(p => p[0]), ys = points.map(p => p[1]);
  const loX = Math.min(...xs), hiX = Math.max(...xs), loY = Math.min(...ys), hiY = Math.max(...ys);
  const span = Math.max(hiX - loX, hiY - loY, 2), pad = Math.max(margin, span * .2);
  return [loX - pad, hiY + pad, hiX + pad, loY - pad];
}
function initBoard(name, boundingbox) {
  if (boards[name]) JXG.JSXGraph.freeBoard(boards[name]);
  bounds[name] = boundingbox;
  boards[name] = JXG.JSXGraph.initBoard(`${name}-board`, {
    boundingbox, axis: true, grid: true, keepaspectratio: true, renderer: 'svg',
    showCopyright: false, showNavigation: true, showInfobox: false,
    pan: { enabled: true, needShift: true }, zoom: { enabled: true, wheel: true, needShift: false },
    defaultAxes: { x: { strokeColor: '#a3b0a6', ticks: { strokeColor: '#ccd4cc', label: { fontSize: 10 } } }, y: { strokeColor: '#a3b0a6', ticks: { strokeColor: '#ccd4cc', label: { fontSize: 10 } } } }
  });
  return boards[name];
}
function highlight(key) {
  for (const group of groups) {
    const active = group.key === key;
    for (const item of group.items) item.element.setAttribute({ ...item.base, ...(active ? item.active : {}) });
    group.button.setAttribute('aria-pressed', String(group.key === selected));
  }
  $('selection').textContent = groups.find(group => group.key === key)?.description || defaultSelection;
}
function register(key, label, description, color, items) {
  const button = document.createElement('button');
  button.type = 'button'; button.className = 'feature'; button.textContent = label;
  button.style.setProperty('--feature-color', color); button.setAttribute('aria-pressed', 'false');
  const toggle = () => { selected = selected === key ? null : key; highlight(selected); };
  button.addEventListener('click', toggle);
  button.addEventListener('mouseenter', () => highlight(key));
  button.addEventListener('mouseleave', () => highlight(selected));
  button.addEventListener('focus', () => highlight(key));
  button.addEventListener('blur', () => highlight(selected));
  for (const item of items) {
    item.element.on('over', () => highlight(key)); item.element.on('out', () => highlight(selected));
    item.element.on('down', toggle);
  }
  $('features').appendChild(button); groups.push({ key, button, items, description });
}
function render(data) {
  // Validate all display conversions before replacing the current boards.
  const curvePoints = [...data.vertices.map(v => point(v.point)), ...data.edges.flatMap(e => e.end ? [point(e.start), point(e.end)] : [point(e.start)])];
  data.edges.forEach(edgeEndpoints);
  const termPoints = data.terms.map(t => [number(t.x), number(t.y)]);
  const curve = initBoard('curve', fit(curvePoints)), newton = initBoard('newton', fit(termPoints, .7));
  groups = []; selected = null; $('features').replaceChildren();
  const fixed = { fixed: true, highlight: false }, vertexElements = new Map();
  for (const vertex of data.vertices) {
    const color = palette[vertex.id % palette.length];
    const element = curve.create('point', point(vertex.point), { ...fixed, size: 4, name: exactPoint(vertex.point), strokeColor: color, fillColor: color, label: { display: 'internal', fontSize: 12, offset: [9, 12] } });
    vertexElements.set(vertex.id, { element, base: { size: 4 }, active: { size: 7 } });
  }
  for (const cell of data.cells) {
    const color = palette[cell.vertex % palette.length];
    const polygon = newton.create('polygon', cell.boundary.map(index => termPoints[index]), { ...fixed, fillColor: color, fillOpacity: .12, borders: { strokeWidth: 1, strokeColor: color, highlight: false }, vertices: { visible: false }, hasInnerPoints: true });
    const vertex = data.vertices.find(v => v.id === cell.vertex);
    register(`cell-${cell.id}`, `Vertex ${exactPoint(vertex.point)} ↔ ${cell.boundary.length}-gon`, `Curve vertex ${exactPoint(vertex.point)} ↔ subdivision cell with ${cell.boundary.length} corners. Terms ${cell.terms.map(i => i + 1).join(', ')} attain the minimum.`, color, [vertexElements.get(cell.vertex), { element: polygon, base: { fillOpacity: .12 }, active: { fillOpacity: .4 } }]);
  }
  for (const edge of data.edges) {
    const color = palette[(edge.id + 2) % palette.length], [start, end] = edgeEndpoints(edge);
    const attrs = { ...fixed, strokeColor: color, strokeWidth: 2.5, straightFirst: edge.kind === 'line', straightLast: edge.kind !== 'segment' };
    const line = curve.create('line', [start, end], attrs);
    const dual = newton.create('segment', edge.dual.map(i => termPoints[i]), { ...fixed, strokeColor: color, strokeWidth: 2.5 });
    const detail = edge.kind === 'segment' ? `${exactPoint(edge.start)} to ${exactPoint(edge.end)}` : `through ${exactPoint(edge.start)}, direction ${exactPoint(edge.direction)}`;
    register(`edge-${edge.id}`, `${edge.kind} ${edge.id + 1} · weight ${edge.weight}`, `${edge.kind[0].toUpperCase() + edge.kind.slice(1)} ${detail}; weight ${edge.weight}. Dual edge ${edge.dual.map(i => exactPoint(termPoints[i])).join(' to ')}. Terms ${edge.terms.map(i => i + 1).join(', ')} attain the minimum.`, color, [line, dual].map(element => ({ element, base: { strokeWidth: 2.5 }, active: { strokeWidth: 5 } })));
  }
  data.terms.forEach((term, i) => newton.create('point', termPoints[i], { ...fixed, size: 2, name: `${i + 1}`, strokeColor: '#435b4d', fillColor: '#fff', label: { display: 'internal', fontSize: 11, offset: [7, -11] } }));
  $('selection').textContent = defaultSelection;
  $('formula').textContent = `min(${data.terms.map(t => `${t.coefficient} + ${t.x}·x + ${t.y}·y`).join(', ')})`;
  $('normalized-terms').textContent = data.terms.map((t, i) => `${i + 1}: ${t.coefficient} + ${t.x}·x + ${t.y}·y`).join(' ; ');
  document.querySelector('.explorer').classList.remove('stale');
  ['export-curve', 'export-newton'].forEach(id => $(id).disabled = false);
  result = data;
}
async function compute(event) {
  event?.preventDefault();
  invalidate(); const id = requestId; controller = new AbortController();
  try {
    const terms = readTerms(); status('Computing exact geometry…', 'pending');
    const response = await fetch('/api/curve', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ terms }), signal: controller.signal });
    const data = await response.json();
    if (id !== requestId) return;
    if (!response.ok) throw new Error(data.error || `Geometry request failed (${response.status}).`);
    render(data);
    const normalized = data.terms.length !== terms.length ? ' Repeated exponents were combined; plot labels use the normalized terms below.' : '';
    status(`${data.vertices.length} vertices · ${data.edges.length} edges · ${data.cells.length} cells.${normalized}${data.edges.length ? '' : ' The tropical curve is empty.'}`);
  } catch (error) {
    if (id !== requestId || error.name === 'AbortError') return;
    status(error.message || 'Could not compute geometry.', 'error');
  }
}
function exportSvg(name) {
  if ($(name === 'curve' ? 'export-curve' : 'export-newton').disabled) return;
  const source = $(`${name}-board`).querySelector('svg'), svg = source.cloneNode(true);
  svg.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
  svg.setAttribute('width', source.getAttribute('width') || source.clientWidth);
  svg.setAttribute('height', source.getAttribute('height') || source.clientHeight);
  const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
  title.textContent = `${name === 'curve' ? 'Tropical curve' : 'Newton subdivision'}: ${$('formula').textContent}`;
  svg.prepend(title);
  const description = document.createElementNS('http://www.w3.org/2000/svg', 'desc');
  description.textContent = JSON.stringify(result); svg.prepend(description);
  const url = URL.createObjectURL(new Blob([new XMLSerializer().serializeToString(svg)], { type: 'image/svg+xml' }));
  const anchor = document.createElement('a'); anchor.href = url; anchor.download = `tropical-${name}.svg`; anchor.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}
$('add-term').addEventListener('click', () => { addRow(); invalidate(); });
$('polynomial-form').addEventListener('submit', compute);
$('example').addEventListener('change', () => { $('terms').replaceChildren(); examples[$('example').value].forEach(addRow); compute(); });
$('clear-selection').addEventListener('click', () => { selected = null; highlight(null); });
for (const name of ['curve', 'newton']) {
  $(`reset-${name}`).addEventListener('click', () => boards[name]?.setBoundingBox(bounds[name], true));
  $(`export-${name}`).addEventListener('click', () => exportSvg(name));
}
examples.line.forEach(addRow);
compute();
