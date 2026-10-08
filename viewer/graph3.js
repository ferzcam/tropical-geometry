// Three-variable explorer: the exact one-skeleton of a tropical surface beside
// the dual Newton subdivision. Geometry comes exclusively from the exact
// Haskell API; numbers are converted to floating point only at the three.js
// rendering boundary, after the same precision checks as the 2D viewer.
import * as THREE from 'three';
import { OrbitControls } from '/vendor/three/OrbitControls.js';

const $ = id => document.getElementById(id);
const palette = ['#157768', '#bc6534', '#6558a5', '#367eb0', '#ad4776', '#7b842b'];
const methodLabels = { direct: 'direct solver', hull: 'tailored hull', lrs: 'LRS' };
const examples = {
  plane: [[0, 0, 0, '0'], [1, 0, 0, '0'], [0, 1, 0, '0'], [0, 0, 1, '0']],
  fractional: [[0, 0, 0, '0'], [1, 0, 0, '-1/2'], [0, 1, 0, '-2/3'], [0, 0, 1, '-3/4']],
  split: [[0, 0, 0, '0'], [1, 0, 0, '-1'], [2, 0, 0, '0'], [0, 1, 0, '0'], [0, 0, 1, '0']],
  cubic: [[0, 0, 0, '0'], [1, 0, 0, '1'], [0, 1, 0, '1'], [2, 0, 0, '4'], [1, 1, 1, '3'], [0, 2, 0, '4'], [3, 0, 0, '9'], [2, 1, 0, '7'], [1, 2, 0, '7'], [0, 3, 0, '9']],
  cubes: [0, 1, 2].flatMap(x => [0, 1, 2].flatMap(y => [0, 1, 2].map(z => [x, y, z, String(x * x + y * y + z * z)])))
};
const defaultSelection = 'Hover over geometry or select an item below. Matching colors connect the two views.';
let views = {}, groups = [], selected = null, hovered = null, requestId = 0, importId = 0, controller, result;

function status(message, kind = '') { $('status').textContent = message; $('status').className = kind; }
function invalidate() {
  requestId++;
  controller?.abort();
  document.querySelector('.explorer').classList.add('stale');
  ['export-graph', 'export-newton'].forEach(id => $(id).disabled = true);
  document.querySelectorAll('.feature').forEach(button => button.disabled = true);
  status('Polynomial changed. Draw it to update both views.', 'pending');
}
function addRow(values = [0, 0, 0, '0']) {
  const row = document.createElement('tr');
  row.appendChild(document.createElement('td'));
  ['a', 'b', 'd', 'c'].forEach((name, index) => {
    const cell = document.createElement('td'), input = document.createElement('input');
    input.type = 'text'; input.value = values[index]; input.required = true;
    input.dataset.field = name; input.autocomplete = 'off';
    if (index < 3) input.inputMode = 'numeric';
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
// Keep these file/editor limits aligned with serve.py's validate_request.
const coefficientPattern = /^-?[0-9]{1,18}(?:\/[1-9][0-9]{0,17})?$/;
function hasKeys(value, keys) {
  return value !== null && typeof value === 'object' && !Array.isArray(value)
    && Object.keys(value).length === keys.length
    && keys.every(key => Object.prototype.hasOwnProperty.call(value, key));
}
function validatePolynomial(value) {
  if (!hasKeys(value, ['terms'])) throw new Error('Expected an object containing "terms".');
  if (!Array.isArray(value.terms) || value.terms.length < 1 || value.terms.length > 32) throw new Error('Provide between 1 and 32 terms.');
  value.terms.forEach((term, i) => {
    if (!hasKeys(term, ['x', 'y', 'z', 'coefficient'])) throw new Error(`Term ${i + 1}: each term needs x, y, z, and coefficient.`);
    if (![term.x, term.y, term.z].every(n => Number.isInteger(n) && Math.abs(n) <= 100)) throw new Error(`Term ${i + 1}: exponents must be integers between -100 and 100.`);
    if (typeof term.coefficient !== 'string' || coefficientPattern.exec(term.coefficient)?.[0] !== term.coefficient) throw new Error(`Term ${i + 1}: coefficients must be integer or fraction strings, with at most 18 digits per part and a positive denominator.`);
  });
  return value.terms;
}
function readTerms() {
  const terms = [...$('terms').children].map((row, i) => {
    const values = [...row.querySelectorAll('input')].map(input => input.value.trim());
    if (!values.slice(0, 3).every(v => /^[+-]?\d+$/.test(v) && Number.isSafeInteger(Number(v)))) throw new Error(`Term ${i + 1}: exponents must be integers in the supported range.`);
    return { x: Number(values[0]), y: Number(values[1]), z: Number(values[2]), coefficient: values[3] };
  });
  return validatePolynomial({ terms });
}
function fileStatus(message, kind = '') {
  $('file-status').textContent = message;
  $('file-status').className = `hint ${kind}`;
}
async function importPolynomial() {
  const file = $('polynomial-file').files[0];
  $('polynomial-file').value = ''; // Allow selecting the same file again.
  if (!file) return;
  const id = ++importId, revision = requestId;
  try {
    if (file.size > 32768) throw new Error('Polynomial files must be at most 32768 bytes.');
    const content = await file.text();
    if (id !== importId || revision !== requestId) return;
    let value;
    try { value = JSON.parse(content); }
    catch { throw new Error('The file must contain valid JSON.'); }
    const terms = validatePolynomial(value);
    $('terms').replaceChildren();
    terms.forEach(term => addRow([term.x, term.y, term.z, term.coefficient]));
    $('example').value = 'custom';
    fileStatus(`Imported ${file.name}.`);
    compute();
  } catch (error) {
    if (id !== importId || revision !== requestId) return;
    fileStatus(`Could not import polynomial: ${error.message}`, 'error');
  }
}
function exportPolynomial() {
  try {
    const terms = readTerms();
    download(URL.createObjectURL(new Blob([JSON.stringify({ terms }, null, 2) + '\n'], { type: 'application/json' })), 'tropical-polynomial-3d.json');
    fileStatus('Exported tropical-polynomial-3d.json.');
  } catch (error) {
    fileStatus(`Could not export polynomial: ${error.message}`, 'error');
  }
}
function download(url, name) {
  const anchor = document.createElement('a');
  anchor.href = url; anchor.download = name; anchor.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
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
// Returns the float start plus either the float end (segments) or the exact
// primitive direction (rays), after checking the display can tell them apart.
function edgeEndpoints(edge) {
  const start = point(edge.start);
  const expected = edge.end ? edge.end.map((v, i) => exactDifference(v, edge.start[i])) : point(edge.direction);
  const end = edge.end ? point(edge.end) : expected.map((v, i) => start[i] + v);
  if (expected.some((v, i) => Math.abs((end[i] - start[i]) - v) > Math.abs(v) * 1e-10)) throw new Error('This geometry exceeds the display precision of the browser. Try smaller coefficients; exact geometry has not been rounded.');
  return { start, end, direction: edge.end ? null : expected };
}

// --- three.js views ---------------------------------------------------------
const raycaster = new THREE.Raycaster();
function createView(name) {
  const container = $(`${name}-view`);
  const renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true, preserveDrawingBuffer: true });
  renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
  container.appendChild(renderer.domElement);
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(38, 1, 0.01, 1000);
  camera.add(new THREE.DirectionalLight(0xffffff, 2.2).translateX(0.6).translateY(1).translateZ(1.2));
  const controls = new OrbitControls(camera, renderer.domElement);
  controls.enableDamping = false;
  const view = { name, container, renderer, scene, camera, controls, pickables: [], home: null, pending: false, downAt: null };
  controls.addEventListener('change', () => requestRender(view));
  const resize = () => {
    const width = container.clientWidth, height = container.clientHeight;
    if (!width || !height) return;
    renderer.setSize(width, height, false);
    camera.aspect = width / height; camera.updateProjectionMatrix();
    requestRender(view);
  };
  new ResizeObserver(resize).observe(container);
  resize();
  const dom = renderer.domElement;
  // Without a usable GPU (or software WebGL) browsers drop the context; say so
  // instead of leaving two blank panels beside exact-looking numbers.
  dom.addEventListener('webglcontextlost', event => {
    event.preventDefault();
    view.lost = true;
    status('The browser lost its WebGL context, so the 3D views are blank. The computed geometry listed below is still exact; enable hardware or software WebGL and draw again.', 'error');
  });
  dom.addEventListener('webglcontextrestored', () => { view.lost = false; requestRender(view); });
  dom.addEventListener('pointerdown', event => { view.downAt = [event.clientX, event.clientY]; });
  dom.addEventListener('pointerup', event => {
    const down = view.downAt; view.downAt = null;
    if (down && Math.hypot(event.clientX - down[0], event.clientY - down[1]) < 5) {
      const key = pickKey(view, event);
      if (key) toggle(key);
    }
  });
  dom.addEventListener('pointermove', event => {
    if (event.buttons) return;
    const key = pickKey(view, event);
    if (key !== hovered) { hovered = key; highlight(hovered ?? selected); dom.style.cursor = key ? 'pointer' : ''; }
  });
  dom.addEventListener('pointerleave', () => { if (hovered) { hovered = null; highlight(selected); } dom.style.cursor = ''; });
  return view;
}
function requestRender(view) {
  if (view.pending) return;
  view.pending = true;
  requestAnimationFrame(() => { view.pending = false; view.renderer.render(view.scene, view.camera); });
}
function renderAll() { Object.values(views).forEach(requestRender); }
function pickKey(view, event) {
  const rect = view.renderer.domElement.getBoundingClientRect();
  if (!rect.width || !rect.height) return null;
  raycaster.setFromCamera(new THREE.Vector2(((event.clientX - rect.left) / rect.width) * 2 - 1, -((event.clientY - rect.top) / rect.height) * 2 + 1), view.camera);
  const hits = raycaster.intersectObjects(view.pickables, false);
  return hits.length ? hits[0].object.userData.key : null;
}
function clearView(view) {
  view.scene.traverse(object => { object.geometry?.dispose(); if (object.material) { object.material.map?.dispose(); object.material.dispose(); } });
  view.scene.clear();
  view.pickables = [];
  view.scene.add(view.camera);
  view.scene.add(new THREE.AmbientLight(0xffffff, 1.4));
}
function frame(view, center, radius) {
  const distance = radius / Math.sin(THREE.MathUtils.degToRad(view.camera.fov / 2)) * 1.1;
  view.camera.position.copy(center).addScaledVector(new THREE.Vector3(1, 0.72, 1.35).normalize(), distance);
  view.camera.near = distance / 200; view.camera.far = distance * 50; view.camera.updateProjectionMatrix();
  view.controls.target.copy(center); view.controls.minDistance = distance / 20; view.controls.maxDistance = distance * 10; view.controls.update();
  view.home = { position: view.camera.position.clone(), target: center.clone() };
  requestRender(view);
}
function resetView(view) {
  if (!view.home) return;
  view.camera.position.copy(view.home.position); view.controls.target.copy(view.home.target); view.controls.update(); requestRender(view);
}
const up = new THREE.Vector3(0, 1, 0);
function tube(a, b, radius, material) {
  const direction = new THREE.Vector3().subVectors(b, a), length = direction.length();
  const mesh = new THREE.Mesh(new THREE.CylinderGeometry(radius, radius, length, 14, 1), material);
  mesh.position.copy(a).addScaledVector(direction, 0.5);
  mesh.quaternion.setFromUnitVectors(up, direction.normalize());
  return mesh;
}
function cone(tip, direction, radius, height, material) {
  const mesh = new THREE.Mesh(new THREE.ConeGeometry(radius, height, 16), material);
  const unit = direction.clone().normalize();
  mesh.position.copy(tip).addScaledVector(unit, -height / 2);
  mesh.quaternion.setFromUnitVectors(up, unit);
  return mesh;
}
function sphere(center, radius, material) {
  const mesh = new THREE.Mesh(new THREE.SphereGeometry(radius, 20, 14), material);
  mesh.position.copy(center);
  return mesh;
}
function label(text, position, size, color = '#435b4d') {
  const canvas = document.createElement('canvas'); canvas.width = 128; canvas.height = 64;
  const context = canvas.getContext('2d');
  context.font = '600 40px system-ui, sans-serif'; context.textAlign = 'center'; context.textBaseline = 'middle';
  context.fillStyle = color; context.fillText(text, 64, 34);
  const texture = new THREE.CanvasTexture(canvas); texture.colorSpace = THREE.SRGBColorSpace;
  const sprite = new THREE.Sprite(new THREE.SpriteMaterial({ map: texture, depthTest: false, transparent: true }));
  sprite.position.copy(position); sprite.scale.set(size * 2, size, 1); sprite.renderOrder = 2;
  return sprite;
}
// A convex polygon drawn as its true boundary; the fill uses a fan only as a
// rasterization device and adds no edges of its own.
function polygon(points, material) {
  const positions = [];
  for (let i = 1; i + 1 < points.length; i++) positions.push(...points[0].toArray(), ...points[i].toArray(), ...points[i + 1].toArray());
  const geometry = new THREE.BufferGeometry();
  geometry.setAttribute('position', new THREE.Float32BufferAttribute(positions, 3));
  geometry.computeVertexNormals();
  return new THREE.Mesh(geometry, material);
}
function outline(points, color) {
  const geometry = new THREE.BufferGeometry().setFromPoints(points.concat([points[0]]));
  return new THREE.Line(geometry, new THREE.LineBasicMaterial({ color, transparent: true, opacity: 0.9 }));
}
function axes(view, center, radius, length) {
  const material = new THREE.LineBasicMaterial({ color: 0xa3b0a6 });
  [[1, 0, 0, 'x'], [0, 1, 0, 'y'], [0, 0, 1, 'z']].forEach(([x, y, z, name]) => {
    const direction = new THREE.Vector3(x, y, z);
    const points = [center.clone().addScaledVector(direction, -length), center.clone().addScaledVector(direction, length)];
    view.scene.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints(points), material));
    view.scene.add(label(name, points[1].clone().addScaledVector(direction, radius * 0.08), radius * 0.1, '#7b8a80'));
  });
}
function bounds(points) {
  const box = new THREE.Box3();
  points.forEach(p => box.expandByPoint(p));
  if (box.isEmpty()) box.expandByPoint(new THREE.Vector3());
  const center = box.getCenter(new THREE.Vector3());
  const radius = Math.max(box.getSize(new THREE.Vector3()).length() / 2, 1);
  return { center, radius };
}

// --- linked highlighting ----------------------------------------------------
function highlight(key) {
  for (const group of groups) for (const item of group.items) item.apply(false);
  const group = groups.find(g => g.key === key);
  group?.items.forEach(item => item.apply(true));
  for (const g of groups) g.button.setAttribute('aria-pressed', String(g.key === selected));
  $('selection').textContent = group?.description || defaultSelection;
  renderAll();
}
function toggle(key) { selected = selected === key ? null : key; highlight(hovered ?? selected); }
function register(key, label, description, color, items) {
  const button = document.createElement('button');
  button.type = 'button'; button.className = 'feature'; button.textContent = label;
  button.style.setProperty('--feature-color', color); button.setAttribute('aria-pressed', 'false');
  button.addEventListener('click', () => toggle(key));
  button.addEventListener('mouseenter', () => highlight(key));
  button.addEventListener('mouseleave', () => highlight(selected));
  button.addEventListener('focus', () => highlight(key));
  button.addEventListener('blur', () => highlight(selected));
  $('features').appendChild(button); groups.push({ key, button, items, description });
}
const meshItem = (mesh, activeScale, baseColor, activeColor) => ({
  apply(active) {
    mesh.scale.setScalar(active ? activeScale : 1);
    mesh.material.color.set(active ? activeColor : baseColor);
  }
});
const tubeItem = (mesh, baseColor, activeColor) => ({
  apply(active) { mesh.scale.set(active ? 1.9 : 1, 1, active ? 1.9 : 1); mesh.material.color.set(active ? activeColor : baseColor); }
});
const faceItem = (face, activeColor, activeOpacity) => ({
  apply(active) {
    face.fill.material.color.set(active ? activeColor : face.baseColor);
    face.fill.material.opacity = active ? activeOpacity : 0.16;
    face.line.material.color.set(active ? activeColor : face.baseColor);
    face.line.material.opacity = active ? 1 : 0.9;
  }
});
function darker(color) { return `#${new THREE.Color(color).multiplyScalar(0.72).getHexString()}`; }

function render(data) {
  // Validate every display conversion before touching the scenes.
  const vertexPoints = data.vertices.map(v => point(v.point));
  const edgeShapes = data.edges.map(edgeEndpoints);
  const termPoints = data.terms.map(t => [t.x, t.y, t.z].map(number));
  if (data.edges.some(e => e.kind === 'line')) throw new Error('The backend reported a complete line, which this view does not represent.');
  const graph = views.graph, newton = views.newton;
  clearView(graph); clearView(newton);
  groups = []; selected = null; hovered = null; $('features').replaceChildren();

  // Graph view: vertices as spheres, bounded edges as tubes, rays as tubes with a cone.
  const vertexVectors = vertexPoints.map(p => new THREE.Vector3(...p));
  const graphBounds = bounds(vertexVectors);
  const rayLength = Math.max(2, graphBounds.radius * 1.4);
  const extent = graphBounds.radius + rayLength;
  axes(graph, graphBounds.center, extent, extent * 1.1);
  const vertexMeshes = new Map(), vertexColor = v => palette[v.id % palette.length], edgeColor = e => palette[(e.id + 2) % palette.length];
  for (const vertex of data.vertices) {
    const mesh = sphere(vertexVectors[vertex.id], extent * 0.028, new THREE.MeshLambertMaterial({ color: vertexColor(vertex) }));
    mesh.userData.key = `cell-${vertex.cell}`;
    graph.scene.add(mesh); graph.pickables.push(mesh); vertexMeshes.set(vertex.id, mesh);
  }
  const edgeMeshes = new Map();
  data.edges.forEach((edge, index) => {
    const shape = edgeShapes[index], color = edgeColor(edge);
    const start = new THREE.Vector3(...shape.start);
    const direction = shape.direction ? new THREE.Vector3(...shape.direction).normalize() : null;
    const end = direction ? start.clone().addScaledVector(direction, rayLength) : new THREE.Vector3(...shape.end);
    const material = new THREE.MeshLambertMaterial({ color });
    const meshes = [tube(start, end, extent * 0.011, material)];
    if (direction) meshes.push(cone(end.clone().addScaledVector(direction, extent * 0.05), direction, extent * 0.03, extent * 0.06, material));
    meshes.forEach(mesh => { mesh.userData.key = `edge-${edge.id}`; graph.scene.add(mesh); graph.pickables.push(mesh); });
    edgeMeshes.set(edge.id, meshes);
  });
  frame(graph, graphBounds.center, extent);

  // Newton view: term points, labels, and the true polygonal faces of the lower cells.
  const termVectors = termPoints.map(p => new THREE.Vector3(...p));
  const newtonBounds = bounds(termVectors);
  axes(newton, newtonBounds.center, newtonBounds.radius, newtonBounds.radius * 1.25);
  termVectors.forEach((vector, i) => {
    newton.scene.add(sphere(vector, newtonBounds.radius * 0.022, new THREE.MeshLambertMaterial({ color: 0x435b4d })));
    newton.scene.add(label(String(i + 1), vector.clone().add(new THREE.Vector3(0.06, 0.09, 0.06).multiplyScalar(newtonBounds.radius)), newtonBounds.radius * 0.11));
  });
  const faces = new Map();
  for (const face of data.faces) {
    const edge = data.edges[face.edge], color = edgeColor(edge);
    const corners = face.boundary.map(i => termVectors[i]);
    const fill = polygon(corners, new THREE.MeshLambertMaterial({ color, transparent: true, opacity: 0.16, side: THREE.DoubleSide, depthWrite: false }));
    fill.userData.key = `edge-${edge.id}`;
    const line = outline(corners, color);
    newton.scene.add(fill); newton.scene.add(line); newton.pickables.push(fill);
    faces.set(face.id, { fill, line, baseColor: color });
  }
  frame(newton, newtonBounds.center, newtonBounds.radius * 1.3);

  // Correspondence: vertex ↔ cell, edge ↔ face.
  for (const cell of data.cells) {
    const vertex = data.vertices[cell.vertex], color = vertexColor(vertex);
    const items = [meshItem(vertexMeshes.get(vertex.id), 1.7, color, darker(color)), ...cell.faces.map(id => faceItem(faces.get(id), color, 0.5))];
    register(`cell-${cell.id}`, `Vertex ${exactPoint(vertex.point)} ↔ cell with ${cell.faces.length} faces`,
      `Graph vertex ${exactPoint(vertex.point)} ↔ subdivision cell bounded by ${cell.faces.length} faces. Terms ${cell.terms.map(i => i + 1).join(', ')} attain the minimum.`, color, items);
  }
  for (const edge of data.edges) {
    const face = data.faces[edge.face], color = edgeColor(edge), shape = edgeShapes[edge.id];
    const items = [...edgeMeshes.get(edge.id).map(mesh => tubeItem(mesh, color, darker(color))), faceItem(faces.get(face.id), color, 0.6)];
    const detail = edge.kind === 'segment' ? `from ${exactPoint(edge.start)} to ${exactPoint(edge.end)}` : `from ${exactPoint(edge.start)} in direction ${exactPoint(edge.direction)}`;
    const sharing = face.cells.length === 2 ? 'shared by two cells' : 'on the boundary of the subdivision';
    register(`edge-${edge.id}`, `${edge.kind} ${edge.id + 1} ↔ ${face.boundary.length}-gon`,
      `${edge.kind[0].toUpperCase() + edge.kind.slice(1)} ${detail} ↔ face with ${face.boundary.length} corners (terms ${face.boundary.map(i => i + 1).join(', ')}), ${sharing}. Terms ${edge.terms.map(i => i + 1).join(', ')} attain the minimum along it.`, color, items);
    void shape;
  }
  highlight(null);
  $('formula').textContent = `min(${data.terms.map(t => `${t.coefficient} + ${t.x}·x + ${t.y}·y + ${t.z}·z`).join(', ')})`;
  $('normalized-terms').textContent = data.terms.map((t, i) => `${i + 1}: ${t.coefficient} + ${t.x}·x + ${t.y}·y + ${t.z}·z`).join(' ; ');
  document.querySelector('.explorer').classList.remove('stale');
  ['export-graph', 'export-newton'].forEach(id => $(id).disabled = false);
  result = data;
}
async function compute(event) {
  event?.preventDefault();
  invalidate(); const id = requestId; controller = new AbortController();
  try {
    const terms = readTerms(), method = $('method').value;
    status(`Computing the exact one-skeleton with the ${methodLabels[method] || method}…`, 'pending');
    const response = await fetch('/api/graph3', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ terms, method }), signal: controller.signal });
    const data = await response.json();
    if (id !== requestId) return;
    if (!response.ok) throw new Error(data.error || `Geometry request failed (${response.status}).`);
    render(data);
    const normalized = data.terms.length !== terms.length ? ' Repeated exponents were combined; plot labels use the normalized terms below.' : '';
    const segments = data.edges.filter(e => e.kind === 'segment').length;
    status(`${data.vertices.length} vertices · ${data.edges.length} edges (${segments} bounded, ${data.edges.length - segments} rays) · ${data.cells.length} cells · ${data.faces.length} faces · ${methodLabels[data.method] || data.method} · one-skeleton only.${normalized}`);
  } catch (error) {
    if (id !== requestId || error.name === 'AbortError') return;
    status(error.message || 'Could not compute geometry.', 'error');
  }
}
function exportPng(name) {
  if ($(`export-${name}`).disabled) return;
  const view = views[name];
  view.renderer.render(view.scene, view.camera);
  download(view.renderer.domElement.toDataURL('image/png'), `tropical-${name}.png`);
}
$('import-polynomial').addEventListener('click', () => $('polynomial-file').click());
$('polynomial-file').addEventListener('change', importPolynomial);
$('export-polynomial').addEventListener('click', exportPolynomial);
$('add-term').addEventListener('click', () => { addRow(); invalidate(); });
$('polynomial-form').addEventListener('submit', compute);
$('example').addEventListener('change', () => { if (!examples[$('example').value]) return; fileStatus(''); $('terms').replaceChildren(); examples[$('example').value].forEach(addRow); compute(); });
$('method').addEventListener('change', () => compute());
$('clear-selection').addEventListener('click', () => { selected = null; highlight(null); });
for (const name of ['graph', 'newton']) {
  $(`reset-${name}`).addEventListener('click', () => resetView(views[name]));
  $(`export-${name}`).addEventListener('click', () => exportPng(name));
}
try {
  views = { graph: createView('graph'), newton: createView('newton') };
} catch (error) {
  status(`WebGL is unavailable in this browser: ${error.message}`, 'error');
}
window.tropicalViews = views; // Exposed for browser tests only.
examples.plane.forEach(addRow);
if (views.graph) compute(); else document.querySelectorAll('.feature, #compute').forEach(button => button.disabled = true);
