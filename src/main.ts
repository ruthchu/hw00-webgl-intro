import {vec3, vec4} from 'gl-matrix';
const Stats = require('stats-js');
import * as DAT from 'dat.gui';
import Icosphere from './geometry/Icosphere';
import Square from './geometry/Square';
import OpenGLRenderer from './rendering/gl/OpenGLRenderer';
import Camera from './Camera';
import {setGL} from './globals';
import ShaderProgram, {Shader} from './rendering/gl/ShaderProgram';
import Cube from './geometry/Cube';

// Define an object with application parameters and button callbacks
// This will be referred to by dat.GUI's functions that add GUI elements.
const controls = {
  tesselations: 5,
  'Load Scene': loadScene, // A function pointer, essentially
  terrainFreq: 1,
  cloudHeight: .9,
};

const palette = {
  color: [ 163, 120, 191 ]
};

let icosphere: Icosphere;
let square: Square;
let cube: Cube;
let prevTesselations: number = 6;
let u_tick: number = 0.0;

function loadScene() {
  icosphere = new Icosphere(vec3.fromValues(0, 0, 0), 1, controls.tesselations);
  icosphere.create();
  square = new Square(vec3.fromValues(0, 0, 0));
  square.create();
  //cube = new Cube(vec3.fromValues(0, 0, 0));
  //cube.create();
}

function main() {
  // Initial display for framerate
  const stats = Stats();
  stats.setMode(0);
  stats.domElement.style.position = 'absolute';
  stats.domElement.style.left = '0px';
  stats.domElement.style.top = '0px';
  document.body.appendChild(stats.domElement);

  // Add controls to the gui
  const gui = new DAT.GUI();
  gui.add(controls, 'tesselations', 0, 8).step(1);
  gui.add(controls, 'Load Scene');
  gui.add(controls, 'terrainFreq', 0, 2).step(.1);
  gui.add(controls, 'cloudHeight', .6, 1.1).step(.05);
  gui.addColor(palette, 'color');

  // get canvas and webgl context
  const canvas = <HTMLCanvasElement> document.getElementById('canvas');
  const gl = <WebGL2RenderingContext> canvas.getContext('webgl2');
  if (!gl) {
    alert('WebGL 2 not supported!');
  }
  // `setGL` is a function imported above which sets the value of `gl` in the `globals.ts` module.
  // Later, we can import `gl` from `globals.ts` to access it
  setGL(gl);

  // Initial call to load scene
  loadScene();

  const camera = new Camera(vec3.fromValues(0, 0, 5), vec3.fromValues(0, 0, 0));

  const renderer = new OpenGLRenderer(canvas);
  renderer.setClearColor(0.2, 0.2, 0.2, 1);
  gl.enable(gl.DEPTH_TEST);
  gl.enable(gl.BLEND);
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
  gl.enable(gl.CULL_FACE);
  gl.frontFace(gl.CW);

  const lambert = new ShaderProgram([
    new Shader(gl.VERTEX_SHADER, require('./shaders/lambert-vert.glsl')),
    new Shader(gl.FRAGMENT_SHADER, require('./shaders/lambert-frag.glsl')),
  ]);

  const perlin = new ShaderProgram([
    new Shader(gl.VERTEX_SHADER, require('./shaders/perlin-vert.glsl')),
    new Shader(gl.FRAGMENT_SHADER, require('./shaders/perlin-frag.glsl')),
  ])

  const planet = new ShaderProgram([
    new Shader(gl.VERTEX_SHADER, require('./shaders/planet-vert.glsl')),
    new Shader(gl.FRAGMENT_SHADER, require('./shaders/planet-frag.glsl')),
  ])

  const raymarch = new ShaderProgram([
    new Shader(gl.VERTEX_SHADER, require('./shaders/raymarch-vert.glsl')),
    new Shader(gl.FRAGMENT_SHADER, require('./shaders/raymarch-frag.glsl')),
  ])

  // This function will be called every frame
  function tick() {
    u_tick++;
    camera.update();
    stats.begin();
    gl.viewport(0, 0, window.innerWidth, window.innerHeight);

    renderer.clear();
    if(controls.tesselations != prevTesselations)
    {
      prevTesselations = controls.tesselations;
      icosphere = new Icosphere(vec3.fromValues(0, 0, 0), 1, prevTesselations);
      icosphere.create();
    }
    renderer.render(camera, planet, [
      icosphere,
      //square,
      //cube,
    ], vec4.fromValues(palette.color[0] / 255., 
      palette.color[1] / 255., 
      palette.color[2] / 255., 1), 
      u_tick, controls.terrainFreq, controls.cloudHeight);
    renderer.render(camera, raymarch, [
      icosphere,
      //square,
      //cube,
    ], vec4.fromValues(palette.color[0] / 255., 
      palette.color[1] / 255., 
      palette.color[2] / 255., 1), 
      u_tick, controls.terrainFreq, controls.cloudHeight);

    stats.end();

    // Tell the browser to call `tick` again whenever it renders a new frame
    requestAnimationFrame(tick);
  }

  window.addEventListener('resize', function() {
    renderer.setSize(window.innerWidth, window.innerHeight);
    camera.setAspectRatio(window.innerWidth / window.innerHeight);
    camera.updateProjectionMatrix();
  }, false);

  renderer.setSize(window.innerWidth, window.innerHeight);
  camera.setAspectRatio(window.innerWidth / window.innerHeight);
  camera.updateProjectionMatrix();

  // Start the render loop
  tick();
}

main();
