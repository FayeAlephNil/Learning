var graph;
var step = 1;

function setup() {
  createCanvas(400, 400);

  graph = new Graph(function(x) {
    return sin(x) + saw(x);
  }, -1, 1, 0, TWO_PI);
}

function saw(t) {
  var tprime = t / (TWO_PI);
  var toinvert = 2 * (tprime - floor(1 / 2 + tprime));
  return -toinvert;
}

function triang(t) {
  return map(abs(saw(t)), 0, 1, -1, 1);
}

function sgn(x) {
  if (x < 0) {
    return -1;
  } else if (x > 0) {
    return 1;
  } else {
    return 0;
  }
}

function square(t) {
  return sgn(sin(t));
}

function draw() {
  background(51);

  fill(255);
  stroke(255);
  graph.show();
}
