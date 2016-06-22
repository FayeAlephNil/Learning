var n1 = 1;
var n2 = 1;
var n3 = 1;

var a = 1;
var b = 1;
var m = 0;

var totalSlide;
var scalar;
var moff;
var mmax = 20;

function setup() {
  createCanvas(400, 400);
  totalSlide = createSlider(0, 500, 12, 1);
  colorMode(HSB);

  moff = 3 * PI / 2;
  scalar = width / 2;
}

function draw() {
  if (key == ".") {
    scalar += 1;
  } else if (key == ",") {
    scalar -= 1;
  }

  if (scalar < 1) {
    scalar = 1;
  } else if (scalar > width / 2) {
    scalar = width / 2;
  }

  m = map(sin(moff), -1, 1, 0, mmax);
  moff += TWO_PI / (mmax * 100);

  background(51);

  drawShape();
}

function drawShape() {
  translate(width / 2, height / 2);

  var da = TWO_PI / totalSlide.value();
  var prevx;
  var prevy;

  for (var theta = 0; theta < TWO_PI; theta += da) {
    var r = supershape(theta);

    var x = scalar * r * cos(theta);
    var y = scalar * r * sin(theta);

    if (prevx != null && prevy != null) {
      stroke(map(theta, 0, TWO_PI, 0, 360), 100, 100)
      line(prevx, prevy, x, y);
    }

    prevx = x;
    prevy = y;
  }

  line(prevx, prevy, scalar * supershape(0), 0);
}

function supershape(theta) {
  var part1 = (1 / a) * cos(m / 4 * theta);
  part1 = abs(part1);
  part1 = pow(part1, n2);

  var part2 = (1 / b) * sin(m / 4 * theta);
  part2 = abs(part2);
  part2 = pow(part2, n3);

  var part3 = pow(part1 + part2, 1 / n1);

  return (1 / part3);
}
