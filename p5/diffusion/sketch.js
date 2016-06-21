var grid = [];
var next = [];
var slider;
var da = 1;
var db = 0.5;
var feed = 0.055;
var k = 0.062;

function setup() {
  createCanvas(100, 100);
  pixelDensity(1);
  for (var x = 0; x < width; x++) {
    for (var y = 0; y < height; y++) {
      grid[x] = grid[x] || [];
      next[x] = next[x] || [];
      grid[x][y] = {
        a: 1,
        b: 0
      };
      next[x][y] = {
        a: grid[x][y].a,
        b: grid[x][y].b
      };
    }
  };
  for (var i = 50; i < 60; i++) {
    for (var j = 50; j < 60; j++) {
      grid[i][j].b = 1;
    }
  }
}

function draw() {
  background(51);

  loadPixels();
  for (var x = 0; x < width; x++) {
    for (var y = 0; y < height; y++) {
      var a = grid[x][y].a;
      var b = grid[x][y].b;
      next[x][y].a = a + (da * laplace(x, y, "a")) - (a * b * b) + (feed * (
        1 - a));
      next[x][y].b = b + (db * laplace(x, y, "b")) + (a * b * b) - ((k +
          feed) *
        b);

      var pix = (x + y * width) * 4;
      pixels[pix + 0] = floor(grid[x][y].a * 255);
      pixels[pix + 1] = 0;
      pixels[pix + 2] = floor(grid[x][y].b * 255);
      pixels[pix + 3] = 255;
    }
  }
  updatePixels();

  swap();
}

function laplace(x, y, s) {
  var sum = 0;
  for (var i = -1; i <= 1; i++) {
    for (var j = -1; j <= 1; j++) {
      var checkX = x + i < width && x + i > 0;
      var checkY = y + j < height && y + j > 0;
      if (checkX && checkY) {
        var absSum = abs(i) + abs(j);
        var val = grid[x + i][y + j][s];
        if (absSum == 0) {
          sum -= val
        } else if (absSum == 1) {
          sum += val * 0.2;
        } else if (absSum == 2) {
          sum += val * 0.05
        }
      }
    }
  }

  return sum;
}

function swap() {
  var tmp = grid;
  grid = next;
  next = tmp;
}
