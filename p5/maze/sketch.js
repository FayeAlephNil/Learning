var cols, rows;
var w = 20;
var grid = [];
var current;
var stack = [];

function setup() {
  createCanvas(400, 400);
  cols = floor(width / w);
  rows = floor(height / w);

  for (var j = 0; j < cols; j++) {
    for (var i = 0; i < rows; i++) {
      grid.push(new Cell(i, j));
    }
  }
  current = grid[0];
}

function draw() {
  background(51);

  for (var i = 0; i < grid.length; i++) {
    grid[i].show();
  }

  current.visited = true;
  current.highlight();
  var next = current.randNeighbor();
  if (next) {
    removeWalls(current, next);
    stack.push(current);

    current = next;
  } else if (stack.length != 0) {
    current = stack.pop();
  }
}

function index(i, j) {
  if (i < 0 || j < 0 || i > cols - 1 || j > rows - 1) {
    return -1;
  }

  return i + j * cols;
}

function removeWalls(a, b) {
  var x = a.i - b.i;

  if (x === 1) {
    a.walls[3] = false;
    b.walls[1] = false;
  } else if (x === -1) {
    removeWalls(b, a);
  }

  var y = a.j - b.j;

  if (y === 1) {
    a.walls[0] = false;
    b.walls[2] = false;
  } else if (y === -1) {
    removeWalls(b, a);
  }
}
