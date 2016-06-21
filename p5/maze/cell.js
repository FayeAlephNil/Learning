function Cell(i, j) {
  this.i = i;
  this.j = j;
  this.visited = false;

  // Boolean values for walls in order [top, right, bottom, left]
  this.walls = [true, true, true, true];

  this.getNeighbors = function() {
    var neighbors = [];

    var top = grid[index(this.i, this.j - 1)];
    var right = grid[index(this.i + 1, this.j)];
    var bottom = grid[index(this.i, this.j + 1)];
    var left = grid[index(this.i - 1, this.j)];
    neighbors.push(top, right, bottom, left);


    return _.filter(neighbors, function(cell) {
      return cell != undefined;
    });
  }

  this.unvisitedNeighbors = function() {
    return _.filter(this.getNeighbors(), function(cell) {
      return !cell.visited;
    });
  }

  this.randNeighbor = function() {
    var unvisited = this.unvisitedNeighbors();
    if (unvisited.length > 0) {
      return unvisited[floor(random(0, unvisited.length))];
    }
    return undefined;
  }



  this.show = function() {
    var x = this.i * w;
    var y = this.j * w;
    stroke(255);

    if (this.walls[0]) {
      line(x, y, x + w, y);
    }

    if (this.walls[1]) {
      line(x + w, y, x + w, y + w)
    }

    if (this.walls[2]) {
      line(x, y + w, x + w, y + w)

    }

    if (this.walls[3]) {
      line(x, y, x, y + w);
    }

    if (this.visited) {
      fill(255, 0, 255, 100);
      noStroke();
      rect(x, y, w, w);
    }
  }

  this.highlight = function() {
    var x = this.i * w;
    var y = this.j * w;
    noStroke();
    fill(0, 0, 255, 100);
    rect(x, y, w, w);
  }

  this.unlight = function() {
    var x = this.i * w;
    var y = this.j * w;
    noStroke();
    fill(51);
    rect(x, y, w, w);
    fill(255, 0, 255, 100);
    noStroke();
    rect(x, y, w, w);
  }
}
