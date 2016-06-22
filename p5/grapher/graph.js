function Graph(func, fmin, fmax, xmin, xmax) {
  this.func = func;
  this.min = fmin;
  this.max = fmax;
  this.xoff = 0;

  this.xmin = xmin;
  this.xmax = xmax;

  this.get = function(x) {
    var newx = map(x + this.xoff,
      0, width / 2,
      this.xmin, this.xmax);
    return map(this.func(newx), this.min, this.max, 1, height - 1);
  }

  this.show = function() {
    stroke(255)
    noFill();
    beginShape();
    for (var x = 0; x <= width / 2; x++) {
      vertex(x, this.get(x));
    }
    endShape();

    var x = floor(width / 2);
    var y = this.get(x);

    stroke(255, 0, 0, 100);
    fill(255, 0, 0);
    line(0, y, x, y);
    ellipse(x, y, width / 80, height / 80);
    this.xoff += step;
  }
}
