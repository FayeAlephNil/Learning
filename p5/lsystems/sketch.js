var system, angleSlider, zoomSlider, gen;

function setup() {
  createCanvas(400, 400);

  // Koch snowflake
  // system = new System("F", {
  //   "F": "F+F-F-F+F",
  // });

  // Dragon Curve
  // system = new System("FX", {
  //   "X": "X+YF+",
  //   "Y": "-FX-Y",
  // });

  // Twin Dragon
  // system = new System("FX+FX+", {
  //   "X": "X+YF",
  //   "Y": "FX-Y"
  // });

  // Terdragon
  // system = new System("F", {
  //   "F": "F+F-F"
  // });

  // Levy C. Curve
  // system = new System("F", {
  //   "F": "+F--F+"
  // });

  // Tree-like system (https://youtu.be/E1B4UoSQMFw)
  // system = new System("F", {
  //   "F": "FF+[+F-F-F]-[-F+F+F]"
  // });

  // Sierpinski triangle
  // system = new System("A", {
  //   "A": "+B-A-B+",
  //   "B": "-A+B+A-"
  // });

  // Hilbert Curve
  // system = new System("X", {
  //   "X": "-YF+XFX+FY-",
  //   "Y": "+XF-YFY-FX+"
  // });

  // Gosper Curve
  system = new System("A", {
    "A": "A-B--B+A++AA+B-",
    "B": "+A-BB--B-A++A+B"
  });

  colorMode(HSB);

  gen = createP("");
  angleSlider = createSlider(0, PI, PI / 3, 0.01)
  zoomSlider = createSlider(0.001, 1, 1, 0.001)
  curr = createP("");
}

function turtle(s, angle) {
  var len = 10;
  for (var i = 0; i < s.length; i++) {
    var char = s[i];
    if (char == "A" || char == "B" || char == "F") {
      var c = map(i, 0, s.length - 1, 0, 360);
      stroke(c, 100, 100, 0.67);
      line(0, 0, 0, -len);
      translate(0, -len);
    } else if (char == "+") {
      rotate(-angle);
    } else if (char == "-") {
      rotate(angle);
    } else if (char == "[") {
      push();
    } else if (char == "]") {
      pop();
    }
  }
}

function draw() {
  background(0, 0, 40);

  translate(width / 2, height / 2);
  rotate(PI / 2);

  scale(zoomSlider.value());
  strokeWeight(1 / zoomSlider.value())

  gen.html("Gen: " + system.count);
  curr.html("Current: <p></p>" + system.current)
  turtle(system.current, angleSlider.value());
}

function keyPressed() {
  if (keyCode == LEFT_ARROW) {
    system = system.parent;
  } else if (keyCode == RIGHT_ARROW) {
    system = system.next();
  }
}
