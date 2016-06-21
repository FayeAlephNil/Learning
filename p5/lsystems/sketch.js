var system;
var angleSlider;

var colorBox;

var gen;
var curr;

var xstart, ystart;

var zoom = 1;

function setup() {
  createCanvas(400, 400);
  xstart = width / 2;
  ystart = height / 2;

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
  system = new System("F", {
    "F": "+F--F+"
  });

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
  // system = new System("A", {
  //   "A": "A-B--B+A++AA+B-",
  //   "B": "+A-BB--B-A++A+B"
  // });

  angleSlider = createSlider(0, PI, PI / 6, 0.01);

  colorMode(HSB);
  colorBox = createCheckbox('Color', true);

  gen = createP("");
  curr = createP("");
}

function turtle(s, angle) {
  var len = 10;
  for (var i = 0; i < s.length; i++) {
    var char = s[i];
    if (char == "A" || char == "B" || char == "F") {
      if (colorBox.checked()) {
        var c = map(i, 0, s.length - 1, 0, 360);
        stroke(c, 100, 100, 0.67);
      }

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
  if (key == ".") {
    zoom += 0.05;
  } else if (key == ",") {
    zoom -= 0.05;
  }

  background(0, 0, 40);

  translate(xstart, ystart);
  rotate(PI / 2);

  scale(zoom);
  stroke(0, 0, 100, 0.8)
  strokeWeight(1 / zoom)

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


function within(val, min, max) {
  if (max) {
    return val > min && val < max;
  }
  return val > 0 && val < min;
}

function mouseIn() {
  return within(mouseX, width) && within(mouseY, height);
}

function mouseDragged() {
  if (mouseIn()) {
    xstart = mouseX;
    ystart = mouseY;
  }
}
