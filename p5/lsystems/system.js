function System(axiom, rules, parent, count) {
  this.rules = rules;
  this.current = axiom;
  this.parent = parent || this;
  this.count = count || 0;

  this.next = function() {
    var newCurrent = "";
    for (var i = 0; i < this.current.length; i++) {
      var char = this.current[i];
      var newChar = this.rules[char];
      if (newChar != null) {
        newCurrent += newChar;
      } else {
        newCurrent += char;
      }
    }
    return new System(newCurrent, this.rules, this, this.count + 1);
  }

  this.show = function() {
    createP(this.current + ": " + this.count);
  }
}
