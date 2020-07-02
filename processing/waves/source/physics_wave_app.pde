import g4p_controls.*;
import ddf.minim.*;
import ddf.minim.ugens.*;

Minim minim;
AudioOutput out;
Panel panel1;
Panel panel2;
Panel panel3;
Panel panel4;

void setup() {
  size(520, 700);
  
  minim = new Minim(this);
  out = minim.getLineOut();
  panel1 = new Panel(this, out, 0, 100);
  panel2 = new Panel(this, out, 0, 250);
  panel3 = new Panel(this, out, 0, 400);
  panel4 = new Panel(this, out, 0, 550);
}

void draw() {
  background(255);
  
  panel1.draw(); 
  panel2.draw();
  panel3.draw();
  panel4.draw();
  
  float last = 50;
  
  for(int i = 0; i < width - 1; ++i) {
    float current = 50 + (panel1.getValue(i) + panel2.getValue(i) + panel3.getValue(i) + panel4.getValue(i)) / 4;
    line(i - 1, last, i, current); 
    last = current;
  }  
}

public void handleSliderEvents(GValueControl slider, GEvent event) {
  panel1.handleSliderEvents(slider, event);
  panel2.handleSliderEvents(slider, event);
  panel3.handleSliderEvents(slider, event);
  panel4.handleSliderEvents(slider, event);
}

public void handleButtonEvents(GButton button, GEvent event) {
  panel1.handleButtonEvents(button, event);
  panel2.handleButtonEvents(button, event);
  panel3.handleButtonEvents(button, event);
  panel4.handleButtonEvents(button, event);
}