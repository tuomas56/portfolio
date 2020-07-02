import processing.core.*; 
import processing.data.*; 
import processing.event.*; 
import processing.opengl.*; 

import g4p_controls.*; 
import ddf.minim.*; 
import ddf.minim.ugens.*; 
import g4p_controls.*; 
import ddf.minim.*; 
import ddf.minim.ugens.*; 

import java.util.HashMap; 
import java.util.ArrayList; 
import java.io.File; 
import java.io.BufferedReader; 
import java.io.PrintWriter; 
import java.io.InputStream; 
import java.io.OutputStream; 
import java.io.IOException; 

public class physics_wave_app extends PApplet {





Minim minim;
AudioOutput out;
Panel panel1;
Panel panel2;
Panel panel3;
Panel panel4;

public void setup() {
  
  
  minim = new Minim(this);
  out = minim.getLineOut();
  panel1 = new Panel(this, out, 0, 100);
  panel2 = new Panel(this, out, 0, 250);
  panel3 = new Panel(this, out, 0, 400);
  panel4 = new Panel(this, out, 0, 550);
}

public void draw() {
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




class Panel {
   float frequency;
   float amplitude;
   int x;
   int y;
   GCustomSlider amplitude_slider;
   GCustomSlider frequency_slider;
   GButton pause_button;
   Oscil wave;
   boolean paused = false;
  
   public Panel(PApplet app, AudioOutput out, int x, int y) {
     this.frequency = 1005f; 
     this.amplitude = 0f;
     this.frequency_slider = new GCustomSlider(app, x + 20, y, 250, 50, null);
     this.frequency_slider.setShowDecor(false, true, true, true);
     this.frequency_slider.setNbrTicks(5);
     this.frequency_slider.setLimits(10, 2000);
     this.amplitude_slider = new GCustomSlider(app, x + 270, y, 250, 50, null);
     this.amplitude_slider.setShowDecor(false, true, true, true);
     this.amplitude_slider.setNbrTicks(5);
     this.amplitude_slider.setLimits(0, 0, 100);
     this.pause_button = new GButton(app, x, y+15, 20, 20, "||");
     this.x = x;
     this.y = y;
     this.wave = new Oscil(this.frequency, this.amplitude, Waves.SINE);
     this.wave.patch(out);
   }
   
   public void draw() {
     pushMatrix();
     translate(this.x, this.y + 50);
  
     stroke(0);
     
     line(0, -50, width, -50);
     line(0, -51, width, -51);
     
     stroke(!this.paused ? 0 : color(255, 0, 0));
  
     float last = 50;
  
     for(int i = 0; i < width - 1; ++i) {
       float current = 50 + getValue(i);
       line(i - 1, last, i, current); 
       last = current;
     }  
     
     stroke(0);
     
     popMatrix();
   }
   
   public float getValue(float i) {
     return 2 * amplitude * 50 *  sin(i*frequency/width/20); 
   }
   
   public void handleSliderEvents(GValueControl slider, GEvent event) {
     if (slider == this.frequency_slider) {
       this.frequency = slider.getValueF();
       this.wave.setFrequency(this.frequency);
     } else if (slider == this.amplitude_slider) {
       this.amplitude = slider.getValueF() / 200; 
       this.wave.setAmplitude(this.amplitude);
     }
   }
   
   public void handleButtonEvents(GButton button, GEvent event) {
     if (button == this.pause_button && event == GEvent.CLICKED) {
       if (this.paused) {
         this.paused = false;
         this.wave.setAmplitude(this.amplitude);
       } else {
         this.paused = true;
         this.wave.setAmplitude(0);
       }
     }
   }
}
  public void settings() {  size(520, 700); }
  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "physics_wave_app" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}
