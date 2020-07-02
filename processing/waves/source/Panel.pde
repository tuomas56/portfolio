import g4p_controls.*;
import ddf.minim.*;
import ddf.minim.ugens.*;

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