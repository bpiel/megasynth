#include <Arduino.h>
#include <Wire.h>
#include <U8g2lib.h>

#define TCA_ADDR 0x70

// For many 1.3" 128x64 OLEDs:
U8G2_SH1106_128X64_NONAME_F_HW_I2C display(
  U8G2_R0,
  U8X8_PIN_NONE
);

// If your display worked with SSD1306 instead, comment the SH1106
// constructor above and use this instead:
//
// U8G2_SSD1306_128X64_NONAME_F_HW_I2C display(
//   U8G2_R0,
//   U8X8_PIN_NONE
// );

void selectMuxChannel(uint8_t channel) {
  if (channel > 7) return;

  Wire.beginTransmission(TCA_ADDR);
  Wire.write(1 << channel);
  Wire.endTransmission();
}

void drawTestScreen(uint8_t channel, const char* title, const char* value, int barValue) {
  selectMuxChannel(channel);

  display.clearBuffer();

  display.setFont(u8g2_font_6x13B_tf);
  display.drawStr(0, 13, title);

  display.setFont(u8g2_font_logisoso20_tf);
  display.drawStr(0, 48, value);

  int barWidth = map(barValue, 0, 100, 0, 128);
  display.drawFrame(0, 56, 128, 8);
  display.drawBox(1, 57, max(0, barWidth - 2), 6);

  display.sendBuffer();
}

void setup() {
  Serial.begin(115200);
  delay(1000);

  Serial.println("MEGASYNTH TCA9548A OLED mux test");

  Wire.begin();          // uses Teensy pins 18/19
  Wire.setClock(400000); // fast I2C

  // Initialize display on channel 0
  selectMuxChannel(0);
  display.begin();
  display.setContrast(255);

  // Initialize display on channel 1
  selectMuxChannel(1);
  display.begin();
  display.setContrast(255);

  Serial.println("Displays initialized.");
}

void loop() {
  static int value = 0;
  static int dir = 1;

  drawTestScreen(0, "DISPLAY 0", "CUTOFF", value);
  drawTestScreen(1, "DISPLAY 1", "RES", 100 - value);

  value += dir;
  if (value >= 100) dir = -1;
  if (value <= 0) dir = 1;

  delay(50);
}