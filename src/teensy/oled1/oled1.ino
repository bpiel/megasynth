#include <Arduino.h>
#include <U8g2lib.h>

// Temporary test wiring:
// OLED GND -> Teensy GND
// OLED VCC -> Teensy 3.3V
// OLED SDA -> Teensy pin 11
// OLED SCK -> Teensy pin 12

#define OLED_SDA 11
#define OLED_SCK 12

// 0.91" 128x32 OLED, usually SSD1306
U8G2_SSD1306_128X32_UNIVISION_F_SW_I2C display(
  U8G2_R0,
  OLED_SCK,
  OLED_SDA,
  U8X8_PIN_NONE
);

void setup() {
  Serial.begin(115200);
  delay(500);

  display.begin();
  display.setContrast(180);

  display.clearBuffer();

  display.setFont(u8g2_font_6x10_tf);
  display.drawStr(0, 9,  "MEGASYNTH");

  display.setFont(u8g2_font_5x8_tf);
  display.drawStr(0, 20, "VCF CUTOFF");
  display.drawStr(0, 30, "1.24 kHz");

  display.sendBuffer();

  Serial.println("OLED test sent.");
}

void loop() {
  static int value = 0;
  static int dir = 1;

  display.clearBuffer();

  display.setFont(u8g2_font_10x20_tf);
  // display.setFont(u8g2_font_logisoso24_tf);
  display.drawStr(0, 13, "TREVOR LEVEL");
  //display.drawStr(0, 32, "VCF CUTOFF 123 456");

  char buf[32];
  snprintf(buf, sizeof(buf), "%d Hz", 200 + value * 20);

  // display.setFont(u8g2_font_5x8_tf);
  // display.drawStr(0, 20, buf);
  display.drawStr(0, 32, buf);

  // Simple value bar
  int barWidth = map(value, 0, 100, 0, 128);
  // display.drawFrame(0, 24, 128, 8);
  display.drawBox(1, 14, max(0, barWidth - 2), 4);

  display.sendBuffer();

  value += dir;
  if (value >= 100) dir = -1;
  if (value <= 0) dir = 1;

  delay(40);
}