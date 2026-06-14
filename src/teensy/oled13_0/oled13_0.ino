#include <Arduino.h>
#include <U8g2lib.h>

// Temporary test wiring:
// OLED GND -> Teensy GND
// OLED VCC -> Teensy 3.3V
// OLED SDA -> Teensy pin 11
// OLED SCK -> Teensy pin 12

#define OLED_SDA 11
#define OLED_SCK 12

U8G2_SH1106_128X64_NONAME_F_SW_I2C display(
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

  //display.setFont(u8g2_font_7x13B_tf);
  //display.setFont(u8g2_font_helvB24_tf);
  // display.setFont(u8g2_font_helvR18_tf);
  //display.setFont(u8g2_font_10x20_tf);
  display.setFont(u8g2_font_logisoso18_tf);
  //display.setFont(u8g2_font_logisoso24_tf);
  display.drawStr(0, 28, "TREVOR LEVEL");
  //display.drawStr(0, 32, "VCF CUTOFF 123 456");

  char buf[32];
  snprintf(buf, sizeof(buf), "%d Hz", 200 + value * 20);

  // display.setFont(u8g2_font_5x8_tf);
  // display.drawStr(0, 20, buf);
  display.drawStr(0, 64, buf);

  // Simple value bar
  int barWidth = map(value, 0, 100, 0, 128);
  // display.drawFrame(0, 24, 128, 8);
  display.drawBox(1, 36, max(0, barWidth - 2), 6);

  display.sendBuffer();

  value += dir;
  if (value >= 100) dir = -1;
  if (value <= 0) dir = 1;

  delay(40);
}