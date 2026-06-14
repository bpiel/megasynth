#include <Arduino.h>
#include <Wire.h>
#include <U8g2lib.h>

#define TCA_ADDR 0x70

// Your physical layout:
// OLED #2 is on the LEFT and should say MEGASYNTH.
// OLED #1 is on the RIGHT and should say LIVE.
#define OLED1_CH 0
#define OLED2_CH 1

// Common for many 1.3" 128x64 OLEDs
U8G2_SH1106_128X64_NONAME_F_HW_I2C display(
  U8G2_R0,
  U8X8_PIN_NONE
);

// If your OLEDs are SSD1306 instead, try this constructor instead:
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

int centeredX(const char* s) {
  int w = display.getStrWidth(s);
  return max(0, (128 - w) / 2);
}

void drawSparkles(uint16_t frame, uint8_t count) {
  // Random twinkling dots/stars. Deterministic-ish per frame.
  for (uint8_t i = 0; i < count; i++) {
    uint8_t x = (frame * 17 + i * 37 + (i * i * 3)) % 128;
    uint8_t y = (frame * 11 + i * 23 + (i * i * 5)) % 64;

    // Don't completely cover the center text area too much
    uint8_t phase = (frame + i * 13) % 16;

    if (phase < 5) {
      display.drawPixel(x, y);
    } else if (phase < 8) {
      // Tiny plus sparkle
      display.drawPixel(x, y);
      if (x > 0) display.drawPixel(x - 1, y);
      if (x < 127) display.drawPixel(x + 1, y);
      if (y > 0) display.drawPixel(x, y - 1);
      if (y < 63) display.drawPixel(x, y + 1);
    }
  }
}

void drawCornerStars(uint16_t frame) {
  // Little animated corner accents
  uint8_t pulse = frame % 12;

  if (pulse < 6) {
    display.drawPixel(2, 2);
    display.drawPixel(3, 2);
    display.drawPixel(2, 3);

    display.drawPixel(125, 2);
    display.drawPixel(124, 2);
    display.drawPixel(125, 3);

    display.drawPixel(2, 61);
    display.drawPixel(3, 61);
    display.drawPixel(2, 60);

    display.drawPixel(125, 61);
    display.drawPixel(124, 61);
    display.drawPixel(125, 60);
  } else {
    display.drawCircle(3, 3, 2);
    display.drawCircle(124, 3, 2);
    display.drawCircle(3, 60, 2);
    display.drawCircle(124, 60, 2);
  }
}

void drawAnimatedBorder(uint16_t frame) {
  // Border with little moving gaps/highlights
  display.drawFrame(0, 0, 128, 64);

  uint8_t pos = (frame * 3) % 128;

  // moving top/bottom glints
  display.drawBox(pos, 0, 8, 4);
  display.drawBox(127 - pos, 60, 8, 4);

  // moving side glints
  uint8_t ypos = (frame * 2) % 64;
  display.drawBox(0, ypos, 4, 8);
  display.drawBox(126, 61 - ypos, 4, 8);
}

void drawMegaSynthScreen(uint16_t frame) {
  display.clearBuffer();

  drawSparkles(frame, 18);
  drawAnimatedBorder(frame);
  drawCornerStars(frame);

  // Top small tag
  display.setFont(u8g2_font_6x10_tf);
  //display.drawStr(centeredX("PROJECT"), 10, "PROJECT");

  // Main title
  //display.setFont(u8g2_font_10x20_tf);
  display.setFont(u8g2_font_logisoso18_tf);
  display.drawStr(centeredX("MEGASYNTH"), 40, "MEGASYNTH");

  // Bottom animated status line
  display.setFont(u8g2_font_6x10_tf);

  const char* msg;
  switch ((frame / 15) % 4) {
    case 0: msg = "OLED MATRIX"; break;
    case 1: msg = "DYNAMIC UI"; break;
    case 2: msg = "SYNTH CORE"; break;
    default: msg = "BOOTING..."; break;
  }

  //display.drawStr(centeredX(msg), 58, msg);

  display.sendBuffer();
}

void drawLiveScreen(uint16_t frame) {
  display.clearBuffer();

  drawSparkles(frame + 100, 22);
  drawAnimatedBorder(frame + 40);
  drawCornerStars(frame + 7);

  // Small top line
  display.setFont(u8g2_font_6x13B_tf);
  //display.drawStr(centeredX("STATUS"), 13, "STATUS");

  // Big LIVE text
  display.setFont(u8g2_font_logisoso32_tf);
  display.drawStr(centeredX("LIVES"), 50, "LIVES");

  // Tiny pulsing underline / activity meter
  uint8_t w = 20 + ((frame * 4) % 88);
  display.drawBox((128 - w) / 2, 58, w, 3);

  display.sendBuffer();
}

void setup() {
  Serial.begin(115200);
  delay(1000);

  Wire.begin();          // Teensy pins 18/19
  Wire.setClock(400000); // Fast I2C

  Serial.println("MEGASYNTH 2-OLED sparkle demo");

  selectMuxChannel(OLED1_CH);
  display.begin();
  display.setContrast(255);

  selectMuxChannel(OLED2_CH);
  display.begin();
  display.setContrast(255);

  Serial.println("Displays initialized.");
}

void loop() {
  static uint16_t frame = 0;

  // OLED #2 / left screen
  selectMuxChannel(OLED2_CH);
  drawMegaSynthScreen(frame);

  // OLED #1 / right screen
  selectMuxChannel(OLED1_CH);
  drawLiveScreen(frame);

  frame++;

  delay(60);
}