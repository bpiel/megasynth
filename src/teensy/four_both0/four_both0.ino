#include <Arduino.h>
#include <Wire.h>
#include <U8g2lib.h>

#define PCA_ADDR 0x70
#define MCP_ADDR 0x27

// Display update rate.
// Try 40, 50, 75, 100 and see what feels best.
#define DISPLAY_UPDATE_MS 75

// MCP23017 registers, default BANK=0 addressing
#define MCP_IODIRA 0x00
#define MCP_GPPUA  0x0C
#define MCP_GPIOA  0x12

// PCA/OLED bus uses default Wire = Teensy pins 18 SDA / 19 SCL.
// MCP/encoder bus uses Wire1 = Teensy pins 24 SDA2 / 25 SCL2.

// Common for many 1.3" 128x64 OLEDs
U8G2_SH1106_128X64_NONAME_F_HW_I2C display(
  U8G2_R0,
  U8X8_PIN_NONE
);

struct EncoderState {
  uint8_t bitA;
  uint8_t bitB;
  int32_t position;
  uint8_t lastState;
  bool dirty;
  bool invert;
};

EncoderState encoders[4] = {
  // bitA, bitB, position, lastState, dirty, invert

  // Encoder 1: MCP PA0 / PA1
  {0, 1, 0, 0, true, true},

  // Encoder 2: MCP PA2 / PA3
  {2, 3, 0, 0, true, true},

  // Encoder 3: MCP PA4 / PA5
  {4, 5, 0, 0, true, true},

  // Encoder 4: MCP PA6 / PA7
  {6, 7, 0, 0, true, true}
};

const char* labels[4] = {
  "CUTOFF",
  "RES",
  "LFO RATE",
  "LEVEL"
};

// -------------------- MCP on Wire1 --------------------

void mcpWriteRegister(uint8_t reg, uint8_t value) {
  Wire1.beginTransmission(MCP_ADDR);
  Wire1.write(reg);
  Wire1.write(value);
  Wire1.endTransmission();
}

uint8_t mcpReadRegister(uint8_t reg) {
  Wire1.beginTransmission(MCP_ADDR);
  Wire1.write(reg);
  Wire1.endTransmission(false);

  Wire1.requestFrom(MCP_ADDR, (uint8_t)1);

  if (Wire1.available()) {
    return Wire1.read();
  }

  return 0xFF;
}

// -------------------- PCA/OLED on Wire --------------------

void selectPcaChannel(uint8_t channel) {
  if (channel > 7) return;

  Wire.beginTransmission(PCA_ADDR);
  Wire.write(1 << channel);
  Wire.endTransmission();
}

// -------------------- Encoder decoding --------------------

uint8_t encoderStateFromPort(uint8_t portA, uint8_t bitA, uint8_t bitB) {
  uint8_t a = (portA >> bitA) & 1;
  uint8_t b = (portA >> bitB) & 1;

  // Packed state: AB
  return (a << 1) | b;
}

// Quadrature transition table.
// Index is previous_state << 2 | current_state.
int8_t decodeTransition(uint8_t prev, uint8_t curr) {
  uint8_t transition = (prev << 2) | curr;

  switch (transition) {
    case 0b0001:
    case 0b0111:
    case 0b1110:
    case 0b1000:
      return +1;

    case 0b0010:
    case 0b1011:
    case 0b1101:
    case 0b0100:
      return -1;

    default:
      return 0;
  }
}

int valueFromPosition(int32_t pos) {
  int value = pos % 101;
  if (value < 0) value += 101;
  return value;
}

// -------------------- OLED drawing --------------------

int centeredX(const char* s) {
  int w = display.getStrWidth(s);
  return max(0, (128 - w) / 2);
}

void drawControl(uint8_t index) {
  selectPcaChannel(index);

  int value = valueFromPosition(encoders[index].position);

  char valueText[24];
  snprintf(valueText, sizeof(valueText), "%d%%", value);

  display.clearBuffer();

  // Label
  display.setFont(u8g2_font_7x13B_tf);
  display.drawStr(centeredX(labels[index]), 13, labels[index]);

  // Big value
  display.setFont(u8g2_font_logisoso20_tf);
  display.drawStr(centeredX(valueText), 48, valueText);

  // Value bar
  int barWidth = map(value, 0, 100, 0, 128);
  display.drawFrame(0, 56, 128, 8);
  display.drawBox(1, 57, max(0, barWidth - 2), 6);

  display.sendBuffer();
}

// -------------------- Setup helpers --------------------

void setupMcp23017() {
  // Configure GPIOA PA0-PA7 as inputs
  mcpWriteRegister(MCP_IODIRA, 0xFF);

  // Enable pullups on GPIOA
  mcpWriteRegister(MCP_GPPUA, 0xFF);

  delay(10);

  uint8_t portA = mcpReadRegister(MCP_GPIOA);

  Serial.print("Initial MCP GPIOA = 0b");
  Serial.println(portA, BIN);

  for (int i = 0; i < 4; i++) {
    encoders[i].lastState =
      encoderStateFromPort(portA, encoders[i].bitA, encoders[i].bitB);
  }
}

void setupDisplays() {
  for (int ch = 0; ch < 4; ch++) {
    selectPcaChannel(ch);
    display.begin();
    display.setContrast(255);
  }

  for (int i = 0; i < 4; i++) {
    drawControl(i);
    encoders[i].dirty = false;
  }
}

void scanBus(TwoWire& bus, const char* name) {
  Serial.print("Scanning ");
  Serial.println(name);

  int count = 0;

  for (uint8_t addr = 1; addr < 127; addr++) {
    bus.beginTransmission(addr);
    uint8_t err = bus.endTransmission();

    if (err == 0) {
      Serial.print("  found 0x");
      if (addr < 16) Serial.print("0");
      Serial.println(addr, HEX);
      count++;
    }
  }

  Serial.print("  total: ");
  Serial.println(count);
}

// -------------------- Arduino setup/loop --------------------

void setup() {
  Serial.begin(115200);
  delay(1000);

  Serial.println("MEGASYNTH split-I2C MCP + PCA test");

  // OLED/PCA bus
  Wire.begin();            // Teensy pins 18/19
  Wire.setClock(400000);

  // Encoder/MCP bus
  Wire1.begin();           // Teensy pins 24/25
  Wire1.setClock(400000);

  scanBus(Wire,  "Wire / OLED-PCA bus");
  scanBus(Wire1, "Wire1 / MCP bus");

  setupMcp23017();
  setupDisplays();

  Serial.println("Ready. Turn encoders.");
}

void loop() {
  static uint32_t lastDisplayUpdate = 0;
  static uint8_t nextDisplayToCheck = 0;

  // Fast path:
  // Read all 8 MCP GPIOA pins in one transaction on Wire1.
  // This no longer competes with OLED I2C traffic on Wire.
  uint8_t portA = mcpReadRegister(MCP_GPIOA);

  for (int i = 0; i < 4; i++) {
    EncoderState &e = encoders[i];

    uint8_t curr = encoderStateFromPort(portA, e.bitA, e.bitB);

    if (curr != e.lastState) {
      int8_t delta = decodeTransition(e.lastState, curr);

      if (delta != 0) {
        if (e.invert) delta = -delta;

        e.position += delta;
        e.dirty = true;

        Serial.print("E");
        Serial.print(i + 1);
        Serial.print(" pos=");
        Serial.println(e.position);
      }

      e.lastState = curr;
    }
  }

  // Slow path:
  // Update at most ONE dirty OLED per display tick.
  // This avoids one big burst where four OLED redraws block encoder polling.
  if (millis() - lastDisplayUpdate >= DISPLAY_UPDATE_MS) {
    lastDisplayUpdate = millis();

    for (int tries = 0; tries < 4; tries++) {
      uint8_t i = nextDisplayToCheck;
      nextDisplayToCheck = (nextDisplayToCheck + 1) % 4;

      if (encoders[i].dirty) {
        drawControl(i);
        encoders[i].dirty = false;
        break;
      }
    }
  }
}