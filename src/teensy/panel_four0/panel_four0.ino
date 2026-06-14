#include <Arduino.h>
#include <Wire.h>
#include <U8g2lib.h>

#define PCA_ADDR 0x70
#define MCP_ADDR 0x27

#define SERIAL_BAUD 115200
#define MSG_START 0x2A // *  // 0xAA
#define MSG_END 0x0A // newline

// MCP23017 registers, BANK=0 default addressing
#define MCP_IODIRA 0x00
#define MCP_GPPUA  0x0C
#define MCP_GPIOA  0x12

// Wire  = PCA/OLED bus on Teensy pins 18 SDA / 19 SCL
// Wire1 = MCP bus      on Teensy pins 17 SDA / 16 SCL

U8G2_SH1106_128X64_NONAME_F_HW_I2C display(
  U8G2_R0,
  U8X8_PIN_NONE
);

struct EncoderState {
  uint8_t bitA;
  uint8_t bitB;
  uint8_t lastState;
  bool invert;
};

struct DisplayState {
  char label[24];
  char value[24];
  int16_t amount;   // 0-1000 for bar, -1 for no bar
  bool dirty;
};

EncoderState encoders[4] = {
  // bitA, bitB, lastState, invert
  {0, 1, 0, false},  // Encoder 0: MCP PA0 / PA1
  {2, 3, 0, false},  // Encoder 1: MCP PA2 / PA3
  {4, 5, 0, false},  // Encoder 2: MCP PA4 / PA5
  {6, 7, 0, false}   // Encoder 3: MCP PA6 / PA7
};

// Blank on boot. Clojure owns real display state.
DisplayState displays[4] = {
  {"", "", -1, false},
  {"", "", -1, false},
  {"", "", -1, false},
  {"", "", -1, false}
};

// -------------------- MCP23017 on Wire1 --------------------

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

// -------------------- PCA/TCA OLED mux on Wire --------------------

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

  return (a << 1) | b;
}

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

void sendEncoderEvent(uint8_t encoderIndex, int8_t delta) {
  Serial.write((uint8_t)MSG_START);
  Serial.write((uint8_t)'E');
  Serial.write(encoderIndex);
  Serial.write((uint8_t)delta);
  Serial.write((uint8_t)MSG_END);
}

// Send a no-argument framed message: MSG_START <type> MSG_END.
void sendMsg(char type) {
  Serial.write((uint8_t)MSG_START);
  Serial.write((uint8_t)type);
  Serial.write((uint8_t)MSG_END);
}

// -------------------- OLED rendering --------------------

int centeredX(const char* s) {
  int w = display.getStrWidth(s);
  return max(0, (128 - w) / 2);
}

void drawDisplay(uint8_t index) {
  if (index >= 4) return;

  selectPcaChannel(index);

  DisplayState &d = displays[index];

  display.clearBuffer();

  if (d.label[0] != '\0') {
    display.setFont(u8g2_font_7x13B_tf);
    display.drawStr(centeredX(d.label), 13, d.label);
  }

  if (d.value[0] != '\0') {
    display.setFont(u8g2_font_logisoso20_tf);
    display.drawStr(centeredX(d.value), 48, d.value);
  }

  if (d.amount >= 0) {
    int clamped = constrain(d.amount, 0, 1000);
    int barWidth = map(clamped, 0, 1000, 0, 128);

    display.drawFrame(0, 56, 128, 8);

    if (barWidth > 2) {
      display.drawBox(1, 57, barWidth - 2, 6);
    }
  }

  display.sendBuffer();
}

void clearPhysicalDisplay(uint8_t index) {
  if (index >= 4) return;

  selectPcaChannel(index);
  display.clearBuffer();
  display.sendBuffer();
}

void renderOneDirtyDisplayIfAny() {
  static uint8_t nextDisplayToCheck = 0;

  for (int tries = 0; tries < 4; tries++) {
    uint8_t i = nextDisplayToCheck;
    nextDisplayToCheck = (nextDisplayToCheck + 1) % 4;

    if (displays[i].dirty) {
      drawDisplay(i);
      displays[i].dirty = false;
      break;
    }
  }
}

// -------------------- Serial input from Clojure --------------------
//
// Clojure -> Teensy text commands are newline terminated.
//
// Display command:
// D<TAB>index<TAB>label<TAB>value<TAB>amount
//
// Examples:
// D    0    CUTOFF    1.24kHz    642
// D    1    RES       62%        620
//
// amount:
//   0-1000 = draw bar
//   -1     = no bar
//
// Other commands:
// PING
// READY?
// CLEAR
// INV<TAB>index<TAB>0
// INV<TAB>index<TAB>1

void setDisplayState(uint8_t index, const char* label, const char* value, int16_t amount) {
  if (index >= 4) return;

  DisplayState &d = displays[index];

  strncpy(d.label, label, sizeof(d.label) - 1);
  d.label[sizeof(d.label) - 1] = '\0';

  strncpy(d.value, value, sizeof(d.value) - 1);
  d.value[sizeof(d.value) - 1] = '\0';

  d.amount = amount;
  d.dirty = true;
}

void handleDisplayCommand() {
  char* indexText = strtok(NULL, "\t");
  char* label = strtok(NULL, "\t");
  char* value = strtok(NULL, "\t");
  char* amountText = strtok(NULL, "\t");

  if (!indexText || !label || !value) return;

  uint8_t index = atoi(indexText);

  int16_t amount = -1;
  if (amountText) {
    amount = atoi(amountText);
  }

  setDisplayState(index, label, value, amount);
}

void handleInvertCommand() {
  char* indexText = strtok(NULL, "\t");
  char* invertText = strtok(NULL, "\t");

  if (!indexText || !invertText) return;

  uint8_t index = atoi(indexText);
  if (index >= 4) return;

  encoders[index].invert = atoi(invertText) != 0;
}

void handleSerialLine(char* line) {
  if (strcmp(line, "PING") == 0) {
    sendMsg('P');  // pong
    return;
  }

  if (strcmp(line, "READY?") == 0) {
    sendMsg('R');  // ready
    return;
  }

  if (strcmp(line, "CLEAR") == 0) {
    for (int i = 0; i < 4; i++) {
      setDisplayState(i, "", "", -1);
    }
    return;
  }

  char* cmd = strtok(line, "\t");
  if (!cmd) return;

  if (strcmp(cmd, "D") == 0) {
    handleDisplayCommand();
    return;
  }

  if (strcmp(cmd, "INV") == 0) {
    handleInvertCommand();
    return;
  }
}

void readSerialNonBlocking() {
  static char buffer[128];
  static uint8_t len = 0;

  while (Serial.available() > 0) {
    char c = Serial.read();

    if (c == '\r') {
      continue;
    }

    if (c == '\n') {
      buffer[len] = '\0';

      if (len > 0) {
        handleSerialLine(buffer);
      }

      len = 0;
      continue;
    }

    if (len < sizeof(buffer) - 1) {
      buffer[len++] = c;
    } else {
      // Overflow: discard partial line
      len = 0;
    }
  }
}

// -------------------- Setup helpers --------------------

void setupMcp23017() {
  mcpWriteRegister(MCP_IODIRA, 0xFF); // PA0-PA7 inputs
  mcpWriteRegister(MCP_GPPUA, 0xFF);  // PA0-PA7 pullups enabled

  delay(10);

  uint8_t portA = mcpReadRegister(MCP_GPIOA);

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
    display.clearBuffer();
    display.sendBuffer();
  }

  for (int i = 0; i < 4; i++) {
    displays[i].dirty = false;
  }
}

// -------------------- Main --------------------

void setup() {
  Serial.begin(SERIAL_BAUD);
  delay(1000);

  Wire.begin();          // OLED/PCA bus: pins 18/19
  Wire.setClock(400000);

  Wire1.begin();         // MCP bus: pins 17/16
  Wire1.setClock(400000);

  setupMcp23017();
  setupDisplays();

  sendMsg('R');  // ready
}

void loop() {
  // 1. Read encoders fast.
  uint8_t portA = mcpReadRegister(MCP_GPIOA);

  for (int i = 0; i < 4; i++) {
    EncoderState &e = encoders[i];

    uint8_t curr = encoderStateFromPort(portA, e.bitA, e.bitB);

    if (curr != e.lastState) {
      int8_t delta = decodeTransition(e.lastState, curr);

      if (delta != 0) {
        if (e.invert) {
          delta = -delta;
        }

        sendEncoderEvent(i, delta);
      }

      e.lastState = curr;
    }
  }

  // 2. Read commands from Clojure without blocking.
  readSerialNonBlocking();

  // 3. Render at most one changed OLED per loop pass.
  renderOneDirtyDisplayIfAny();
}
