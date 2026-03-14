#include <Encoder.h>

constexpr int MIDI_CH = 1;
constexpr int NUM_ENCS = 12;

// Adjust if needed (2 or 4 are common for detented encoders)
constexpr int COUNTS_PER_EVENT = 2;

// --------------------
// Pin definitions
// --------------------
// Each entry: {A_pin, B_pin}
constexpr int ENC_PINS[NUM_ENCS][2] = {
  {0, 1},
  {2, 3},
  {4, 5},
  {6, 7},
  {8, 9},
  {10, 11},
  {12, 13},
  {14, 15},
  {16, 17},
  {18, 19},
  {20, 21},
  {22, 23}
};

// CC numbers (edit as desired)
constexpr int CC_NUMS[NUM_ENCS] = {
  10, 11, 12, 13,
  14, 15, 16, 17,
  18, 19, 20, 21
};

// --------------------
// Encoder objects
// --------------------
Encoder encs[NUM_ENCS] = {
  Encoder(0, 1),
  Encoder(2, 3),
  Encoder(4, 5),
  Encoder(6, 7),
  Encoder(8, 9),
  Encoder(10, 11),
  Encoder(12, 13),
  Encoder(14, 15),
  Encoder(16, 17),
  Encoder(18, 19),
  Encoder(20, 21),
  Encoder(22, 23)
};

// --------------------
// State
// --------------------
struct EncState {
  long lastRaw = 0;
  long accum = 0;
};

EncState states[NUM_ENCS];

// --------------------
// MIDI helper
// --------------------
static inline void sendDir(int cc, int dir) {
  // Two's complement relative CC
  // +1 => 1
  // -1 => 127
  int value = (dir > 0) ? 1 : 127;
  usbMIDI.sendControlChange(cc, value, MIDI_CH);
}

void setup() {
  for (int i = 0; i < NUM_ENCS; i++) {
    pinMode(ENC_PINS[i][0], INPUT_PULLUP);
    pinMode(ENC_PINS[i][1], INPUT_PULLUP);

    encs[i].write(0);
    states[i].lastRaw = 0;
    states[i].accum = 0;
  }
}

void loop() {
  for (int i = 0; i < NUM_ENCS; i++) {
    long raw = encs[i].read();
    if (raw == states[i].lastRaw) continue;

    long delta = raw - states[i].lastRaw;
    states[i].lastRaw = raw;
    states[i].accum += delta;

    if (states[i].accum >= COUNTS_PER_EVENT) {
      sendDir(CC_NUMS[i], +1);
      states[i].accum = 0;
    }
    else if (states[i].accum <= -COUNTS_PER_EVENT) {
      sendDir(CC_NUMS[i], -1);
      states[i].accum = 0;
    }
  }

  while (usbMIDI.read()) {}
}