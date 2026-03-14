#include <Encoder.h>

constexpr int MIDI_CH = 1;
constexpr int COUNTS_PER_STEP = 4;

// Pins + CCs
// Encoder enc1(0, 1);
// Encoder enc2(2, 3);
// Encoder enc3(4, 5);
// Encoder enc4(6, 7);

Encoder enc1(8, 9);
Encoder enc2(10, 11);
Encoder enc3(12, 13);
Encoder enc4(14, 15);


constexpr int CC1 = 10, CC2 = 11, CC3 = 12, CC4 = 13;

struct EncState {
  long lastRaw = 0;
  long accum = 0;
  int ccNum = 0;
};

static inline void sendRelCC(int ccNum, long steps) {
  // Two's complement relative CC:
  // +n => n (1..63), -n => 128 - n (e.g. -1 => 127)
  while (steps != 0) {
    long s = steps;
    if (s > 63) s = 63;
    if (s < -63) s = -63;

    int v = 0;
    if (s > 0) v = (int)s;          // +1 => 1
    else       v = 128 + (int)s;    // -1 => 127, -2 => 126

    usbMIDI.sendControlChange(ccNum, v, MIDI_CH);
    steps -= s;
  }
}

static void service(Encoder &enc, EncState &st) {
  long raw = enc.read();
  if (raw == st.lastRaw) return;

  long delta = raw - st.lastRaw;
  st.lastRaw = raw;

  st.accum += delta;

  long steps = 0;
  if (st.accum >= COUNTS_PER_STEP) {
    steps = st.accum / COUNTS_PER_STEP;
    st.accum = st.accum % COUNTS_PER_STEP;
  } else if (st.accum <= -COUNTS_PER_STEP) {
    steps = st.accum / COUNTS_PER_STEP; // negative
    st.accum = st.accum % COUNTS_PER_STEP;
  } else {
    return;
  }

  if (steps != 0) sendRelCC(st.ccNum, steps);
}

void setup() {
  // pinMode(0, INPUT_PULLUP); pinMode(1, INPUT_PULLUP);
  // pinMode(2, INPUT_PULLUP); pinMode(3, INPUT_PULLUP);
  // pinMode(4, INPUT_PULLUP); pinMode(5, INPUT_PULLUP);
  // pinMode(6, INPUT_PULLUP); pinMode(7, INPUT_PULLUP);

  pinMode(8, INPUT_PULLUP); pinMode(9, INPUT_PULLUP);
  pinMode(10, INPUT_PULLUP); pinMode(11, INPUT_PULLUP);
  pinMode(12, INPUT_PULLUP); pinMode(13, INPUT_PULLUP);
  pinMode(14, INPUT_PULLUP); pinMode(15, INPUT_PULLUP);

  
  enc1.write(0); enc2.write(0); enc3.write(0); enc4.write(0);
}

void loop() {
  static EncState s1{0, 0, CC1};
  static EncState s2{0, 0, CC2};
  static EncState s3{0, 0, CC3};
  static EncState s4{0, 0, CC4};

  service(enc1, s1);
  service(enc2, s2);
  service(enc3, s3);
  service(enc4, s4);

  while (usbMIDI.read()) {}
}
