#include <Arduino.h>
#include <Wire.h>

void setup() {
  pinMode(13, OUTPUT);

  Serial.begin(115200);
  delay(1500);

  Serial.println();
  Serial.println("MEGASYNTH I2C scanner");
  Serial.println("Expected:");
  Serial.println("  PCA/TCA mux around 0x70");
  Serial.println("  MCP23017 around 0x20");
  Serial.println();

  Wire.begin();          // Teensy pins 18/19
  Wire.setClock(100000); // slow/safe
}

void loop() {
  digitalWrite(13, !digitalRead(13));

  int count = 0;

  Serial.println("Scanning...");

  for (uint8_t addr = 1; addr < 127; addr++) {
    Wire.beginTransmission(addr);
    uint8_t err = Wire.endTransmission();

    if (err == 0) {
      Serial.print("Found device at 0x");
      if (addr < 16) Serial.print("0");
      Serial.println(addr, HEX);
      count++;
    }
  }

  Serial.print("Found ");
  Serial.print(count);
  Serial.println(" device(s).");
  Serial.println();

  delay(2000);
}