/******************************************************
 *  directed_study_dbh12_final.ino
 *
 *  Water transfer controller for dual-channel H-bridge
 *
 *  Hardware model:
 *    Pump1 transfers water A -> B
 *    Pump2 transfers water B -> A
 *
 *  Network commands:
 *    hello
 *    status
 *    measure
 *    stop
 *    dispense,<liters>          -> legacy alias for A -> B
 *    transfer,A,B,<liters>
 *    transfer,B,A,<liters>
 *
 *  Flow calibration:
 *    F(Hz) = 98 * Q(L/min)
 *    pulses/L = 98 * 60 = 5880
 ******************************************************/

#include <WiFiS3.h>
#include "arduinoSecrets.h"

// =====================================================
// H-BRIDGE PINS
// =====================================================
// Pump1 channel
#define PUMP1_IN_A 3
#define PUMP1_IN_B 5

// Pump2 channel
#define PUMP2_IN_A 6
#define PUMP2_IN_B 9

// =====================================================
// SENSOR PINS
// =====================================================
#define FLOW_PIN 2
#define TRIG_PIN 12
#define ECHO_PIN 13

// =====================================================
// CONFIG
// =====================================================
const float PULSES_PER_LITER = 5880.0;
const float MAX_LITERS = 50.0;
const int PUMP_PWM = 120;   // safe starting value for testing

#define DEBUG 1
const char* PROTO_VERSION = "transfer_dualpump_v1";

// =====================================================
// WIFI / SERVER
// =====================================================
char ssid[] = SECRET_SSID;
char pass[] = SECRET_PASS;
int wifiStatus = WL_IDLE_STATUS;

const int SERVER_PORT = 4080;
WiFiServer server(SERVER_PORT);
WiFiClient client;

// =====================================================
// STATE MACHINE
// =====================================================
enum MainState {
  INIT,
  GET_REQUEST,
  PARSE_REQUEST,
  SENSE,
  THINK,
  ACT
};

MainState state = INIT;

// =====================================================
// DIRECTION MODEL
// =====================================================
enum Direction {
  DIR_IDLE,
  DIR_A_TO_B,
  DIR_B_TO_A
};

// =====================================================
// RUNTIME STATE
// =====================================================
volatile unsigned long flowPulses = 0;

bool transferActive = false;
bool pumpRunning = false;

Direction currentDirection = DIR_IDLE;

float targetLiters = 0.0;
float dispensedLiters = 0.0;
unsigned long startPulseCount = 0;

float lastDistanceCm = -1.0;
String msgFromClient = "";

// =====================================================
// INTERRUPT SERVICE ROUTINE
// =====================================================
void flowISR() {
  flowPulses++;
}

// =====================================================
// LOW-LEVEL PUMP HARDWARE CONTROL
// =====================================================
// Based on the demo pattern:
// one side LOW, PWM on the other side

void stopPump1Hardware() {
  analogWrite(PUMP1_IN_A, 0);
  analogWrite(PUMP1_IN_B, 0);
}

void stopPump2Hardware() {
  analogWrite(PUMP2_IN_A, 0);
  analogWrite(PUMP2_IN_B, 0);
}

void stopAllPumpsHardware() {
  stopPump1Hardware();
  stopPump2Hardware();
}

// Pump1 = A -> B
void runPump1Hardware(int pwmValue) {
  stopPump2Hardware();
  digitalWrite(PUMP1_IN_A, LOW);
  analogWrite(PUMP1_IN_B, pwmValue);
}

// Pump2 = B -> A
void runPump2Hardware(int pwmValue) {
  stopPump1Hardware();
  digitalWrite(PUMP2_IN_B, LOW);
  analogWrite(PUMP2_IN_A, pwmValue);
}

// =====================================================
// UTILITY HELPERS
// =====================================================
const char* directionToString(Direction dir) {
  switch (dir) {
    case DIR_A_TO_B: return "A_TO_B";
    case DIR_B_TO_A: return "B_TO_A";
    default:         return "IDLE";
  }
}

unsigned long getFlowPulses() {
  unsigned long pulsesNow;
  noInterrupts();
  pulsesNow = flowPulses;
  interrupts();
  return pulsesNow;
}

float computeDispensedLiters() {
  if (!transferActive) {
    return dispensedLiters;
  }

  unsigned long pulsesNow = getFlowPulses();
  unsigned long delta = pulsesNow - startPulseCount;
  return ((float)delta / PULSES_PER_LITER);
}

float readDistanceCm() {
  digitalWrite(TRIG_PIN, LOW);
  delayMicroseconds(2);

  digitalWrite(TRIG_PIN, HIGH);
  delayMicroseconds(10);
  digitalWrite(TRIG_PIN, LOW);

  long duration = pulseIn(ECHO_PIN, HIGH, 30000);
  if (duration == 0) {
    return -1.0;
  }

  return duration / 58.0;
}

void printWifiStatus() {
  Serial.print("SSID: ");
  Serial.println(WiFi.SSID());

  IPAddress ip = WiFi.localIP();
  Serial.print("IP Address: ");
  Serial.println(ip);

  long rssi = WiFi.RSSI();
  Serial.print("Signal strength (RSSI): ");
  Serial.print(rssi);
  Serial.println(" dBm");

  Serial.print("TCP Server Port: ");
  Serial.println(SERVER_PORT);
}

void fetchLine(WiFiClient &sock) {
  msgFromClient = "";
  bool done = false;
  unsigned long start = millis();

  while (sock.connected() && !done && (millis() - start < 2000)) {
    while (sock.available() && !done) {
      char c = sock.read();
      if (c == '\n') {
        done = true;
        break;
      }
      if (c != '\r') {
        msgFromClient += c;
      }
    }
  }

  msgFromClient.trim();
}

// =====================================================
// HIGH-LEVEL TRANSFER CONTROL
// =====================================================
void stopTransfer() {
  stopAllPumpsHardware();

  transferActive = false;
  pumpRunning = false;
  currentDirection = DIR_IDLE;

  targetLiters = 0.0;
  dispensedLiters = 0.0;

  if (DEBUG) {
    Serial.println("STOP TRANSFER");
  }
}

void startTransfer(Direction dir, float liters) {
  stopAllPumpsHardware();
  delay(100);

  noInterrupts();
  startPulseCount = flowPulses;
  interrupts();

  targetLiters = liters;
  dispensedLiters = 0.0;
  transferActive = true;
  pumpRunning = true;
  currentDirection = dir;

  if (dir == DIR_A_TO_B) {
    runPump1Hardware(PUMP_PWM);
  } else if (dir == DIR_B_TO_A) {
    runPump2Hardware(PUMP_PWM);
  } else {
    stopTransfer();
    return;
  }

  if (DEBUG) {
    Serial.print("START TRANSFER: dir=");
    Serial.print(directionToString(dir));
    Serial.print(" targetLiters=");
    Serial.print(targetLiters, 3);
    Serial.print(" pwm=");
    Serial.println(PUMP_PWM);
  }
}

// =====================================================
// COMMAND HANDLERS
// =====================================================
void handleHello() {
  client.print("ok:");
  client.println(PROTO_VERSION);
}

void handleMeasure() {
  lastDistanceCm = readDistanceCm();

  if (lastDistanceCm < 0.0) {
    client.println("err:no_echo");
  } else {
    client.print("measure:");
    client.println(lastDistanceCm, 2);
  }
}

void handleStop() {
  stopTransfer();
  client.println("ok:stopped");
}

void handleStatus() {
  lastDistanceCm = readDistanceCm();

  float currentDispensed = transferActive ? computeDispensedLiters() : dispensedLiters;
  unsigned long pulsesNow = getFlowPulses();
  unsigned long deltaPulses = transferActive ? (pulsesNow - startPulseCount) : 0;

  client.print("status:");
  client.print("dir=");
  client.print(directionToString(currentDirection));
  client.print(" pulse=");
  client.print(deltaPulses);
  client.print(" dispensedL=");
  client.print(currentDispensed, 3);
  client.print(" target=");
  client.print(targetLiters, 3);
  client.print(" running=");
  client.print(pumpRunning ? "YES" : "NO");
  client.print(" distance_cm=");

  if (lastDistanceCm < 0.0) {
    client.println("NA");
  } else {
    client.println(lastDistanceCm, 2);
  }
}

void handleDispenseLegacy() {
  // dispense,<liters> => Pump1 => A -> B
  if (transferActive || pumpRunning) {
    client.println("err:busy");
    return;
  }

  String litersStr = msgFromClient.substring(9);
  litersStr.trim();
  float liters = litersStr.toFloat();

  if (liters > 0.0 && liters <= MAX_LITERS) {
    startTransfer(DIR_A_TO_B, liters);
    client.println("ok:dispense_set");
  } else {
    client.println("err:bad_value");
  }
}

void handleTransferCommand() {
  // transfer,A,B,1.5
  // transfer,B,A,0.75

  if (transferActive || pumpRunning) {
    client.println("err:busy");
    return;
  }

  int firstComma = msgFromClient.indexOf(',');
  int secondComma = msgFromClient.indexOf(',', firstComma + 1);
  int thirdComma = msgFromClient.indexOf(',', secondComma + 1);

  if (firstComma < 0 || secondComma < 0 || thirdComma < 0) {
    client.println("err:bad_format");
    return;
  }

  String source = msgFromClient.substring(firstComma + 1, secondComma);
  String dest = msgFromClient.substring(secondComma + 1, thirdComma);
  String litersStr = msgFromClient.substring(thirdComma + 1);

  source.trim();
  dest.trim();
  litersStr.trim();

  source.toUpperCase();
  dest.toUpperCase();

  float liters = litersStr.toFloat();

  if (!(liters > 0.0 && liters <= MAX_LITERS)) {
    client.println("err:bad_value");
    return;
  }

  if (source == "A" && dest == "B") {
    startTransfer(DIR_A_TO_B, liters);
    client.println("ok:transfer_set:A_TO_B");
    return;
  }

  if (source == "B" && dest == "A") {
    startTransfer(DIR_B_TO_A, liters);
    client.println("ok:transfer_set:B_TO_A");
    return;
  }

  client.println("err:bad_route");
}

// =====================================================
// SETUP
// =====================================================
void setup() {
  Serial.begin(9600);
  while (!Serial) {}

  pinMode(PUMP1_IN_A, OUTPUT);
  pinMode(PUMP1_IN_B, OUTPUT);
  pinMode(PUMP2_IN_A, OUTPUT);
  pinMode(PUMP2_IN_B, OUTPUT);
  stopAllPumpsHardware();

  pinMode(FLOW_PIN, INPUT_PULLUP);
  attachInterrupt(digitalPinToInterrupt(FLOW_PIN), flowISR, RISING);

  pinMode(TRIG_PIN, OUTPUT);
  pinMode(ECHO_PIN, INPUT);

  state = INIT;
}

// =====================================================
// MAIN LOOP
// =====================================================
void loop() {
  switch (state) {

    case INIT: {
      if (DEBUG) Serial.println("\nINIT");

      int attempts = 0;
      while (wifiStatus != WL_CONNECTED && attempts < 10) {
        Serial.print("Connecting to WiFi: ");
        Serial.println(ssid);

        wifiStatus = WiFi.begin(ssid, pass);
        attempts++;
        delay(2000);
      }

      if (wifiStatus == WL_CONNECTED) {
        Serial.println("WiFi connected.");
        printWifiStatus();
        server.begin();
      } else {
        Serial.println("WiFi NOT connected (continuing anyway).");
      }

      state = GET_REQUEST;
      break;
    }

    case GET_REQUEST: {
      client = server.available();

      if (client && client.connected()) {
        if (client.available()) {
          fetchLine(client);
          state = PARSE_REQUEST;
        } else {
          state = SENSE;
        }
      } else {
        state = SENSE;
      }
      break;
    }

    case PARSE_REQUEST: {
      if (DEBUG) {
        Serial.print("\nPARSE_REQUEST: ");
        Serial.println(msgFromClient);
      }

      if (msgFromClient.equals("hello")) {
        handleHello();
      } else if (msgFromClient.equals("status")) {
        handleStatus();
      } else if (msgFromClient.equals("measure")) {
        handleMeasure();
      } else if (msgFromClient.equals("stop")) {
        handleStop();
      } else if (msgFromClient.startsWith("dispense,")) {
        handleDispenseLegacy();
      } else if (msgFromClient.startsWith("transfer,")) {
        handleTransferCommand();
      } else {
        client.println("err:unknown_command");
      }

      client.stop();
      state = SENSE;
      break;
    }

    case SENSE: {
      lastDistanceCm = readDistanceCm();

      if (transferActive) {
        dispensedLiters = computeDispensedLiters();

        if (DEBUG) {
          Serial.print("SENSE: dir=");
          Serial.print(directionToString(currentDirection));
          Serial.print(" dispensedL=");
          Serial.print(dispensedLiters, 3);
          Serial.print(" target=");
          Serial.print(targetLiters, 3);
          Serial.print(" running=");
          Serial.print(pumpRunning ? "YES" : "NO");
          Serial.print(" distance_cm=");
          if (lastDistanceCm < 0.0) Serial.println("NA");
          else Serial.println(lastDistanceCm, 2);
        }
      } else {
        if (DEBUG) {
          Serial.print("SENSE: idle distance_cm=");
          if (lastDistanceCm < 0.0) Serial.println("NA");
          else Serial.println(lastDistanceCm, 2);
        }
      }

      state = THINK;
      break;
    }

    case THINK: {
      if (transferActive && pumpRunning) {
        if (dispensedLiters >= targetLiters) {
          state = ACT;
        } else {
          state = GET_REQUEST;
        }
      } else {
        state = GET_REQUEST;
      }
      break;
    }

    case ACT: {
      if (DEBUG) {
        Serial.print("ACT: TARGET REACHED. Final dispensedL=");
        Serial.println(dispensedLiters, 3);
      }

      stopTransfer();
      state = GET_REQUEST;
      break;
    }

    default:
      state = GET_REQUEST;
      break;
  }

  delay(50);
}
