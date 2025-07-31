# Changelog

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog,
and this project adheres to Semantic Versioning.

## [1.0.0] - 2025-07-31

### Added

- Initial public release of Eustaquia.
- Core application modules:
  - eustaquia: Main application with humidity-check loop and public test API.
  - hum_sensor: Reads soil moisture via I²C.
  - servo_emo: Controls servo motor via PWM to express emotions.
- Testing utilities:
  - eustaquia:test_moisture/0, eustaquia:test_servo_happy/0, eustaquia:test_servo_sad/0.

### Known Issues

- Initial state (out of soil) looks the same as “sad”.
- Servo position may drift over time due to lack of feedback.

### TODO

- Write the how to connect components section
- Improve error handling for hardware faults.
- Add unit and integration tests.
- Implement notification system (SMS, MQTT).
- Create a multi-plant network for shared BEAM-based communication.
