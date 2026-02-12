# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Initial public release of Eustaquia.
- Core application modules:
  - eustaquia: Main application with humidity-check loop and public test API.
  - hum_sensor: Reads soil moisture via I²C.
  - servo_emo: Controls servo motor via PWM to express emotions.
- Testing utilities:
  - eustaquia:test_moisture/0, eustaquia:test_servo_happy/0, eustaquia:test_servo_sad/0.
- **docs/** folder with implementation guides:
  - **docs/IMPLEMENTATION.md** — Full implementation guide in English (hardware, I²C, PMOD, PWM, software layers).
  - **docs/IMPLEMENTACION.md** — Same guide in Spanish.

### Changed

- README links to implementation guides now point to **docs/IMPLEMENTATION.md** and **docs/IMPLEMENTACION.md**.
- Spelling and consistency pass on README and implementation docs.

### Known Issues

- Initial state (out of soil) looks the same as “sad”.
- Servo position may drift over time due to lack of feedback.

### TODO

- Write the how to connect components section, upload images, and explain the PMOD CON3 connection (6 pins into 12 GRiSP PMOD) and how to use GPIO1_4.
- Improve error handling for hardware faults.
- Add unit and integration tests.
- Implement notification system (SMS, MQTT).
- Create a multi-plant network for shared BEAM-based communication.
