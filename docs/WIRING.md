## 🛠️ Components and how to connect them

### Components

To bring your plant project to life, you’ll need:

- 🟩 **[GRiSP2 embedded board](https://www.grisp.org/hardware)** – runs Erlang/Elixir directly on RTEMS, no Linux needed.
- 🔌 **USB cable (micro-USB)** – To power the board and access the console
- 💾 **microSD card** (optional) – For deploying your application. If your GRiSP board is already linked to [GRiSP.io](https://grisp.io/), you can deploy software updates remotely (OTA) without needing a microSD card or USB cable.
- 🪴 **Capacitive soil moisture sensor** – I used an [Adafruit Soil Moisture Sensor (I²C)](https://www.adafruit.com/product/4026) model; corrosion-proof and stable readings.
- ⚙️ **SG90 servo motor** (or similar) – To move Eustaquia’s face
- 🔌 [PMOD CON3: R/C Servo Connectors](https://digilent.com/shop/pmod-con3-r-c-servo-connectors/) – A small add-on board that lets you easily connect and control up to four servo motors.
- 🔋 **Power source** for the servo
- 🧵 **Jumper wires** – For connections.
- 🎨 **A face for Eustaquia** – Cardboard, 3D print, markers… get creative!

### How to connect them

Diagram: 

<img width="500"  alt="image that shows how to connect all items" src="https://github.com/user-attachments/assets/6ca1e3c3-18f6-48b6-b549-914c0ee077ed" />


<table>
<tr><th>Real Image 1</th><th>Real Image 2</th></tr>
<tr><td><img src="https://github.com/user-attachments/assets/741c4778-5f97-4125-b1aa-3f3f10294eb1" alt="real image" width="200" /></td><td><img src="https://github.com/user-attachments/assets/353e9b85-fd41-4bae-b505-8102d1480fb9" alt="real image that shows how to connect all items" width="200" /></td></tr>
</table>


#### Servo Motor

We want to connect a PMOD CON3, a battery and a servo:

<table>
<tr><th>Servo</th><th>PMOD CON3</th><th>Battery</th></tr>
<tr><td><img src="../assets/SG90.png" alt="Servo" width="200" /></td><td><img src="../assets/PMODCON3.png" alt="PMOD CON3" width="200" /></td><td><img src="../assets/battery_holder.jpg" alt="Battery" width="200" /></td></tr>
</table>

To connect the servo, plug the PMOD R/C Servo module directly into the GRiSP board GPIO and attach the servo’s signal wire to the GPIO1_4 pin.

**How?**

🔋 Power and ground for the servo are provided through the PMOD connector, but since servos often need more current than the GRiSP board can safely supply, you should use a small external battery. The SG90 servo operating Voltage: 4.8V to 6V.

> ℹ️ Recommended: A small 5V battery pack (for example, 4x AA batteries = 6V, or a regulated USB 5V source).

To do this, connect the battery directly to the PMOD connector:

- Loosen the small screws on the PMOD’s power terminals (the blue block).
- Insert the battery wires into the terminals:
  - The negative wire (usually black) goes to the terminal marked “–”.
  - The positive wire (usually red) goes to the terminal marked “+”.
- Tighten the screws to secure the wires.

This setup allows the servo to receive enough power while still being controlled by the GRiSP board through the signal pin.

⚙️ To connect a servo to the PMOD R/C Servo (Pmod CON3), simply match the three wires of the servo to the corresponding pins on the PMOD:

- The signal wire (usually orange, yellow, or white) goes to SIG
- The power wire (red) goes to VS for voltage supply
- The ground wire (black or brown) connects to GND.

#### Soil moisture sensor

To connect the I²C soil moisture sensor, use a PMOD I²C module plugged into the GRiSP board and wire SCL to SCL, SDA to SDA, VCC to 3.3V or 5V (depending on the sensor), and GND to ground. The optional INT and RESET pins can be left unconnected.

**How?**

<img alt="Soil sensor CON3" src="../assets/adafruit_soil.jpg" width="500">

Looking at the PMOD R/C Servo (Pmod CON3) with the triangle marker pointing to the left, the pinout from top to bottom is:

- GND - power and logic ground
- VIN - 3-5V DC (use the same power voltage as you would for I2C logic)
- I2C SDA - there's a 10K pullup to VIN
- I2C SCL - there's a 10K pullup to VIN

More info at [Pinouts AdaFruit Stemma Soil Sensor](https://learn.adafruit.com/adafruit-stemma-soil-sensor-i2c-capacitive-moisture-sensor/pinouts).

Digilent Pmod Interface Specification that is updating the I2C spec to be 6-pin with:

<img alt="PMOD I2C GRiSP" src="../assets/i2cPMOD.png" width="200">

- I2C SCL - signals on pin 3.
- I2C SDA - signals on pin 4.
- GND - Pin 5
- VIN - PIN 6
- Optional, interrupt and reset pins on 1 and 2 respectively.

Check the 1 marker on the board to know where is the pin 1.

More info at [PMOD I2C Spec](https://digilent.com/blog/new-i2c-standard-for-pmods/?srsltid=AfmBOoptLmLxP8FrLFza-cjVbrfgA9ECXlfR_V6dQ86XCC2ZdKUZdG3h).