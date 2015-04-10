unit i2cdev_PCA9685;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,baseUnix,i2cdev_base;

{
/***************************************************
  This is a library for our Adafruit 16-channel PWM & Servo driver

  Pick one up today in the adafruit shop!
  ------> http://www.adafruit.com/products/815

  These displays use I2C to communicate, 2 pins are required to
  interface. For Arduino UNOs, thats SCL -> Analog 5, SDA -> Analog 4

  Adafruit invests time and resources providing this open source code,
  please support Adafruit and open-source hardware by purchasing
  products from Adafruit!

  Written by Limor Fried/Ladyada for Adafruit Industries.
  BSD license, all text above must be included in any redistribution
 ****************************************************/
 }

type

{ TPCA9685 }

TPCA9685 = class(TIc2Base)


private

public

    constructor Create(); override;

//  procedure begin();
  procedure  reset();
  procedure setPWMFreq(freq : Extended  );
  procedure setPWM(  num :Cint ;   on  ,  off : Word );
  procedure setPin(  num :Cint ;   val : Word  ;   invert : Boolean  = false);

Private

 CONST
   PCA9685_ADDRESS = $40;

//# Registers
 PCA9685_SUBADR1  = $02;
 PCA9685_SUBADR2  = $03;
 PCA9685_SUBADR3  = $04;

 PCA9685_MODE1  = $00;
 PCA9685_PRESCALE  = $FE;

 LED0_ON_L  = $06;
 LED0_ON_H  = $07;
 LED0_OFF_L  = $08;
 LED0_OFF_H  = $09;

 ALLLED_ON_L  = $FA;
 ALLLED_ON_H  = $FB;
 ALLLED_OFF_L  = $FC;
 ALLLED_OFF_H  = $FD;



 end;



implementation
// uses math;
{ TPCA9685 }

constructor TPCA9685.Create();
begin
  inherited Create(PCA9685_ADDRESS);

end;

procedure TPCA9685.reset();
begin
  connect ;
 I2C_Write8(hdev ,PCA9685_MODE1, 0);
end;

procedure TPCA9685.setPWMFreq(freq: Extended);
begin
   {
  //Serial.print("Attempting to set freq ");
  //Serial.println(freq);
  freq *= 0.9;  // Correct for overshoot in the frequency setting (see issue #11).
  float prescaleval = 25000000;
  prescaleval /= 4096;
  prescaleval /= freq;
  prescaleval -= 1;

  uint8_t prescale = floor(prescaleval + 0.5);


  uint8_t oldmode = read8(PCA9685_MODE1);
  uint8_t newmode = (oldmode&0x7F) | 0x10; // sleep
  write8(PCA9685_MODE1, newmode); // go to sleep
  write8(PCA9685_PRESCALE, prescale); // set the prescaler
  write8(PCA9685_MODE1, oldmode);
  delay(5);
  write8(PCA9685_MODE1, oldmode | 0xa1);  //  This sets the MODE1 register to turn on auto increment.
                                          // This is why the beginTransmission below was not working.
  //  Serial.print("Mode now 0x"); Serial.println(read8(PCA9685_MODE1), HEX);
    }
end;

procedure TPCA9685.setPWM(num: Cint; on, off: Word);
begin
connect ;

 // WIRE.write(LED0_ON_L+4*num);
 // WIRE.write(on);
 // WIRE.write(on>>8);
 // WIRE.write(off);
 // WIRE.write(off>>8);
 // WIRE.endTransmission();

end;

procedure TPCA9685.setPin(num: Cint; val: Word; invert: Boolean);
begin
  // Clamp value between 0 and 4095 inclusive.
 // val = min(val, 4095);
  if val > 4095 then
  val := 4095 ;


  if (invert) then
  begin
    if (val = 0) then
    begin
      // Special value for signal fully on.
      setPWM(num, 4096, 0);
    end
    else if (val = 4095) then
    begin
      // Special value for signal fully off.
      setPWM(num, 0, 4096);
    end
    else
    begin
      setPWM(num, 0, 4095-val);
    end;
  end
  else
  begin
    if (val = 4095) then
    begin
      // Special value for signal fully on.
      setPWM(num, 4096, 0);
    end
    else if (val = 0) then
    begin
      // Special value for signal fully off.
      setPWM(num, 0, 4096);
    end
    else
    begin
      setPWM(num, 0, val);
    end;
  end;
end;


end.
