unit i2cdev_ADS1015;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseUnix, i2cdev_base;

const
  ADS1015_ADDRESS = $48;    // 1001 000 (ADDR = GND)


  ADS1015_REG_POINTER_CONVERT = $00;
  ADS1015_REG_POINTER_CONFIG = $01;
  ADS1015_REG_POINTER_LOWTHRESH = $02;
  ADS1015_REG_POINTER_HITHRESH = $03;

  //  ADS1015_REG_CONFIG_OS_MASK      = $8000;
  ADS1015_REG_CONFIG_OS_SINGLE = $8000;  // Write: Set to start a single-conversion
  //   ADS1015_REG_CONFIG_OS_BUSY      = $0000;  // Read: Bit = 0 when conversion is in progress
  ADS1015_REG_CONFIG_OS_NOTBUSY = $8000;
  // Read: Bit = 1 when device is not performing a conversion

  //   ADS1015_REG_CONFIG_MUX_MASK     = $7000;
  ADS1015_REG_CONFIG_MUX_DIFF_0_1 = $0000;  // Differential P = AIN0, N = AIN1 (default;
  ADS1015_REG_CONFIG_MUX_DIFF_0_3 = $1000;  // Differential P = AIN0, N = AIN3
  ADS1015_REG_CONFIG_MUX_DIFF_1_3 = $2000;  // Differential P = AIN1, N = AIN3
  ADS1015_REG_CONFIG_MUX_DIFF_2_3 = $3000;  // Differential P = AIN2, N = AIN3
  ADS1015_REG_CONFIG_MUX_SINGLE_0 = $4000;  // Single-ended AIN0
  ADS1015_REG_CONFIG_MUX_SINGLE_1 = $5000;  // Single-ended AIN1
  ADS1015_REG_CONFIG_MUX_SINGLE_2 = $6000;  // Single-ended AIN2
  ADS1015_REG_CONFIG_MUX_SINGLE_3 = $7000;  // Single-ended AIN3

  //    ADS1015_REG_CONFIG_PGA_MASK     = $0E00;
  ADS1015_REG_CONFIG_PGA_6_144V = $0000;  // +/-6.144V range = Gain 2/3
  ADS1015_REG_CONFIG_PGA_4_096V = $0200;  // +/-4.096V range = Gain 1
  ADS1015_REG_CONFIG_PGA_2_048V = $0400;  // +/-2.048V range = Gain 2 (default;
  ADS1015_REG_CONFIG_PGA_1_024V = $0600;  // +/-1.024V range = Gain 4
  ADS1015_REG_CONFIG_PGA_0_512V = $0800;  // +/-0.512V range = Gain 8
  ADS1015_REG_CONFIG_PGA_0_256V = $0A00;  // +/-0.256V range = Gain 16

  //   ADS1015_REG_CONFIG_MODE_MASK    = $0100;
  //   ADS1015_REG_CONFIG_MODE_CONTIN  = $0000;  // Continuous conversion mode
  ADS1015_REG_CONFIG_MODE_SINGLE = $0100;  // Power-down single-shot mode (default;

  ADS1015_REG_CONFIG_DR_MASK = $00E0;
  ADS1015_REG_CONFIG_DR_128SPS = $0000;  // 128 samples per second
  ADS1015_REG_CONFIG_DR_250SPS = $0020;  // 250 samples per second
  ADS1015_REG_CONFIG_DR_490SPS = $0040;  // 490 samples per second
  ADS1015_REG_CONFIG_DR_920SPS = $0060;  // 920 samples per second
  ADS1015_REG_CONFIG_DR_1600SPS = $0080;  // 1600 samples per second (default;
  ADS1015_REG_CONFIG_DR_2400SPS = $00A0;  // 2400 samples per second
  ADS1015_REG_CONFIG_DR_3300SPS = $00C0;  // 3300 samples per second

  //   ADS1015_REG_CONFIG_CMODE_MASK   = $0010;
  //    ADS1015_REG_CONFIG_CMODE_TRAD   = $0000;  // Traditional comparator with hysteresis (default;
  ADS1015_REG_CONFIG_CMODE_WINDOW = $0010;  // Window comparator

  //  ADS1015_REG_CONFIG_CPOL_MASK    = $0008;
  //   ADS1015_REG_CONFIG_CPOL_ACTVLOW = $0000;  // ALERT/RDY pin is low when active (default;
  ADS1015_REG_CONFIG_CPOL_ACTVHI = $0008;  // ALERT/RDY pin is high when active

  //    ADS1015_REG_CONFIG_CLAT_MASK    = $0004;  // Determines if ALERT/RDY pin latches once asserted
  //    ADS1015_REG_CONFIG_CLAT_NONLAT  = $0000;  // Non-latching comparator (default;
  ADS1015_REG_CONFIG_CLAT_LATCH = $0004;  // Latching comparator

  //  ADS1015_REG_CONFIG_CQUE_MASK    = $0003;
  //   ADS1015_REG_CONFIG_CQUE_1CONV   = $0000;  // Assert ALERT/RDY after one conversions
  ADS1015_REG_CONFIG_CQUE_2CONV = $0001;  // Assert ALERT/RDY after two conversions
  ADS1015_REG_CONFIG_CQUE_4CONV = $0002;  // Assert ALERT/RDY after four conversions
  ADS1015_REG_CONFIG_CQUE_NONE = $0003;  // Disable the comparator

type

  { TADS1015 }
   {
    GAIN_TWOTHIRDS    = ADS1015_REG_CONFIG_PGA_6_144V,
  GAIN_ONE          = ADS1015_REG_CONFIG_PGA_4_096V,
  GAIN_TWO          = ADS1015_REG_CONFIG_PGA_2_048V,
  GAIN_FOUR         = ADS1015_REG_CONFIG_PGA_1_024V,
  GAIN_EIGHT        = ADS1015_REG_CONFIG_PGA_0_512V,
  GAIN_SIXTEEN      = ADS1015_REG_CONFIG_PGA_0_256V
  }

  TADSgain = (
    GAIN_TWOTHIRDS = ADS1015_REG_CONFIG_PGA_6_144V,
    GAIN_ONE = ADS1015_REG_CONFIG_PGA_4_096V,
    GAIN_TWO = ADS1015_REG_CONFIG_PGA_2_048V,
    GAIN_FOUR = ADS1015_REG_CONFIG_PGA_1_024V,
    GAIN_EIGHT = ADS1015_REG_CONFIG_PGA_0_512V,
    GAIN_SIXTEEN = ADS1015_REG_CONFIG_PGA_0_256V
    );

  TADSmuxmode = (
    mux0_1 = ADS1015_REG_CONFIG_MUX_DIFF_0_1,  // Differential P = AIN0, N = AIN1 (default;
    mux0_3 = ADS1015_REG_CONFIG_MUX_DIFF_0_3,  // Differential P = AIN0, N = AIN3
    mux1_3 = ADS1015_REG_CONFIG_MUX_DIFF_1_3,  // Differential P = AIN1, N = AIN3
    mux2_3 = ADS1015_REG_CONFIG_MUX_DIFF_2_3   // Differential P = AIN2, N = AIN3
    );

  TADSsamplepersecond = (
    sps_128 = ADS1015_REG_CONFIG_DR_128SPS,    // 128 samples per second
    sps_250 = ADS1015_REG_CONFIG_DR_250SPS,  // 250 samples per second
    sps_490 = ADS1015_REG_CONFIG_DR_490SPS,   // 490 samples per second
    sps_920 = ADS1015_REG_CONFIG_DR_920SPS,  // 920 samples per second
    sps_1600 = ADS1015_REG_CONFIG_DR_1600SPS,    // 1600 samples per second (default;
    sps_2400 = ADS1015_REG_CONFIG_DR_2400SPS,    // 2400 samples per second
    sps_3300 = ADS1015_REG_CONFIG_DR_3300SPS    // 3300 samples per second
    );

  TADS1015 = class(TIc2Base)

  private
    FbitShift: smallint;
    FconversionDelay: integer;
    Fgain: tADSgain;
    Fsamplepersecond: TADSsamplepersecond;

    function ADSstartComparator(channel: Byte; Highthreshold: Byte): integer;
    procedure SetbitShift(const AValue: smallint);
    procedure SetconversionDelay(const AValue: integer);
    procedure Setgain(const AValue: tADSgain);
    procedure Setsamplepersecond(const AValue: TADSsamplepersecond);

  public

    constructor Create(); override;
    constructor Create(aADS1015_ADDRESS: Byte); override;
    property conversionDelay: integer read FconversionDelay write SetconversionDelay;
    property bitShift: smallint read FbitShift write SetbitShift;
    property samplepersecond: TADSsamplepersecond
      read Fsamplepersecond write Setsamplepersecond;
    property gain: tADSgain read Fgain write Setgain;
    function ADSread_SingleEnded(channel: Byte): word;
    function Getconfig(): word;
    function configToStr(): string;
    function getLastConversionResults(): word;
    function  ADSreadDifferential(muxmode: TADSmuxmode): integer;
  end;

implementation



{ TADS1015 }

procedure TADS1015.Setgain(const AValue: tADSgain);
begin
  if Fgain = AValue then
    exit;
  Fgain := AValue;
end;

procedure TADS1015.Setsamplepersecond(const AValue: TADSsamplepersecond);
begin
  if Fsamplepersecond = AValue then
    exit;
  Fsamplepersecond := AValue;
end;

constructor TADS1015.Create();
begin
  Create(ADS1015_ADDRESS);
end;

constructor TADS1015.Create(aADS1015_ADDRESS: Byte);
begin

  inherited Create(aADS1015_ADDRESS);
  Fgain := GAIN_ONE;
  FconversionDelay := 5;
  FbitShift := 4;
  Fsamplepersecond := sps_2400;

end;



function TADS1015.ADSread_SingleEnded(channel: Byte): word;
var
  config, rawdata: word;
begin

  connect;

  if (channel > 4) or (channel < 0) then
  begin
    Result := 0;
    Exit;
  end;
  config := 0;

  config := ADS1015_REG_CONFIG_CQUE_NONE or
    // Disable the comparator (default val)
    //                    ADS1015_REG_CONFIG_CLAT_NONLAT  or // Non-latching (default val)
    //                    ADS1015_REG_CONFIG_CPOL_ACTVLOW or // Alert/Rdy active low   (default val)
    //                    ADS1015_REG_CONFIG_CMODE_TRAD   or // Traditional comparator (default val)

    ADS1015_REG_CONFIG_MODE_SINGLE;   // Single-shot mode (

  if Ord(Fsamplepersecond) > 0 then
    config := config or (Ord(Fsamplepersecond));

  if Ord(Fgain) > 0 then
    config := config or (Ord(Fgain));
  ;
  case channel of

    0:
      config := config or ADS1015_REG_CONFIG_MUX_SINGLE_0;

    1:
      config := config or ADS1015_REG_CONFIG_MUX_SINGLE_1;

    2:
      config := config or ADS1015_REG_CONFIG_MUX_SINGLE_2;

    3:
      config := config or ADS1015_REG_CONFIG_MUX_SINGLE_3;

  end;

  // Set 'start single-conversion' bit
  config := config or ADS1015_REG_CONFIG_OS_SINGLE;



  I2C_Write16(hdev, ADS1015_REG_POINTER_CONFIG, config);
  Result := getLastConversionResults;
         {
         sleep(FconversionDelay  );

         rawdata  := I2C_Read16 (hdev, ADS1015_REG_POINTER_CONVERT);

         result := rawdata shr 4 ;
// result := config ;

          }

end;



{/**************************************************************************/
/*!
    @brief  Sets up the comparator to operate in basic mode, causing the
            ALERT/RDY pin to assert (go from high to low) when the ADC
            value exceeds the specified threshold.

            This will also set the ADC in continuous conversion mode.
*/
/**************************************************************************/ }

function TADS1015.ADSstartComparator(channel: Byte;
  Highthreshold   : Byte  ) : integer ;
 var
  config, rawdata: word;

begin
   connect;

  if (channel > 4) or (channel < 0) then
  begin
    Result := 0;
    Exit;
  end;
  config := 0;

  config :=  ADS1015_REG_CONFIG_CLAT_LATCH;

          // ADS1015_REG_CONFIG_CQUE_1CONV   or // Comparator enabled and asserts on 1 match
                 //   ADS1015_REG_CONFIG_CLAT_LATCH   or // Latching mode
                 //   ADS1015_REG_CONFIG_CPOL_ACTVLOW | // Alert/Rdy active low   (default val)
                 //   ADS1015_REG_CONFIG_CMODE_TRAD   | // Traditional comparator (default val)
                 //   ADS1015_REG_CONFIG_DR_1600SPS   | // 1600 samples per second (default)
                 //   ADS1015_REG_CONFIG_MODE_CONTIN ; // Continuous conversion mode
                 ;



  if Ord(Fsamplepersecond) > 0 then
    config := config or (Ord(Fsamplepersecond));

  if Ord(Fgain) > 0 then
    config := config or (Ord(Fgain));
  ;
  case channel of

    0:
      config := config or ADS1015_REG_CONFIG_MUX_SINGLE_0;

    1:
      config := config or ADS1015_REG_CONFIG_MUX_SINGLE_1;

    2:
      config := config or ADS1015_REG_CONFIG_MUX_SINGLE_2;

    3:
      config := config or ADS1015_REG_CONFIG_MUX_SINGLE_3;

  end;



   I2C_Write16(hdev, ADS1015_REG_POINTER_HITHRESH, (Highthreshold shl FbitShift)  );

  I2C_Write16(hdev, ADS1015_REG_POINTER_CONFIG, config);

 // Result := getLastConversionResults;
end;



 {/**************************************************************************/
/*!
    @brief  Reads the conversion results, measuring the voltage
            difference between the P (AIN2) and N (AIN3) input.  Generates
            a signed value since the difference can be either
            positive or negative.
*/
/**************************************************************************/ }
function TADS1015.ADSreadDifferential(muxmode: TADSmuxmode): integer;
var

  config, rawdata: word;
begin
  // Start with default values

  connect;


  config := 0;

  config := ADS1015_REG_CONFIG_CQUE_NONE or
    // Disable the comparator (default val)
    //                    ADS1015_REG_CONFIG_CLAT_NONLAT  or // Non-latching (default val)
    //                    ADS1015_REG_CONFIG_CPOL_ACTVLOW or // Alert/Rdy active low   (default val)
    //                    ADS1015_REG_CONFIG_CMODE_TRAD   or // Traditional comparator (default val)


    ADS1015_REG_CONFIG_MODE_SINGLE;   // Single-shot mode (
  if Ord(Fsamplepersecond) > 0 then
    config := config or (Ord(Fsamplepersecond));

  if Ord(Fgain) > 0 then
    config := config or (Ord(Fgain));

  if Ord(muxmode) > 0 then
    config := config or (Ord(muxmode));

  //   ADS1015_REG_CONFIG_MUX_DIFF_0_1 = $0000;  // Differential P = AIN0, N = AIN1 (default;
  //  ADS1015_REG_CONFIG_MUX_DIFF_0_3 = $1000;  // Differential P = AIN0, N = AIN3
  //  ADS1015_REG_CONFIG_MUX_DIFF_1_3 = $2000;  // Differential P = AIN1, N = AIN3
  //  ADS1015_REG_CONFIG_MUX_DIFF_2_3 = $3000;  // Differential P = AIN2, N = AIN3


  // Set 'start single-conversion' bit
  config := config or ADS1015_REG_CONFIG_OS_SINGLE;


  // Write config register to the ADC
  I2C_Write16(hdev, ADS1015_REG_POINTER_CONFIG, config);
  Result := getLastConversionResults;

end;


{/**************************************************************************/
/*!
    @brief  In order to clear the comparator, we need to read the
            conversion results.  This function reads the last conversion
            results without changing the config value.
*/
/**************************************************************************/ }
function TADS1015.getLastConversionResults(): word;
var
  rawdata: word;
begin
  // Wait for the conversion to complete
  sleep(FconversionDelay);
  rawdata := I2C_Read16(hdev, ADS1015_REG_POINTER_CONVERT);
  if FbitShift <> 0 then
    Result := rawdata shr FbitShift
  else
    Result := rawdata;

end;

procedure TADS1015.SetbitShift(const AValue: smallint);
begin
  if FbitShift = AValue then
    exit;
  FbitShift := AValue;
end;

procedure TADS1015.SetconversionDelay(const AValue: integer);
begin
  if FconversionDelay = AValue then
    exit;
  FconversionDelay := AValue;
end;

function TADS1015.Getconfig(): word;
begin
  connect;
  Result := I2C_Read16(hdev, ADS1015_REG_POINTER_CONFIG);
end;

function TADS1015.configToStr(): string;
var
  i: integer;
  tmpconfig: word;
begin

  tmpconfig := Getconfig;

  Result := '';
  for i := 15 downto 0 do
  begin
    if tmpconfig and (1 shl i) <> 0 then
    begin
      Result := Result + '1';
    end
    else
    begin
      Result := Result + '0';
    end;
    case i of
      0: Result := Result + '<Comparator queue and disable ' + #13#10;
      2: Result := Result + '<Latching comparato ' + #13#10;
      3: Result := Result + '<Comparator polarity ' + #13#10;
      4: Result := Result + '<Comparator mode ' + #13#10;
      5: Result := Result + '<Data rate ' + #13#10;
      8: Result := Result + '<Device operating mode ' + #13#10;
      9: Result := Result + '< gain   ' + #13#10;
      12: Result := Result + '<multiplexer configuration ' + #13#10;
      15: Result := Result + '<os ' + #13#10;

    end;
  end;

end;

end.

