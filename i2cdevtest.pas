program i2cdevtest;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp { you can add units after this },
  baseUnix,
  i2cdev_base,
  i2cdev_ADS1015,
  i2cdev_MCP4725,
  i2cdev_PCA9685;

type

  { ic2dev }

  ic2dev = class(TCustomApplication)
  private

    ADS: TADS1015;
    MCP: TMCP4725;
    PCA: TPCA9685;
    procedure servotest;

  protected
    procedure DoRun; override;
    procedure volt;
  public
  end;

  { ic2dev }
  procedure ic2dev.volt;
  var
    volt0, volt1, volt2, volt3: integer;
    diff0_3, diff0_1, diff1_3, diff2_3: integer;

    vdiff: extended;
  begin
    begin
      ADS.conversionDelay := 250;
      ADS.samplepersecond := sps_128;
      ads.gain := GAIN_TWOTHIRDS;

      volt0 := ADS.ADSread_SingleEnded(0);
      volt1 := ADS.ADSread_SingleEnded(1);
      volt2 := ADS.ADSread_SingleEnded(2);
      volt3 := ADS.ADSread_SingleEnded(3);

      diff0_3 := ADS.ADSreadDifferential(mux0_3);
      diff0_1 := ADS.ADSreadDifferential(mux0_1);
      diff1_3 := ADS.ADSreadDifferential(mux1_3);
      diff2_3 := ADS.ADSreadDifferential(mux2_3);
      case ads.gain of

        GAIN_TWOTHIRDS: vdiff := (6144 / 2048); //6_144
        GAIN_ONE: vdiff := (4096 / 2048);   // 4_096
      end;



      //     Ic2Write16(fh, ADS1015_REG_POINTER_CONFIG, 50083);
      //     volt1 := Ic2Read16(fh, ADS1015_REG_POINTER_CONVERT);
      writeln('A0=', volt0 * vdiff);
      writeln('A1=', volt1 * vdiff);
      writeln('A2=', volt2 * vdiff);
      writeln('A3=', volt3 * vdiff);

      writeln('diff0_3=', diff0_3);
      writeln('diff0_1=', diff0_1);
      writeln('diff1_3=', diff1_3);
      writeln('diff2_3=', diff2_3);

      writeln('_______________________');
    end;

  end;

  procedure ic2dev.servotest;

      const

    MINSERVO = 150;
    MAXSERVO =500;
    PWMFREQ = 60;
       var
      cnt : integer ;
  procedure submove( subPIN :  byte );
  begin

      pca.pins[subPIN].led_off :=MINSERVO;
        pca.pins[subPIN].led_on :=0;
        pca.pins[subPIN].updatepin;
             sleep(250);
              pca.pins[subPIN].led_on :=0;
                  pca.pins[subPIN].led_off :=MAXSERVO ;
                  pca.pins[subPIN].updatepin;
        sleep(250);

  end;

  begin

    pca.connect;




    writeln('MODE1=', PCA.getMODE1);
    writeln('MODE2=', PCA.getMODE2);
    writeln('ADD1=', PCA.getSUBADD1);
    writeln('ADD2=', PCA.getSUBADD2);
    writeln('DADD3=', PCA.getSUBADD3);
    writeln('LEADALLCALL=', PCA.getLEDALLCALLADD);
    writeln('FREQ=', PCA.getPWMFreqbit);




    //pca.setPin(0, 500, False);

    pca.reset();
    PCA.setPWMFreq(PWMFREQ);



    writeln('MODE1=', PCA.getMODE1);
    writeln('MODE2=', PCA.getMODE2);
    writeln('ADD1=', PCA.getSUBADD1);
    writeln('ADD2=', PCA.getSUBADD2);
    writeln('DADD3=', PCA.getSUBADD3);
    writeln('LEADALLCALL=', PCA.getLEDALLCALLADD);
    writeln('FREQ=', PCA.getPWMFreqbit);


   // while True do
    begin
      //move one then the the other
      submove(0);
      submove(1);
      sleep (500);
       pca.pins[0].led_off :=MINSERVO;
         pca.pins[1].led_off :=MINSERVO;
         // move at same time
        pca.pins[0].updatepin;
        pca.pins[1].updatepin;

           sleep(1000);

     {
      for cnt := 0 to 10 do
       begin

        pca.setPin(0, MINSERVO + (cnt * 40) , False);
             sleep(250);
       end;
         for cnt := 0 to 10 do
       begin

        pca.setPin(0, MAXSERVO - (cnt * 40) , False);
             sleep(100);
       end;

   //   I2C_Write8 (pca.hdev , 9,1);
      sleep(1000);
             pca.setPin(0, round((MAXSERVO - MINSERVO ) /2) + MINSERVO , False);
              sleep(1000);
       pca.setPin(0, MAXSERVO, False);
     //  I2C_Write8 (pca.hdev , 9,0);

          sleep(1000);
         }
    end;



  end;

  procedure ic2dev.DoRun;
  var

    ainput: string;
    cntt, cnt: longint;

  begin
    //   getconfig;
    try




      ADS := TADS1015.Create();
      MCP := TMCP4725.Create();
      PCA :=
        TPCA9685.Create();


    //   volt;
     // servotest;

      Terminate;


      while True do
      begin
        ReadLn(ainput);


        if (ainput) = 'd' then
        begin
          for cntt := 0 to 2000 do
          begin

            MCP.MCP4725_Write12bit(0);
            MCP.MCP4725_Write12bit(4095);

          end;

        end;

        if (ainput) = 's' then
        begin
          for cntt := 0 to 4 do
          begin
            for cnt := low(Swave9) to High(Swave9) do
              MCP.MCP4725_Write12bit(Swave9[cnt]);

          end;

        end;
        if (ainput) = 'q' then
          break;

        if (ainput) = 't' then
        begin
          for cntt := 0 to 4 do
          begin
            for cnt := 0 to 4095 do
              MCP.MCP4725_Write12bit(cnt);

            for cnt := 4095 downto 0 do
              MCP.MCP4725_Write12bit(cnt);

          end;
        end;

        if (ainput) = 'c' then
        begin
          writeln(ADS.configToStr);
          writeln(MCP.Getconfig);
        end;

        if (ainput) = 'volt' then
          volt;

          if (ainput) = 'servotest' then
          servotest;

      end;


    except
      writeln('Error initalizing i2c');
    end;

    ADS.Free;
    MCP.Free;



    Terminate;

  end;



var
  Application: ic2dev;

{$R *.res}

begin
  Application := ic2dev.Create(nil);
  Application.Title := 'ic2dev';
  Application.Run;
  Application.Free;
end.

