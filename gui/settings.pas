unit settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSettings = class
    procedure Defaults; virtual; abstract;
    function Compare(const Value :TSettings) :boolean; virtual; abstract;
    procedure Assign(const Value :TSettings); virtual; abstract;
  end;

  { TProcessingSettings }

  TProcessingSettings = class(TSettings)
  public
    ThreadsUsed :integer;
    StopOnError :boolean;
    procedure Defaults; override;
    function Compare(const Value :TSettings) :boolean; override;
    procedure Assign(const Value :TSettings); override;
  end;

  //TGUISettings = class(TSettings)
  //end;

implementation

{ TProcessingSettings }

procedure TProcessingSettings.Defaults;
begin
  ThreadsUsed := 0;
  StopOnError := True;
end;

function TProcessingSettings.Compare(const Value: TSettings): boolean;
var
  Source :TProcessingSettings;
begin
  Source := Value as TProcessingSettings;
  result := (ThreadsUsed = Source.ThreadsUsed)
    and (StopOnError = Source.StopOnError);
end;

procedure TProcessingSettings.Assign(const Value: TSettings);
var
  Source :TProcessingSettings;
begin
  Source := Value as TProcessingSettings;
  ThreadsUsed := Source.ThreadsUsed;
  StopOnError := Source.StopOnError;
end;

end.

