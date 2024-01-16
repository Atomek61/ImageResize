unit appsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Settings;

type

  { TProcessingSettings }

  TProcessingSettings = class(TSettings)
  public
    ThreadsUsed :TIntegerSetting;
    StopOnError :TBooleanSetting;
    constructor Create; override;
  end;

  { TDialogSettings }

  TDialogSettings = class(TSettings)
  public
    AutoSave :TBooleanSetting;
    WarnDirty :TBooleanSetting;
    constructor Create; override;
  end;

  //TGUISettings = class(TSettings)
  //end;

implementation

{ TDialogSettings }

constructor TDialogSettings.Create;
begin
  inherited Create;
  AutoSave := TBooleanSetting.Create(self, 'AutoSave');
  AutoSave.TextDefault := 'True';
  Add(AutoSave);
  WarnDirty := TBooleanSetting.Create(self, 'WarnDirty');
  Add(WarnDirty);
end;

{ TProcessingSettings }

constructor TProcessingSettings.Create;
begin
  inherited Create;
  ThreadsUsed := TIntegerSetting.Create(self, 'ThreadsUsed');
  ThreadsUsed.Min := 0;
  Add(ThreadsUsed);
  StopOnError := TBooleanSetting.Create(self, 'StopOnError');
  Add(StopOnError);
end;

end.

