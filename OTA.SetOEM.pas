unit OTA.SetOEM;

interface

uses
  ToolsAPI;

type
  TSetOEM = class(TNotifierObject, IOTAIDENotifier)
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  System.SysUtils,
  OTA.IDE;

resourcestring
  StrOEM = 'OEM';

procedure TSetOEM.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TSetOEM.AfterConstruction;
var s: string;
begin
  inherited;
  s := GetEnvironmentVariable(StrOEM);
  if s = '' then
    TOTAUtil.SetVariable(StrOEM, 'developer');
end;

procedure TSetOEM.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure TSetOEM.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin

end;

end.
