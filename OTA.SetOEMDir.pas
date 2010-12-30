unit OTA.SetOEMDir;

interface

uses
  OTA.IDE, ToolsAPI;

type
  TSetOEMDir = class(TNotifierObject, IOTAIDENotifier, IOTAIDENotifier50)
  private
    function GetOEMDir(const aFileName: string; out aOEMDir: string): Boolean;
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses Windows, Classes, FileCtrl, SysUtils;

resourcestring
  StrFactoryDir            = 'FactoryDir';
  StrOEMDir                = 'OEMDir';

procedure TSetOEMDir.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TSetOEMDir.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin

end;

procedure TSetOEMDir.AfterConstruction;
var s: string;
begin
  inherited;
  s := GetEnvironmentVariable(StrFactoryDir);
  if (s = '') or not DirectoryExists(s) then begin
    if SelectDirectory(Format('Choose Factory Folder (%s)', [StrFactoryDir]), '', s) then
      TOTAUtil.SetVariable(StrFactoryDir, s);
  end;
end;

procedure TSetOEMDir.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
var sOEMDir, sEnvOEMDir: string;
    M: IOTAModuleServices;
begin
  if IsCodeInsight then Exit;
  
  M := BorlandIDEServices as IOTAModuleServices;
  if GetOEMDir(M.MainProjectGroup.FileName, sOEMDir) then begin
    sEnvOEMDir := GetEnvironmentVariable(StrOEMDir);
    if sEnvOEMDir <> sOEMDir then
      TOTAUtil.SetVariable(StrOEMDir, sOEMDir);
  end;
end;

procedure TSetOEMDir.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure TSetOEMDir.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var sExt: string;
    sGroup, sProj: string;
    sOEMDir: string;
begin
  if NotifyCode = ofnFileOpening then begin
    sExt := ExtractFileExt(FileName);
    sGroup := {$if CompilerVersion<=18} '.bdsgroup' {$else} '.groupproj' {$ifend};
    sProj := {$if CompilerVersion<=18} '.bdsproj' {$else} '.dproj' {$ifend};
    if SameText(sExt, sGroup) or SameText(sExt, sProj) then begin
      if GetOEMDir(FileName, sOEMDir) then
        TOTAUtil.SetVariable(StrOEMDir, sOEMDir);
    end;
  end;
  Cancel := False;
end;

function TSetOEMDir.GetOEMDir(const aFileName: string; out aOEMDir: string):
    Boolean;
var N: TStringList;
    sIniFile, sOEM, sBranch: string;
begin
  Result := False;
  if TOTAUtil.GetSetupIni(aFileName, sIniFile) then begin
    N := TStringList.Create;
    try
      N.CaseSensitive := False;
      N.LoadFromFile(sIniFile);

      sOEM := N.Values['OEM'];
      if sOEM = '' then sOEM := 'developer';

      sBranch := N.Values['Branch'];
      if sBranch <> '' then
        sBranch := '.' + sBranch;

      aOEMDir := Format('%s\%s\oem%s\%s', [GetEnvironmentVariable(StrFactoryDir), N.Values['name'], sBranch, sOEM]);
      Result := True;
    finally
      N.Free;
    end;
  end;
end;

initialization
  TOTAFactory.Register(TNotifierOTA_Services.Create(TSetOEMDir));
end.
