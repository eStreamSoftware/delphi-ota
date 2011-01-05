unit OTA.CGRC;

interface

uses
  Classes, ToolsAPI;

type
  (*
  This Delphi IDE notifier is used by Delphi 2007 only.  Delphi 2007 IDE use
  brcc32 to compile rc files.  There is a restriction on naming resource entry in
  rc file with brcc32: Dot (.) is not allowed in resource name.

  In Delphi 2009 and above, we may specify Resource Compiler in
  Project | Options | Resource Compiler | Resource compiler to use.
  The "Windows SDK Resource Compiler" (CGRC.EXE -> RC.EXE) may compile rc file
  contain dot (.) in resource name.

  This class attempt to use cgrc.exe compile the rc files before brcc32 does and
  it will prohibit further brcc32 compilation during project build phase.

  Reference:

  1. http://stackoverflow.com/questions/4545119/can-delphi-2007-ide-use-delphi-2010s-cgrc-exe-while-compile-rc-files
  2. http://stackoverflow.com/questions/4538131/include-file-behave-differently-using-rc-exe-or-brcc32-exe-to-build-rc-files

  *)
  TResourceCompiler_CGRC = class(TNotifierObject, IOTAIDENotifier, IOTAIDENotifier50, IOTAIDENotifier80)
  {$if CompilerVersion > 18.5}{$Message Fatal 'This unit is used by Delphi 2007 only'}{$ifend}
  private
    FWasModified: boolean;
    FRCFiles: TStringList;
    procedure Log(aText: string);
  protected
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure AfterCompile(const Project: IOTAProject; Succeeded:
      Boolean; IsCodeInsight: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses SysUtils, Console;

procedure TResourceCompiler_CGRC.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TResourceCompiler_CGRC.AfterCompile(Succeeded,
  IsCodeInsight: Boolean);
begin

end;

procedure TResourceCompiler_CGRC.AfterCompile(const Project: IOTAProject; Succeeded:
      Boolean; IsCodeInsight: Boolean);
var F: string;
    i: integer;
    M: IOTAModuleInfo;
begin
  if IsCodeInsight then Exit;
  if FRCFiles.Count = 0 then Exit;  

  // Add rc files back to project
  for F in FRCFiles do
    Project.AddFile(F, False);

  FRCFiles.Clear;

  if not FWasModified then begin
    FWasModified := False;

    // Check if files in project was modified
    for i := 0 to Project.GetModuleCount - 1 do begin
      M := Project.GetModule(i);
      if not FileExists(M.FileName) then Continue;
      if SameText(ExtractFileExt(M.FileName), '.res') then Continue;

      if M.OpenModule.CurrentEditor.Modified then begin
        FWasModified := True;
        Break;
      end;
    end;

    // Save the project if project never modified previously
    if not FWasModified then
      Project.Save(False, True);
  end;
end;

procedure TResourceCompiler_CGRC.AfterConstruction;
begin
  inherited;
  FRCFiles := TStringList.Create;
end;

procedure TResourceCompiler_CGRC.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure TResourceCompiler_CGRC.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean);
var sRC, sRES: string;
    i: integer;
    L: TConsoleRedirector;
begin
  if IsCodeInsight then Exit;

  FWasModified := Project.CurrentEditor.Modified;

  FRCFiles.Clear;
  for i := Project.GetModuleCount - 1 downto 0 do begin
    sRC := Project.GetModule(i).FileName;

    if SameText(ExtractFileExt(sRC), '.rc') then begin
      // These 2 rc files contain version info.
      // It should skip to avoid crashing with Delphi default .res file when compiling
      if Pos('library.rc', sRC) > 0 then Continue;
      if Pos('app.rc', sRC) > 0 then Continue;

      // Generate output file name to keep the .res file in same folder as project folder
      sRES := ExtractFileName(ChangeFileExt(sRC, '.res'));

      // Manually use cgrc.exe (Resource Compiler Binder utility) to generate resource file
      L := TConsoleRedirector.Create('', Format('cgrc.exe -v -fo%s %s', [sRES, sRC]));
      try
        L.Execute;
        while not L.EOF do
          L.GetNextLine;
      finally
        L.Free;
      end;

      // Add rc file to a list.  The rc file shoudl add back to project after compile
      FRCFiles.Add(sRC);

      // Remove rc file from project to make sure it won't compiled by brcc32
      Project.RemoveFile(sRC);
    end;
  end;
end;

procedure TResourceCompiler_CGRC.BeforeDestruction;
begin
  inherited;
  FRCFiles.Free;
end;

procedure TResourceCompiler_CGRC.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin

end;

procedure TResourceCompiler_CGRC.Log(aText: string);
var G: IOTAMessageGroup;
    V: IOTAMessageServices;
begin
  V := BorlandIDEServices as IOTAMessageServices;
  G := V.AddMessageGroup('OTA.CGRC');
  V.AddTitleMessage(aText, G);
end;

end.
