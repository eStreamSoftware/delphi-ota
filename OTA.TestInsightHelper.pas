unit OTA.TestInsightHelper;

interface

uses
  System.SysUtils, ToolsAPI;

type
  TTestInsightHelper = class(TNotifierObject, IOTADebuggerNotifier,
      IOTADebuggerNotifier90, TFunc<IOTADebuggerNotifier>)
  protected
    procedure ProcessCreated(const Process: IOTAProcess);
    procedure ProcessDestroyed(const Process: IOTAProcess);
    procedure BreakpointAdded(const Breakpoint: IOTABreakpoint);
    procedure BreakpointDeleted(const Breakpoint: IOTABreakpoint);

    procedure BreakpointChanged(const Breakpoint: IOTABreakpoint);
    procedure CurrentProcessChanged(const Process: IOTAProcess);
    procedure ProcessStateChanged(const Process: IOTAProcess);
    function BeforeProgramLaunch(const Project: IOTAProject): Boolean;
    procedure ProcessMemoryChanged;
  protected
    function Invoke: IOTADebuggerNotifier;
  end;

implementation

uses
  System.Classes, System.IOUtils, System.Variants,
  OTA.IDE;

resourcestring
  StrTestInsightApp        = 'TestInsightApp';
  StrTestInsightAppParams  = 'TestInsightAppParams';

function TTestInsightHelper.BeforeProgramLaunch(const Project: IOTAProject): Boolean;
begin
  var sTargetName := Project.ProjectOptions.TargetName;
  var sTestInsightApp := TPath.Combine(TPath.GetDirectoryName(sTargetName), 'TestInsight.exe');
  var sTestInsightAppParams := TPath.GetFileName(sTargetName);

  TOTAUtil.SetVariable(StrTestInsightApp, sTestInsightApp);
  TOTAUtil.SetVariable(StrTestInsightAppParams, sTestInsightAppParams);

  Result := True;
end;

procedure TTestInsightHelper.BreakpointAdded(const Breakpoint: IOTABreakpoint);
begin

end;

procedure TTestInsightHelper.BreakpointChanged(const Breakpoint: IOTABreakpoint);
begin

end;

procedure TTestInsightHelper.BreakpointDeleted(const Breakpoint: IOTABreakpoint);
begin

end;

procedure TTestInsightHelper.CurrentProcessChanged(const Process: IOTAProcess);
begin

end;

function TTestInsightHelper.Invoke: IOTADebuggerNotifier;
begin
  Result := Self as IOTADebuggerNotifier;
end;

procedure TTestInsightHelper.ProcessCreated(const Process: IOTAProcess);
begin

end;

procedure TTestInsightHelper.ProcessDestroyed(const Process: IOTAProcess);
begin

end;

procedure TTestInsightHelper.ProcessMemoryChanged;
begin

end;

procedure TTestInsightHelper.ProcessStateChanged(const Process: IOTAProcess);
begin

end;

end.
