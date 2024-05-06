unit OTA.Register;

interface

(*
This is the list of IDE version numbers that are being used in various OTAPI interfaces:

60 = Delphi 7
80 = Delphi 8
90 = Delphi 2005
100 = Delphi 2006
110 = Delphi 2007
120 = Delphi 2009
140 = Delphi 2010
145 = Delphi XE

*)

implementation

uses
  System.SysUtils, ToolsAPI,
  OTA.FormatUses, OTA.IDE, OTA.SearchProject, OTA.SetActiveProjectModule,
  OTA.BuildAllFromHere;

initialization
  TOTAFactory
    .Register(TNotifierOTA_Services.Create(TSetActiveProjectModule))
    .Register(TNotifierOTA_EditorServices.Create(TEditor_FormatUses))
    ;

  TOTAFactory.RegisterProc(TNotifier_ProjectManager.Create(TSearchProject.Create as TFunc<IOTAProjectMenuItemCreatorNotifier>));
  TOTAFactory.RegisterProc(TNotifier_KeyboardServices.Create(TSearchProject.Create as TFunc<IOTAKeyboardBinding>));
  TOTAFactory.RegisterProc(TNotifier_KeyboardServices.Create(TBuildAllFromHere.Create as TFunc<IOTAKeyboardBinding>));
end.
