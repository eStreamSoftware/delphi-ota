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
  OTA.BuildAllFromHere, OTA.FormatUses, OTA.IDE, OTA.SearchProject,
  OTA.SetActiveProjectModule;

initialization
  TOTAFactory.RegisterProc(TNotifier_Services.Create(TSetActiveProjectModule.Create as TFunc<IOTAIDENotifier>));

  TOTAFactory.RegisterProc(TNotifier_ProjectManager.Create(TSearchProject.Create as TFunc<IOTAProjectMenuItemCreatorNotifier>));
  TOTAFactory.RegisterProc(TNotifier_KeyboardServices.Create(TSearchProject.Create as TFunc<IOTAKeyboardBinding>));

  TOTAFactory.RegisterProc(TNotifier_KeyboardServices.Create(TBuildAllFromHere.Create as TFunc<IOTAKeyboardBinding>));

  TOTAFactory.RegisterProc(TNotifier_ProjectManager.Create(TFormatUses.Create as TFunc<IOTAProjectMenuItemCreatorNotifier>));
  TOTAFactory.RegisterProc(TNotifier_KeyboardServices.Create(TFormatUses.Create as TFunc<IOTAKeyboardBinding>));

  TOTAFactory.RegisterProc(TFormatUses.Create as TProc);
end.
