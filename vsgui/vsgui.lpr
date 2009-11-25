program vsgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, main, cli_caller, parameditor, com_setup, fileselector;

{$IFDEF WINDOWS}{$R vsgui.rc}{$ENDIF}

begin
  {$I vsgui.lrs}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormComSetup, FormComSetup);
  Application.CreateForm(TFormParaEditor, FormParaEditor);
  Application.CreateForm(TFormFileSelector, FormFileSelector);
  Application.Run;
end.

