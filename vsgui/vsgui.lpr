program vsgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, main, cli_caller{, hexeditor, checkeditor},
  LResources, com_setup;

{$IFDEF WINDOWS}{$R vsgui.rc}{$ENDIF}

begin
  {$I vsgui.lrs}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormComSetup, FormComSetup);
  Application.Run;
end.

