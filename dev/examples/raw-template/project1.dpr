program project1; {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

uses
  pwinit, pwmain;

var
  MyMacroVar: string;
  
begin
  MyMacroVar:= '<b>really, really</b> splendid and <i>nice</i> ';
  SetVar('MacroVar', MyMacroVar);
  { TemplateRaw is less secure than TemplateOut and allows special characters }
  TemplateRaw('template.htm');
end.
