
program Substitute;

{$IFDEF MSWINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

{$IFDEF UNIX}
{$DEFINE USECTHREADS}
{$ENDIF}

uses
{$IFDEF UNIX}
  CThreads,
  CWString,
{$ENDIF}
  Classes,
  SysUtils,
  IniFiles,
  Connect;

{$I version.inc}
    
procedure LogLn(const ALine: string; const AForceRewrite: boolean = FALSE);
var
  LFileName: string;
  LFile: text;
begin
  LFileName := ChangeFileExt(ParamStr(0), '.log');
  Assign(LFile, LFileName);
  if FileExists(LFileName) and not AForceRewrite then
    Append(LFile)
  else
    Rewrite(LFile);
  WriteLn(LFile, ALine);
  Close(LFile);
end;

type
  TListener = class(TThread)
  private
    FMessage: string;
    procedure OnEngineMessage;
  protected
    procedure Execute; override;
  end;

procedure TListener.Execute;
const
  CDelay = 50;
begin
  while not Terminated do
  begin
    FMessage := ReadProcessOutput;
    if FMessage <> '' then
      OnEngineMessage;
    Sleep(CDelay);
  end;
end;

procedure TListener.OnEngineMessage;
begin
  Write(FMessage);
  Flush(output);
  LogLn(Format('** Process message:' + LineEnding + '%s', [Trim(FMessage)]));
end;

var
  LIniName, LExeName: string;
  LListener: TThread;
  LInput: string;
  
begin
  LogLn(Format('** %s %s build %s %s Free Pascal %s', [CAppName, CAppVersion, {$I %DATE%}, {$I %TIME%}, {$I %FPCVERSION%}]), TRUE);
  LogLn('** ' + TimeToStr(Time));
  
  LIniName := ChangeFileExt(ParamStr(0), '.ini');
  LExeName := './Fruit-2-3-1-Linux';
  
  if FileExists(LIniName) then
    (* Try to read executable name from INI file. *)
    with TIniFile.Create(LIniName) do
    try
      LExeName := ReadString('settings', 'executable', '???');
    finally
      Free;
    end
  else
    (* Create INI file if it doesn't exist. *)
    with TIniFile.Create(LIniName) do
    try
      WriteString('settings', 'executable', LExeName);
      UpdateFile;
    finally
      Free;
    end;
  
  (* Try to read executable name from command line. *)
  if ParamCount = 1 then
    LExeName := ParamStr(1);
  
  if not FileExists(LExeName) then
  begin
    WriteLn(StdErr, 'Cannot find ' + LExeName);
    Exit;
  end;
  
  LogLn(Format('** Executable name: %s', [LExeName]));
  
  if CreateConnectedProcess(LExeName) then
  begin
    LListener := TListener.Create(TRUE);
    LListener.Priority := tpNormal;
    LListener.Start;
    
    while ProcessRunning and not Eof do
    begin
      ReadLn(LInput);
      LogLn(Format('** User message:' + LineEnding + '%s', [LInput]));
      WriteProcessInput(LInput);
      Sleep(50);
    end;
    
    LListener.Terminate;
    LListener.WaitFor;
    LListener.Free;
    
    FreeConnectedProcess;
  end;
end.
