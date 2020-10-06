
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
  WriteLn(LFile, FormatDateTime('hh:nn:ss:zzz', Time) + ' ' + ALine);
  Close(LFile);
end;

type
  TListener = class(TThread)
  private
    FMessage: TStringList;
    FWaitAfterRead: cardinal;
    procedure SendMessage;
  protected
    procedure Execute; override;
  public
    constructor Create(const ACreateSuspended: boolean; const AWaitAfterRead: cardinal);
    destructor Destroy; override;
  end;

constructor TListener.Create(const ACreateSuspended: boolean; const AWaitAfterRead: cardinal);
begin
  inherited Create(ACreateSuspended);
  FreeOnTerminate := FALSE;
  FMessage := TStringList.Create;
  FWaitAfterRead := AWaitAfterRead;
end;

destructor TListener.Destroy;
begin
  FMessage.Free;
  inherited Destroy;
end;

procedure TListener.Execute;
begin
  while not Terminated do
  begin
    FMessage.Text := ReadProcessOutput;
    if FMessage.Count > 0 then
      SendMessage;
    Sleep(FWaitAfterRead);
  end;
end;

procedure TListener.SendMessage;
var
  i: integer;
begin
  Write(FMessage.Text);
  Flush(output);
  for i := 0 to Pred(FMessage.Count) do
    LogLn('<< ' + FMessage[i]);
end;

const
  (* Default settings *)
  CProcessName    = 'NEG';
  CWaitAfterRead  = 50;
  CWaitAfterWrite = 50;
  (* INI file sections and keys *)
  CSectionProcess    = 'process';
  CSectionTime       = 'time';
  CKeyProcessName    = 'name';
  CKeyWaitAfterRead  = 'afterRead';
  CKeyWaitAfterWrite = 'afterWrite';

var
  LIniFileName,
  LProcessName: string;
  LListener: TThread;
  LInput: string;
  LWaitAfterRead,
  LWaitAfterWrite: cardinal;
  
begin
  LogLn('** ' + CAppInfo, TRUE);
  
  LIniFileName := ChangeFileExt(ParamStr(0), '.ini');
  
  (* Try to read executable name from INI file. *)
  with TIniFile.Create(LIniFileName) do
  try
    LProcessName := ReadString(CSectionProcess, CKeyProcessName, CProcessName);
    LWaitAfterRead := ReadInteger(CSectionTime, CKeyWaitAfterRead, CWaitAfterRead);
    LWaitAfterWrite := ReadInteger(CSectionTime, CKeyWaitAfterWrite, CWaitAfterWrite);
  finally
    Free;
  end;
  
  (* Try to read executable name from command line. *)
  if ParamCount = 1 then
    LProcessName := ParamStr(1);
  
  if not FileExists(LProcessName) then
  begin
    WriteLn(StdErr, 'Cannot find ' + LProcessName);
    Exit;
  end;
  
  (* Create INI file if it doesn't exist. *)
  if not FileExists(LIniFileName) then
    with TIniFile.Create(LIniFileName) do
    try
      WriteString(CSectionProcess, CKeyProcessName, CProcessName);
      WriteInteger(CSectionTime, CKeyWaitAfterRead, CWaitAfterRead);
      WriteInteger(CSectionTime, CKeyWaitAfterWrite, CWaitAfterWrite);
      UpdateFile;
    finally
      Free;
    end;
  
  LogLn('** Executable: ' + LProcessName);
  
  if CreateConnectedProcess(LProcessName) then
  begin
    LListener := TListener.Create(TRUE, LWaitAfterRead);
    LListener.Priority := tpNormal;
    LListener.Start;
    
    while ProcessRunning and not Eof do
    begin
      ReadLn(LInput);
      LogLn('>> ' + LInput);
      WriteProcessInput(LInput);
      Sleep(LWaitAfterWrite);
    end;
    
    LListener.Terminate;
    LListener.WaitFor;
    LListener.Free;
    
    FreeConnectedProcess;
  end else
    LogLn('** Cannot create process');
end.
