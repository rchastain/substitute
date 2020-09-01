
unit Connect;

interface

function CreateConnectedProcess(const AProcessName: string; const ASecondProcess: boolean = FALSE): boolean;
procedure FreeConnectedProcess(const ASecondProcess: boolean = FALSE);
function ReadProcessOutput(const ASecondProcess: boolean = FALSE): string;
procedure WriteProcessInput(const AStr: string; const ASecondProcess: boolean = FALSE);
function ProcessRunning: boolean;

implementation

uses
  SysUtils, Classes, Process;

var
  LProcess: array[boolean] of TProcess;

function CreateConnectedProcess(const AProcessName: string; const ASecondProcess: boolean): boolean;
begin
  result := FALSE;
  LProcess[ASecondProcess] := TProcess.Create(nil);
  LProcess[ASecondProcess].Options := [poUsePipes, poStdErrToOutput, poNoConsole];
  LProcess[ASecondProcess].Executable := AProcessName;
  LProcess[ASecondProcess].Execute;
  result := TRUE;
end;

procedure FreeConnectedProcess(const ASecondProcess: boolean);
begin
  if LProcess[ASecondProcess].Running then
    LProcess[ASecondProcess].Terminate(0);
  LProcess[ASecondProcess].Free;
end;

function ReadProcessOutput(const ASecondProcess: boolean): string;
var
  LNoMoreOutput: boolean;

  procedure ReadOutput(const AProcess: TProcess);
  var
    LBuffer: string;
    LBytesAvailable: DWord;
    LBytesRead: LongInt;
  begin
    if AProcess.Running then
    begin
      LBytesAvailable := AProcess.Output.NumBytesAvailable;
      LBytesRead := 0;
      while LBytesAvailable > 0 do
      begin
        SetLength(LBuffer, LBytesAvailable);
        LBytesRead := AProcess.Output.Read(LBuffer[1], LBytesAvailable);
        result := result + Copy(LBuffer, 1, LBytesRead);
        LBytesAvailable := AProcess.Output.NumBytesAvailable;
        LNoMoreOutput := FALSE;
      end;
    end;
  end;
  
begin
  result := '';
  repeat
    LNoMoreOutput := TRUE;
    ReadOutput(LProcess[ASecondProcess]);
  until LNoMoreOutput;
end;

procedure WriteProcessInput(const AStr: string; const ASecondProcess: boolean);
var
  LStr: string;
begin
  if LProcess[ASecondProcess].Running then
  begin
    LStr := AStr + LineEnding;
    LProcess[ASecondProcess].Input.Write(LStr[1], Length(LStr));
  end;
end;

function ProcessRunning: boolean;
begin
  result := LProcess[FALSE].Running;
end;

end.
