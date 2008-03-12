Unit ThreadPool;

Interface

Uses
  {$IFDEF UNIX}
  CThreads,
  {$ENDIF}
  Classes,
  Sysutils,
  DateUtils;

Type
  TThreadPoolEntry = Record
     ThreadID  : LongWord;
     Timeout   : TDateTime;
     TheThread : TThread;
  End;

  TThreadPool = Class
  Private
    fThread : Array Of TThreadPoolEntry;
  Public
    Destructor Destroy; Override;
    Procedure AddThread(ID : LongWord; TT : TThread; Timeout : Int64);
    Procedure DelThread(ID : LongWord);
    Procedure CheckTimeout;
  End;

Implementation

Destructor TThreadPool.Destroy;
Var
  Ctrl : LongWord;
Begin
  // Debug WriteLn('Destroying Thread pool. ', Length(fThread), ' threads running...');
  If Length(fThread) > 0 Then
    For Ctrl := Low(fThread) To High(fThread) Do
      If Assigned(fThread[Ctrl].TheThread) Then
      Begin
        fThread[Ctrl].TheThread.Suspend;
        fThread[Ctrl].TheThread.Free;
      End;
  // Debug WriteLn('Threads freed.');
  Inherited Destroy;
End;

Procedure TThreadPool.AddThread(ID : LongWord; TT : TThread; Timeout : Int64);
Begin
  SetLength(fThread, Length(fThread) + 1);
  fThread[High(fThread)].ThreadID  := ID;
  fThread[High(fThread)].Timeout   := IncMillisecond(Now, Timeout);
  fThread[High(fThread)].TheThread := TT;
  fThread[High(fThread)].TheThread.Resume;
End;

Procedure TThreadPool.DelThread(ID : LongWord);
Var
  Ctrl : LongWord;
Begin
  If Length(fThread) > 0 Then
    For Ctrl := Low(fThread) To High(fThread) Do
      If fThread[Ctrl].ThreadID = ID Then
      Begin
        fThread[Ctrl] := fThread[High(fThread)];
        SetLength(fThread, Length(fThread) - 1);
      End;
End;

Procedure TThreadPool.CheckTimeout;
Var
  Ctrl : LongWord;
Begin
  If Length(fThread) > 0 Then
    For Ctrl := Low(fThread) To High(fThread) Do
      If fThread[Ctrl].Timeout < Now Then
      Begin
        // Debug WriteLn('Thread number ', fThread[Ctrl].ThreadID, ' timed out.');
        fThread[Ctrl].TheThread.Suspend;
        fThread[Ctrl].TheThread.Free;
        If Length(fThread) > 1 Then
        Begin
          fThread[Ctrl].ThreadID  := fThread[High(fThread)].ThreadID;
          fThread[Ctrl].TheThread := fThread[High(fThread)].TheThread;
          fThread[Ctrl].Timeout   := fThread[High(fThread)].Timeout;
        End;
        SetLength(fThread, Length(fThread) - 1);
      End;
End;

End.
