unit FormConvExtractDataMain;

{$mode objfpc}
{$H+}

interface

uses
	Classes, SyncObjs, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
	ComCtrls, StdCtrls, ActnList;

type
	TCEDThreadStatus = (cesInitialising, cesReady, cesProcessing, cesDone,
    		cesError);
    TCEDThreadOperation = (ceoIdle, ceoPreparing, ceoConverting);

    TCEDThreadState = record
        lock: TCriticalSection;
    	status: TCEDThreadStatus;
        operation: TCEDThreadOperation;
        progress: Integer;
        progMax: Integer;
    	procFile: string;
	end;

const
	ARR_LIT_CEDSTAT: array[TCEDThreadStatus] of string = (
    		'Initialising...', 'Ready.', 'Processing...', 'Complete.',
            'Error.');
	ARR_LIT_CEDOPER: array[TCEDThreadOperation] of string = (
    		'', 'Preparing...', 'Converting...');

type
{ TCEDThread }

 TCEDThread = class(TThread)
    protected
    	FFiles: TStringList;

        FInputFile,
        FOutputFile: TFileStream;

        FProcess: TSimpleEvent;
        FPath: string;

        procedure Execute; override;

    public
        State: TCEDThreadState;

    	constructor Create;
        destructor  Destroy; override;

        procedure Process(const APath: string);
    end;

{ TConvExtractDataMainForm }
	TConvExtractDataMainForm = class(TForm)
		actSelectPath: TAction;
		actProcessExecute: TAction;
		ActionList1: TActionList;
		ApplicationProperties1: TApplicationProperties;
		Button1: TButton;
		Button2: TButton;
		Edit1: TEdit;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		ProgressBar1: TProgressBar;
		SelectDirectoryDialog1: TSelectDirectoryDialog;
		StatusBar1: TStatusBar;
		procedure actProcessExecuteExecute(Sender: TObject);
		procedure actProcessExecuteUpdate(Sender: TObject);
		procedure actSelectPathExecute(Sender: TObject);
		procedure actSelectPathUpdate(Sender: TObject);
		procedure ApplicationProperties1Idle(Sender: TObject;
        		var Done: Boolean);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);

	private
		FWasProcessing: Boolean;
        FCEDThread: TCEDThread;

    public
		{ public declarations }
	end;

var
	ConvExtractDataMainForm: TConvExtractDataMainForm;

implementation

{$R *.lfm}

{ TCEDThread }

procedure TCEDThread.Execute;
	var
    astatus: TCEDThreadStatus;
    aoperation: TCEDThreadOperation;
    aprocFile: string;
    aprogMax,
    aprogress: Integer;
    i: Integer;

    procedure SetStateReady;
    	begin
        State.lock.Acquire;
        try
            State.status:= cesReady;

        	finally
            State.lock.Release;
            end;
		end;

    procedure UpdateState;
    	begin
        State.lock.Acquire;
        try
            State.status:= astatus;
            State.operation:= aoperation;
            State.procFile:= aprocFile;
            State.progMax:= aprogMax;
            State.progress:= aprogress;

        	finally
            State.lock.Release;
            end;
		end;

    procedure CopyState;
    	begin
        State.lock.Acquire;
        try
            astatus:= State.status;
            aoperation:= State.operation;
            aprocFile:= State.procFile;
            aprogMax:= State.progMax;
            aprogress:= State.progress;

        	finally
            State.lock.Release;
            end;
		end;

    procedure TransitionStateProcessing;
    	begin
        i:= -1;
        aprogress:= 0;
        aprogMax:= 0;
        aoperation:= ceoPreparing;
        astatus:= cesProcessing;
		end;

    procedure TransitionStateDone;
    	begin
        if  Assigned(FOutputFile) then
        	begin
        	FOutputFile.Free;
            FOutputFile:= nil;
			end;

    	aoperation:= ceoIdle;
    	astatus:= cesDone;
    	FProcess.ResetEvent;
		end;

    procedure TransitionStateError;
    	begin
        if  Assigned(FOutputFile) then
        	begin
        	FOutputFile.Free;
            FOutputFile:= nil;
			end;

        if  Assigned(FInputFile) then
        	begin
        	FInputFile.Free;
            FInputFile:= nil;
            end;

    	aoperation:= ceoIdle;
    	astatus:= cesError;
    	FProcess.ResetEvent;
        end;

	procedure GetFileList;
        var
        sr: TSearchRec;

        begin
        FFiles.Clear;

        if  FindFirst(IncludeTrailingPathDelimiter(FPath) + '*.dat',
        		faAnyFile, sr) = 0 then
            begin
        	repeat
                FFiles.AddObject(IncludeTrailingPathDelimiter(FPath) + sr.Name,
                		TObject(Integer(sr.Size)));
				until FindNext(sr) <> 0;

            FindClose(sr);
            end;
        end;

	procedure CalculateProgMax;
        var
        j: Integer;

        begin
        aprogMax:= 0;
        for j:= 0 to FFiles.Count - 1 do
        	Inc(aprogMax, Integer(FFiles.Objects[j]));
		end;

	const
		LIT_LBL_CEDTITL = 'RS, NC, N2, N1, NS, AR, CF, DA, DNVZC, HA, ' +
        	    'HNVZC, N1H, N1L, N2L, NF, VF, ZF, N2H' + #13#10;

    procedure OpenOutputFile;
        begin
        try
        	FOutputFile:= TFileStream.Create(
            		IncludeTrailingPathDelimiter(FPath) + 'TestDMode.csv',
                    fmCreate);

            FOutputFile.Write(LIT_LBL_CEDTITL[1], Length(LIT_LBL_CEDTITL));

			except
            FOutputFile:= nil;
			end;
		end;

    procedure GetNextInputFile;
    	begin
        if  Assigned(FInputFile) then
        	begin
        	FInputFile.Free;
            FInputFile:= nil;
            end;

    	Inc(i);
        if  i < FFiles.Count then
        	try
				FInputFile:= TFileStream.Create(FFiles[i], fmOpenRead);
            	aprocFile:= ExtractFileName(FFiles[i]);

            	except
                FInputFile:= nil;
            	end;
		end;

	type
		TTestDModeRec = packed record
    		RS,
    	    NC,
	        N2,
	        N1,
    	    NS,
	        AR,
	        CF,
	        DA,
	        DNVZC,
	        HA,
	        HNVZC,
	        N1H,
	        N1L,
	        N2L,
	        NF,
	        VF,
	        ZF: Byte;
	        N2H: Word;
	    end;

	const
        ARR_LIT_CEDRESL: array[0..1] of string = ('SUCC', 'FAIL');
        ARR_LIT_CEDFUNC: array[0..1] of string = ('ADD', 'SUB');

    procedure ProcessInputRecord;
        var
        buf: TTestDModeRec;
        s: string;

        begin
        FillChar(buf, SizeOf(TTestDModeRec), 0);
        if  FInputFile.Read(buf, SizeOf(TTestDModeRec)) = SizeOf(TTestDModeRec) then
        	begin
            s:= ARR_LIT_CEDRESL[buf.RS] + ',' +
            		IntToStr(buf.NC) + ',' +
                    IntToStr(buf.N2) + ',' +
                    IntToStr(buf.N1) + ',' +
                    ARR_LIT_CEDFUNC[buf.NS]  + ',' +
                    IntToStr(buf.AR) + ',' +
                    IntToStr(buf.CF) + ',' +
                    IntToStr(buf.DA) + ',' +
                    Format('$%2.2x', [buf.DNVZC]) + ',' +
                    IntToStr(buf.HA) + ',' +
                    Format('$%2.2x', [buf.HNVZC]) + ',' +
                    IntToStr(buf.N1H) + ',' +
                    IntToStr(buf.N1L) + ',' +
                    IntToStr(buf.N2L) + ',' +
                    IntToStr(buf.NF) + ',' +
                    IntToStr(buf.VF) + ',' +
                    IntToStr(buf.ZF) + ',' +
                    IntToStr(buf.N2H) + #13#10;

            FOutputFile.Write(s[1], Length(s));
            Inc(aprogress, SizeOf(TTestDModeRec));
            end;
		end;

    begin
    SetStateReady;
    CopyState;

    while (not Terminated) do
		if  FProcess.WaitFor(10) = wrSignaled then
            begin
            case astatus of
            	cesReady, cesDone, cesError:
                	TransitionStateProcessing;

				cesProcessing:
                	case aoperation of
                        ceoPreparing:
                        	begin
                            GetFileList;
							CalculateProgMax;
							OpenOutputFile;
							GetNextInputFile;

                            if  (not Assigned(FOutputFile))
                            or  (not Assigned(FInputFile)) then
                            	TransitionStateError
                            else
                            	aoperation:= ceoConverting;
							end;

						ceoConverting:
                        	begin
                            ProcessInputRecord;

                            if  Assigned(FInputFile)
                            and (FInputFile.Position >= FInputFile.Size) then
                            	GetNextInputFile;

							if  not Assigned(FInputFile) then
                            	TransitionStateDone;
							end;
						end;
				end;

			UpdateState;
			end;
	end;

constructor TCEDThread.Create;
	begin
    FFiles:= TStringList.Create;

    FInputFile:= nil;
    FOutputFile:= nil;

    State.lock:= TCriticalSection.Create;
    State.status:= cesInitialising;
    State.operation:= ceoIdle;
    State.procFile:= '';
    State.progMax:= 0;
    State.progress:= 0;

    FProcess:= TSimpleEvent.Create;
    FProcess.ResetEvent;

    inherited Create(False);
	end;

destructor TCEDThread.Destroy;
	begin
    FProcess.Free;
    State.lock.Free;

    FFiles.Free;

    if  Assigned(FInputFile) then
    	FInputFile.Free;
    if  Assigned(FOutputFile) then
    	FOutputFile.Free;

    inherited Destroy;
	end;

procedure TCEDThread.Process(const APath: string);
	begin
    if  FProcess.WaitFor(0) = wrSignaled then
    	Exit;

	FPath:= APath;
    FProcess.SetEvent;
	end;

{ TConvExtractDataMainForm }

procedure TConvExtractDataMainForm.actProcessExecuteExecute(Sender: TObject);
	begin
	FCEDThread.Process(Edit1.Text);
	end;

procedure TConvExtractDataMainForm.actProcessExecuteUpdate(Sender: TObject);
	begin
	actProcessExecute.Enabled:= (Length(Edit1.Text) > 0) and
    		(not FWasProcessing);
	end;

procedure TConvExtractDataMainForm.actSelectPathExecute(Sender: TObject);
	begin
    if  SelectDirectoryDialog1.Execute then
    	Edit1.Text:= SelectDirectoryDialog1.FileName;
	end;

procedure TConvExtractDataMainForm.actSelectPathUpdate(Sender: TObject);
	begin
    actSelectPath.Enabled:= not FWasProcessing;
	end;

procedure TConvExtractDataMainForm.ApplicationProperties1Idle(Sender: TObject;
		var Done: Boolean);
	begin
	FCEDThread.State.lock.Acquire;
    try
		if  FWasProcessing then
    		begin
            if  FCEDThread.State.status <> cesProcessing then
                begin
            	FWasProcessing:= False;
                Screen.Cursor:= crDefault;
                Label5.Caption:= '';
                ProgressBar1.Position:= 0;
                ProgressBar1.Max:= 0;
				end
        	else
            	begin
                Label5.Caption:= FCEDThread.State.procFile;
                ProgressBar1.Max:= FCEDThread.State.progMax;
                ProgressBar1.Position:= FCEDThread.State.progress;
				end;
			end
    	else
        	begin
			if  FCEDThread.State.status = cesProcessing then
				begin
              	FWasProcessing:= True;
                Screen.Cursor:= crHourGlass;
                ProgressBar1.Max:= FCEDThread.State.progMax;
                Label5.Caption:= FCEDThread.State.procFile;
                ProgressBar1.Position:= FCEDThread.State.progress;
  				end
            else
            	begin
                Label5.Caption:= '';
                ProgressBar1.Position:= 0;
				end;
			end;

		Label6.Caption:= ARR_LIT_CEDOPER[FCEDThread.State.operation];
        StatusBar1.SimpleText:= ARR_LIT_CEDSTAT[FCEDThread.State.status];

		finally
        FCEDThread.State.lock.Release;
        end;

    Application.ProcessMessages;
    Done:= True;
	end;

procedure TConvExtractDataMainForm.FormCreate(Sender: TObject);
	begin
    FWasProcessing:= False;
    FCEDThread:= TCEDThread.Create;
	end;

procedure TConvExtractDataMainForm.FormDestroy(Sender: TObject);
	begin
    FCEDThread.Terminate;
    FCEDThread.WaitFor;
    FCEDThread.Free;
	end;

end.

