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
        error: string;
    end;

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

resourcestring
    STR_LBL_CEDSINI = 'Initialising...';
    STR_LBL_CEDSRDY = 'Ready.';
    STR_LBL_CEDSPRC = 'Processing...';
    STR_LBL_CEDSDNE = 'Complete.';
    STR_LBL_CEDSERR = 'Error.';

    STR_LBL_CEDOIDL = '';
    STR_LBL_CEDOPRP = 'Preparing...';
    STR_LBL_CEDOCNV = 'Converting...';

    STR_LBL_DLGTERR = 'Conversion error';
    STR_MSG_DLGMERR = 'Conversion process failed with message:';

    STR_MSG_CEDOPER = 'Unable to initialise open of input or output file/s.';
    STR_MSG_CEDPRER = 'Invalid data while reading input file.';

const
    LIT_LBL_CEDTITL = 'RS, NC, N2, N1, NS, AR, CF, DA, %DNVZC, HA, ' +
            '%HNVZC, N1H, N1L, N2L, NF, VF, ZF, N2H' + #13#10;
    LIT_LBL_CEDBLKL = ' ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  '+#13#10;

    ARR_LIT_CEDRESL: array[0..1] of string = ('SUCC', 'FAIL');
    ARR_LIT_CEDFUNC: array[0..1] of string = ('ADC', 'SBC');

var
    ARR_STR_CEDSTAT: array[TCEDThreadStatus] of string = (
            STR_LBL_CEDSINI, STR_LBL_CEDSRDY, STR_LBL_CEDSPRC, STR_LBL_CEDSDNE,
            STR_LBL_CEDSERR);
    ARR_STR_CEDOPER: array[TCEDThreadOperation] of string = (
            STR_LBL_CEDOIDL, STR_LBL_CEDOPRP, STR_LBL_CEDOCNV);


function ByteToBinaryStr(const AByte: Byte): string;
    var
    m: Byte;

    begin
    m:= $80;
    Result:= '';

    while m > 0 do
        begin
        if  (m and AByte) <> 0 then
            Result:= Result + '1'
        else
            Result:= Result + '0';

        m:= m shr 1;
        end;
    end;

function DModeRecIsValid(const ARec: TTestDModeRec): Boolean;
    begin
    Result:= (ARec.RS <= 1) and (ARec.NC <= 1) and (ARec.NS <= 1);
    end;

function DModeRecGenerateIndex(const ARec: TTestDModeRec): Integer;
    begin
//  ~NC 1  N2 8  N1 8  NS 1
    Result:= (Byte(1 - ARec.NC) shl 17) or (ARec.N2 shl 9) or
            (ARec.N1 shl 1) or ARec.NS;
    end;

function DModeRecConvertToCSV(const ARec: TTestDModeRec): string;
    begin
    Result:= ARR_LIT_CEDRESL[ARec.RS] + ',' +
            IntToStr(ARec.NC) + ',' +
            IntToStr(ARec.N2) + ',' +
            IntToStr(ARec.N1) + ',' +
            ARR_LIT_CEDFUNC[ARec.NS]  + ',' +
            IntToStr(ARec.AR) + ',' +
            IntToStr(ARec.CF) + ',' +
            IntToStr(ARec.DA) + ',' +
            '%' + ByteToBinaryStr(ARec.DNVZC) + ',' +
            IntToStr(ARec.HA) + ',' +
            '%' + ByteToBinaryStr(ARec.HNVZC) + ',' +
            IntToStr(ARec.N1H) + ',' +
            IntToStr(ARec.N1L) + ',' +
            IntToStr(ARec.N2L) + ',' +
            IntToStr(ARec.NF) + ',' +
            IntToStr(ARec.VF) + ',' +
            IntToStr(ARec.ZF) + ',' +
            IntToStr(ARec.N2H) + #13#10;
    end;

{ TCEDThread }

procedure TCEDThread.Execute;
    var
    astatus: TCEDThreadStatus;
    aoperation: TCEDThreadOperation;
    aprocFile: string;
    aprogMax,
    aprogress: Integer;
    aerror: string;
    i,
    r: Integer;
    buf: TTestDModeRec;

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
            State.error:= aerror;

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

    procedure AdvanceOutput(const ARecord: Integer);
        begin
        while r < ARecord do
            begin
            FOutputFile.Write(LIT_LBL_CEDBLKL[1], Length(LIT_LBL_CEDBLKL));
            Inc(aprogress);
            Inc(r);
            end;
        end;

    procedure TransitionStateProcessing;
        begin
        i:= -1;
        r:= 0;
        aprogress:= 0;
        aprogMax:= 0;
        aoperation:= ceoPreparing;
        astatus:= cesProcessing;
        end;

    procedure TransitionStateDone;
        begin
        if  Assigned(FOutputFile) then
            begin
            if  r < aprogMax then
                AdvanceOutput(aprogMax + 1);

            FOutputFile.Free;
            FOutputFile:= nil;
            end;

        aoperation:= ceoIdle;
        astatus:= cesDone;
        FProcess.ResetEvent;
        end;

    procedure TransitionStateError(const AReason: string);
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
        aerror:= AReason;
        UniqueString(aerror);

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
        begin
        buf.NC:= 0;
        buf.N2:= $FF;
        buf.N1:= $FF;
        buf.NS:= 1;

        aprogMax:= DModeRecGenerateIndex(buf);
        end;

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

    function ProcessInputRecord: Boolean;
        var
        c: Integer;
        s: string;

        begin
        Result:= False;

        if  FInputFile.Read(buf, SizeOf(TTestDModeRec)) = SizeOf(TTestDModeRec) then
            if  DModeRecIsValid(buf) then
                begin
                c:= DModeRecGenerateIndex(buf);

                if  c >= r then
                    begin
                    if  c > r then
                        AdvanceOutput(c);

                    s:= DModeRecConvertToCSV(buf);
                    FOutputFile.Write(s[1], Length(s));

                    Inc(aprogress);
                    Inc(r);
                    Result:= True;
                    end;
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
                                TransitionStateError(STR_MSG_CEDOPER)
                            else
                                aoperation:= ceoConverting;
                            end;

                        ceoConverting:
                            begin
                            if  not ProcessInputRecord then
                                TransitionStateError(STR_MSG_CEDPRER)
                            else
                                begin
                                if  Assigned(FInputFile)
                                and (FInputFile.Position >= FInputFile.Size) then
                                    GetNextInputFile;

                                if  not Assigned(FInputFile) then
                                    TransitionStateDone;
                                end;
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
    var
    e: Boolean;
    m: string;

    begin
    e:= False;

    FCEDThread.State.lock.Acquire;
    try
        if  FWasProcessing then
            begin
            if  FCEDThread.State.status <> cesProcessing then
                begin
                e:= FCEDThread.State.status = cesError;
                m:= FCEDThread.State.error;

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
                Screen.Cursor:= crAppStart;
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

        Label6.Caption:= ARR_STR_CEDOPER[FCEDThread.State.operation];
        StatusBar1.SimpleText:= ARR_STR_CEDSTAT[FCEDThread.State.status];

        finally
        FCEDThread.State.lock.Release;
        end;

    if  e then
        MessageDlg(STR_LBL_DLGTERR, STR_MSG_DLGMERR + #13#10#13#10#9 +
                QuotedStr(m), mtError, [mbOK], 0);

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

