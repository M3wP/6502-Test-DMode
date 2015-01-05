program ConvertExtractData;

{$mode objfpc}{$H+}

uses
		{$IFDEF UNIX}{$IFDEF UseCThreads}
		cthreads,
		{$ENDIF}{$ENDIF}
		Interfaces, // this includes the LCL widgetset
		Forms, FormConvExtractDataMain
		{ you can add units after this };

{$R *.res}

begin
		RequireDerivedFormResource := True;
		Application.Initialize;
	Application.CreateForm(TConvExtractDataMainForm, ConvExtractDataMainForm);
		Application.Run;
end.

