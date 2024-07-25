with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
package body ACoreTools.Tests is
	package TIO renames Ada.Text_IO;
	package IIO renames Ada.Integer_Text_IO;
	package E renames Ada.Exceptions;
	package CL renames Ada.Command_Line;
	procedure Test is
		Pass : Boolean := False;
	begin
		Total_Tests := Total_Tests + 1;
		TIO.Put("--- Test: " & Description & "... ");
		Pass := Test_Body;
		if Pass then
			TIO.Put_Line("passed");
			Tests_Passed := Tests_Passed + 1;
		else
			TIO.Put_Line("failed");
			Tests_Failed := Tests_Failed + 1;
		end if;
	exception
		when Exc : others =>
			TIO.Put_Line("failed with exception: " & E.Exception_Message(Exc));
			Tests_Failed := Tests_Failed + 1;
	end Test;

	procedure Show_Test_Header is
	begin
		TIO.Put_Line("***** " & Suite_Description & " - Begin Test Run *****");
	end Show_Test_Header;
	
	procedure Show_Test_Results is
	begin
		TIO.Put_Line("***** " & Suite_Description & "  -  End Test Run *****");
		TIO.Put_Line("Results");
		TIO.Put_Line("-------");
		TIO.Put("Tests Run   : ");
		IIO.Put(Total_Tests);
		TIO.New_Line;
		TIO.Put("Tests Passed: ");
		IIO.Put(Tests_Passed);
		TIO.New_Line;
		TIO.Put("Tests Failed: ");
		IIO.Put(Tests_Failed);
		TIO.New_Line;
		if Tests_Failed > 0 then
			CL.Set_Exit_Status(CL.Failure);
		end if;
	end Show_Test_Results;
end ACoreTools.Tests;
