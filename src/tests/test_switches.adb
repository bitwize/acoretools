with Ada.Text_IO;
with Ada.Containers.Vectors;
with ACoreTools.Switches;
with ACoreTools.Tests;
with Ada.Strings.Unbounded;

use Ada.Text_IO;

procedure Test_Switches is
	package SU renames Ada.Strings.Unbounded;
	use type SU.Unbounded_String;
	package SVec is new
	  Ada.Containers.Vectors(
	    Index_Type   => Positive,
	    Element_Type => SU.Unbounded_String,
	    "="          => SU."="
	  );
	package T is new ACoreTools.Tests("Test Common Switch Code");
	Test_CmdLine : SVec.Vector;

	function Test_CmdLine_ArgCount return Natural is
	begin
		return Natural(Test_CmdLine.Length);
	end Test_CmdLine_ArgCount;

	function Test_CmdLine_Argument(Number : Positive) return String is
	begin
		return Su.To_String(Test_CmdLine(Number));
	end Test_CmdLine_Argument;

	package SW is new ACoreTools.Switches(
		Argument_Count => Test_CmdLine_ArgCount,
		Argument       => Test_CmdLine_Argument
	);

	function Test_Body_First_Nonswitch_Index_With_All_Switches return Boolean is
	begin
		Test_CmdLine.Clear;
		Test_CmdLine.Append(SU.To_Unbounded_String("-ab"));
		Test_CmdLine.Append(SU.To_Unbounded_String("-cd"));
		return (SW.First_Nonswitch_Index = 3);
	end Test_Body_First_Nonswitch_Index_With_All_Switches;

	procedure Test_First_Nonswitch_Index_With_All_Switches is new
		T.Test(
			Description => "First_Nonswitch_Index works with all switches",
			Test_Body => Test_Body_First_Nonswitch_Index_With_All_Switches
		);

	function Test_Body_First_Nonswitch_Index_With_Some_Switches return Boolean is
	begin
		Test_CmdLine.Clear;
		Test_CmdLine.Append(SU.To_Unbounded_String("-ab"));
		Test_CmdLine.Append(SU.To_Unbounded_String("foo"));
		Test_CmdLine.Append(SU.To_Unbounded_String("bar"));
		return (SW.First_Nonswitch_Index = 2);
	end Test_Body_First_Nonswitch_Index_With_Some_Switches;

	procedure Test_First_Nonswitch_Index_With_Some_Switches is new
		T.Test(
			Description => "First_Nonswitch_Index works with some switches",
			Test_Body => Test_Body_First_Nonswitch_Index_With_Some_Switches
		);

	function Test_Body_First_Nonswitch_Index_With_No_Switches return Boolean is
	begin
		Test_CmdLine.Clear;
		Test_CmdLine.Append(SU.To_Unbounded_String("foo"));
		Test_CmdLine.Append(SU.To_Unbounded_String("bar"));
		return (SW.First_Nonswitch_Index = 1);
	end Test_Body_First_Nonswitch_Index_With_No_Switches;

	procedure Test_First_Nonswitch_Index_With_No_Switches is new
		T.Test(
			Description => "First_Nonswitch_Index works with no switches",
			Test_Body => Test_Body_First_Nonswitch_Index_With_No_Switches
		);

	function Test_Body_For_Every_Switch_With_All_Switches return Boolean is
		S : SU.Unbounded_String;
		procedure Accum(Selector : String) is
		begin
			SU.Append(S, Selector);
		end Accum;
		procedure Run_Switches is new SW.For_Every_Switch(Action => Accum);
	begin
		Test_CmdLine.Clear;
		Test_CmdLine.Append(SU.To_Unbounded_String("-z"));
		Test_CmdLine.Append(SU.To_Unbounded_String("-b"));
		Run_Switches;
		return S = "zb";
	end Test_Body_For_Every_Switch_With_All_Switches;

	procedure Test_For_Every_Switch_With_All_Switches is new
		T.Test(
			Description => "For_Every_Switch works with all switches",
			Test_Body => Test_Body_For_Every_Switch_With_All_Switches
		);
	function Test_Body_For_Every_Switch_With_Some_Switches return Boolean is
		S : SU.Unbounded_String;
		procedure Accum(Selector : String) is
		begin
			SU.Append(S, Selector);
		end Accum;
		procedure Run_Switches is new SW.For_Every_Switch(Action => Accum);
	begin
		Test_CmdLine.Clear;
		Test_CmdLine.Append(SU.To_Unbounded_String("-z"));
		Test_CmdLine.Append(SU.To_Unbounded_String("foo"));
		Test_CmdLine.Append(SU.To_Unbounded_String("bar"));
		Run_Switches;
		return (S = "z");
	end Test_Body_For_Every_Switch_With_Some_Switches;

	procedure Test_For_Every_Switch_With_Some_Switches is new
		T.Test(
			Description => "For_Every_Switch works with some switches",
			Test_Body => Test_Body_For_Every_Switch_With_Some_Switches
		);

	function Test_Body_For_Every_Switch_With_No_Switches return Boolean is
		S : SU.Unbounded_String;
		procedure Accum(Selector : String) is
		begin
			SU.Append(S, Selector);
		end Accum;
		procedure Run_Switches is new SW.For_Every_Switch(Action => Accum);
	begin
		Test_CmdLine.Clear;
		Test_CmdLine.Append(SU.To_Unbounded_String("foo"));
		Test_CmdLine.Append(SU.To_Unbounded_String("bar"));
		Run_Switches;
		return (S = "");
	end Test_Body_For_Every_Switch_With_No_Switches;

	procedure Test_For_Every_Switch_With_No_Switches is new
		T.Test(
			Description => "For_Every_Switch works with no switches",
			Test_Body => Test_Body_For_Every_Switch_With_No_Switches
		);
begin
	T.Show_Test_Header;	
	Test_First_Nonswitch_Index_With_All_Switches;
	Test_First_Nonswitch_Index_With_Some_Switches;
	Test_First_Nonswitch_Index_With_No_Switches;
	Test_For_Every_Switch_With_All_Switches;
	Test_For_Every_Switch_With_Some_Switches;
	Test_For_Every_Switch_With_No_Switches;
	T.Show_Test_Results;
end Test_Switches;
