generic
	with function Argument_Count return Natural;
	with function Argument(Number : Positive) return String;
package ACoreTools.Switches is
	type Switch_Record is record
		Selector : Character;
		Value    : Boolean;
	end record;
	function First_Nonswitch_Index return Positive;
	procedure Look_For_Switch(
		Switch : in out Switch_Record
	);
private
	NSI_Set : Boolean := False;
	NSI     : Positive := 1;
end ACoreTools.Switches;
