generic
	with function Argument_Count return Natural;
	with function Argument(Number : Positive) return String;
package ACoreTools.Switches is
	function First_Nonswitch_Index return Positive;
	generic
		with procedure Action(Selector : in String);
	procedure For_Every_Switch;
end ACoreTools.Switches;
