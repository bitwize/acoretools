with Ada.Strings.Fixed;

package body ACoreTools.Switches is
	package Fixed renames Ada.Strings.Fixed;
	function First_Nonswitch_Index return Positive is
		function Maybe_Is_Switch(S : String) return Boolean is
		begin
			return S'Length > 0 and S(S'First) = '-';
		end Maybe_Is_Switch;
		I : Positive := 1;
	begin
		if not NSI_Set then
			while (
				Natural(I) <= Argument_Count and then
				Maybe_Is_Switch(Argument(I))
			) loop
				I := I + 1;
			end loop;
			NSI := I;
			NSI_Set := True;
		end if;
		return NSI;
	end First_Nonswitch_Index;

	procedure Look_For_Switch(Switch : in out Switch_Record) is
		I : Natural;
	begin
		if Argument_Count = 0 then
			return;
		end if;
		for I in 1 .. (First_Nonswitch_Index - 1) loop
			if Fixed.Index(Argument(I), "" & Switch.Selector) /= 0 then
				Switch.Value := True;
			end if;
		end loop;
	end Look_For_Switch;
end ACoreTools.Switches;
