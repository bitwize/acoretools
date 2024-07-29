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
		while (
			Natural(I) <= Argument_Count and then
			Maybe_Is_Switch(Argument(I))
		) loop
			I := I + 1;
		end loop;
		return I;
	end First_Nonswitch_Index;

	procedure For_Every_Switch is
		I : Positive;
		procedure Scan_Arg(S : String) is
			J : Positive;
		begin
			for J in (S'First + 1) .. (S'Last) loop
				Action(S(J..J));
			end loop;
		end Scan_Arg;
	begin
		for I in 1..(First_Nonswitch_Index - 1) loop
			Scan_Arg(Argument(I));
		end loop;
	end For_Every_Switch;
end ACoreTools.Switches;
