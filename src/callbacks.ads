with Interfaces.C.Strings;

package Callbacks is
   -- Callback procedure declarations
   procedure On_Init;
   procedure On_Text_Changed (Index : Integer; Text : Interfaces.C.Strings.Chars_Ptr);
   -- Add other callback procedure declarations if needed
end Callbacks;
