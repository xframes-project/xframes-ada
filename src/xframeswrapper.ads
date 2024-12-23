with Interfaces.C.Strings;

package XFramesWrapper is
   -- Declare the callback types with the foreign convention
   type OnInitCb is access procedure with Convention => C;

   type OnTextChangedCb is access procedure (Id : Integer; Value : Interfaces.C.Strings.Chars_Ptr)
     with Convention => C;

   -- Declare the external Init procedure with proper linking information
   procedure Init (Assets_Base_Path             : Interfaces.C.Strings.Chars_Ptr;
                   Raw_Font_Definitions         : Interfaces.C.Strings.Chars_Ptr;
                   Raw_Style_Override_Definitions : Interfaces.C.Strings.Chars_Ptr;
                   On_Init                      : OnInitCb;
                   On_Text_Changed              : OnTextChangedCb;
                   On_Combo_Changed             : access procedure;
                   On_Numeric_Value_Changed     : access procedure;
                   On_Boolean_Value_Changed     : access procedure;
                   On_Multiple_Numeric_Values_Changed : access procedure;
                   On_Click                     : access procedure)
   with Import => True,
        Convention => C,
        External_Name => "init";
end XFramesWrapper;
