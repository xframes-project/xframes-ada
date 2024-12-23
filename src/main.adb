with XFramesWrapper;
with Interfaces.C.Strings;
with Ada.Text_IO;

procedure Main is
   pragma Convention (C);
   procedure On_Init is
   begin
      Ada.Text_IO.Put_Line("On_Init");
   end On_Init;

   pragma Convention (C);
   procedure On_Text_Changed(Id: Integer; Value: Interfaces.C.Strings.Chars_Ptr) is
   begin
      Ada.Text_IO.Put_Line("On_Text_Changed");
   end On_Text_Changed;

   -- Declare the callback types with the proper convention (C)
   On_Init_CB : constant XFramesWrapper.OnInitCb :=
     XFramesWrapper.OnInitCb'(On_Init'Access);

   On_Text_Changed_CB : constant XFramesWrapper.OnTextChangedCb :=
     XFramesWrapper.OnTextChangedCb'(On_Text_Changed'Access);

   -- Strings for initialization
   Assets_Path : constant Interfaces.C.Strings.Chars_Ptr :=
     Interfaces.C.Strings.New_String("path/to/assets");
   Font_Definitions : constant Interfaces.C.Strings.Chars_Ptr :=
     Interfaces.C.Strings.New_String("font-definitions");
   Style_Override : constant Interfaces.C.Strings.Chars_Ptr :=
     Interfaces.C.Strings.New_String("style-override");

begin
   XFramesWrapper.Init
     (Assets_Base_Path             => Assets_Path,
      Raw_Font_Definitions         => Font_Definitions,
      Raw_Style_Override_Definitions => Style_Override,
      On_Init                      => On_Init_CB,
      On_Text_Changed              => On_Text_Changed_CB,
      On_Combo_Changed             => null,
      On_Numeric_Value_Changed     => null,
      On_Boolean_Value_Changed     => null,
      On_Multiple_Numeric_Values_Changed => null,
      On_Click                     => null);

   Ada.Text_IO.Put_Line("Init function invoked.");
end Main;
