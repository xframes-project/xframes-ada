with System;                use System;
with Interfaces.C;          use Interfaces.C;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with GNATCOLL.JSON;         use GNATCOLL.JSON;
with GNATCOLL.Strings;      use GNATCOLL.Strings;

procedure Show_C_Func is

   type OnInitCb is access procedure;
   type Char_Ptr is access all Interfaces.C.Char;
   type Float_Array is array (Positive range <>) of aliased Float;
   type OnTextChangedCb is access procedure (Id : Integer; Text : Char_Ptr);
   type OnComboChangedCb is
     access procedure (Id : Integer; Selected_Option_Id : Integer);
   type OnNumericValueChangedCb is
     access procedure (Id : Integer; Value : Float);
   type OnBooleanValueChangedCb is
     access procedure (Id : Integer; Value : Boolean);
   type OnMultipleNumericValuesChangedCb is
     access procedure
       (Id : Integer; Values : access Float_Array; NumValues : Integer);
   type OnClickCb is access procedure (Id : Integer);

   type Font_Sizes_Array is array (1 .. 8) of Integer;

   procedure Init;
   pragma Convention (C, Init);

   procedure Init is
   begin
      Put_Line ("Init called with");
   end Init;

   procedure OnTextChanged (Id : Integer; Text : Char_Ptr);
   pragma Convention (C, OnTextChanged);

   procedure OnTextChanged (Id : Integer; Text : Char_Ptr) is
   begin
      Put_Line
        ("OnTextChanged called with ID: "
         & Integer'Image (Id)
         & " and Text: ");
   end OnTextChanged;

   procedure OnComboChanged (Id : Integer; Selected_Option_Id : Integer);
   pragma Convention (C, OnComboChanged);

   procedure OnComboChanged (Id : Integer; Selected_Option_Id : Integer) is
   begin
      Put_Line
        ("OnComboChanged called with ID: " & Integer'Image (Id) & " and: ");
   end OnComboChanged;

   procedure OnNumericValueChanged (Id : Integer; Value : Float);
   pragma Convention (C, OnNumericValueChanged);

   procedure OnNumericValueChanged (Id : Integer; Value : Float) is
   begin
      Put_Line
        ("Callback called with ID: "
         & Integer'Image (Id)
         & " and Value: "
         & Float'Image (Value));
   end OnNumericValueChanged;

   procedure OnBooleanValueChanged (Id : Integer; Value : Boolean);
   pragma Convention (C, OnBooleanValueChanged);

   procedure OnBooleanValueChanged (Id : Integer; Value : Boolean) is
   begin
      Put_Line
        ("OnBooleanValueChanged called with ID: "
         & Integer'Image (Id)
         & " and Value: "
         & Boolean'Image (Value));
   end OnBooleanValueChanged;

   procedure MultipleNumericValuesChanged
     (Id : Integer; Values : access Float_Array; NumValues : Integer);
   pragma Convention (C, MultipleNumericValuesChanged);

   procedure MultipleNumericValuesChanged
     (Id : Integer; Values : access Float_Array; NumValues : Integer) is
   begin
      Ada.Text_IO.Put_Line
        ("MultipleNumericValuesChanged numeric values changed callback invoked.");
      Ada.Text_IO.Put_Line ("ID: " & Integer'Image (Id));
      Ada.Text_IO.Put_Line ("Number of Values: " & Integer'Image (NumValues));

      for I in 1 .. NumValues loop
         Ada.Text_IO.Put_Line
           ("Value " & Integer'Image (I) & ": " & Float'Image (Values (I)));
      end loop;
   end MultipleNumericValuesChanged;

   procedure OnClick (Id : Integer; Value : Boolean);
   pragma Convention (C, OnClick);

   procedure OnClick (Id : Integer; Value : Boolean) is
   begin
      Put_Line ("OnClick called with ID: " & Integer'Image (Id));
   end OnClick;

   procedure Set_Element (Element_Json : Char_Ptr);
   pragma Import (C, Set_Element, "setElement");

   Init_Address                         : System.Address := Init'Address;
   OnTextChanged_Address                : System.Address :=
     OnTextChanged'Address;
   OnComboChanged_Address               : System.Address :=
     OnComboChanged'Address;
   OnNumericValueChanged_Address        : System.Address :=
     OnNumericValueChanged'Address;
   OnBooleanValueChanged_Address        : System.Address :=
     OnBooleanValueChanged'Address;
   MultipleNumericValuesChanged_Address : System.Address :=
     MultipleNumericValuesChanged'Address;
   OnClick_Address                      : System.Address := OnClick'Address;

   procedure Extern_Init
     (Assets_Base_Path               : Char_Ptr;
      Raw_Font_Definitions           : Char_Ptr;
      Raw_Style_Override_Definitions : Char_Ptr;
      OnInit                         : System.Address;
      OnTextChanged                  : System.Address;
      OnComboChanged                 : System.Address;
      OnNumericValueChanged          : System.Address;
      OnBooleanValueChanged          : System.Address;
      MultipleNumericValuesChanged   : System.Address;
      OnClick                        : System.Address);
   pragma Import (C, Extern_Init, "init");

   Assets_Base_Path               : constant String := "./assets/";
   Raw_Font_Definitions           : constant String :=
     "{...font definitions...}";
   Raw_Style_Override_Definitions : constant String :=
     "{...style overrides...}";

   Assets_Base_Path_C               : constant Interfaces.C.char_array :=
     To_C (Assets_Base_Path);
   Raw_Font_Definitions_C           : constant Interfaces.C.char_array :=
     To_C (Raw_Font_Definitions);
   Raw_Style_Override_Definitions_C : constant Interfaces.C.char_array :=
     To_C (Raw_Style_Override_Definitions);

   Assets_Base_Path_Ptr               : constant Char_Ptr :=
     new Interfaces.C.Char'(Assets_Base_Path_C (1));
   Raw_Font_Definitions_Ptr           : constant Char_Ptr :=
     new Interfaces.C.Char'(Raw_Font_Definitions_C (1));
   Raw_Style_Override_Definitions_Ptr : constant Char_Ptr :=
     new Interfaces.C.Char'(Raw_Style_Override_Definitions_C (1));

   Input_String : String (1 .. 100);
   Last_Index   : Natural;

   Font_Sizes                     : Font_Sizes_Array :=
     (16, 18, 20, 24, 28, 32, 36, 48);
   Font_Definitions               : JSON_Array := Empty_Array;
   Tmp_Font_Definition            : JSON_Value;
   Font_Definitions_As_JSON_Value : JSON_Value;
   JSON_String                    : Ada.Strings.Unbounded.Unbounded_String;

begin
   for I in Font_Sizes'Range loop
      Tmp_Font_Definition := Create_Object;
      Tmp_Font_Definition.Set_Field
        (Field_Name => "name", Field => "roboto-regular");
      Tmp_Font_Definition.Set_Field
        (Field_Name => "size", Field => Create (Integer'(Font_Sizes (I))));

      Append (Font_Definitions, Tmp_Font_Definition);
   end loop;

   Font_Definitions_As_JSON_Value := Create (Font_Definitions);

   JSON_String := Write (Font_Definitions_As_JSON_Value);

   Put_Line (To_String (JSON_String));

   Extern_Init
     (Assets_Base_Path_Ptr,
      Raw_Font_Definitions_Ptr,
      Raw_Style_Override_Definitions_Ptr,
      Init_Address,
      OnTextChanged_Address,
      OnComboChanged_Address,
      OnNumericValueChanged_Address,
      OnBooleanValueChanged_Address,
      MultipleNumericValuesChanged_Address,
      OnClick_Address);

   Get_Line (Input_String, Last_Index);
end Show_C_Func;
