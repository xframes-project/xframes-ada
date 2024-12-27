with System; use System;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

procedure Show_C_Func is

    type Callback_Ptr is access procedure (X : Integer);

    procedure My_Callback (X : Integer);
    pragma Convention (C, My_Callback);

    procedure My_Callback (X : Integer) is
    begin
        Put_Line ("Callback called with: " & Integer'Image (X));
    end My_Callback;

    Callback_Address : System.Address := My_Callback'Address;

    function Register_Callback (Callback : System.Address) return Boolean;
    pragma Import (C, Register_Callback, "register_callback");

begin
    if Register_Callback (Callback_Address) then
        Put_Line("Callback registered successfully.");
    else
        Put_Line("Failed to register callback.");
    end if;
end Show_C_Func;

