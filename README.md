# xframes-ada-library

## Requirements

### Install Ada and Alire

Follow these instructions: https://alire.ada.dev/transition_from_gnat_community.html

Start by installing Alire: https://alire.ada.dev/

You can still download GNAT Studio community edition (the official Ada IDE) here: https://github.com/AdaCore/gnatstudio/releases

That said, The `Ada & SPARK` VS Code extension is also available and it is recommended over GNAT Studio. https://marketplace.visualstudio.com/items?itemName=AdaCore.ada

#### Raspberry Pi

You need to install `gnat` and `gpruild` though `apt`. You can then proceed to build `alire` (refer to the official documentation). 

### Running the application

Launch an Alire Powershell then run `alr run`

### Screenshots

#### Windows 11

![image](https://github.com/user-attachments/assets/5cecfba1-38f0-438c-a533-59433c39ba1b)

#### Raspberry Pi

Uses Alire (built from source) and 'local' toolchain based system packages `gnat` and `gprbuild`.

![image](https://github.com/user-attachments/assets/36c971e3-0034-420a-acb7-3720f167f270)

