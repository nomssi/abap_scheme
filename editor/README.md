# ABAP Editor Configuration

## ABAP Editor Configuration Folder

The SAP GUI configuration files are located in the roaming user application directory. They can be reachead using the environment variable %APPDATA%.
 
    cd %APPDATA%\SAP\SAP GUI\ABAP Editor
    Windows 10 => [ C:\Users\<username>\AppData\Roaming\SAP\SAP GUI\ABAP Editor ]


## How To Enable The New ABAP Editor

to configure the powerful SAP GUI ABAP Editor for LISP you need

1) Two configuration files for the language LISPin your local SAP GUI configuration folder for the ABAP Editor.

Manually copy the files

* lisp_spec.xml
* lisp_user.xml 

provided here to your _…\AppData\Roaming\SAP\SAP GUI\ABAP Editor_ folder.

2) A change in the first lines of ABAP Include YY_LISP_IDE

      c_new_abap_editor TYPE flag VALUE abap_true,
      c_source_type TYPE string VALUE 'LISP'.

Restart the workbench.

## Configuration Files

To create your own theme files

* lisp_spec.xml
* lisp_user.xml

read

* https://wiki.scn.sap.com/wiki/display/NWTech/SAP+GUI+Logon+Configuration
* https://github.com/lucattelli/ab4-themes
* https://github.com/alexey-arseniev/ab4-code-templates
* https://blogs.sap.com/2017/08/01/old-new-abap-editor/

The comments in the blog post 

* https://blogs.sap.com/2015/06/24/a-lisp-interpreter-in-abap/

give useful hints:


    In lisp_spec.xml,
    
    add
    <TextType id="52" name="Keywords" ...>
      <Keywords>
    …
        <Keyword text="define"/>
        <Keyword text="set!"/>
        <Keyword text="lambda"/>
    …
      </Keywords>
    </TextType>
    
    <keywords>
    </>
   
    <structures>
      <structure open="(" close==")"\>
      <structure open="[" close=="]"\>
    </structures>
