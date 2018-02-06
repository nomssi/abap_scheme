ABAP Editor Configuration

# How To Enable The New ABAP Editor

## Copy files 
The SAP GUI configuration files  are located in the roaming user application directory. They can be reachead using the environment variable %APPDATA%.
 
    cd %APPDATA%\SAP\SAP GUI\ABAP Editor
    Windows 10 => [ C:\Users\<username>\AppData\Roaming\SAP\SAP GUI\ABAP Editor ]

Manually copy those two files to your _…\AppData\Roaming\SAP\SAP GUI\ABAP Editor_ folder.

* lisp_spec.xslt
* lisp_user.xslt 

## Custom Version of the configuration files

https://wiki.scn.sap.com/wiki/display/NWTech/SAP+GUI+Logon+Configuration
https://github.com/lucattelli/ab4-themes
https://github.com/alexey-arseniev/ab4-code-templates
https://blogs.sap.com/2017/08/01/old-new-abap-editor/

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

## Change ABAP Include YY_LISP_IDE

      c_new_abap_editor TYPE flag VALUE abap_true,
      c_source_type TYPE string VALUE 'LISP'.

Restart the workbench.      
