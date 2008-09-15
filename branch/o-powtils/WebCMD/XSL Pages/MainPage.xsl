<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/MainPage">
<HTML>
  <HEAD>
    <TITLE><xsl:value-of select= "LoginInfo/@UserName"/>@<xsl:value-of select="@HostName"/></TITLE>
  </HEAD>
  
 <BODY alink="#0000cc" bgcolor="#ffffff" link="#0000cc" text="#000000" vlink="#0000cc">
  <FORM action="MainPage" method="post">
   <TABLE bgcolor="#ffffff" border="1" cellpadding="0" cellspacing="0" width="100%">
     <TBODY>
      <TR>
       <TD align="center">
        <CENTER>
         <IMG src="WebCMDICON.jpg" alt="WebCMD Application Logo" height="74" width="138"/>
         <BR/>
         <SMALL>WebCMD 1.1<BR/>
           By Amir Aavani <BR/></SMALL>
        </CENTER>
       </TD>
      </TR>
    </TBODY>
   </TABLE>
  </FORM>
 </BODY>
</HTML>
</xsl:template>

</xsl:stylesheet>
