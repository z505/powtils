<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
<HTML>
  <HEAD>
    <TITLE>WebCMD Application Login Page</TITLE>
  </HEAD>
  
 <BODY alink="#0000cc" bgcolor="#ffffff" link="#0000cc" text="#000000" vlink="#0000cc">
  <FORM action="MainPage" method="get">
   <TABLE bgcolor="#ffffff" border="1" cellpadding="0" cellspacing="0" width="100%">
     <TBODY>
      <TR>
       <TD align="center">
        <CENTER>
         <IMG src="WebCMDICON.jpg" alt="WebCMD Application Logo" height="74" width="138"/>
         <BR/>
         <SMALL>WebCMD 1.1<BR/>
           By Amir Aavani <BR/></SMALL>
         <TABLE bgcolor="#ffffff" border="1" width="350">
          <TBODY>
           <xsl:if test= "not (LoginPage/@RetryMode)">
           <TR><TD align="center" bgcolor="#dcdcdc"><B>Login</B></TD></TR>
           </xsl:if>
           <xsl:if test= "LoginPage/@RetryMode">
            <TR><TD align="center" bgcolor="#dcdcdc"><FONT color= "red"><B>Login Failed</B></FONT></TD></TR>
           </xsl:if>
           <TR>
            <TD align="left" bgcolor="#ffffff">
             <TABLE align="center" bgcolor="#ffffff" border="0" width="100%">
              <TBODY>
               <TR>
                <TD align="right" width="30%">Username:</TD>
                <TD align="left" width="*"> <INPUT name="UserName" value="" type="text"/></TD>
               </TR>
               <TR>
                <TD align="right" width="30%">Password:</TD>
                <TD align="left" width="*"><INPUT name="Password" type="password"/></TD>
               </TR>
               <TR>
                <TD align="right" width="30%"><INPUT type="checkbox" name="RememberUserName" /></TD>
                <TD align="left" width="*">Remeber my username</TD>
               </TR>
              </TBODY>
             </TABLE>
            </TD>
           </TR>
           <TR><TD align="left"><CENTER><INPUT name="SubmitBtn" value="Login" type="submit" onclick="submit ();"/></CENTER></TD></TR>
          </TBODY>
         </TABLE>
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
