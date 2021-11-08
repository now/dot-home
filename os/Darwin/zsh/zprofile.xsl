<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:str="http://exslt.org/strings"
                extension-element-prefixes="str">
  <xsl:import href="../../Linux/zsh/zprofile.xsl"/>

  <xsl:template match="environment">
    <xsl:apply-templates/>
    <xsl:if test="variable[name='PATH']">
      <xsl:text>export</xsl:text>
      <xsl:apply-templates mode="export"/>
      <xsl:text>&#x0a;</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="variable[name!='PATH']"/>

  <xsl:template mode="export" match="variable[name!='PATH']"/>
</xsl:stylesheet>
