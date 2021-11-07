<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exsl="http://exslt.org/common"
                xmlns:str="http://exslt.org/strings"
                extension-element-prefixes="exsl str">
  <xsl:output method="text" encoding="utf-8"/>

  <xsl:template match="variable">
    <xsl:text>&#x09;reg add 'HKCU\Environment' /v </xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text> /t REG_EXPAND_SZ /d </xsl:text>
    <xsl:apply-templates/>
    <xsl:text> /f&#x0a;</xsl:text>
  </xsl:template>

  <xsl:variable name="value-replacements-rfc">
    <replace what='"' with='\"'/>
    <replace what="\" with="\\"/>
  </xsl:variable>

  <xsl:variable name="value-replacements"
                select="exsl:node-set($value-replacements-rfc)/replace"/>

  <xsl:template match="variable[@name='PATH']/value">
    <!-- TODO Replace / with \\ and ^/ with %SYSTEMDRIVE%\\ and ^~/ with %HOME%\\ -->
  </xsl:template>

  <xsl:template match="value">
    <xsl:value-of select="str:replace(.,
                                      $value-replacements/@what,
                                      $value-replacements/@with)"/>
  </xsl:template>

  <xsl:template match="value[@of]">
    <xsl:if test="not(@of=../@name)">
      <xsl:text>$</xsl:text>
      <xsl:value-of select="@of"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="list">
    <xsl:apply-templates mode="list"/>
  </xsl:template>

  <xsl:template mode="list" match="*[1]">
    <xsl:apply-templates select="."/>
  </xsl:template>

  <xsl:template mode="list" match="*">
    <xsl:text>:</xsl:text>
    <xsl:apply-templates select="."/>
  </xsl:template>

  <xsl:template mode="list" match="text()"/>

  <xsl:template match="text()"/>
</xsl:stylesheet>
