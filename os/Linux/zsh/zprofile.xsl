<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:str="http://exslt.org/strings"
                extension-element-prefixes="str">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:strip-space elements="environment variable paths"/>

  <xsl:template match="environment">
    <xsl:apply-templates/>
    <xsl:if test="variable">
      <xsl:text>export</xsl:text>
      <xsl:apply-templates mode="export"/>
      <xsl:text>&#x0a;</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="variable">
    <xsl:apply-templates select="name"/>
    <xsl:text>=</xsl:text>
    <xsl:apply-templates select="value"/>
    <xsl:text>&#x0a;</xsl:text>
  </xsl:template>

  <xsl:template match="path[preceding-sibling::*]">
    <xsl:text>:</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="current-value" name="current-value">
    <xsl:text>$</xsl:text>
    <xsl:value-of select="../../../name"/>
  </xsl:template>

  <xsl:template match="current-value[preceding-sibling::*]">
    <xsl:text>:</xsl:text>
    <xsl:call-template name="current-value"/>
  </xsl:template>

  <xsl:template match="text()[contains(., '&#x09;&#x0a; ')]">
    <xsl:text>'</xsl:text>
    <xsl:value-of select="str:replace(., &quot;'&quot;, &quot;'\''&quot;)"/>
    <xsl:text>'</xsl:text>
  </xsl:template>

  <xsl:template match="value[paths]/text()"/>

  <xsl:template mode="export" match="name">
    <xsl:text> </xsl:text>
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template mode="export" match="text()"/>
</xsl:stylesheet>
