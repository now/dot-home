<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exsl="http://exslt.org/common"
                xmlns:set="http://exslt.org/sets"
                xmlns:str="http://exslt.org/strings"
                extension-element-prefixes="exsl set str">
  <xsl:output method="xml" encoding="UTF-8" indent="yes"
              doctype-public="-//Apple Computer//DTD PLIST 1.0//EN"
              doctype-system="http://www.apple.com/DTDs/PropertyList-1.0.dtd"/>

  <xsl:param name="label"/>
  <xsl:param name="name"/>
  <xsl:param name="current-value"/>

  <xsl:variable name="uppercase-name"
                select="translate($name,
                          'abcdefghijklmnopqrstuvwxyz',
                          'ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>

  <xsl:strip-space elements="environment variable paths"/>

  <xsl:template match="variable">
    <xsl:if test="name = $uppercase-name">
      <plist version="1.0">
        <dict>
          <key>Label</key>
          <string><xsl:value-of select="$label"/></string>

          <key>RunAtLoad</key>
          <true/>

          <key>ProgramArguments</key>
          <array>
            <string>/bin/launchctl</string>
            <string>setenv</string>
            <xsl:apply-templates/>
          </array>
        </dict>
      </plist>
    </xsl:if>
  </xsl:template>

  <xsl:template match="name|value">
    <string><xsl:apply-templates/></string>
  </xsl:template>

  <xsl:template match="path[preceding-sibling::*]">
    <xsl:text>:</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="current-value" name="current-value">
    <xsl:value-of select="$current-value"/>
  </xsl:template>

  <xsl:template match="current-value[preceding-sibling::*]">
    <xsl:if test="$current-value">
      <xsl:text>:</xsl:text>
      <xsl:call-template name="current-value"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="value[paths]/text()"/>
</xsl:stylesheet>
