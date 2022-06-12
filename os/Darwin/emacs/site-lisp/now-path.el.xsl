<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exsl="http://exslt.org/common"
                xmlns:set="http://exslt.org/sets"
                xmlns:str="http://exslt.org/strings"
                extension-element-prefixes="exsl set str">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="home"/>
  <xsl:param name="path" select="'/usr/bin:/bin:/usr/sbin:/sbin'"/>

  <xsl:strip-space elements="environment variable paths"/>

  <xsl:template match="variable[name='PATH']">
    <xsl:variable name="paths">
      <xsl:apply-templates mode="path"/>
    </xsl:variable>
    <xsl:text>;;; now-path.el --- PATH for NS  -*- lexical-binding:t -*-

;;; Code:

(let ((path
       '(</xsl:text>
    <xsl:for-each select="set:distinct(exsl:node-set($paths)/path)">
      <xsl:if test="position() > 1">
        <xsl:text>&#x0a;         </xsl:text>
      </xsl:if>
      <xsl:value-of select="."/>
    </xsl:for-each>
    <xsl:text>)))
  (setenv "PATH" (string-join path ":"))
  (setq exec-path (nconc path exec-path)))

(provide 'now-path)

;;; now-path.el ends here&#x0a;</xsl:text>
  </xsl:template>

  <xsl:variable name="replacements-rfc">
    <replace><what>\</what><with>\\</with></replace>
    <replace><what>"</what><with>\"</with></replace>
  </xsl:variable>

  <xsl:variable name="replacements"
                select="exsl:node-set($replacements-rfc)/replace"/>

  <xsl:template mode="path" match="path" name="path">
    <path>
      <xsl:text>"</xsl:text>
      <xsl:variable name="replaced" select="str:replace(., $replacements/what,
                                              $replacements/with)"/>
      <xsl:choose>
        <xsl:when test="$home">
          <xsl:value-of select="str:replace($replaced, '~/',
                                  concat($home, '/'))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$replaced"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>"</xsl:text>
    </path>
  </xsl:template>

  <xsl:template mode="path" match="current-value" name="current-value">
    <xsl:for-each select="str:split($path, ':')">
      <xsl:call-template name="path"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template mode="path" match="name"/>

  <xsl:template match="variable"/>
</xsl:stylesheet>
