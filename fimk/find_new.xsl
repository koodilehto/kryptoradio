<?xml version="1.0" encoding="UTF-8"?>
<!-- Simplifies FIMK blockexplorer HTML to consume less bandwidth -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" omit-xml-declaration="yes" indent="no" />
  <xsl:strip-space elements="*" />
  <xsl:param name="last_block_url"/>

  <xsl:template match="/">
    <!-- If block ID is not found, we're screwed -->
    <xsl:if test="not(id('blocks')/tbody/tr/td[1]/a/@href=$last_block_url)">
      <xsl:message terminate="yes">
	<xsl:text>Given block is not any of 6 last blocks. Try </xsl:text>
        <xsl:value-of select="id('blocks')/tbody/tr/td[1]/a/@href" />
      </xsl:message>
    </xsl:if>

    <!-- Take the next block ID after the last known -->
    <xsl:for-each select="id('blocks')/tbody/tr[td[1]/a/@href=$last_block_url]">
      <xsl:value-of select="preceding-sibling::tr[1]/td[1]/a/@href"/>
    </xsl:for-each>
    <xsl:text>&#10;</xsl:text>

    <!-- Output latest transaction ID -->
    <xsl:value-of select="id('txs')/tbody/tr[2]/td[1]" />
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
