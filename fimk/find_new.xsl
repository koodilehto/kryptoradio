<?xml version="1.0" encoding="UTF-8"?>
<!-- Simplifies FIMK blockexplorer HTML to consume less bandwidth -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" omit-xml-declaration="yes" indent="no" />
  <xsl:strip-space elements="*" />
  <xsl:param name="last_block_url"/>

  <xsl:template match="/">
    <!-- Take a block ID which has next higher block ID than the last known -->
    <xsl:variable name="greater" select="id('blocks')/tbody/tr/td[1]/a/@href[number(substring(.,9)) &gt; number(substring($last_block_url,9))]" />
    <xsl:value-of select="$greater[last()]" />
    <xsl:text>&#10;</xsl:text>

    <!-- Output latest transaction ID -->
    <xsl:value-of select="id('txs')/tbody/tr[2]/td[1]" />
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
