<?xml version="1.0" encoding="UTF-8"?>
<!-- Simplifies FIMK blockexplorer transaction list -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" omit-xml-declaration="yes" indent="no" />
  <xsl:strip-space elements="*" />

  <!-- Header and table rows are mapped into CSV records -->
  <xsl:template match="h1|tr">
    <xsl:apply-templates />
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Table cells and lists are mapped into CSV fields (comma separated) -->
  <xsl:template match="th[not(position() = last())]|td[not(position() = last())]|li[not(position() = last())]">
    <xsl:apply-templates />
    <xsl:text>,</xsl:text>
  </xsl:template>

  <!-- Change all text to upper case which makes compression rate
       higher (less unique characters) -->
  <xsl:template match="text()">
    <xsl:value-of select="translate(.,
                          'abcdefghijklmnopqrstuvwxyz',
                          'ABCDEFGHIJKLMNOPQRSTUVWXYZ')" />
  </xsl:template> 
</xsl:stylesheet>
