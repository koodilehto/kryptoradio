<?xml version="1.0" encoding="UTF-8"?>
<!-- Simplifies FIMK blockexplorer transaction list -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" omit-xml-declaration="yes" indent="no" />
  <xsl:strip-space elements="*" />
  <xsl:param name="last_tx"/>

  <!-- Start from the payload, skip headers -->
  <xsl:template match="/">
    <xsl:for-each select="//table">
      <h1>Transactions</h1>
      <table>
	<xsl:apply-templates />
      </table>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="tbody">
    <xsl:for-each select="tr[td[2]/a=$last_tx]">
      <xsl:apply-templates select="preceding-sibling::tr"/>
    </xsl:for-each>
  </xsl:template>

  <!-- Strip extra data -->
  <xsl:template match="@class|comment()" />

  <!-- Strip some extra tags (such as links) but preserve contents -->
  <xsl:template match="a">
    <xsl:apply-templates />      
  </xsl:template>

  <!-- Keep the rest -->
  <xsl:template match="node()|@*"> 
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template> 

</xsl:stylesheet>
