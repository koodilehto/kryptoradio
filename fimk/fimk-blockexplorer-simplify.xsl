<?xml version="1.0" encoding="UTF-8"?>
<!-- Simplifies FIMK blockexplorer HTML to consume less bandwidth -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" omit-xml-declaration="yes" indent="no" />
  <xsl:strip-space elements="*" />

  <!-- Start from the payload, skip headers -->
  <xsl:template match="/">
    <xsl:for-each select="/html/body/div[@class='container']">
	<xsl:apply-templates />
    </xsl:for-each>
  </xsl:template>

  <!-- Strip extra data -->
  <xsl:template match="@class|tbody/tr[th]|h1[2]|h3" />

  <!-- Strip some extra tags (such as links) but preserve contents -->
  <xsl:template match="a|small|strong|div|comment()">
    <xsl:apply-templates />      
  </xsl:template>

  <xsl:template match="table">
    <h2>
      <xsl:choose>
	<xsl:when test="tbody/tr/th">
	  <xsl:value-of select="tbody/tr/th" />
	</xsl:when>
	<xsl:otherwise>Transactions</xsl:otherwise>
      </xsl:choose>
    </h2>

    <table><xsl:apply-templates /></table>
  </xsl:template>

  <!-- Keep the rest -->
  <xsl:template match="node()|@*"> 
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template> 

</xsl:stylesheet>
