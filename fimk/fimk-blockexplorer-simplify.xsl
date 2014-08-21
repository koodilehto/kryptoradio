<?xml version="1.0" encoding="UTF-8"?>
<!-- Simplifies FIMK blockexplorer HTML to consume less bandwidth -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" omit-xml-declaration="yes" indent="no" />
  <xsl:strip-space elements="*" />

  <!-- Start from the payload, skip headers -->
  <xsl:template match="/">
    <h1>Block</h1>
    <table>
    <xsl:for-each select="/html/body/div[@class='container']">
      <xsl:apply-templates />
    </xsl:for-each>
    </table>
  </xsl:template>

  <!-- Strip extra data -->
  <xsl:template match="@class|tbody/tr[th]|h1|h3|table|tr[td='Next Block']|tr[td='Number Of Transactions']|tr[td='Total Amount']|tr[td='Total Fees']|tr[td='Previous Block' and ../tr/th='Summary']" />

  <!-- Strip some extra tags (such as links) but preserve contents -->
  <xsl:template match="a|small|strong|div|comment()|tbody">
    <xsl:apply-templates />      
  </xsl:template>

  <!-- Strip extra "Bytes" from byte counter -->
  <xsl:template match="td[preceding-sibling::td = 'Payload Size']">
    <td><xsl:value-of select="substring-before(.,' ')" /></td>
  </xsl:template>

  <!-- Compact two separate tables for summary and hashes into one -->
  <xsl:template match="table[tbody/tr/th]">
    <xsl:apply-templates select="tbody" />
  </xsl:template>

  <!-- Take only IDs from transactions -->
  <xsl:template match="table[not(tbody/tr/th) and tbody/tr]">
    <tr>
      <td>Transactions</td>
      <td><ul>
	<xsl:for-each select="tbody/tr">
	  <li><xsl:apply-templates select="td[2]"/></li>
	</xsl:for-each>
      </ul></td>
    </tr>
  </xsl:template>

  <!-- Keep the rest -->
  <xsl:template match="node()|@*"> 
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template> 

</xsl:stylesheet>
