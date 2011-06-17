#!/usr/bin/perl

print 'val embed = "----- BEGIN EMBEDDED BUILD INFO -----\n';
while(<>) {
  chomp;
  print $_ .  '\n';
}
print '----- END EMBEDDED BUILD INFO -----\n";' . "\n";
print "TextIO.outputSubstr(TextIO.stdOut, Substring.substring(embed, 0, 0));\n";
