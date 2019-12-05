#!/opt/local/bin/perl
use strict;

my $url ='https://en.wikipedia.org/wiki/Independent_agencies_of_the_United_States_government';
my $file ='webpage/indyFedAgencies';

#system "wget -O $file $url";
my $text;
my @lines;
open(FILE, "$file");
@lines = <FILE>;
close(FILE);
$text = join('', @lines);
$text=~s/.*<h2.*?>(Examples of agencies.*)/$1/gsmi;
$text =~s/<h3.*?>Former agencies.*//gsmi;

$text=~s/Edit//gsm;
$text=~s/<.*?>//gsmi;
$text=~s/\[.*?\]//gsmi;
$text =~ s/&#160;//gsmi;
$text =~ s/&.*?;//gsmi;
my @cleantext = split "\n", $text;
my $fullname;
my $acronym;
$text="Agency Name, Acronym, Head\n";
foreach my $line (@cleantext){
  chomp;
  $line=~/^(.*?)\((.*?)\).*/gi;
  $fullname = $1;
  $acronym = $2;
  if($acronym=~m/\"/gi){
    #fix fed's formatting
    $acronym =~ s/.*"(.*)".*/$1/gi;
    $fullname=~s/(.*?)\s[a-z].*/$1/g;
  }elsif($fullname=~m/:/gi){
    $fullname=~s/.*:\s(.*)/$1/gi;
  }
  $text = $text . $fullname . "," . $acronym ."\n"
}



my $filename = '../IndyAgency.csv';
open(FH, '>', $filename) or die $!;

print FH $text, "\n";
close(FH);
