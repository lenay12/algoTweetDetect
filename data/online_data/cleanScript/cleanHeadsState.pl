#!/opt/local/bin/perl
use strict;
my $url = 'https://en.wikipedia.org/wiki/List_of_current_heads_of_state_and_government';
my $file = "webpage/headsOfState";
system "wget -O $file $url";
my $needCountryPrev = 0;

my $text;
my @lines;
open(FILE, "$file");
@lines = <FILE>;
close(FILE);
$text = join('', @lines);
$text=~s/.*?>(Afghanistan.*)/$1/gsmi;
print "$text\n";
$text=~s/<h2><.*?>other states.*?<\/h2>.*//gsmi;
$text=~s/<\/tr>/\*\*ENDROW\*\*/gsmi;
$text=~s/<\/td>/##COL##/gsmi;
$text=~s/##COL##\*\*ENDROW\*\*/\*\*ENDROW\*\*/gsmi;
$text=~s/<.*?>//gsmi;
$text =~ s/&#160;//gsmi;
$text =~ s/&.*?;//gsmi;
$text =~ s/–/,/gsmi;

#$text=~s/\n//gsmi;
$text=~s/\r//gsmi;
$text=~s/\n/,/gsmi;
$text=~s/##COL##/,/gsmi;
$text=~s/\*\*ENDROW\*\*/\n/gsmi;
$text=~s/^,,//gsmi;
$text=~s/^,//gsmi;
$text !~ s/[^[:ascii:]]//gsmi;
$text =~ s/(^|\n)[\n\s]*/$1/g;

#if(1==0){
@lines = split('\n', $text);
my $prevCountry ="";
my @deleteElements;
my $found = 0;
my @swisslookup;
foreach my $i (0 .. $#lines){
  if($lines[$i]=~m/Switzerland,/gi){
    push @deleteElements, $i;

    $prevCountry = "Switzerland,,Federal Council";



  } elsif($lines[$i]=~m/Federal Council/gi){
    push @deleteElements, $i;
    push @swisslookup, $i+1;
    push @swisslookup, $i+2;
  } else {
    if ($i ~~ @swisslookup){

	$lines[$i] = $prevCountry .",".$lines[$i];
      } else {
      }
  }
}
@deleteElements = sort{ $b <=> $a } @deleteElements;
foreach(@deleteElements){

  splice @lines, $_, 1;

}
foreach my $i (0 .. $#lines){
  chomp $lines[$i];


#  $lines[$i]=~ s/^((\w+\s*)+?)(,.*?Republic of the)/$1/gi;
  $lines[$i]=~ s/^((\w+\s*)+?)(,.*?the)/$1/gi;
  $found = $lines[$i]=~ m/^((\w+-*\s*)+?),,/gi;
  my $tmp = $1;
  if(length($tmp)>0){
    $prevCountry = $tmp;
  }
  if($found ==1){
  } else{

    $lines[$i] = $prevCountry .",,".$lines[$i];

  }

  $lines[$i] = $lines[$i]."\n";
 # print "$lines[$i]";

}

print "Array elements to be deleted\n";
foreach(@deleteElements){
  print "$_\n";
}

$text = join(/\n/, @lines);
my $filename = '../HeadsOfState.csv';
open(FH, '>', $filename) or die $!;
print FH "Country,, Head of State, Name,,, Head of Government\n";
print FH "$text\n";
close(FH);
#}

#print "$text\n";
