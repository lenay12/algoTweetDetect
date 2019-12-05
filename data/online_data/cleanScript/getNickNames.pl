#!/opt/local/bin/perl
use strict;

my $url ='https://en.m.wikipedia.org/wiki/List_of_nicknames_used_by_Donald_Trump';
my $file ='webpage/nickNamesByTrump';

#system "wget -O $file $url";

my $text;
my @lines;
open(FILE, "$file");
@lines = <FILE>;
close(FILE);
$text = join('', @lines);

$text=~s/.*?>(Personal name.*)/$1/gsmi;
$text=~s/<div role="navigation".*//gsmi;
$text=~s/\r//gsmi;
$text=~s/\n/,/gsmi;
$text=~s/<\/tr>/\*\*ENDROW\*\*/gsmi;
$text=~s/<\/td>/##COL##/gsmi;
$text=~s/##COL##\*\*ENDROW\*\*/\*\*ENDROW\*\*/gsmi;
$text=~s/Edit//gsm;
$text=~s/<.*?>//gsmi;
$text=~s/\[.*?\]//gsmi;
$text =~ s/&#160;//gsmi;
$text =~ s/&.*?;//gsmi;
$text=~s/##COL##/,/gsmi;
$text=~s/\*\*ENDROW\*\*/\n/gsmi;
$text=~s/,\n/\n/gsmi;
$text=~s/,,,/,,/gsmi;
$text=~s/\//\n/gsmi;


my @cleantext = split "\n", $text;
$text = "";
my $nickname2name;
my $name2description;
my $currentTopic;
my $tmp;
my $nickname;
my $realname;
my $notes;
foreach my $line (@cleantext){
  chomp;
  $line=~s/^\s+|\s+$//g;
  if($line=~m/(.*?),Nickname,,/gi){
    $currentTopic = $1;
  }elsif($line=~m/Personal name,,Notes/gi){
    $currentTopic = "Himself";
  } else {
  #  $text = $line . "\t" . $currentTopic ."\n";
    $tmp = $line =~ m/(.*?),,(.*?),,(.*)/gi;
    if($tmp == 1){
      $nickname= $1;
      $realname= $2;
      $notes = $3;
 #     print "$1,$2,'$3'\n";
    }else {
      $nickname = $line;
    }
    $text =  $text . $nickname .",". $realname . "," . $currentTopic . ",'". $notes . "'\n";
  }
#  print "$currentTopic\n";
}


#print $1, "\n";
my $filename = '../nicknames.csv';
open(FH, '>', $filename) or die $!;
print FH "Nickname,Realname,Topic,Notes\n";
print FH $text, "\n";
close(FH);


