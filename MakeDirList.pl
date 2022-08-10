#!/usr/bin/perl
my $cmd="find -mindepth 1 -maxdepth 1 -type d";
my $res=`$cmd`;
while ($res =~ /\/(\S+)/g) {
    print $1 . " ";
}
print "\n";
