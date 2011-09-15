use strict;
use warnings;
use Test::More qq'no_plan';
use File::Spec;
use Devel::PatchPerl;
my %vers = (
  File::Spec->catdir( 't', 'modern' )           => '5.15.1',
  File::Spec->catdir( 't', 'old' )              => '5.005_04',
  File::Spec->catdir( 't', 'perl-5.8.9' )       => '5.8.9',
  File::Spec->catdir( 't', 'perl-5.10.1' )      => '5.10.1',
);
foreach my $dir ( keys %vers ) {
  my $vers = Devel::PatchPerl::_determine_version($dir);
  ok($vers, 'Got a version');
  is($vers, $vers{$dir}, 'Yes, it is ' . $vers{$dir});
}
