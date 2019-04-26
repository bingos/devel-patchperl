use strict;
use warnings;
use lib 't/lib';
BEGIN {
  $ENV{PERL5_PATCHPERL_PLUGIN} = 'BOLLUCKS';
}
use Test::More qq'no_plan';
use File::Spec;
use Devel::PatchPerl;
my $result = Devel::PatchPerl::_process_plugin(version => '5.14.2');
ok( !$result, 'The result was okay' );
