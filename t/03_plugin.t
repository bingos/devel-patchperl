use strict;
use warnings;
use lib 't/lib';

use Test::More qq'no_plan';
use File::Spec;
use Devel::PatchPerl;

{
  local $ENV{PERL5_PATCHPERL_PLUGIN} = 'TEST';
  my $result = Devel::PatchPerl::_process_plugin(version => '5.14.2');
  ok( $result, 'The result was okay' );
}

{
  local $ENV{PERL5_PATCHPERL_PLUGIN} = 'TEST,TEST2';
  my $result = Devel::PatchPerl::_process_plugin(version => '5.14.2');
  ok( $result, 'The result was okay' );
}
