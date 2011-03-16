use strict;
use warnings;
use Test::More qw[no_plan];
use Devel::PatchPerl::Hints qw[hint_file];

{
  my $content = hint_file('freebsd');
  ok( $content, 'We got content' );
  like( $content, qr/Anton/si, 'Reasonable' );
}

{
  my ($file,$content) = hint_file('freebsd');
  ok( $content, 'We got content' );
  like( $content, qr/Anton/si, 'Reasonable' );
  is( $file, 'freebsd.sh', 'Hints file name okay' );
}

{
  my $content = Devel::PatchPerl::Hints->hint_file('freebsd');
  ok( $content, 'We got content' );
  like( $content, qr/Anton/si, 'Reasonable' );
}

{
  my ($file,$content) = Devel::PatchPerl::Hints->hint_file('freebsd');
  ok( $content, 'We got content' );
  like( $content, qr/Anton/si, 'Reasonable' );
  is( $file, 'freebsd.sh', 'Hints file name okay' );
}

