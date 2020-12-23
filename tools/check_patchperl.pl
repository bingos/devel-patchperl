use 5.012;
use Perl::Version;
use Git::Repository;
use ExtUtils::MakeMaker;
use Capture::Tiny qw[capture_merged];

Git::Repository->run( clone => 'https://github.com/Perl/perl5.git', 'perl.git' ) unless -d 'perl.git';
my $r = Git::Repository->new( work_tree => 'perl.git', { quiet => 1 } );
my @tags = grep { m!^(perl-|v)5\.! && !m!5\.00! && !m!-RC\d+$! && !m!^\Qv5.17.7.0\E$! && !m!^perl-5\.10\.! } $r->run( tag => '-l' );
{
  my @ptags = grep { m!^perl! } @tags;
  my @vtags = sort { Perl::Version->new($a)->numify <=> Perl::Version->new($b)->numify } grep { m!^v! } @tags;
  @tags = ( @ptags, @vtags );
}

foreach my $tag ( reverse @tags ) {
  say "Checking $tag ...";
  $r->run( checkout => $tag );
  my $ret; my $value;
  my $merged = capture_merged {
    $ret = system( $^X, '-Ilib', 'bin/patchperl', 'perl.git' );
  };
  unless ( $ret == 0 ) {
    print $merged;
    $value = prompt("Continue [Y/n]?","y") unless $ret == 0;
  }
  $r->run( reset => '--hard' );
  $r->run( clean => '-dxf' );
  last if $value =~ m!^n!i;
}

$r->run( reset => '--hard' );
$r->run( clean => '-dxf' );
$r->run( checkout => 'blead' );
