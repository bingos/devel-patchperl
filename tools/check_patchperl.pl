use 5.012;
use Perl::Version;
use Git::Repository;
use ExtUtils::MakeMaker;

Git::Repository->run( clone => 'https://github.com/Perl/perl5.git', 'perl.git' ) unless -d 'perl.git';
my $r = Git::Repository->new( work_tree => 'perl.git' );
my @tags = grep { m!^(perl-|v)5\.! && !m!5\.00! && !m!-RC\d+$! && !m!^\Qv5.17.7.0\E$! && !m!^perl-5\.10\.! } $r->run( tag => '-l' );
{
  my @ptags = grep { m!^perl! } @tags;
  my @vtags = sort { Perl::Version->new($a)->numify <=> Perl::Version->new($b)->numify } grep { m!^v! } @tags;
  @tags = ( @ptags, @vtags );
}

foreach my $tag ( @tags ) {
  $r->run( checkout => $tag );
  my $value;
  system( $^X, '-Ilib', 'bin/patchperl', 'perl.git' ) == 0 or $value = prompt("Continue [Y/n]?","y");
  last if $value =~ m!^n!i;
  $r->run( reset => '--hard' );
  $r->run( clean => '-dxf' );
}

$r->run( reset => '--hard' );
$r->run( clean => '-dxf' );
$r->run( checkout => 'blead' );
