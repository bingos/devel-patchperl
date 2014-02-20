use strict;
use warnings;
use feature qw[say];
use File::Spec;
use Devel::PatchPerl::Hints;
use MIME::Base64;
use Text::Diff;

my $path = shift || die "No Path specified to a perl source\n";
die "Not a perl repository\n" unless -d $path or -d File::Spec->catdir($path,'.git') or -d File::Spec->catdir($path,'hints');
my $hintsdir = File::Spec->catdir($path,'hints');

foreach my $os ( sort Devel::PatchPerl::Hints->hints() ) {
  my ($file,$data) = Devel::PatchPerl::Hints->hint_file( $os );
  $data = encode_base64( $data );
  my $fdata;
  {
    open my $fh, '<', File::Spec->catfile( $hintsdir, $file );
    local $/ = undef;
    $fdata = encode_base64(<$fh>)
  }
  chomp $fdata;
  say "'$os' =>\n'$fdata',"; #if diff \$data, \$fdata;
}
