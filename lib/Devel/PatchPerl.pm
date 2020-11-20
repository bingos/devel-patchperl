package Devel::PatchPerl;

# ABSTRACT: Patch perl source a la Devel::PPPort's buildperl.pl

use strict;
use warnings;
use File::pushd qw[pushd];
use File::Spec;
use IO::File;
use Devel::PatchPerl::Hints qw[hint_file];
use MIME::Base64 qw[decode_base64];
use Module::Pluggable search_path => ['Devel::PatchPerl::Plugin'];
use vars qw[@ISA @EXPORT_OK];

use constant CERTIFIED => 5.031004; # Anything less than this
use constant HINTSCERT => 5.033004; # Hints certified to this

@ISA       = qw(Exporter);
@EXPORT_OK = qw(patch_source);

my $patch_exe = _can_run('gpatch') || _can_run('patch');

my @patch = (
  {
    perl => [
              qw/
                5.005
              /,
            ],
    subs => [
              [ \&_patch_5_005, 1 ],
            ],
  },
  {
    perl => [
              qw/
                5.005_01
              /,
            ],
    subs => [
              [ \&_patch_5_005_01, 1 ],
            ],
  },
  {
    perl => [
              qw/
                5.005_02
              /,
            ],
    subs => [
              [ \&_patch_5_005_02, 1 ],
            ],
  },
  {
    perl => [
              qr/^5\.00[2345]/,
              qw/
                5.001n
              /,
            ],
    subs => [
              [ \&_patch_handy, 1 ],
            ],
  },
  {
    perl => [
              qw/
                5.005
                5.005_01
                5.005_02
                5.005_03
                5.005_04
              /,
            ],
    subs => [
              [ \&_replace_makedepend, 1 ],
            ],
  },
  {
    perl => [
              qr/^5\.00[01234]/,
              qw/
                5.005
                5.005_01
                5.005_02
                5.005_03
              /,
            ],
    subs => [
              [ \&_patch_db, 1 ],
            ],
  },
  {
    perl => [
              qr/^5\.6\.[1-2]$/,
              qr/^5\.7\.[0-1]$/,
            ],
    subs => [
              [ \&_patch_makefile_sh_phony ],
            ],
  },
  {
    perl => [
            qw/
                5.6.0
                5.6.1
                5.7.0
                5.7.1
                5.7.2
                5.7.3
                5.8.0
            /,
            ],
    subs => [
              [ \&_patch_db, 3 ],
            ],
  },
  {
    perl => [
              qr/^5\.004_0[1234]$/,
            ],
    subs => [
              [ \&_patch_doio ],
            ],
  },
  {
    perl => [
              qw/
                5.005
                5.005_01
                5.005_02
              /,
            ],
    subs => [
              [ \&_patch_sysv, old_format => 1 ],
            ],
  },
  {
    perl => [
              qw/
                5.005_03
                5.005_04
              /,
              qr/^5\.6\.[0-2]$/,
              qr/^5\.7\.[0-3]$/,
              qr/^5\.8\.[0-8]$/,
              qr/^5\.9\.[0-5]$/
            ],
    subs => [
              [ \&_patch_sysv, old_format => 0 ],
            ],
  },
  {
    perl => [
              qr/^5\.004_05$/,
              qr/^5\.005(?:_0[1-4])?$/,
              qr/^5\.6\.[01]$/,
            ],
    subs => [
              [ \&_patch_configure ],
              [ \&_patch_makedepend_lc ],
            ],
  },
  {
    perl => [
              qr/^5\.6\.[0-2]$/,
            ],
    subs => [
              [ \&_patch_conf_gconvert ],
              [ \&_patch_sort_N ],
            ],
  },
  {
    perl => [
              '5.8.0',
            ],
    subs => [
              [ \&_patch_makedepend_lc ],
            ],
  },
  {
    perl => [
              qr/.*/,
            ],
    subs => [
              [ \&_patch_conf_solaris ],
              [ \&_patch_bitrig ],
              [ \&_patch_patchlevel ],
              [ \&_patch_develpatchperlversion ],
              [ \&_patch_errno_gcc5 ],
              [ \&_patch_conf_fwrapv ],
              [ \&_patch_utils_h2ph ],
              [ \&_patch_lib_h2ph ],
              [ \&_patch_sdbm_file_c ],
              [ \&_patch_mmaix_pm ],
              [ \&_patch_time_local_t ],
              [ \&_patch_pp_c_libc ],
              [ \&_patch_conf_gcc10 ],
            ],
  },
  {
    perl => [
              qr/^5\.6\.[0-2]$/,
              qr/^5\.7\.[0-3]$/,
              qr/^5\.8\.[0-8]$/,
              qr/^5\.9\.[0-4]$/,
            ],
    subs => [
              [ \&_patch_makedepend_SH ],
            ],
  },
  {
    perl => [
              qr/^5\.1[0-2]/,
            ],
    subs => [
              [ \&_patch_archive_tar_tests ],
              [ \&_patch_odbm_file_hints_linux ],
            ],
  },
  {
    perl => [
              qr/^5.1([24].\d+|0.1)/,
            ],
    subs => [
              [ \&_patch_make_ext_pl ],
            ],
  },
  {
    perl => [ qr/^5\.8\.9$/, ],
    subs => [ [ \&_patch_589_perlio_c ], ],
  },
  {
    perl => [ qr/^5\.8\.[89]$/ ],
    subs => [ [ \&_patch_hsplit_rehash_58 ] ],
  },
  {
    perl => [
              qr/^5\.10\.1$/,
              qr/^5\.12\.5$/,
            ],
    subs => [ [ \&_patch_hsplit_rehash_510 ] ],
  },
  {
    perl => [
              qr/^5\.18\.0$/,
            ],
    subs => [ [ \&_patch_regmatch_pointer_5180 ] ],
  },
  {
    perl => [
              qr/^5\.20\.0$/,
            ],
    subs => [ [ \&_patch_cow_speed ] ],
  },
  {
    perl => [
              qr/^5\.6\.[012]$/,
              qr/^5\.8\.[89]$/,
              qr/^5\.10\.[01]$/,
            ],
    subs => [ [ \&_patch_preprocess_options ] ],
  },
  {
    perl => [
              qr/^5\.18\.3$/,
            ],
    subs => [ [ \&_patch_5183_metajson ] ],
  },
  {
    perl => [
              qr/^5\.24\.[012]$/,
            ],
    subs => [ [ \&_patch_time_hires ] ],
  },
  {
    perl => [
              qr/^5\.24\.3$/,
              qr/^5\.25\.(?:[4-9]|10)$/,
              qr/^5\.26\.[01]$/,
              qr/^5\.27\.[0-4]$/,
            ],
    subs => [ [ \&_patch_fp_class_denorm ] ],
  },
  {
    perl => [
              qr/^5\.28\.[01]$/,
            ],
    subs => [ [ \&_patch_useshrplib ] ],
  },
);

sub patch_source {
  my $vers = shift;
  $vers = shift if eval { $vers->isa(__PACKAGE__) };
  my $source = shift || '.';
  if ( !$vers ) {
    $vers = _determine_version($source);
    if ( $vers ) {
      warn "Auto-guessed '$vers'\n";
    }
    else {
      die "You didn't provide a perl version and I don't appear to be in a perl source tree\n";
    }
  }
  my $normver = _norm_ver( $vers );
  $source = File::Spec->rel2abs($source);
  if ( $normver <  HINTSCERT ) {
    my $dir = pushd( $source );
    _patch_hints();
  }
  if ( $normver >= CERTIFIED ) {
      warn "Nothing else to do, '$vers' is fine\n";
      return;
  }
  {
    my $dir = pushd( $source );
    for my $p ( grep { _is( $_->{perl}, $vers ) } @patch ) {
       for my $s (@{$p->{subs}}) {
         my($sub, @args) = @$s;
         push @args, $vers unless scalar @args;
         $sub->(@args);
       }
    }
    _process_plugin( version => $vers, source => $source, patchexe => $patch_exe );
  }
}

sub _process_plugin {
  my %args = @_;
  return unless my $possible = $ENV{PERL5_PATCHPERL_PLUGIN};
  my ($plugin) = grep { $possible eq $_ or /\Q$possible\E$/ } __PACKAGE__->plugins;
  unless ( $plugin ) {
    warn "# You specified a plugin '", $ENV{PERL5_PATCHPERL_PLUGIN},
         "' that isn't installed, just thought you might be interested.\n";
    return;
  }
  {
    local $@;
    eval "require $plugin";
    if ($@) {
      die "# I tried to load '", $ENV{PERL5_PATCHPERL_PLUGIN},
          "' but it didn't work out. Here is what happened '$@'\n";
    }
  }
  {
    local $@;
    eval {
      $plugin->patchperl(
        %args,
      );
    };
    if ($@) {
      warn "# Warnings from the plugin: '$@'\n";
    }
  }
  return 1;
}

sub _can_run {
    my $command = shift;

    # a lot of VMS executables have a symbol defined
    # check those first
    if ( $^O eq 'VMS' ) {
        require VMS::DCLsym;
        my $syms = VMS::DCLsym->new;
        return $command if scalar $syms->getsym( uc $command );
    }

    require File::Spec;
    require ExtUtils::MakeMaker;

    my @possibles;

    if( File::Spec->file_name_is_absolute($command) ) {
        return MM->maybe_command($command);

    } else {
        for my $dir (
            File::Spec->path,
            File::Spec->curdir
        ) {
            next if ! $dir || ! -d $dir;
            my $abs = File::Spec->catfile( $^O eq 'MSWin32' ? Win32::GetShortPathName( $dir ) : $dir, $command);
            push @possibles, $abs if $abs = MM->maybe_command($abs);
        }
    }
    return @possibles if wantarray;
    return shift @possibles;
}

sub _is
{
  my($s1, $s2) = @_;

  defined $s1 != defined $s2 and return 0;

  ref $s2 and ($s1, $s2) = ($s2, $s1);

  if (ref $s1) {
    if (ref $s1 eq 'ARRAY') {
      _is($_, $s2) and return 1 for @$s1;
      return 0;
    }
    return $s2 =~ $s1;
  }

  return $s1 eq $s2;
}

sub _patch_b64 {
  my($base64) = @_;
  my $patch = decode_base64( $base64 );
  _patch( $patch );
}

sub _patch
{
  my($patch) = @_;
  my %mode;
  for my $file ($patch =~ /^\+{3}\s+(\S+)/gm) {
    print "patching $file\n";
    # some filesystems (e.g., Lustre) will kill this process if there
    # is an attempt to write to a file that is 0444, so make these
    # files writable for the duration of the patch
    if (-r $file and not -w _) {
      my $mode = (stat $file)[2];
      $mode{$file} = $mode; # save for chmod back
      chmod $mode | 0200, $file;
    }
  }
  my $diff = 'tmp.diff';
  _write_or_die($diff, $patch);
  die "No patch utility found\n" unless $patch_exe;
  local $ENV{PATCH_GET} = 0; # I can't reproduce this at all, but meh.
  _run_or_die("$patch_exe -f -s -p0 <$diff");
  unlink $diff or die "unlink $diff: $!\n";
  # put back ro to 0444
  for my $file (sort keys %mode) {
    chmod $mode{$file}, $file;
  }
}

sub _write_or_die
{
  my($file, $data) = @_;
  my $fh = IO::File->new(">$file") or die "$file: $!\n";
  $fh->print($data);
}

sub _run_or_die
{
  # print "[running @_]\n";
  die unless system( @_ ) == 0;
}

sub determine_version {
  my $src = shift;
  $src = shift if eval { $src->isa(__PACKAGE__) };
  $src = '.' unless $src;
  _determine_version($src);
}

sub _determine_version {
  my ($source) = @_;
  my $patchlevel_h = File::Spec->catfile($source, 'patchlevel.h');
  return unless -e $patchlevel_h;
  my $version;
  {
    my %defines;
    open my $fh, '<', $patchlevel_h;
    my @vers;
    while (<$fh>) {
      chomp;
      next unless /^#define/;
      my ($foo,$bar) = ( split /\s+/ )[1,2];
      $defines{$foo} = $bar;
    }
    if ( my @wotsits = grep { defined $defines{$_} } qw(PERL_REVISION PERL_VERSION PERL_SUBVERSION) ) {
      $version = join '.', map { $defines{$_} } @wotsits;
    }
    elsif ( my @watsits = grep { defined $defines{$_} } qw(PATCHLEVEL SUBVERSION) ) {
      $version = sprintf '5.%03d_%02d', map { $defines{$_} } @watsits;
    }
    else {
      return;
    }
  }
  return $version;
}

sub _patchperl_version {
  return $Devel::PatchPerl::VERSION || "(unreleased)";
}

# adapted from patchlevel.h for use with perls that predate it
sub _patch_patchlevel {
  return if -d '.git' and !$ENV{PERL5_PATCHPERL_PATCHLEVEL};
  my $dpv = $Devel::PatchPerl::VERSION || "(unreleased)";
  open my $plin, "patchlevel.h" or die "Couldn't open patchlevel.h : $!";
  open my $plout, ">patchlevel.new" or die "Couldn't write on patchlevel.new : $!";
  my $seen=0;
  while (<$plin>) {
      if (/\t,NULL/ and $seen) {
        print {$plout} qq{\t,"Devel::PatchPerl $dpv"\n};
      }
      $seen++ if /local_patches\[\]/;
      print {$plout} $_;
  }
  close $plout or die "Couldn't close filehandle writing to patchlevel.new : $!";
  close $plin or die "Couldn't close filehandle reading from patchlevel.h : $!";
  unlink "patchlevel.bak" or warn "Couldn't unlink patchlevel.bak : $!"
    if -e "patchlevel.bak";
  rename "patchlevel.h", "patchlevel.bak" or
    die "Couldn't rename patchlevel.h to patchlevel.bak : $!";
  rename "patchlevel.new", "patchlevel.h" or
    die "Couldn't rename patchlevel.new to patchlevel.h : $!";
}

sub _patch_hints {
  my @os;
  push @os, $^O;
  push @os, 'linux' if $^O eq 'gnukfreebsd'; # kfreebsd uses linux hints
  foreach my $os ( @os ) {
    return unless my ($file,$data) = hint_file( $os );
    my $path = File::Spec->catfile( 'hints', $file );
    warn "Patching '$path'\n";
    if ( -e $path ) {
      chmod 0644, $path or die "$!\n";
    }
    open my $fh, '>', $path or die "$!\n";
    print $fh $data;
    close $fh;
  }
  return 1;
}

sub _patch_db
{
  my $ver = shift;
  for my $file ('ext/DB_File/DB_File.xs', 'Configure') {
    print "patching $file\n";
    _run_or_die($^X, '-pi.bak', '-e', "s/<db.h>/<db$ver\\/db.h>/", $file);
    unlink "$file.bak" if -e "$file.bak";
  }
}

sub _patch_doio
{
  _patch(<<'END');
--- doio.c.org  2004-06-07 23:14:45.000000000 +0200
+++ doio.c  2003-11-04 08:03:03.000000000 +0100
@@ -75,6 +75,16 @@
 #  endif
 #endif

+#if _SEM_SEMUN_UNDEFINED
+union semun
+{
+  int val;
+  struct semid_ds *buf;
+  unsigned short int *array;
+  struct seminfo *__buf;
+};
+#endif
+
 bool
 do_open(gv,name,len,as_raw,rawmode,rawperm,supplied_fp)
 GV *gv;
END
}

sub _patch_sysv
{
  my %opt = @_;

  # check if patching is required
  return if $^O ne 'linux' or -f '/usr/include/asm/page.h';

  if ($opt{old_format}) {
    _patch(<<'END');
--- ext/IPC/SysV/SysV.xs.org  1998-07-20 10:20:07.000000000 +0200
+++ ext/IPC/SysV/SysV.xs  2007-08-12 10:51:06.000000000 +0200
@@ -3,9 +3,6 @@
 #include "XSUB.h"
 
 #include <sys/types.h>
-#ifdef __linux__
-#include <asm/page.h>
-#endif
 #if defined(HAS_MSG) || defined(HAS_SEM) || defined(HAS_SHM)
 #include <sys/ipc.h>
 #ifdef HAS_MSG
END
  }
  else {
    _patch(<<'END');
--- ext/IPC/SysV/SysV.xs.org  2007-08-11 00:12:46.000000000 +0200
+++ ext/IPC/SysV/SysV.xs  2007-08-11 00:10:51.000000000 +0200
@@ -3,9 +3,6 @@
 #include "XSUB.h"
 
 #include <sys/types.h>
-#ifdef __linux__
-#   include <asm/page.h>
-#endif
 #if defined(HAS_MSG) || defined(HAS_SEM) || defined(HAS_SHM)
 #ifndef HAS_SEM
 #   include <sys/ipc.h>
END
  }
}

sub _patch_configure
{
  _patch(<<'END');
--- Configure
+++ Configure
@@ -3380,6 +3380,18 @@
 test "X$gfpthkeep" != Xy && gfpth=""
 EOSC
 
+# gcc 3.1 complains about adding -Idirectories that it already knows about,
+# so we will take those off from locincpth.
+case "$gccversion" in
+3*)
+    echo "main(){}">try.c
+    for incdir in `$cc -v -c try.c 2>&1 | \
+       sed '1,/^#include <\.\.\.>/d;/^End of search list/,$d;s/^ //'` ; do
+       locincpth=`echo $locincpth | sed s!$incdir!!`
+    done
+    $rm -f try try.*
+esac
+
 : What should the include directory be ?
 echo " "
 $echo $n "Hmm...  $c"
END
}

sub _patch_makedepend_lc
{
  _patch(<<'END');
--- makedepend.SH
+++ makedepend.SH
@@ -58,6 +58,10 @@ case $PERL_CONFIG_SH in
       ;;
 esac
 
+# Avoid localized gcc/cc messages
+LC_ALL=C
+export LC_ALL
+
 # We need .. when we are in the x2p directory if we are using the
 # cppstdin wrapper script.
 # Put .. and . first so that we pick up the present cppstdin, not
END
}


sub _patch_makedepend_SH
{
  my $perl = shift;
  SWITCH: {
  # If 5.6.0
    if ( $perl eq '5.6.0' ) {
  _patch(<<'BADGER');
--- makedepend.SH.org	2000-03-02 18:12:26.000000000 +0000
+++ makedepend.SH	2010-09-01 10:13:37.000000000 +0100
@@ -1,5 +1,5 @@
 #! /bin/sh
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -29,6 +29,13 @@
 !GROK!THIS!
 $spitshell >>makedepend <<'!NO!SUBS!'
 
+if test -d .depending; then
+	echo "$0: Already running, exiting."
+	exit 0
+fi
+
+mkdir .depending
+
 # This script should be called with 
 #     sh ./makedepend MAKE=$(MAKE)
 case "$1" in 
@@ -37,7 +44,7 @@
 
 export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)
 
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -51,6 +58,11 @@
 	;;
 esac
 
+# Avoid localized gcc messages
+case "$ccname" in
+    gcc) LC_ALL=C ; export LC_ALL ;;
+esac
+
 # We need .. when we are in the x2p directory if we are using the
 # cppstdin wrapper script.
 # Put .. and . first so that we pick up the present cppstdin, not
@@ -58,6 +70,10 @@
 PATH=".$path_sep..$path_sep$PATH"
 export PATH
 
+case "$osname" in
+amigaos) cat=/bin/cat ;; # must be absolute
+esac
+
 $cat /dev/null >.deptmp
 $rm -f *.c.c c/*.c.c
 if test -f Makefile; then
@@ -67,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -99,25 +114,20 @@
 	$echo *.c | $tr ' ' $trnl | $egrep -v '\*' >.clist)
 for file in `$cat .clist`; do
 # for file in `cat /dev/null`; do
-	if [ "$osname" = uwin ]; then
-		uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g"
-	else
-		if [ "$osname" = os2 ]; then
-			uwinfix="-e s,\\\\\\\\,/,g"
-		else
-			if [ "$archname" = cygwin ]; then
-				uwinfix="-e s,\\\\\\\\,/,g"
-			else
-				uwinfix=
-			fi
-		fi
-	fi
+    case "$osname" in
+    uwin)     uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g" ;;
+    os2)      uwinfix="-e s,\\\\\\\\,/,g" ;;
+    cygwin)   uwinfix="-e s,\\\\\\\\,/,g" ;;
+    posix-bc) uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/" ;;
+    vos)      uwinfix="-e s/\#/\\\#/" ;;
+    *)        uwinfix="" ;;
+    esac
     case "$file" in
     *.c) filebase=`basename $file .c` ;;
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -130,22 +140,45 @@
 	-e 's|\\$||' \
 	-e p \
 	-e '}' ) >UU/$file.c
+
     if [ "$osname" = os390 -a "$file" = perly.c ]; then
         $echo '#endif' >>UU/$file.c
     fi
-    $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
-    $sed \
-	-e '1d' \
-	-e '/^#.*<stdin>/d' \
-	-e '/^#.*"-"/d' \
-	-e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
-	-e 's/^[	 ]*#[	 ]*line/#/' \
-	-e '/^# *[0-9][0-9]* *[".\/]/!d' \
-	-e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
-	-e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
-	-e 's|: \./|: |' \
-	-e 's|\.c\.c|.c|' $uwinfix | \
-    $uniq | $sort | $uniq >> .deptmp
+
+    if [ "$osname" = os390 ]; then
+        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
+        $sed \
+    	    -e '/^#.*<stdin>/d' \
+	    -e '/^#.*"-"/d' \
+	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
+	    -e 's/^[	 ]*#[	 ]*line/#/' \
+	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
+	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
+	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
+	    -e 's|: \./|: |' \
+	    -e 's|\.c\.c|.c|' $uwinfix | \
+        $uniq | $sort | $uniq >> .deptmp
+    else
+        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c >.cout 2>.cerr
+        $sed \
+	    -e '1d' \
+	    -e '/^#.*<stdin>/d' \
+            -e '/^#.*<builtin>/d' \
+            -e '/^#.*<built-in>/d' \
+            -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
+	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
+	    -e '/: file path prefix .* never used$/d' \
+	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
+	    -e 's/^[	 ]*#[	 ]*line/#/' \
+	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
+	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
+	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
+	    -e 's|: \./|: |' \
+           -e 's|\.c\.c|.c|' $uwinfix .cout .cerr| \
+        $uniq | $sort | $uniq >> .deptmp
+    fi
 done
 
 $sed <$mf >$mf.new -e '1,/^# AUTOMATICALLY/!d'
@@ -177,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
@@ -208,7 +245,8 @@
 $cp $mf.new $mf
 $rm $mf.new
 $echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
-$rm -rf .deptmp UU .shlist .clist .hlist .hsed
+$rm -rf .deptmp UU .shlist .clist .hlist .hsed .cout .cerr
+rmdir .depending
 
 !NO!SUBS!
 $eunicefix makedepend
BADGER
  last SWITCH;
  }
  # If 5.6.1
    if ( $perl eq '5.6.1' ) {
  _patch(<<'BADGER');
--- makedepend.SH.org	2001-03-19 07:33:17.000000000 +0000
+++ makedepend.SH	2010-09-01 10:14:47.000000000 +0100
@@ -1,5 +1,5 @@
 #! /bin/sh
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -29,6 +29,13 @@
 !GROK!THIS!
 $spitshell >>makedepend <<'!NO!SUBS!'
 
+if test -d .depending; then
+	echo "$0: Already running, exiting."
+	exit 0
+fi
+
+mkdir .depending
+
 # This script should be called with 
 #     sh ./makedepend MAKE=$(MAKE)
 case "$1" in 
@@ -37,7 +44,7 @@
 
 export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)
 
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -51,6 +58,11 @@
 	;;
 esac
 
+# Avoid localized gcc messages
+case "$ccname" in
+    gcc) LC_ALL=C ; export LC_ALL ;;
+esac
+
 # We need .. when we are in the x2p directory if we are using the
 # cppstdin wrapper script.
 # Put .. and . first so that we pick up the present cppstdin, not
@@ -58,6 +70,10 @@
 PATH=".$path_sep..$path_sep$PATH"
 export PATH
 
+case "$osname" in
+amigaos) cat=/bin/cat ;; # must be absolute
+esac
+
 $cat /dev/null >.deptmp
 $rm -f *.c.c c/*.c.c
 if test -f Makefile; then
@@ -67,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -99,29 +114,20 @@
 	$echo *.c | $tr ' ' $trnl | $egrep -v '\*' >.clist)
 for file in `$cat .clist`; do
 # for file in `cat /dev/null`; do
-	if [ "$osname" = uwin ]; then
-		uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g"
-	else
-		if [ "$osname" = os2 ]; then
-			uwinfix="-e s,\\\\\\\\,/,g"
-		else
-			if [ "$archname" = cygwin ]; then
-				uwinfix="-e s,\\\\\\\\,/,g"
-			else
-				if [ "$osname" = posix-bc ]; then
-					uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/"
-				else
-					uwinfix=
-				fi
-			fi
-		fi
-	fi
+    case "$osname" in
+    uwin)     uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g" ;;
+    os2)      uwinfix="-e s,\\\\\\\\,/,g" ;;
+    cygwin)   uwinfix="-e s,\\\\\\\\,/,g" ;;
+    posix-bc) uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/" ;;
+    vos)      uwinfix="-e s/\#/\\\#/" ;;
+    *)        uwinfix="" ;;
+    esac
     case "$file" in
     *.c) filebase=`basename $file .c` ;;
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -134,10 +140,12 @@
 	-e 's|\\$||' \
 	-e p \
 	-e '}' ) >UU/$file.c
+
+    if [ "$osname" = os390 -a "$file" = perly.c ]; then
+        $echo '#endif' >>UU/$file.c
+    fi
+
     if [ "$osname" = os390 ]; then
-        if [ "$file" = perly.c ]; then
-            $echo '#endif' >>UU/$file.c
-        fi
         $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
         $sed \
     	    -e '/^#.*<stdin>/d' \
@@ -151,18 +159,24 @@
 	    -e 's|\.c\.c|.c|' $uwinfix | \
         $uniq | $sort | $uniq >> .deptmp
     else
-        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
+        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c >.cout 2>.cerr
         $sed \
 	    -e '1d' \
 	    -e '/^#.*<stdin>/d' \
+            -e '/^#.*<builtin>/d' \
+            -e '/^#.*<built-in>/d' \
+            -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
 	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
+	    -e '/: file path prefix .* never used$/d' \
 	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
 	    -e 's/^[	 ]*#[	 ]*line/#/' \
 	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
 	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's|: \./|: |' \
-	    -e 's|\.c\.c|.c|' $uwinfix | \
+           -e 's|\.c\.c|.c|' $uwinfix .cout .cerr| \
         $uniq | $sort | $uniq >> .deptmp
     fi
 done
@@ -196,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
@@ -227,7 +245,8 @@
 $cp $mf.new $mf
 $rm $mf.new
 $echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
-$rm -rf .deptmp UU .shlist .clist .hlist .hsed
+$rm -rf .deptmp UU .shlist .clist .hlist .hsed .cout .cerr
+rmdir .depending
 
 !NO!SUBS!
 $eunicefix makedepend
BADGER
  last SWITCH;
  }
  # If 5.6.2
    if ( $perl eq '5.6.2' ) {
  _patch(<<'BADGER');
--- makedepend.SH.org	2003-07-30 23:46:59.000000000 +0100
+++ makedepend.SH	2010-09-01 10:15:47.000000000 +0100
@@ -1,5 +1,5 @@
 #! /bin/sh
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -29,6 +29,13 @@
 !GROK!THIS!
 $spitshell >>makedepend <<'!NO!SUBS!'
 
+if test -d .depending; then
+	echo "$0: Already running, exiting."
+	exit 0
+fi
+
+mkdir .depending
+
 # This script should be called with 
 #     sh ./makedepend MAKE=$(MAKE)
 case "$1" in 
@@ -37,7 +44,7 @@
 
 export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)
 
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -63,6 +70,10 @@
 PATH=".$path_sep..$path_sep$PATH"
 export PATH
 
+case "$osname" in
+amigaos) cat=/bin/cat ;; # must be absolute
+esac
+
 $cat /dev/null >.deptmp
 $rm -f *.c.c c/*.c.c
 if test -f Makefile; then
@@ -72,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -104,29 +114,20 @@
 	$echo *.c | $tr ' ' $trnl | $egrep -v '\*' >.clist)
 for file in `$cat .clist`; do
 # for file in `cat /dev/null`; do
-	if [ "$osname" = uwin ]; then
-		uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g"
-	else
-		if [ "$osname" = os2 ]; then
-			uwinfix="-e s,\\\\\\\\,/,g"
-		else
-			if [ "$archname" = cygwin ]; then
-				uwinfix="-e s,\\\\\\\\,/,g"
-			else
-				if [ "$osname" = posix-bc ]; then
-					uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/"
-				else
-					uwinfix=
-				fi
-			fi
-		fi
-	fi
+    case "$osname" in
+    uwin)     uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g" ;;
+    os2)      uwinfix="-e s,\\\\\\\\,/,g" ;;
+    cygwin)   uwinfix="-e s,\\\\\\\\,/,g" ;;
+    posix-bc) uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/" ;;
+    vos)      uwinfix="-e s/\#/\\\#/" ;;
+    *)        uwinfix="" ;;
+    esac
     case "$file" in
     *.c) filebase=`basename $file .c` ;;
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -139,10 +140,12 @@
 	-e 's|\\$||' \
 	-e p \
 	-e '}' ) >UU/$file.c
+
+    if [ "$osname" = os390 -a "$file" = perly.c ]; then
+        $echo '#endif' >>UU/$file.c
+    fi
+
     if [ "$osname" = os390 ]; then
-        if [ "$file" = perly.c ]; then
-            $echo '#endif' >>UU/$file.c
-        fi
         $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
         $sed \
     	    -e '/^#.*<stdin>/d' \
@@ -156,21 +159,24 @@
 	    -e 's|\.c\.c|.c|' $uwinfix | \
         $uniq | $sort | $uniq >> .deptmp
     else
-        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
+        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c >.cout 2>.cerr
         $sed \
 	    -e '1d' \
 	    -e '/^#.*<stdin>/d' \
-	    -e '/^#.*<builtin>/d' \
-	    -e '/^#.*<built-in>/d' \
-	    -e '/^#.*<command line>/d' \
+            -e '/^#.*<builtin>/d' \
+            -e '/^#.*<built-in>/d' \
+            -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
 	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
+	    -e '/: file path prefix .* never used$/d' \
 	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
 	    -e 's/^[	 ]*#[	 ]*line/#/' \
 	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
 	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's|: \./|: |' \
-	    -e 's|\.c\.c|.c|' $uwinfix | \
+           -e 's|\.c\.c|.c|' $uwinfix .cout .cerr| \
         $uniq | $sort | $uniq >> .deptmp
     fi
 done
@@ -204,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
@@ -235,7 +245,8 @@
 $cp $mf.new $mf
 $rm $mf.new
 $echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
-$rm -rf .deptmp UU .shlist .clist .hlist .hsed
+$rm -rf .deptmp UU .shlist .clist .hlist .hsed .cout .cerr
+rmdir .depending
 
 !NO!SUBS!
 $eunicefix makedepend
BADGER
  last SWITCH;
  }
  # If 5.7.0
    if ( $perl eq '5.7.0' ) {
  _patch(<<'BADGER');
--- makedepend.SH.org	2000-08-13 19:35:04.000000000 +0100
+++ makedepend.SH	2010-09-01 10:47:14.000000000 +0100
@@ -1,5 +1,5 @@
 #! /bin/sh
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -29,6 +29,13 @@
 !GROK!THIS!
 $spitshell >>makedepend <<'!NO!SUBS!'
 
+if test -d .depending; then
+	echo "$0: Already running, exiting."
+	exit 0
+fi
+
+mkdir .depending
+
 # This script should be called with 
 #     sh ./makedepend MAKE=$(MAKE)
 case "$1" in 
@@ -37,7 +44,7 @@
 
 export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)
 
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -51,6 +58,11 @@
 	;;
 esac
 
+# Avoid localized gcc messages
+case "$ccname" in
+    gcc) LC_ALL=C ; export LC_ALL ;;
+esac
+
 # We need .. when we are in the x2p directory if we are using the
 # cppstdin wrapper script.
 # Put .. and . first so that we pick up the present cppstdin, not
@@ -58,6 +70,10 @@
 PATH=".$path_sep..$path_sep$PATH"
 export PATH
 
+case "$osname" in
+amigaos) cat=/bin/cat ;; # must be absolute
+esac
+
 $cat /dev/null >.deptmp
 $rm -f *.c.c c/*.c.c
 if test -f Makefile; then
@@ -67,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -99,25 +114,20 @@
 	$echo *.c | $tr ' ' $trnl | $egrep -v '\*' >.clist)
 for file in `$cat .clist`; do
 # for file in `cat /dev/null`; do
-	if [ "$osname" = uwin ]; then
-		uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g"
-	else
-		if [ "$osname" = os2 ]; then
-			uwinfix="-e s,\\\\\\\\,/,g"
-		else
-			if [ "$archname" = cygwin ]; then
-				uwinfix="-e s,\\\\\\\\,/,g"
-			else
-				uwinfix=
-			fi
-		fi
-	fi
+    case "$osname" in
+    uwin)     uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g" ;;
+    os2)      uwinfix="-e s,\\\\\\\\,/,g" ;;
+    cygwin)   uwinfix="-e s,\\\\\\\\,/,g" ;;
+    posix-bc) uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/" ;;
+    vos)      uwinfix="-e s/\#/\\\#/" ;;
+    *)        uwinfix="" ;;
+    esac
     case "$file" in
     *.c) filebase=`basename $file .c` ;;
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -130,10 +140,12 @@
 	-e 's|\\$||' \
 	-e p \
 	-e '}' ) >UU/$file.c
+
+    if [ "$osname" = os390 -a "$file" = perly.c ]; then
+        $echo '#endif' >>UU/$file.c
+    fi
+
     if [ "$osname" = os390 ]; then
-        if [ "$file" = perly.c ]; then
-            $echo '#endif' >>UU/$file.c
-        fi
         $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
         $sed \
     	    -e '/^#.*<stdin>/d' \
@@ -147,18 +159,24 @@
 	    -e 's|\.c\.c|.c|' $uwinfix | \
         $uniq | $sort | $uniq >> .deptmp
     else
-        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
+        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c >.cout 2>.cerr
         $sed \
 	    -e '1d' \
 	    -e '/^#.*<stdin>/d' \
+            -e '/^#.*<builtin>/d' \
+            -e '/^#.*<built-in>/d' \
+            -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
 	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
+	    -e '/: file path prefix .* never used$/d' \
 	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
 	    -e 's/^[	 ]*#[	 ]*line/#/' \
 	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
 	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's|: \./|: |' \
-	    -e 's|\.c\.c|.c|' $uwinfix | \
+           -e 's|\.c\.c|.c|' $uwinfix .cout .cerr| \
         $uniq | $sort | $uniq >> .deptmp
     fi
 done
@@ -192,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
@@ -223,7 +245,8 @@
 $cp $mf.new $mf
 $rm $mf.new
 $echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
-$rm -rf .deptmp UU .shlist .clist .hlist .hsed
+$rm -rf .deptmp UU .shlist .clist .hlist .hsed .cout .cerr
+rmdir .depending
 
 !NO!SUBS!
 $eunicefix makedepend
BADGER
  last SWITCH;
  }
  # If 5.7.1
    if ( $perl eq '5.7.1' ) {
  _patch(<<'BADGER');
--- makedepend.SH.org	2001-03-11 16:30:08.000000000 +0000
+++ makedepend.SH	2010-09-01 10:44:54.000000000 +0100
@@ -1,5 +1,5 @@
 #! /bin/sh
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -29,6 +29,13 @@
 !GROK!THIS!
 $spitshell >>makedepend <<'!NO!SUBS!'
 
+if test -d .depending; then
+	echo "$0: Already running, exiting."
+	exit 0
+fi
+
+mkdir .depending
+
 # This script should be called with 
 #     sh ./makedepend MAKE=$(MAKE)
 case "$1" in 
@@ -37,7 +44,7 @@
 
 export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)
 
-case $CONFIGDOTSH in
+case $PERL_CONFIG_SH in
 '')
 	if test -f config.sh; then TOP=.;
 	elif test -f ../config.sh; then TOP=..;
@@ -51,6 +58,11 @@
 	;;
 esac
 
+# Avoid localized gcc messages
+case "$ccname" in
+    gcc) LC_ALL=C ; export LC_ALL ;;
+esac
+
 # We need .. when we are in the x2p directory if we are using the
 # cppstdin wrapper script.
 # Put .. and . first so that we pick up the present cppstdin, not
@@ -58,6 +70,10 @@
 PATH=".$path_sep..$path_sep$PATH"
 export PATH
 
+case "$osname" in
+amigaos) cat=/bin/cat ;; # must be absolute
+esac
+
 $cat /dev/null >.deptmp
 $rm -f *.c.c c/*.c.c
 if test -f Makefile; then
@@ -67,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -99,29 +114,20 @@
 	$echo *.c | $tr ' ' $trnl | $egrep -v '\*' >.clist)
 for file in `$cat .clist`; do
 # for file in `cat /dev/null`; do
-	if [ "$osname" = uwin ]; then
-		uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g"
-	else
-		if [ "$osname" = os2 ]; then
-			uwinfix="-e s,\\\\\\\\,/,g"
-		else
-			if [ "$archname" = cygwin ]; then
-				uwinfix="-e s,\\\\\\\\,/,g"
-			else
-				if [ "$osname" = posix-bc ]; then
-					uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/"
-				else
-					uwinfix=
-				fi
-			fi
-		fi
-	fi
+    case "$osname" in
+    uwin)     uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g" ;;
+    os2)      uwinfix="-e s,\\\\\\\\,/,g" ;;
+    cygwin)   uwinfix="-e s,\\\\\\\\,/,g" ;;
+    posix-bc) uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/" ;;
+    vos)      uwinfix="-e s/\#/\\\#/" ;;
+    *)        uwinfix="" ;;
+    esac
     case "$file" in
     *.c) filebase=`basename $file .c` ;;
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -134,10 +140,12 @@
 	-e 's|\\$||' \
 	-e p \
 	-e '}' ) >UU/$file.c
+
+    if [ "$osname" = os390 -a "$file" = perly.c ]; then
+        $echo '#endif' >>UU/$file.c
+    fi
+
     if [ "$osname" = os390 ]; then
-        if [ "$file" = perly.c ]; then
-            $echo '#endif' >>UU/$file.c
-        fi
         $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
         $sed \
     	    -e '/^#.*<stdin>/d' \
@@ -151,18 +159,24 @@
 	    -e 's|\.c\.c|.c|' $uwinfix | \
         $uniq | $sort | $uniq >> .deptmp
     else
-        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
+        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c >.cout 2>.cerr
         $sed \
 	    -e '1d' \
 	    -e '/^#.*<stdin>/d' \
+            -e '/^#.*<builtin>/d' \
+            -e '/^#.*<built-in>/d' \
+            -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
 	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
+	    -e '/: file path prefix .* never used$/d' \
 	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
 	    -e 's/^[	 ]*#[	 ]*line/#/' \
 	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
 	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's|: \./|: |' \
-	    -e 's|\.c\.c|.c|' $uwinfix | \
+           -e 's|\.c\.c|.c|' $uwinfix .cout .cerr| \
         $uniq | $sort | $uniq >> .deptmp
     fi
 done
@@ -196,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
@@ -227,7 +245,8 @@
 $cp $mf.new $mf
 $rm $mf.new
 $echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
-$rm -rf .deptmp UU .shlist .clist .hlist .hsed
+$rm -rf .deptmp UU .shlist .clist .hlist .hsed .cout .cerr
+rmdir .depending
 
 !NO!SUBS!
 $eunicefix makedepend
BADGER
  last SWITCH;
  }
  # If 5.7.2
    if ( $perl eq '5.7.2' ) {
  _patch(<<'BADGER');
--- makedepend.SH.org	2001-07-09 15:11:05.000000000 +0100
+++ makedepend.SH	2010-09-01 10:45:32.000000000 +0100
@@ -18,10 +18,6 @@
 */*) cd `expr X$0 : 'X\(.*\)/'` ;;
 esac
 
-case "$osname" in
-amigaos) cat=/bin/cat ;; # must be absolute
-esac
-
 echo "Extracting makedepend (with variable substitutions)"
 rm -f makedepend
 $spitshell >makedepend <<!GROK!THIS!
@@ -33,6 +29,13 @@
 !GROK!THIS!
 $spitshell >>makedepend <<'!NO!SUBS!'
 
+if test -d .depending; then
+	echo "$0: Already running, exiting."
+	exit 0
+fi
+
+mkdir .depending
+
 # This script should be called with 
 #     sh ./makedepend MAKE=$(MAKE)
 case "$1" in 
@@ -55,6 +58,11 @@
 	;;
 esac
 
+# Avoid localized gcc messages
+case "$ccname" in
+    gcc) LC_ALL=C ; export LC_ALL ;;
+esac
+
 # We need .. when we are in the x2p directory if we are using the
 # cppstdin wrapper script.
 # Put .. and . first so that we pick up the present cppstdin, not
@@ -62,6 +70,10 @@
 PATH=".$path_sep..$path_sep$PATH"
 export PATH
 
+case "$osname" in
+amigaos) cat=/bin/cat ;; # must be absolute
+esac
+
 $cat /dev/null >.deptmp
 $rm -f *.c.c c/*.c.c
 if test -f Makefile; then
@@ -71,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -103,29 +114,20 @@
 	$echo *.c | $tr ' ' $trnl | $egrep -v '\*' >.clist)
 for file in `$cat .clist`; do
 # for file in `cat /dev/null`; do
-	if [ "$osname" = uwin ]; then
-		uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g"
-	else
-		if [ "$osname" = os2 ]; then
-			uwinfix="-e s,\\\\\\\\,/,g"
-		else
-			if [ "$archname" = cygwin ]; then
-				uwinfix="-e s,\\\\\\\\,/,g"
-			else
-				if [ "$osname" = posix-bc ]; then
-					uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/"
-				else
-					uwinfix=
-				fi
-			fi
-		fi
-	fi
+    case "$osname" in
+    uwin)     uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g" ;;
+    os2)      uwinfix="-e s,\\\\\\\\,/,g" ;;
+    cygwin)   uwinfix="-e s,\\\\\\\\,/,g" ;;
+    posix-bc) uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/" ;;
+    vos)      uwinfix="-e s/\#/\\\#/" ;;
+    *)        uwinfix="" ;;
+    esac
     case "$file" in
     *.c) filebase=`basename $file .c` ;;
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -138,10 +140,12 @@
 	-e 's|\\$||' \
 	-e p \
 	-e '}' ) >UU/$file.c
+
+    if [ "$osname" = os390 -a "$file" = perly.c ]; then
+        $echo '#endif' >>UU/$file.c
+    fi
+
     if [ "$osname" = os390 ]; then
-        if [ "$file" = perly.c ]; then
-            $echo '#endif' >>UU/$file.c
-        fi
         $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
         $sed \
     	    -e '/^#.*<stdin>/d' \
@@ -155,18 +159,24 @@
 	    -e 's|\.c\.c|.c|' $uwinfix | \
         $uniq | $sort | $uniq >> .deptmp
     else
-        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
+        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c >.cout 2>.cerr
         $sed \
 	    -e '1d' \
 	    -e '/^#.*<stdin>/d' \
+            -e '/^#.*<builtin>/d' \
+            -e '/^#.*<built-in>/d' \
+            -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
 	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
+	    -e '/: file path prefix .* never used$/d' \
 	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
 	    -e 's/^[	 ]*#[	 ]*line/#/' \
 	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
 	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's|: \./|: |' \
-	    -e 's|\.c\.c|.c|' $uwinfix | \
+           -e 's|\.c\.c|.c|' $uwinfix .cout .cerr| \
         $uniq | $sort | $uniq >> .deptmp
     fi
 done
@@ -200,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
@@ -231,7 +245,8 @@
 $cp $mf.new $mf
 $rm $mf.new
 $echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
-$rm -rf .deptmp UU .shlist .clist .hlist .hsed
+$rm -rf .deptmp UU .shlist .clist .hlist .hsed .cout .cerr
+rmdir .depending
 
 !NO!SUBS!
 $eunicefix makedepend
BADGER
  last SWITCH;
  }
  # If 5.7.3
    if ( $perl eq '5.7.3' ) {
  _patch(<<'BADGER');
--- makedepend.SH.org	2002-03-05 01:10:22.000000000 +0000
+++ makedepend.SH	2010-09-01 10:46:13.000000000 +0100
@@ -18,10 +18,6 @@
 */*) cd `expr X$0 : 'X\(.*\)/'` ;;
 esac
 
-case "$osname" in
-amigaos) cat=/bin/cat ;; # must be absolute
-esac
-
 echo "Extracting makedepend (with variable substitutions)"
 rm -f makedepend
 $spitshell >makedepend <<!GROK!THIS!
@@ -33,6 +29,13 @@
 !GROK!THIS!
 $spitshell >>makedepend <<'!NO!SUBS!'
 
+if test -d .depending; then
+	echo "$0: Already running, exiting."
+	exit 0
+fi
+
+mkdir .depending
+
 # This script should be called with 
 #     sh ./makedepend MAKE=$(MAKE)
 case "$1" in 
@@ -55,6 +58,11 @@
 	;;
 esac
 
+# Avoid localized gcc messages
+case "$ccname" in
+    gcc) LC_ALL=C ; export LC_ALL ;;
+esac
+
 # We need .. when we are in the x2p directory if we are using the
 # cppstdin wrapper script.
 # Put .. and . first so that we pick up the present cppstdin, not
@@ -62,6 +70,10 @@
 PATH=".$path_sep..$path_sep$PATH"
 export PATH
 
+case "$osname" in
+amigaos) cat=/bin/cat ;; # must be absolute
+esac
+
 $cat /dev/null >.deptmp
 $rm -f *.c.c c/*.c.c
 if test -f Makefile; then
@@ -71,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -116,7 +127,7 @@
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -129,6 +140,11 @@
 	-e 's|\\$||' \
 	-e p \
 	-e '}' ) >UU/$file.c
+
+    if [ "$osname" = os390 -a "$file" = perly.c ]; then
+        $echo '#endif' >>UU/$file.c
+    fi
+
     if [ "$osname" = os390 ]; then
         $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
         $sed \
@@ -143,13 +159,16 @@
 	    -e 's|\.c\.c|.c|' $uwinfix | \
         $uniq | $sort | $uniq >> .deptmp
     else
-        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c 2>&1 |
+        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c >.cout 2>.cerr
         $sed \
 	    -e '1d' \
 	    -e '/^#.*<stdin>/d' \
             -e '/^#.*<builtin>/d' \
+            -e '/^#.*<built-in>/d' \
             -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
 	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
 	    -e '/: file path prefix .* never used$/d' \
 	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
 	    -e 's/^[	 ]*#[	 ]*line/#/' \
@@ -157,7 +176,7 @@
 	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
 	    -e 's|: \./|: |' \
-	    -e 's|\.c\.c|.c|' $uwinfix | \
+           -e 's|\.c\.c|.c|' $uwinfix .cout .cerr| \
         $uniq | $sort | $uniq >> .deptmp
     fi
 done
@@ -191,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
@@ -222,7 +245,8 @@
 $cp $mf.new $mf
 $rm $mf.new
 $echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
-$rm -rf .deptmp UU .shlist .clist .hlist .hsed
+$rm -rf .deptmp UU .shlist .clist .hlist .hsed .cout .cerr
+rmdir .depending
 
 !NO!SUBS!
 $eunicefix makedepend
BADGER
  last SWITCH;
  }
  # If 5.8.0
    if ( $perl eq '5.8.0' ) {
  _patch(<<'BADGER');
--- makedepend.SH.org	2002-07-09 15:06:42.000000000 +0100
+++ makedepend.SH	2010-09-01 10:16:37.000000000 +0100
@@ -58,6 +58,11 @@
 	;;
 esac
 
+# Avoid localized gcc messages
+case "$ccname" in
+    gcc) LC_ALL=C ; export LC_ALL ;;
+esac
+
 # We need .. when we are in the x2p directory if we are using the
 # cppstdin wrapper script.
 # Put .. and . first so that we pick up the present cppstdin, not
@@ -78,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -123,7 +127,7 @@
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -136,6 +140,11 @@
 	-e 's|\\$||' \
 	-e p \
 	-e '}' ) >UU/$file.c
+
+    if [ "$osname" = os390 -a "$file" = perly.c ]; then
+        $echo '#endif' >>UU/$file.c
+    fi
+
     if [ "$osname" = os390 ]; then
         $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
         $sed \
@@ -157,7 +166,9 @@
             -e '/^#.*<builtin>/d' \
             -e '/^#.*<built-in>/d' \
             -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
 	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
 	    -e '/: file path prefix .* never used$/d' \
 	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
 	    -e 's/^[	 ]*#[	 ]*line/#/' \
@@ -199,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
BADGER
  last SWITCH;
  }
  # If 5.9.4
  if ( $perl eq '5.9.4' ) {
    _patch_b64(<<'BADGER');
LS0tIG1ha2VkZXBlbmQuU0gJMjAyMC0wNi0wOSAxNjoxNDo1NC43Njc2MTI2OTAgKzAxMDAKKysr
IG1ha2VkZXBlbmQuU0gJMjAyMC0wNi0wOSAxNjoxNTowNC40MTEwODI2ODUgKzAxMDAKQEAgLTEy
OCw3ICsxMjgsNyBAQAogICAgICoueSkgZmlsZWJhc2U9YGJhc2VuYW1lICRmaWxlIC55YCA7Owog
ICAgIGVzYWMKICAgICBjYXNlICIkZmlsZSIgaW4KLSAgICAqLyopIGZpbmM9Ii1JYGVjaG8gJGZp
bGUgfCBzZWQgJ3MjL1teL10qJCMjYCIgOzsKKyAgICAqLyopIGZpbmM9Ii1JYGVjaG8gJGZpbGUg
fCBzZWQgJ3MjL1teL10qJCMjJ2AiIDs7CiAgICAgKikgICBmaW5jPSA7OwogICAgIGVzYWMKICAg
ICAkZWNobyAiRmluZGluZyBkZXBlbmRlbmNpZXMgZm9yICRmaWxlYmFzZSRfby4iCkBAIC0xNjks
NiArMTY5LDcgQEAKICAgICAgICAgICAgIC1lICcvXiMuKjxjb21tYW5kIGxpbmU+L2QnIFwKICAg
ICAgICAgICAgIC1lICcvXiMuKjxjb21tYW5kLWxpbmU+L2QnIFwKIAkgICAgLWUgJy9eIy4qIi0i
L2QnIFwKKwkgICAgLWUgJy9eIy4qIlwvLipcLyIvZCcgXAogCSAgICAtZSAnLzogZmlsZSBwYXRo
IHByZWZpeCAuKiBuZXZlciB1c2VkJC9kJyBcCiAJICAgIC1lICdzI1wuWzAtOV1bMC05XSpcLmMj
JyIkZmlsZS5jIyIgXAogCSAgICAtZSAncy9eWwkgXSojWwkgXSpsaW5lLyMvJyBcCg==
BADGER
  last SWITCH;
  }
  # If 5.8.[12345678] and 5.9.[0123]
  _patch(<<'BADGER');
--- makedepend.SH.org	2003-06-05 19:11:10.000000000 +0100
+++ makedepend.SH	2010-09-01 10:24:39.000000000 +0100
@@ -83,7 +83,6 @@
     # to be out of date.  I don't know if OS/2 has touch, so do this:
     case "$osname" in
     os2) ;;
-    netbsd) ;;
     *) $touch $firstmakefile ;;
     esac
 fi
@@ -128,7 +127,7 @@
     *.y) filebase=`basename $file .y` ;;
     esac
     case "$file" in
-    */*) finc="-I`echo $file | sed 's#/[^/]*$##`" ;;
+    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
     *)   finc= ;;
     esac
     $echo "Finding dependencies for $filebase$_o."
@@ -167,7 +166,9 @@
             -e '/^#.*<builtin>/d' \
             -e '/^#.*<built-in>/d' \
             -e '/^#.*<command line>/d' \
+            -e '/^#.*<command-line>/d' \
 	    -e '/^#.*"-"/d' \
+	    -e '/^#.*"\/.*\/"/d' \
 	    -e '/: file path prefix .* never used$/d' \
 	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
 	    -e 's/^[	 ]*#[	 ]*line/#/' \
@@ -209,6 +210,10 @@
     $echo "Updating $mf..."
     $echo "# If this runs make out of memory, delete /usr/include lines." \
 	>> $mf.new
+    if [ "$osname" = vos ]; then
+        $sed 's|.incl.c|.h|' .deptmp >.deptmp.vos
+        mv -f .deptmp.vos .deptmp
+    fi
     $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
        >>$mf.new
 else
BADGER
  }
}

sub _patch_conf_gconvert
{
  my $perl = shift;
  _patch(<<'END');
--- Configure
+++ Configure
@@ -7851,6 +7851,21 @@ int main()
 	Gconvert((DOUBLETYPE)0.1, 8, 0, buf);
 	checkit("0.1", buf);
 
+	Gconvert((DOUBLETYPE)0.01, 8, 0, buf);
+	checkit("0.01", buf);
+
+	Gconvert((DOUBLETYPE)0.001, 8, 0, buf);
+	checkit("0.001", buf);
+
+	Gconvert((DOUBLETYPE)0.0001, 8, 0, buf);
+	checkit("0.0001", buf);
+
+	Gconvert((DOUBLETYPE)0.00009, 8, 0, buf);
+	if (strlen(buf) > 5)
+	    checkit("9e-005", buf); /* for Microsoft ?? */
+	else
+	    checkit("9e-05", buf);
+
 	Gconvert((DOUBLETYPE)1.0, 8, 0, buf); 
 	checkit("1", buf);
 
@@ -7889,6 +7904,19 @@ int main()
 	Gconvert((DOUBLETYPE)123.456, 8, 0, buf); 
 	checkit("123.456", buf);
 
+	/* Testing of 1e+129 in bigintpm.t must not get extra '.' here. */
+	Gconvert((DOUBLETYPE)1e34, 8, 0, buf);
+	/* 34 should be enough to scare even long double
+	 * places into using the e notation. */
+	if (strlen(buf) > 5)
+	    checkit("1e+034", buf); /* for Microsoft */
+	else
+	    checkit("1e+34", buf);
+
+	/* For Perl, if you add additional tests here, also add them to
+	 * t/base/num.t for benefit of platforms not using Configure or
+	 * overriding d_Gconvert */
+
 	exit(0);
 }
 EOP
END
}

sub _patch_sort_N {
  system($^X, '-pi.bak', '-e', 's!\$sort \-n \+1!(\$sort -n -k 2 2>/dev/null || \$sort -n +1)!', 'Configure');
}

sub _patch_archive_tar_tests
{
  my $perl = shift;
  if ($perl =~ /^5\.10/) {
    _patch(<<'END');
--- lib/Archive/Tar/t/02_methods.t
+++ lib/Archive/Tar/t/02_methods.t
@@ -70,6 +70,20 @@ my $LONG_FILE = qq[directory/really-really-really-really-really-really-really-re
 my $TOO_LONG    =   ($^O eq 'MSWin32' or $^O eq 'cygwin' or $^O eq 'VMS')
                     && length( cwd(). $LONG_FILE ) > 247;
 
+if(!$TOO_LONG) {
+    my $alt = File::Spec->catfile( cwd(), $LONG_FILE);
+    eval 'mkpath([$alt]);';
+    if($@)
+    {
+        $TOO_LONG = 1;
+    }
+    else
+    {
+        $@ = '';
+        my $base = File::Spec->catfile( cwd(), 'directory');
+        rmtree $base;
+    }
+}
 ### warn if we are going to skip long file names
 if ($TOO_LONG) {
     diag("No long filename support - long filename extraction disabled") if ! $ENV{PERL_CORE};
END
  }
  else {
    _patch(<<'END');
--- cpan/Archive-Tar/t/02_methods.t
+++ cpan/Archive-Tar/t/02_methods.t
@@ -70,6 +70,20 @@ my $LONG_FILE = qq[directory/really-really-really-really-really-really-really-re
 my $TOO_LONG    =   ($^O eq 'MSWin32' or $^O eq 'cygwin' or $^O eq 'VMS')
                     && length( cwd(). $LONG_FILE ) > 247;
 
+if(!$TOO_LONG) {
+    my $alt = File::Spec->catfile( cwd(), $LONG_FILE);
+    eval 'mkpath([$alt]);';
+    if($@)
+    {
+        $TOO_LONG = 1;
+    }
+    else
+    {
+        $@ = '';
+        my $base = File::Spec->catfile( cwd(), 'directory');
+        rmtree $base;
+    }
+}
 ### warn if we are going to skip long file names
 if ($TOO_LONG) {
     diag("No long filename support - long filename extraction disabled") if ! $ENV{PERL_CORE};
END
  }
}

sub _patch_odbm_file_hints_linux
{
    _patch(<<'END');
--- ext/ODBM_File/hints/linux.pl
+++ ext/ODBM_File/hints/linux.pl
@@ -1,8 +1,8 @@
 # uses GDBM dbm compatibility feature - at least on SuSE 8.0
 $self->{LIBS} = ['-lgdbm'];
 
-# Debian/Ubuntu have /usr/lib/libgdbm_compat.so.3* but not this file,
+# Debian/Ubuntu have libgdbm_compat.so but not this file,
 # so linking may fail
-if (-e '/usr/lib/libgdbm_compat.so' or -e '/usr/lib64/libgdbm_compat.so') {
-    $self->{LIBS}->[0] .= ' -lgdbm_compat';
+foreach (split / /, $Config{libpth}) {
+    $self->{LIBS}->[0] .= ' -lgdbm_compat' if -e $_.'/libgdbm_compat.so';
 }
END
}

sub _patch_make_ext_pl
{
  _patch(<<'END');
--- make_ext.pl
+++ make_ext.pl
@@ -377,6 +377,10 @@ WriteMakefile(
 EOM
 	    close $fh or die "Can't close Makefile.PL: $!";
 	}
+  eval {
+    my $ftime = time - 4;
+    utime $ftime, $ftime, 'Makefile.PL';
+  };
 	print "\nRunning Makefile.PL in $ext_dir\n";
 
 	# Presumably this can be simplified
END
}

sub _patch_589_perlio_c
{
  _patch(<<'END');
--- perlio.c
+++ perlio.c
@@ -2323,6 +2323,12 @@ PerlIO_init(pTHX)
 {
     /* MUTEX_INIT(&PL_perlio_mutex) is done in PERL_SYS_INIT3(). */
     PERL_UNUSED_CONTEXT;
+    /*
+     * No, for backwards compatibility (before PERL_SYS_INIT3 changed to be
+     * defined as a separate function call), we need to call
+     * MUTEX_INIT(&PL_perlio_mutex) (via the PERLIO_INIT macro).
+     */
+    PERLIO_INIT;
 }
 
 void
END
}

# http://perl5.git.perl.org/perl.git/commit/2674b61957c26a4924831d5110afa454ae7ae5a6
sub _patch_hsplit_rehash_58
{
  my $perl = shift;

  my $patch = <<'END';
--- hv.c
+++ hv.c
@@ -31,7 +31,8 @@ holds the key and hash value.
 #define PERL_HASH_INTERNAL_ACCESS
 #include "perl.h"
 
-#define HV_MAX_LENGTH_BEFORE_SPLIT 14
+#define HV_MAX_LENGTH_BEFORE_REHASH 14
+#define SHOULD_DO_HSPLIT(xhv) ((xhv)->xhv_keys > (xhv)->xhv_max) /* HvTOTALKEYS(hv) > HvMAX(hv) */
 
 STATIC void
 S_more_he(pTHX)
@@ -705,23 +706,8 @@ Perl_hv_common(pTHX_ HV *hv, SV *keysv, const char *key, STRLEN klen,
 	xhv->xhv_keys++; /* HvTOTALKEYS(hv)++ */
 	if (!counter) {				/* initial entry? */
 	    xhv->xhv_fill++; /* HvFILL(hv)++ */
-	} else if (xhv->xhv_keys > (IV)xhv->xhv_max) {
+	} else if ( SHOULD_DO_HSPLIT(xhv) ) {
 	    hsplit(hv);
-	} else if(!HvREHASH(hv)) {
-	    U32 n_links = 1;
-
-	    while ((counter = HeNEXT(counter)))
-		n_links++;
-
-	    if (n_links > HV_MAX_LENGTH_BEFORE_SPLIT) {
-		/* Use only the old HvKEYS(hv) > HvMAX(hv) condition to limit
-		   bucket splits on a rehashed hash, as we're not going to
-		   split it again, and if someone is lucky (evil) enough to
-		   get all the keys in one list they could exhaust our memory
-		   as we repeatedly double the number of buckets on every
-		   entry. Linear search feels a less worse thing to do.  */
-		hsplit(hv);
-	    }
 	}
     }
 
@@ -1048,7 +1034,7 @@ S_hsplit(pTHX_ HV *hv)
 
 
     /* Pick your policy for "hashing isn't working" here:  */
-    if (longest_chain <= HV_MAX_LENGTH_BEFORE_SPLIT /* split worked?  */
+    if (longest_chain <= HV_MAX_LENGTH_BEFORE_REHASH /* split worked?  */
 	|| HvREHASH(hv)) {
 	return;
     }
@@ -1966,8 +1952,8 @@ S_share_hek_flags(pTHX_ const char *str, I32 len, register U32 hash, int flags)
 	xhv->xhv_keys++; /* HvTOTALKEYS(hv)++ */
 	if (!next) {			/* initial entry? */
 	    xhv->xhv_fill++; /* HvFILL(hv)++ */
-	} else if (xhv->xhv_keys > (IV)xhv->xhv_max /* HvKEYS(hv) > HvMAX(hv) */) {
-		hsplit(PL_strtab);
+	} else if ( SHOULD_DO_HSPLIT(xhv) ) {
+            hsplit(PL_strtab);
 	}
     }
 
--- t/op/hash.t
+++ t/op/hash.t
@@ -39,22 +39,36 @@ use constant THRESHOLD => 14;
 use constant START     => "a";
 
 # some initial hash data
-my %h2 = map {$_ => 1} 'a'..'cc';
+my %h2;
+my $counter= "a";
+$h2{$counter++}++ while $counter ne 'cd';
 
 ok (!Internals::HvREHASH(%h2), 
     "starting with pre-populated non-pathological hash (rehash flag if off)");
 
 my @keys = get_keys(\%h2);
+my $buckets= buckets(\%h2);
 $h2{$_}++ for @keys;
+$h2{$counter++}++ while buckets(\%h2) == $buckets; # force a split
 ok (Internals::HvREHASH(%h2), 
-    scalar(@keys) . " colliding into the same bucket keys are triggering rehash");
+    scalar(@keys) . " colliding into the same bucket keys are triggering rehash after split");
+
+# returns the number of buckets in a hash
+sub buckets {
+    my $hr = shift;
+    my $keys_buckets= scalar(%$hr);
+    if ($keys_buckets=~m!/([0-9]+)\z!) {
+        return 0+$1;
+    } else {
+        return 8;
+    }
+}
 
 sub get_keys {
     my $hr = shift;
 
     # the minimum of bits required to mount the attack on a hash
     my $min_bits = log(THRESHOLD)/log(2);
-
     # if the hash has already been populated with a significant amount
     # of entries the number of mask bits can be higher
     my $keys = scalar keys %$hr;
-- 
1.7.4.1

END

  if ($perl =~ qr/^5\.8\.8$/) {
    $patch =~ s/non-pathological/non-pathalogical/;
    $patch =~ s/triggering/triggerring/;
  }
  _patch($patch);
}

# http://perl5.git.perl.org/perl.git/commit/f14269908e5f8b4cab4b55643d7dd9de577e7918
# http://perl5.git.perl.org/perl.git/commit/9d83adcdf9ab3c1ac7d54d76f3944e57278f0e70
sub _patch_hsplit_rehash_510 {
  _patch(<<'END');
--- ext/Hash-Util-FieldHash/t/10_hash.t
+++ ext/Hash-Util-FieldHash/t/10_hash.t
@@ -46,15 +46,29 @@ use constant START     => "a";
 
 # some initial hash data
 fieldhash my %h2;
-%h2 = map {$_ => 1} 'a'..'cc';
+my $counter= "a";
+$h2{$counter++}++ while $counter ne 'cd';
 
 ok (!Internals::HvREHASH(%h2), 
     "starting with pre-populated non-pathological hash (rehash flag if off)");
 
 my @keys = get_keys(\%h2);
+my $buckets= buckets(\%h2);
 $h2{$_}++ for @keys;
+$h2{$counter++}++ while buckets(\%h2) == $buckets; # force a split
 ok (Internals::HvREHASH(%h2), 
-    scalar(@keys) . " colliding into the same bucket keys are triggering rehash");
+    scalar(@keys) . " colliding into the same bucket keys are triggering rehash after split");
+
+# returns the number of buckets in a hash
+sub buckets {
+    my $hr = shift;
+    my $keys_buckets= scalar(%$hr);
+    if ($keys_buckets=~m!/([0-9]+)\z!) {
+        return 0+$1;
+    } else {
+        return 8;
+    }
+}
 
 sub get_keys {
     my $hr = shift;
--- hv.c
+++ hv.c
@@ -35,7 +35,8 @@ holds the key and hash value.
 #define PERL_HASH_INTERNAL_ACCESS
 #include "perl.h"
 
-#define HV_MAX_LENGTH_BEFORE_SPLIT 14
+#define HV_MAX_LENGTH_BEFORE_REHASH 14
+#define SHOULD_DO_HSPLIT(xhv) ((xhv)->xhv_keys > (xhv)->xhv_max) /* HvTOTALKEYS(hv) > HvMAX(hv) */
 
 static const char S_strtab_error[]
     = "Cannot modify shared string table in hv_%s";
@@ -818,23 +819,8 @@ Perl_hv_common(pTHX_ HV *hv, SV *keysv, const char *key, STRLEN klen,
 	xhv->xhv_keys++; /* HvTOTALKEYS(hv)++ */
 	if (!counter) {				/* initial entry? */
 	    xhv->xhv_fill++; /* HvFILL(hv)++ */
-	} else if (xhv->xhv_keys > (IV)xhv->xhv_max) {
+	} else if ( SHOULD_DO_HSPLIT(xhv) ) {
 	    hsplit(hv);
-	} else if(!HvREHASH(hv)) {
-	    U32 n_links = 1;
-
-	    while ((counter = HeNEXT(counter)))
-		n_links++;
-
-	    if (n_links > HV_MAX_LENGTH_BEFORE_SPLIT) {
-		/* Use only the old HvKEYS(hv) > HvMAX(hv) condition to limit
-		   bucket splits on a rehashed hash, as we're not going to
-		   split it again, and if someone is lucky (evil) enough to
-		   get all the keys in one list they could exhaust our memory
-		   as we repeatedly double the number of buckets on every
-		   entry. Linear search feels a less worse thing to do.  */
-		hsplit(hv);
-	    }
 	}
     }
 
@@ -1180,7 +1166,7 @@ S_hsplit(pTHX_ HV *hv)
 
 
     /* Pick your policy for "hashing isn't working" here:  */
-    if (longest_chain <= HV_MAX_LENGTH_BEFORE_SPLIT /* split worked?  */
+    if (longest_chain <= HV_MAX_LENGTH_BEFORE_REHASH /* split worked?  */
 	|| HvREHASH(hv)) {
 	return;
     }
@@ -2506,8 +2492,8 @@ S_share_hek_flags(pTHX_ const char *str, I32 len, register U32 hash, int flags)
 	xhv->xhv_keys++; /* HvTOTALKEYS(hv)++ */
 	if (!next) {			/* initial entry? */
 	    xhv->xhv_fill++; /* HvFILL(hv)++ */
-	} else if (xhv->xhv_keys > (IV)xhv->xhv_max /* HvKEYS(hv) > HvMAX(hv) */) {
-		hsplit(PL_strtab);
+	} else if ( SHOULD_DO_HSPLIT(xhv) ) {
+            hsplit(PL_strtab);
 	}
     }
 
diff --git a/t/op/hash.t b/t/op/hash.t
index 9bde518..45eb782 100644
--- t/op/hash.t
+++ t/op/hash.t
@@ -39,22 +39,36 @@ use constant THRESHOLD => 14;
 use constant START     => "a";
 
 # some initial hash data
-my %h2 = map {$_ => 1} 'a'..'cc';
+my %h2;
+my $counter= "a";
+$h2{$counter++}++ while $counter ne 'cd';
 
 ok (!Internals::HvREHASH(%h2), 
     "starting with pre-populated non-pathological hash (rehash flag if off)");
 
 my @keys = get_keys(\%h2);
+my $buckets= buckets(\%h2);
 $h2{$_}++ for @keys;
+$h2{$counter++}++ while buckets(\%h2) == $buckets; # force a split
 ok (Internals::HvREHASH(%h2), 
-    scalar(@keys) . " colliding into the same bucket keys are triggering rehash");
+    scalar(@keys) . " colliding into the same bucket keys are triggering rehash after split");
+
+# returns the number of buckets in a hash
+sub buckets {
+    my $hr = shift;
+    my $keys_buckets= scalar(%$hr);
+    if ($keys_buckets=~m!/([0-9]+)\z!) {
+        return 0+$1;
+    } else {
+        return 8;
+    }
+}
 
 sub get_keys {
     my $hr = shift;
 
     # the minimum of bits required to mount the attack on a hash
     my $min_bits = log(THRESHOLD)/log(2);
-
     # if the hash has already been populated with a significant amount
     # of entries the number of mask bits can be higher
     my $keys = scalar keys %$hr;
-- 
1.7.4.1


END
}

sub _patch_bitrig {
  return unless $^O eq 'bitrig';
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num < 5.019004;
  unless ( $num < 5.00800 ) {
  _patch(<<'BOOGLE');
diff --git a/Configure b/Configure
index 19bed50..e4e4075 100755
--- Configure
+++ Configure
@@ -3312,6 +3312,9 @@ EOM
 			;;
 		next*) osname=next ;;
 		nonstop-ux) osname=nonstopux ;;
+		bitrig) osname=bitrig
+			osvers="$3"
+			;;
 		openbsd) osname=openbsd
                 	osvers="$3"
                 	;;
BOOGLE
  }
  if ( $num < 5.008009 ) {
  _patch(<<'BITRIGM1');
diff --git a/Makefile.SH b/Makefile.SH
index 17298fa..ecaa8ac 100755
--- Makefile.SH
+++ Makefile.SH
@@ -77,7 +77,7 @@ true)
 	sunos*)
 		linklibperl="-lperl"
 		;;
-	netbsd*|freebsd[234]*|openbsd*)
+	netbsd*|freebsd[234]*|openbsd*|bitrig*)
 		linklibperl="-L. -lperl"
 		;;
 	interix*)
BITRIGM1
  }
  else {
  _patch(<<'BITRIGMX');
diff --git a/Makefile.SH b/Makefile.SH
index 17298fa..ecaa8ac 100755
--- Makefile.SH
+++ Makefile.SH
@@ -77,7 +77,7 @@ true)
 	sunos*)
 		linklibperl="-lperl"
 		;;
-	netbsd*|freebsd[234]*|openbsd*|dragonfly*)
+	netbsd*|freebsd[234]*|openbsd*|dragonfly*|bitrig*)
 		linklibperl="-L. -lperl"
 		;;
 	interix*)
BITRIGMX
  }
  if ( $num < 5.008001 ) {
    # NOOP
  }
  elsif ( $num < 5.008007 ) {
    _patch(<<'BITRIGC3');
diff --git a/Configure b/Configure
index 19bed50..e4e4075 100755
--- Configure	Thu Aug 22 23:20:14 2013
+++ Configure	Thu Aug 22 23:20:35 2013
@@ -7855,7 +7855,7 @@
 	solaris)
 		xxx="-R $shrpdir"
 		;;
-	freebsd|netbsd|openbsd)
+	freebsd|netbsd|openbsd|bitrig)
 		xxx="-Wl,-R$shrpdir"
 		;;
 	bsdos|linux|irix*|dec_osf)
BITRIGC3
  }
  elsif ( $num < 5.008009 ) {
    _patch(<<'BITRIGC2');
diff --git a/Configure b/Configure
index 19bed50..e4e4075 100755
--- Configure	Thu Aug 22 22:56:04 2013
+++ Configure	Thu Aug 22 22:56:25 2013
@@ -7892,7 +7892,7 @@
 	solaris)
 		xxx="-R $shrpdir"
 		;;
-	freebsd|netbsd|openbsd|interix)
+	freebsd|netbsd|openbsd|interix|bitrig)
 		xxx="-Wl,-R$shrpdir"
 		;;
 	bsdos|linux|irix*|dec_osf|gnu*)
BITRIGC2
  }
  elsif ( $num < 5.013000 ) {
    _patch(<<'BITRIGC1');
diff --git a/Configure b/Configure
index 19bed50..e4e4075 100755
--- Configure
+++ Configure
@@ -8328,7 +8331,7 @@ if "$useshrplib"; then
 	solaris)
 		xxx="-R $shrpdir"
 		;;
-	freebsd|netbsd|openbsd|interix|dragonfly)
+	freebsd|netbsd|openbsd|interix|dragonfly|bitrig)
 		xxx="-Wl,-R$shrpdir"
 		;;
 	bsdos|linux|irix*|dec_osf|gnu*)
BITRIGC1
  }
  else {
    _patch(<<'BITRIGCX');
diff --git a/Configure b/Configure
index 19bed50..e4e4075 100755
--- Configure
+++ Configure
@@ -8328,7 +8331,7 @@ if "$useshrplib"; then
 	solaris)
 		xxx="-R $shrpdir"
 		;;
-	freebsd|mirbsd|netbsd|openbsd|interix|dragonfly)
+	freebsd|mirbsd|netbsd|openbsd|interix|dragonfly|bitrig)
 		xxx="-Wl,-R$shrpdir"
 		;;
 	bsdos|linux|irix*|dec_osf|gnu*)
BITRIGCX
  }
}

sub _patch_conf_solaris {
  return unless $^O eq 'solaris';
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num < 5.018000;
  _patch(<<'BUBBLE');
diff --git a/Configure b/Configure
index ff511d3..30ab78a 100755
--- Configure
+++ Configure
@@ -8048,7 +8048,20 @@ EOM
 			      ;;
 			linux|irix*|gnu*)  dflt="-shared $optimize" ;;
 			next)  dflt='none' ;;
-			solaris) dflt='-G' ;;
+			solaris) # See [perl #66604].  On Solaris 11, gcc -m64 on amd64
+				# appears not to understand -G.  gcc versions at
+				# least as old as 3.4.3 support -shared, so just
+				# use that with Solaris 11 and later, but keep
+				# the old behavior for older Solaris versions.
+				case "$gccversion" in
+					'') dflt='-G' ;;
+					*)	case "$osvers" in
+							2.?|2.10) dflt='-G' ;;
+							*) dflt='-shared' ;;
+						esac
+						;;
+				esac
+				;;
 			sunos) dflt='-assert nodefinitions' ;;
 			svr4*|esix*|nonstopux) dflt="-G $ldflags" ;;
 	        *)     dflt='none' ;;
BUBBLE
}

#commit 4149c7198d9b78d861df289cce40dd865cab57e7
sub _patch_regmatch_pointer_5180 {
  _patch(<<'BOBBLE');
diff --git a/regexec.c b/regexec.c
index bc38839..b865b46 100644
--- regexec.c
+++ regexec.c
@@ -6662,7 +6662,7 @@ S_regrepeat(pTHX_ regexp *prog, char **startposp, const regnode *p,
     scan = *startposp;
     if (max == REG_INFTY)
 	max = I32_MAX;
-    else if (! utf8_target && scan + max < loceol)
+    else if (! utf8_target && loceol - scan > max)
 	loceol = scan + max;
 
     /* Here, for the case of a non-UTF-8 target we have adjusted <loceol> down
@@ -6711,7 +6711,7 @@ S_regrepeat(pTHX_ regexp *prog, char **startposp, const regnode *p,
 	    scan = loceol;
 	break;
     case CANY:  /* Move <scan> forward <max> bytes, unless goes off end */
-        if (utf8_target && scan + max < loceol) {
+        if (utf8_target && loceol - scan > max) {
 
             /* <loceol> hadn't been adjusted in the UTF-8 case */
             scan +=  max;
@@ -6730,7 +6730,7 @@ S_regrepeat(pTHX_ regexp *prog, char **startposp, const regnode *p,
          * can use UTF8_IS_INVARIANT() even if the pattern isn't UTF-8, as it's
          * true iff it doesn't matter if the argument is in UTF-8 or not */
         if (UTF8_IS_INVARIANT(c) || (! utf8_target && ! is_utf8_pat)) {
-            if (utf8_target && scan + max < loceol) {
+            if (utf8_target && loceol - scan > max) {
                 /* We didn't adjust <loceol> because is UTF-8, but ok to do so,
                  * since here, to match at all, 1 char == 1 byte */
                 loceol = scan + max;
@@ -6910,7 +6910,7 @@ S_regrepeat(pTHX_ regexp *prog, char **startposp, const regnode *p,
         /* FALLTHROUGH */
 
     case POSIXA:
-        if (utf8_target && scan + max < loceol) {
+        if (utf8_target && loceol - scan > max) {
 
             /* We didn't adjust <loceol> at the beginning of this routine
              * because is UTF-8, but it is actually ok to do so, since here, to
diff --git a/t/re/pat_rt_report.t b/t/re/pat_rt_report.t
index 2244fdf..9a9b5f5 100644
--- t/re/pat_rt_report.t
+++ t/re/pat_rt_report.t
@@ -22,7 +22,7 @@ BEGIN {
 }
 
 
-plan tests => 2530;  # Update this when adding/deleting tests.
+plan tests => 2532;  # Update this when adding/deleting tests.
 
 run_tests() unless caller;
 
@@ -1158,6 +1158,21 @@ EOP
             '$_ = "abc"; /b/g; $_ = "hello"; print eval q|$\'|,"\n"',
             "c\n", {}, '$\' first mentioned after match');
     }
+
+    {
+	# [perl #118175] threaded perl-5.18.0 fails pat_rt_report_thr.t
+	# this tests some related failures
+	#
+	# The tests in the block *only* fail when run on 32-bit systems
+	# with a malloc that allocates above the 2GB line.  On the system
+	# in the report above that only happened in a thread.
+	my $s = "\x{1ff}" . "f" x 32;
+	ok($s =~ /\x{1ff}[[:alpha:]]+/gca, "POSIXA pointer wrap");
+
+	# this one segfaulted under the conditions above
+	# of course, CANY is evil, maybe it should crash
+	ok($s =~ /.\C+/, "CANY pointer wrap");
+    }
 } # End of sub run_tests
 
 1;
BOBBLE
}

sub _patch_makefile_sh_phony {
  _patch(<<'END');
diff --git a/Makefile.SH b/Makefile.SH
index ac5ade4..8e66603 100755
--- Makefile.SH
+++ Makefile.SH
@@ -295,6 +295,30 @@ obj = $(obj1) $(obj2) $(obj3) $(ARCHOBJS)
 # EMBEDDING is on by default, and MULTIPLICITY doesn't work.
 #

+.PHONY: all compile translators utilities \
+       FORCE \
+       preplibrary \
+       install install-strip install-all install-verbose install-silent \
+       no-install install.perl install.man installman install.html installhtml \
+       check_byacc run_byacc \
+       regen_headers regen_pods regen_all \
+       clean _tidy _mopup _cleaner1 _cleaner2 \
+       realclean _realcleaner clobber _clobber \
+       distclean veryclean _verycleaner \
+       lint \
+       depend \
+       test check test_prep _test_prep \
+       test_tty test-tty _test_tty test_notty test-notty _test_notty \
+       utest ucheck test.utf8 check.utf8 \
+       test.third check.third utest.third ucheck.third test_notty.third \
+       test.deparse test_notty.deparse \
+       minitest \
+       ok okfile oknack okfilenack nok nokfile noknack nokfilenack \
+       clist hlist shlist pllist \
+       distcheck \
+       elc \
+       etags ctags tags
+
 lintflags = -hbvxac

 .c$(OBJ_EXT):
END
}

sub _patch_cow_speed {
  _patch(<<'COWSAY');
diff --git a/sv.c b/sv.c
index 06c0b83..ac1d972 100644
--- sv.c
+++ sv.c
@@ -1574,14 +1574,19 @@ Perl_sv_grow(pTHX_ SV *const sv, STRLEN newlen)
         newlen++;
 #endif
 
+#if defined(PERL_USE_MALLOC_SIZE) && defined(Perl_safesysmalloc_size)
+#define PERL_UNWARANTED_CHUMMINESS_WITH_MALLOC
+#endif
+
     if (newlen > SvLEN(sv)) {		/* need more room? */
 	STRLEN minlen = SvCUR(sv);
 	minlen += (minlen >> PERL_STRLEN_EXPAND_SHIFT) + 10;
 	if (newlen < minlen)
 	    newlen = minlen;
-#ifndef Perl_safesysmalloc_size
-        if (SvLEN(sv))
+#ifndef PERL_UNWARANTED_CHUMMINESS_WITH_MALLOC
+        if (SvLEN(sv)) {
             newlen = PERL_STRLEN_ROUNDUP(newlen);
+        }
 #endif
 	if (SvLEN(sv) && s) {
 	    s = (char*)saferealloc(s, newlen);
@@ -1593,7 +1598,7 @@ Perl_sv_grow(pTHX_ SV *const sv, STRLEN newlen)
 	    }
 	}
 	SvPV_set(sv, s);
-#ifdef Perl_safesysmalloc_size
+#ifdef PERL_UNWARANTED_CHUMMINESS_WITH_MALLOC
 	/* Do this here, do it once, do it right, and then we will never get
 	   called back into sv_grow() unless there really is some growing
 	   needed.  */
COWSAY
}

sub _patch_preprocess_options {
  my $perl = shift;

  if ($perl =~ /^5\.(?:8|10)\./) {
    _patch(<<'END');
diff --git a/perl.c b/perl.c
index 82e5538..b9e02fe 100644
--- perl.c
+++ perl.c
@@ -3758,7 +3758,7 @@ S_open_script(pTHX_ const char *scriptname, bool dosearch, SV *sv,
 #       ifdef VMS
             cpp_discard_flag = "";
 #       else
-            cpp_discard_flag = "-C";
+            cpp_discard_flag = "-C -ffreestanding";
 #       endif
 
 #       ifdef OS2
END
  } elsif ($perl =~ /^5\.6\./) {
    _patch(<<'END');
diff --git a/perl.c b/perl.c
index 623f9be..014d318 100644
--- perl.c
+++ perl.c
@@ -2631,7 +2631,7 @@ sed %s -e \"/^[^#]/b\" \
  -e '/^#[ 	]*undef[ 	]/b' \
  -e '/^#[ 	]*endif/b' \
  -e 's/^[ 	]*#.*//' \
- %s | %"SVf" -C %"SVf" %s",
+ %s | %"SVf" -C -ffreestanding %"SVf" %s",
 #  endif
 #ifdef LOC_SED
 	  LOC_SED,
END
  }
}

sub _patch_5183_metajson {
_patch(<<'DOGSAY');
diff --git a/META.json b/META.json
index 64caea7..200e324 100644
--- META.json
+++ META.json
@@ -118,7 +118,7 @@
          "TestInit.pm"
       ]
    },
-   "release_status" : "testing",
+   "release_status" : "stable",
    "resources" : {
       "bugtracker" : {
          "web" : "http://rt.perl.org/perlbug/"
DOGSAY
}

sub _patch_handy {
  _patch(<<'END');
--- handy.h
+++ handy.h
@@ -43,12 +43,10 @@
    this file first, then you will have to manually set -DHAS_BOOL in 
    your command line to avoid a conflict.
 */
-#ifdef _G_HAVE_BOOL
-# if _G_HAVE_BOOL
+#ifdef __GNUG__
 #  ifndef HAS_BOOL
 #   define HAS_BOOL 1
 #  endif
-# endif
 #endif
 
 /* The NeXT dynamic loader headers will not build with the bool macro
END
}

sub _replace_makedepend {
  # Replace makedepend with blead's version
  _write_or_die('makedepend.SH', <<'END');
#! /bin/sh
case $PERL_CONFIG_SH in
'')
	if test -f config.sh; then TOP=.;
	elif test -f ../config.sh; then TOP=..;
	elif test -f ../../config.sh; then TOP=../..;
	elif test -f ../../../config.sh; then TOP=../../..;
	elif test -f ../../../../config.sh; then TOP=../../../..;
	else
		echo "Can't find config.sh."; exit 1
	fi
	. $TOP/config.sh
	;;
esac
: This forces SH files to create target in same directory as SH file.
: This is so that make depend always knows where to find SH derivatives.
case "$0" in
*/*) cd `expr X$0 : 'X\(.*\)/'` ;;
esac

echo "Extracting makedepend (with variable substitutions)"
rm -f makedepend
$spitshell >makedepend <<!GROK!THIS!
$startsh
# makedepend.SH
#
MAKE=$make
trnl='$trnl'
!GROK!THIS!
$spitshell >>makedepend <<'!NO!SUBS!'

if test -d .depending; then
	echo "$0: Already running, exiting."
	exit 0
fi

mkdir .depending

# This script should be called with 
#     sh ./makedepend MAKE=$(MAKE)
case "$1" in 
	MAKE=*) eval $1; shift ;;
esac

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

case $PERL_CONFIG_SH in
'')
	if test -f config.sh; then TOP=.;
	elif test -f ../config.sh; then TOP=..;
	elif test -f ../../config.sh; then TOP=../..;
	elif test -f ../../../config.sh; then TOP=../../..;
	elif test -f ../../../../config.sh; then TOP=../../../..;
	else
		echo "Can't find config.sh."; exit 1
	fi
	. $TOP/config.sh
	;;
esac

# Avoid localized gcc messages
case "$ccname" in
    gcc) LC_ALL=C ; export LC_ALL ;;
esac

# We need .. when we are in the x2p directory if we are using the
# cppstdin wrapper script.
# Put .. and . first so that we pick up the present cppstdin, not
# an older one lying about in /usr/local/bin.
PATH=".$path_sep..$path_sep$PATH"
export PATH

case "$osname" in
amigaos) cat=/bin/cat ;; # must be absolute
esac

$cat /dev/null >.deptmp
$rm -f *.c.c c/*.c.c
if test -f Makefile; then
    rm -f $firstmakefile
    cp Makefile $firstmakefile
    # On QNX, 'cp' preserves timestamp, so $firstmakefile appears
    # to be out of date.  I don't know if OS/2 has touch, so do this:
    case "$osname" in
    os2) ;;
    *) $touch $firstmakefile ;;
    esac
fi
mf=$firstmakefile
if test -f $mf; then
    defrule=`<$mf sed -n		\
	-e '/^\.c\$(OBJ_EXT):.*;/{'	\
	-e    's/\$\*\.c//'		\
	-e    's/^[^;]*;[	 ]*//p'	\
	-e    q				\
	-e '}'				\
	-e '/^\.c\$(OBJ_EXT): *$/{'	\
	-e    N				\
	-e    's/\$\*\.c//'		\
	-e    's/^.*\n[	 ]*//p'		\
	-e    q				\
	-e '}'`
fi
case "$defrule" in
'') defrule='$(CC) -c $(CFLAGS)' ;;
esac

: Create files in UU directory to avoid problems with long filenames
: on systems with 14 character filename limits so file.c.c and file.c
: might be identical
$test -d UU || mkdir UU

$MAKE clist || ($echo "Searching for .c files..."; \
	$echo *.c | $tr ' ' $trnl | $egrep -v '\*' >.clist)
for file in `$cat .clist`; do
# for file in `cat /dev/null`; do
    case "$osname" in
    uwin)     uwinfix="-e s,\\\\\\\\,/,g -e s,\\([a-zA-Z]\\):/,/\\1/,g" ;;
    os2)      uwinfix="-e s,\\\\\\\\,/,g" ;;
    cygwin)   uwinfix="-e s,\\\\\\\\,/,g" ;;
    posix-bc) uwinfix="-e s/\\*POSIX(\\(.*\\))/\\1/" ;;
    vos)      uwinfix="-e s/\#/\\\#/" ;;
    *)        uwinfix="" ;;
    esac
    case "$file" in
    *.c) filebase=`basename $file .c` ;;
    *.y) filebase=`basename $file .y` ;;
    esac
    case "$file" in
    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
    *)   finc= ;;
    esac
    $echo "Finding dependencies for $filebase$_o."
    # Below, we strip out all but preprocessor directives.
    # We have to take care of situations like
    #       #if defined(FOO) BAR   /* comment line 1
    #                                 more comment lines */
    # If we just delete text starting from the '/*' to the end of line, we will
    # screw up cases like
    #      #if defined(FOO)    /* comment */ \
    #          && defined(BAR) /* comment */ \
    #          && defined(BAZ) /* comment */ \
    #               etc.
    # Also, in lines like
    #      #defined FOO(a,b)    a/**/b
    # the comment may be important and so needs to be retained.
    # This code processes the single-line comments first; it assumes there is
    # at most one straightforward comment per continued preprocessor line,
    # replacing each non-empty comment (and its surrounding white space) by a
    # single space.  (sed only has a greedy '*' quantifier, so this doesn't
    # work right if there are multiple comments per line, and strings can look
    # like comments to it; both are unlikely in a preprocessor statement.) Any
    # continuation line is joined, and the process repeated on the enlarged
    # line as long as there are continuations.  At the end, if there are any
    # comments remaining, they are either completely empty or are like the
    # first situation.  The latter are just deleted by first deleting to the
    # end of line (including preceding white space) things that start with '/*'
    # and the next char isn't a '*'; then things that start with '/**', but the
    # next char isn't a '/'.  (Subsequent lines of the comment are irrelevant
    # and get dropped.)  At the end, we unjoin very long lines to avoid
    # preprocessor limitations
    ( $echo "#line 2 \"$file\"";                               \
      $sed -n <$file                                           \
	-e "/^${filebase}_init(/q"                             \
        -e ': testcont'                                        \
	-e '/^[ 	]*#/s|[ 	]*/\*..*\*/[ 	]*| |' \
        -e '/\\$/{'                                            \
            -e 'N'                                             \
            -e 'b testcont'                                    \
        -e '}'                                                 \
	-e 's/\\\n//g'                                         \
	-e '/^#line/d'                                         \
	-e '/^[ 	]*#/{'                                 \
	    -e 's|[ 	]*/\*[^*].*$||'                        \
	    -e 's|[ 	]*/\*\*[^/].*$||'                      \
            -e 's/.\{255\}/&\\\n/g'                           \
	    -e p                                               \
	-e '}' ) >UU/$file.c

    # We're not sure why this was there; the #endif is extraneous on modern z/OS
    #if [ "$osname" = os390 -a "$file" = perly.c ]; then
    #   $echo '#endif' >>UU/$file.c
    #fi

    if [ "$osname" = os390 ]; then
        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c |
        $sed \
    	    -e '/^#.*<stdin>/d' \
	    -e '/^#.*"-"/d' \
	    -e '/^#.*git_version\.h/d' \
	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
	    -e 's/^[	 ]*#[	 ]*line/#/' \
	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
	    -e 's|: \./|: |' \
	    -e 's|\.c\.c|.c|' $uwinfix | \
        $uniq | $sort | $uniq >> .deptmp
    else
        $cppstdin $finc -I. $cppflags $cppminus <UU/$file.c >.cout 2>.cerr
        $sed \
	    -e '1d' \
	    -e '/^#.*<stdin>/d' \
            -e '/^#.*<builtin>/d' \
            -e '/^#.*<built-in>/d' \
            -e '/^#.*<command line>/d' \
            -e '/^#.*<command-line>/d' \
	    -e '/^#.*"-"/d' \
	    -e '/^#.*"\/.*\/"/d' \
	    -e '/: file path prefix .* never used$/d' \
	    -e '/^#.*git_version\.h/d' \
	    -e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
	    -e 's/^[	 ]*#[	 ]*line/#/' \
	    -e '/^# *[0-9][0-9]* *[".\/]/!d' \
	    -e 's/^.*"\(.*\)".*$/'$filebase'\$(OBJ_EXT): \1/' \
	    -e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'\$(OBJ_EXT): \1/' \
	    -e 's|: \./|: |' \
           -e 's|\.c\.c|.c|' $uwinfix .cout .cerr| \
        $uniq | $sort | $uniq >> .deptmp
    fi
    echo "$filebase\$(OBJ_EXT): $@" >> .deptmp
done

$sed <$mf >$mf.new -e '1,/^# AUTOMATICALLY/!d'

if $test -s .deptmp; then
    $echo "Updating $mf..."
    $echo "# If this runs make out of memory, delete /usr/include lines." \
	>> $mf.new
    if [ "$osname" = vos ]; then
        $sed 's|\.incl\.c|.h|' .deptmp >.deptmp.vos
        mv -f .deptmp.vos .deptmp
    fi
    $sed 's|^\(.*\$(OBJ_EXT):\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
       >>$mf.new
else
    $MAKE hlist || ($echo "Searching for .h files..."; \
	$echo *.h | $tr ' ' $trnl | $egrep -v '\*' >.hlist)
    $echo "You don't seem to have a proper C preprocessor.  Using grep instead."
    $egrep '^#include ' `cat .clist` `cat .hlist`  >.deptmp
    $echo "Updating $mf..."
    <.clist $sed -n							\
	-e '/\//{'							\
	-e   's|^\(.*\)/\(.*\)\.c|\2\$(OBJ_EXT): \1/\2.c; '"$defrule \1/\2.c|p"	\
	-e   d								\
	-e '}'								\
	-e 's|^\(.*\)\.c|\1\$(OBJ_EXT): \1.c|p' >> $mf.new
    <.hlist $sed -n 's|\(.*/\)\(.*\)|s= \2= \1\2=|p' >.hsed
    <.deptmp $sed -n 's|c:#include "\(.*\)".*$|o: \1|p' | \
       $sed 's|^[^;]*/||' | \
       $sed -f .hsed >> $mf.new
    <.deptmp $sed -n 's|h:#include "\(.*\)".*$|h: \1|p' | \
       $sed -f .hsed >> $mf.new
fi
$rm -f $mf.old
$cp $mf $mf.old
$rm -f $mf
$cp $mf.new $mf
$rm $mf.new
$echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
$rm -rf .deptmp UU .clist .hlist .hsed .cout .cerr
rmdir .depending

!NO!SUBS!
$eunicefix makedepend
chmod +x makedepend
END
}  

sub _patch_5_005_02 {
  _patch(<<'END');
--- Configure
+++ Configure
@@ -21,7 +21,7 @@
 # $Id: Head.U,v 3.0.1.9 1997/02/28 15:02:09 ram Exp $
 #
 # Generated on Tue Jul  7 10:10:21 EDT 1998 [metaconfig 3.0 PL70]
-# (with additional metaconfig patches by doughera@lafayette.edu)
+# (with additional metaconfig patches by jhi@iki.fi)
 
 cat >/tmp/c1$$ <<EOF
 ARGGGHHHH!!!!!
@@ -56,33 +56,6 @@ case "$0" in
 	;;
 esac
 
-: the newline for tr
-if test X"$trnl" = X; then
-	case "`echo foo|tr '\n' x 2>/dev/null`" in
-	foox)
-		trnl='\n'
-		;;
-	esac
-fi
-if test X"$trnl" = X; then
-	case "`echo foo|tr '\012' x 2>/dev/null`" in
-	foox)
-		trnl='\012'
-		;;
-	esac
-fi
-if test -n "$DJGPP"; then
-	trnl='\012'
-fi
-if test X"$trnl" = X; then
-	cat <<EOM >&2
-
-$me: Fatal Error: cannot figure out how to translate newlines with 'tr'.
-
-EOM
-	exit 1
-fi
-
 : Proper separator for the PATH environment variable
 p_=:
 : On OS/2 this directory should exist if this is not floppy only system :-]
@@ -391,7 +364,6 @@ d_getservprotos=''
 d_getsbyname=''
 d_getsbyport=''
 d_gnulibc=''
-i_arpainet=''
 d_htonl=''
 d_inetaton=''
 d_isascii=''
@@ -540,6 +512,7 @@ dlsrc=''
 ld=''
 lddlflags=''
 usedl=''
+ebcdic=''
 doublesize=''
 fpostype=''
 gidtype=''
@@ -548,6 +521,7 @@ h_fcntl=''
 h_sysfile=''
 db_hashtype=''
 db_prefixtype=''
+i_arpainet=''
 i_db=''
 i_dbm=''
 i_rpcsvcdbm=''
@@ -633,6 +607,7 @@ libpth=''
 loclibpth=''
 plibpth=''
 xlibpth=''
+ignore_versioned_solibs=''
 libs=''
 lns=''
 lseektype=''
@@ -697,11 +672,13 @@ randbits=''
 installscript=''
 scriptdir=''
 scriptdirexp=''
+selectminbits=''
 selecttype=''
 sh=''
 sig_name=''
 sig_name_init=''
 sig_num=''
+sig_num_init=''
 installsitearch=''
 sitearch=''
 sitearchexp=''
@@ -719,6 +696,7 @@ startperl=''
 startsh=''
 stdchar=''
 sysman=''
+trnl=''
 uidtype=''
 nm_opt=''
 nm_so_opt=''
@@ -733,7 +711,6 @@ mips_type=''
 usrinc=''
 defvoidused=''
 voidflags=''
-ebcdic=''
 CONFIG=''
 
 define='define'
@@ -836,6 +813,8 @@ plibpth=''
 
 : default library list
 libswanted=''
+: some systems want only to use the non-versioned libso:s
+ignore_versioned_solibs=''
 : Possible local include directories to search.
 : Set locincpth to "" in a hint file to defeat local include searches.
 locincpth="/usr/local/include /opt/local/include /usr/gnu/include"
@@ -904,7 +883,7 @@ case "$sh" in
 $me:  Fatal Error:  I can't find a Bourne Shell anywhere.  
 
 Usually it's in /bin/sh.  How did you even get this far?
-Please contact me (Andy Dougherty) at doughera@lafayette.edu and 
+Please contact me (Jarkko Hietaniemi) at jhi@iki.fi and 
 we'll try to straighten this all out.
 EOM
 	exit 1
@@ -1240,7 +1219,7 @@ cat >extract <<'EOS'
 CONFIG=true
 echo "Doing variable substitutions on .SH files..."
 if test -f $src/MANIFEST; then
-	set x `awk '{print $1}' <$src/MANIFEST | grep '\.SH'`
+	set x `awk '{print $1}' <$src/MANIFEST | grep '\.SH$'`
 else
 	echo "(Looking for .SH files under the source directory.)"
 	set x `(cd $src; find . -name "*.SH" -print)`
@@ -1373,7 +1352,7 @@ THIS PACKAGE SEEMS TO BE INCOMPLETE.
 You have the option of continuing the configuration process, despite the
 distinct possibility that your kit is damaged, by typing 'y'es.  If you
 do, don't blame me if something goes wrong.  I advise you to type 'n'o
-and contact the author (doughera@lafayette.edu).
+and contact the author (jhi@iki.fi).
 
 EOM
 		echo $n "Continue? [n] $c" >&4
@@ -1396,6 +1375,30 @@ else
 fi
 rm -f missing x??
 
+echo " "
+: Find the appropriate value for a newline for tr
+if test -n "$DJGPP"; then
+       trnl='\012'
+fi
+if test X"$trnl" = X; then
+	case "`echo foo|tr '\n' x 2>/dev/null`" in
+	foox) trnl='\n' ;;
+	esac
+fi
+if test X"$trnl" = X; then
+	case "`echo foo|tr '\012' x 2>/dev/null`" in
+	foox) trnl='\012' ;;
+	esac
+fi
+if test X"$trnl" = X; then
+	cat <<EOM >&2
+
+$me: Fatal Error: cannot figure out how to translate newlines with 'tr'.
+
+EOM
+	exit 1
+fi
+
 : compute the number of columns on the terminal for proper question formatting
 case "$COLUMNS" in
 '') COLUMNS='80';;
@@ -1574,7 +1577,7 @@ Much effort has been expended to ensure that this shell script will run on any
 Unix system.  If despite that it blows up on yours, your best bet is to edit
 Configure and run it again.  If you can't run Configure for some reason,
 you'll have to generate a config.sh file by hand.  Whatever problems you
-have, let me (doughera@lafayette.edu) know how I blew it.
+have, let me (jhi@iki.fi) know how I blew it.
 
 This installation script affects things in two ways:
 
@@ -1841,14 +1844,14 @@ ABYZ)
 	    *C9D1*|*c9d1*)
 		echo "Hey, this might be EBCDIC." >&4
 		if test "X$up" = X -o "X$low" = X; then
-		    case "`echo IJ | tr '[A-IJ-RS-Z]' '[a-ij-rs-z]' 2>/dev/null`" in
+		    case "`echo IJ | $tr '[A-IJ-RS-Z]' '[a-ij-rs-z]' 2>/dev/null`" in
 		    ij) up='[A-IJ-RS-Z]'
 		        low='[a-ij-rs-z]'
 			;;
 		    esac
 		fi
 		if test "X$up" = X -o "X$low" = X; then
-		    case "`echo IJ | tr A-IJ-RS-Z a-ij-rs-z 2>/dev/null`" in
+		    case "`echo IJ | $tr A-IJ-RS-Z a-ij-rs-z 2>/dev/null`" in
 		    ij) up='A-IJ-RS-Z'
 		        low='a-ij-rs-z'
 			;;
@@ -1941,7 +1944,7 @@ EOM
 	(cd $src/hints; ls -C *.sh) | $sed 's/\.sh/   /g' >&4
 	dflt=''
 	: Half the following guesses are probably wrong... If you have better
-	: tests or hints, please send them to doughera@lafayette.edu
+	: tests or hints, please send them to jhi@iki.fi
 	: The metaconfig authors would also appreciate a copy...
 	$test -f /irix && osname=irix
 	$test -f /xenix && osname=sco_xenix
@@ -2025,7 +2028,7 @@ EOM
 			osvers="$3"
 			;;
 		dynixptx*) osname=dynixptx
-			osvers="$3"
+			osvers=`echo "$4" | $sed 's/^v//'`
 			;;
 		freebsd) osname=freebsd 
 			osvers="$3" ;;
@@ -3442,7 +3445,11 @@ cat <<'EOT' >testcpp.c
 ABC.XYZ
 EOT
 cd ..
+if test ! -f cppstdin; then
 echo 'cat >.$$.c; '"$cc"' -E ${1+"$@"} .$$.c; rm .$$.c' >cppstdin
+else
+	echo "Keeping your $hint cppstdin wrapper."
+fi
 chmod 755 cppstdin
 wrapper=`pwd`/cppstdin
 ok='false'
@@ -3693,7 +3700,8 @@ case "$libswanted" in
 esac
 for thislib in $libswanted; do
 	
-	if xxx=`./loc lib$thislib.$so.[0-9]'*' X $libpth`; $test -f "$xxx"; then
+	if xxx=`./loc lib$thislib.$so.[0-9]'*' X $libpth`;
+		$test -f "$xxx" -a "X$ignore_versioned_solibs" = "X"; then
 		echo "Found -l$thislib (shared)."
 		case " $dflt " in
 		*"-l$thislib "*);;
@@ -3980,10 +3988,21 @@ rmlist="$rmlist pdp11"
 : coherency check
 echo " "
 echo "Checking your choice of C compiler and flags for coherency..." >&4
+$cat > try.c <<'EOF'
+#include <stdio.h>
+main() { printf("Ok\n"); exit(0); }
+EOF
 set X $cc $optimize $ccflags -o try $ldflags try.c $libs
 shift
-$cat >try.msg <<EOM
-I've tried to compile and run a simple program with:
+$cat >try.msg <<'EOM'
+I've tried to compile and run the following simple program:
+
+EOM
+$cat try.c
+
+$cat >> try.msg <<EOM
+
+I used the command:
 
 	$*
 	./try
@@ -3991,10 +4010,6 @@ I've tried to compile and run a simple program with:
 and I got the following output:
 
 EOM
-$cat > try.c <<'EOF'
-#include <stdio.h>
-main() { printf("Ok\n"); exit(0); }
-EOF
 dflt=y
 if sh -c "$cc $optimize $ccflags -o try $ldflags try.c $libs" >>try.msg 2>&1; then
 	if sh -c './try' >>try.msg 2>&1; then
@@ -4031,7 +4046,7 @@ y)
 	$cat try.msg >&4
 	case "$knowitall" in
 	'')
-		echo "(The supplied flags might be incorrect with this C compiler.)"
+		echo "(The supplied flags or libraries might be incorrect.)"
 		;;
 	*) dflt=n;;
 	esac
@@ -4149,9 +4164,8 @@ eval $inhdr
 : determine which malloc to compile in
 echo " "
 case "$usemymalloc" in
-''|y*|true)	dflt='y' ;;
-n*|false)	dflt='n' ;;
-*)	dflt="$usemymalloc" ;;
+''|[yY]*|true|$define)	dflt='y' ;;
+*)	dflt='n' ;;
 esac
 rp="Do you wish to attempt to use the malloc that comes with $package?"
 . ./myread
@@ -4253,7 +4267,7 @@ understands function prototypes.  Unfortunately, your C compiler
 	$cc $ccflags
 doesn't seem to understand them.  Sorry about that.
 
-If GNU cc is avaiable for your system, perhaps you could try that instead.  
+If GNU cc is available for your system, perhaps you could try that instead.  
 
 Eventually, we hope to support building Perl with pre-ANSI compilers.
 If you would like to help in that effort, please contact <perlbug@perl.org>.
@@ -4308,32 +4322,6 @@ shift;
 $cc $optimize $ccflags $ldflags -o ${mc_file} $* ${mc_file}.c $libs;'
 
 echo " "
-echo "Determining whether or not we are on an EBCDIC system..." >&4
-cat >tebcdic.c <<EOM
-int main()
-{
-  if ('M'==0xd4) return 0;
-  return 1;
-}
-EOM
-val=$undef
-set tebcdic
-if eval $compile_ok; then
-	if ./tebcdic; then
-		echo "You have EBCDIC." >&4
-		val="$define"
-	else
-		echo "Nope, no EBCDIC.  Assuming ASCII or some ISO Latin." >&4
-	fi
-else
-	echo "I'm unable to compile the test program." >&4
-	echo "I'll asuume ASCII or some ISO Latin." >&4
-fi
-$rm -f tebcdic.c tebcdic
-set ebcdic
-eval $setvar
-
-echo " "
 echo "Checking for GNU C Library..." >&4
 cat >gnulibc.c <<EOM
 #include <stdio.h>
@@ -5147,7 +5135,7 @@ case "$shrpdir" in
 *)	$cat >&4 <<EOM
 WARNING:  Use of the shrpdir variable for the installation location of
 the shared $libperl is not supported.  It was never documented and
-will not work in this version.  Let me (doughera@lafayette.edu)
+will not work in this version.  Let me (jhi@iki.fi)
 know of any problems this may cause.
 
 EOM
@@ -6703,6 +6691,10 @@ eval $setvar
 set difftime d_difftime
 eval $inlibc
 
+: see if sys/stat.h is available
+set sys/stat.h i_sysstat
+eval $inhdr
+
 : see if this is a dirent system
 echo " "
 if xinc=`./findhdr dirent.h`; $test "$xinc"; then
@@ -6771,6 +6763,23 @@ set d_dirnamlen
 eval $setvar
 $rm -f try.c
 
+hasfield='varname=$1; struct=$2; field=$3; shift; shift; shift;
+while $test $# -ge 2; do
+	case "$1" in
+	$define) echo "#include <$2>";;
+	esac ;
+    shift 2;
+done > try.c;
+echo "int main () { struct $struct foo; foo.$field = 0; }" >> try.c;
+if eval $cc $optimize $ccflags -c try.c >/dev/null 2>&1; then
+	val="$define";
+else
+	val="$undef";
+fi;
+set $varname;
+eval $setvar;
+$rm -f try.c try.o'
+
 : see if dlerror exists
 xxx_runnm="$runnm"
 runnm=false
@@ -7305,7 +7314,7 @@ esac
 set netinet/in.h i_niin sys/in.h i_sysin
 eval $inhdr
 
-: see if this is an arpa/inet.h
+: see if arpa/inet.h has to be included
 set arpa/inet.h i_arpainet
 eval $inhdr
 
@@ -7635,7 +7644,7 @@ case "$osname" in
 freebsd)
     case "`ipcs 2>&1`" in
     "SVID messages"*"not configured"*)
-	echo "But your FreeBSD kernel does not have the msg*(2) configured." >&4
+	echo "But your $osname does not have the msg*(2) configured." >&4
         h_msg=false
 	val="$undef"
 	set msgctl d_msgctl
@@ -7678,7 +7687,7 @@ set poll d_poll
 eval $inlibc
 
 
-: see whether the various POSIXish _yields exist within given cccmd
+: see whether the various POSIXish _yields exist
 $cat >try.c <<EOP
 #include <pthread.h>
 main() {
@@ -8136,7 +8145,7 @@ case "$osname" in
 freebsd)
     case "`ipcs 2>&1`" in
     "SVID messages"*"not configured"*)
-	echo "But your FreeBSD kernel does not have the sem*(2) configured." >&4
+	echo "But your $osname does not have the sem*(2) configured." >&4
         h_sem=false
 	val="$undef"
 	set semctl d_semctl
@@ -8185,6 +8194,31 @@ case "$d_sem" in
 $define)
     : see whether semctl IPC_STAT can use union semun
     echo " "
+    $cat > try.h <<END
+#ifndef S_IRUSR
+#   ifdef S_IREAD
+#	define S_IRUSR S_IREAD
+#	define S_IWUSR S_IWRITE
+#	define S_IXUSR S_IEXEC
+#   else
+#	define S_IRUSR 0400
+#	define S_IWUSR 0200
+#	define S_IXUSR 0100
+#   endif
+#   define S_IRGRP (S_IRUSR>>3)
+#   define S_IWGRP (S_IWUSR>>3)
+#   define S_IXGRP (S_IXUSR>>3)
+#   define S_IROTH (S_IRUSR>>6)
+#   define S_IWOTH (S_IWUSR>>6)
+#   define S_IXOTH (S_IXUSR>>6)
+#endif
+#ifndef S_IRWXU
+#   define S_IRWXU (S_IRUSR|S_IWUSR|S_IXUSR)
+#   define S_IRWXG (S_IRGRP|S_IWGRP|S_IXGRP)
+#   define S_IRWXO (S_IROTH|S_IWOTH|S_IXOTH)
+#endif
+END
+
     $cat > try.c <<END
 #include <sys/types.h>
 #include <sys/ipc.h>
@@ -8259,6 +8293,7 @@ END
 #include <sys/stat.h>
 #include <stdio.h>
 #include <errno.h>
+#include "try.h"
 #ifndef errno
 extern int errno;
 #endif
@@ -8305,6 +8340,7 @@ END
     *)  echo "You cannot use struct semid_ds * for semctl IPC_STAT." >&4
         ;;
     esac
+    $rm -f try.h
     ;;
 *)  val="$undef"
 
@@ -8499,7 +8535,7 @@ case "$osname" in
 freebsd)
     case "`ipcs 2>&1`" in
     "SVID shared memory"*"not configured"*)
-	echo "But your FreeBSD kernel does not have the shm*(2) configured." >&4
+	echo "But your $osname does not have the shm*(2) configured." >&4
         h_shm=false
 	val="$undef"
 	set shmctl d_shmctl
@@ -8652,21 +8688,8 @@ eval $inlibc
 
 : see if stat knows about block sizes
 echo " "
-xxx=`./findhdr sys/stat.h`
-if $contains 'st_blocks;' "$xxx" >/dev/null 2>&1 ; then
-	if $contains 'st_blksize;' "$xxx" >/dev/null 2>&1 ; then
-		echo "Your stat() knows about block sizes." >&4
-		val="$define"
-	else
-		echo "Your stat() doesn't know about block sizes." >&4
-		val="$undef"
-	fi
-else
-	echo "Your stat() doesn't know about block sizes." >&4
-	val="$undef"
-fi
-set d_statblks
-eval $setvar
+set d_statblks stat st_blocks $i_sysstat sys/stat.h
+eval $hasfield
 
 : see if _ptr and _cnt from stdio act std
 echo " "
@@ -9610,6 +9633,32 @@ EOCP
 esac
 $rm -f try.c try
 
+echo " "
+echo "Determining whether or not we are on an EBCDIC system..." >&4
+$cat >tebcdic.c <<EOM
+int main()
+{
+  if ('M'==0xd4) return 0;
+  return 1;
+}
+EOM
+val=$undef
+set tebcdic
+if eval $compile_ok; then
+	if ./tebcdic; then
+		echo "You have EBCDIC." >&4
+		val="$define"
+	else
+		echo "Nope, no EBCDIC.  Assuming ASCII or some ISO Latin." >&4
+	fi
+else
+	echo "I'm unable to compile the test program." >&4
+	echo "I'll assume ASCII or some ISO Latin." >&4
+fi
+$rm -f tebcdic.c tebcdic
+set ebcdic
+eval $setvar
+
 : see what type file positions are declared as in the library
 rp="What is the type for file position used by fsetpos()?"
 set fpos_t fpostype long stdio.h sys/types.h
@@ -10217,8 +10266,10 @@ EOM
 		: The first arg can be int, unsigned, or size_t
 		: The last arg may or may not be 'const'
 		val=''
+		: void pointer has been seen but using that
+		: breaks the selectminbits test
 		for xxx in 'fd_set *' 'int *'; do
-			for nfd in 'int' 'size_t' 'unsigned' ; do
+			for nfd in 'int' 'size_t' 'unsigned' 'unsigned long'; do
 				for tmo in 'struct timeval *' 'const struct timeval *'; do
 					case "$val" in
 					'')	try="extern select _(($nfd, $xxx, $xxx, $xxx, $tmo));"
@@ -10250,6 +10301,100 @@ EOM
 	;;
 esac
 
+: check for the select 'width'
+case "$selectminbits" in
+'') case "$d_select" in
+	$define)
+		$cat <<EOM
+
+Checking to see on how many bits at a time your select() operates...
+EOM
+		$cat >try.c <<EOCP
+#include <sys/types.h>
+#$i_time I_TIME
+#$i_systime I_SYS_TIME
+#$i_systimek I_SYS_TIME_KERNEL
+#ifdef I_TIME
+#   include <time.h>
+#endif
+#ifdef I_SYS_TIME
+#   ifdef I_SYS_TIME_KERNEL
+#	define KERNEL
+#   endif
+#   include <sys/time.h>
+#   ifdef I_SYS_TIME_KERNEL
+#	undef KERNEL
+#   endif
+#endif
+#$i_sysselct I_SYS_SELECT
+#ifdef I_SYS_SELECT
+#include <sys/select.h>
+#endif
+#include <stdio.h>
+$selecttype b;
+#define S sizeof(*(b))
+#define MINBITS	64
+#define NBYTES (S * 8 > MINBITS ? S : MINBITS/8)
+#define NBITS  (NBYTES * 8)
+int main() {
+    char s[NBYTES];
+    struct timeval t;
+    int i;
+    FILE* fp;
+    int fd;
+
+    fclose(stdin);
+    fp = fopen("try.c", "r");
+    if (fp == 0)
+      exit(1);
+    fd = fileno(fp);
+    if (fd < 0)
+      exit(2);
+    b = ($selecttype)s;
+    for (i = 0; i < NBITS; i++)
+	FD_SET(i, b);
+    t.tv_sec  = 0;
+    t.tv_usec = 0;
+    select(fd + 1, b, 0, 0, &t);
+    for (i = NBITS - 1; i > fd && FD_ISSET(i, b); i--);
+    printf("%d\n", i + 1);
+    return 0;
+}
+EOCP
+		set try
+		if eval $compile_ok; then
+			selectminbits=`./try`
+			case "$selectminbits" in
+			'')	cat >&4 <<EOM
+Cannot figure out on how many bits at a time your select() operates.
+I'll play safe and guess it is 32 bits.
+EOM
+				selectminbits=32
+				bits="32 bits"
+				;;
+			1)	bits="1 bit" ;;
+			*)	bits="$selectminbits bits" ;;
+			esac
+			echo "Your select() operates on $bits at a time." >&4
+		else
+			rp='What is the minimum number of bits your select() operates on?'
+			case "$byteorder" in
+			1234|12345678)	dflt=32 ;;
+			*)		dflt=1	;;
+			esac
+			. ./myread
+			val=$ans
+			selectminbits="$val"
+		fi
+		$rm -f try.* try
+		;;
+	*)	: no select, so pick a harmless default
+		selectminbits='32'
+		;;
+	esac
+	;;
+esac
+
 : Trace out the files included by signal.h, then look for SIGxxx names.
 : Remove SIGARRAYSIZE used by HPUX.
 : Remove SIGTYP void lines used by OS2.
@@ -10458,7 +10603,13 @@ $eunicefix signal_cmd
 : generate list of signal names
 echo " "
 case "$sig_name_init" in
-'')
+'') doinit=yes ;;
+*)  case "$sig_num_init" in
+    ''|*,*) doinit=yes ;;
+    esac ;;
+esac
+case "$doinit" in
+yes)
 	echo "Generating a list of signal names and numbers..." >&4
 	. ./signal_cmd
 	sig_name=`$awk '{printf "%s ", $1}' signal.lst`
@@ -10466,7 +10617,9 @@ case "$sig_name_init" in
 	sig_name_init=`$awk 'BEGIN { printf "\"ZERO\", " }
 						{ printf "\"%s\", ", $1 }
 						END { printf "0\n" }' signal.lst`
-	sig_num=`$awk 'BEGIN { printf "0, " }
+	sig_num=`$awk '{printf "%d ", $2}' signal.lst`
+	sig_num="0 $sig_num"
+	sig_num_init=`$awk 'BEGIN { printf "0, " }
 					{ printf "%d, ", $2}
 					END { printf "0\n"}' signal.lst`
 	;;
@@ -10830,7 +10983,13 @@ $rm -f try.c
 EOS
 chmod +x ccsym
 $eunicefix ccsym
-./ccsym | $sort | $uniq >ccsym.raw
+./ccsym > ccsym1.raw
+if $test -s ccsym1.raw; then
+       $sort ccsym1.raw | $uniq >ccsym.raw
+else
+       mv ccsym1.raw ccsym.raw
+fi
+
 $awk '/\=/ { print $0; next }
 	{ print $0"=1" }' ccsym.raw >ccsym.list
 $awk '{ print $0"=1" }' Cppsym.true >ccsym.true
@@ -11055,10 +11214,6 @@ eval $inhdr
 set sys/resource.h i_sysresrc
 eval $inhdr
 
-: see if sys/stat.h is available
-set sys/stat.h i_sysstat
-eval $inhdr
-
 : see if this is a sys/un.h system
 set sys/un.h i_sysun
 eval $inhdr
@@ -11195,6 +11350,7 @@ for xxx in $known_extensions ; do
 		esac
 		;;
 	IPC/SysV|ipc/sysv)
+		: XXX Do we need a useipcsysv variable here
 		case "${d_msg}${d_sem}${d_shm}" in 
 		*"${define}"*) avail_ext="$avail_ext $xxx" ;;
 		esac
@@ -11774,6 +11930,7 @@ i_values='$i_values'
 i_varargs='$i_varargs'
 i_varhdr='$i_varhdr'
 i_vfork='$i_vfork'
+ignore_versioned_solibs='$ignore_versioned_solibs'
 incpath='$incpath'
 inews='$inews'
 installarchlib='$installarchlib'
@@ -11882,6 +12039,7 @@ runnm='$runnm'
 scriptdir='$scriptdir'
 scriptdirexp='$scriptdirexp'
 sed='$sed'
+selectminbits='$selectminbits'
 selecttype='$selecttype'
 sendmail='$sendmail'
 sh='$sh'
@@ -11894,6 +12052,7 @@ shsharp='$shsharp'
 sig_name='$sig_name'
 sig_name_init='$sig_name_init'
 sig_num='$sig_num'
+sig_num_init='$sig_num_init'
 signal_t='$signal_t'
 sitearch='$sitearch'
 sitearchexp='$sitearchexp'
@@ -12023,51 +12182,6 @@ esac
 : if this fails, just run all the .SH files by hand
 . ./config.sh
 
-case "$ebcdic" in
-$define)
-    xxx=''
-    echo "This is an EBCDIC system, checking if any parser files need regenerating." >&4
-    rm -f y.tab.c y.tab.h
-    yacc -d perly.y >/dev/null 2>&1
-    if cmp -s y.tab.c perly.c; then
-        rm -f y.tab.c
-    else
-        echo "perly.y -> perly.c" >&4
-        mv -f y.tab.c perly.c
-        chmod u+w perly.c
-        sed -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
-            -e 's/y\.tab/perly/g' perly.c >perly.tmp && mv perly.tmp perly.c
-        xxx="$xxx perly.c"
-    fi
-    if cmp -s y.tab.h perly.h; then
-        rm -f y.tab.h
-    else
-        echo "perly.y -> perly.h" >&4
-        mv -f y.tab.h perly.h
-        xxx="$xxx perly.h"
-    fi
-    echo "x2p/a2p.y" >&4
-    cd x2p
-    rm -f y.tab.c
-    yacc a2p.y >/dev/null 2>&1
-    if cmp -s y.tab.c a2p.c
-    then
-        rm -f y.tab.c
-    else
-        echo "a2p.y -> a2p.c" >&4
-        mv -f y.tab.c a2p.c
-        chmod u+w a2p.c
-        sed -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
-            -e 's/y\.tab/a2p/g' a2p.c >a2p.tmp && mv a2p.tmp a2p.c
-        xxx="$xxx a2p.c"
-    fi
-    cd ..
-    case "$xxx" in
-    '') echo "No parser files were regenerated.  That's okay." >&4 ;;
-    esac
-    ;;
-esac
-
 echo " "
 exec 1>&4
 . ./UU/extract
--- Makefile.SH
+++ Makefile.SH
@@ -644,3 +644,83 @@ case `pwd` in
     ;;
 esac
 $rm -f $firstmakefile
+
+# Now do any special processing required before building.
+
+case "$ebcdic" in
+$define)
+    xxx=''
+    echo "This is an EBCDIC system, checking if any parser files need regenerating." >&4
+case "$osname" in
+os390)
+    rm -f y.tab.c y.tab.h
+    yacc -d perly.y >/dev/null 2>&1
+    if cmp -s y.tab.c perly.c; then
+        rm -f y.tab.c
+    else
+        echo "perly.y -> perly.c" >&2
+        mv -f y.tab.c perly.c
+        chmod u+w perly.c
+        sed -e '/^#include "perl\.h"/a\
+\
+#define yydebug    PL_yydebug\
+#define yynerrs    PL_yynerrs\
+#define yyerrflag  PL_yyerrflag\
+#define yychar     PL_yychar\
+#define yyval      PL_yyval\
+#define yylval     PL_yylval'				\
+            -e '/YYSTYPE *yyval;/D'			\
+            -e '/YYSTYPE *yylval;/D'			\
+            -e '/int  yychar,/,/yynerrs;/D'		\
+            -e 's/int yydebug = 0;/yydebug = 0;/'	\
+            -e 's/[^_]realloc(/PerlMem_realloc(/g'	\
+            -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
+            -e 's/y\.tab/perly/g' perly.c >perly.tmp && mv perly.tmp perly.c
+        xxx="$xxx perly.c"
+    fi
+    if cmp -s y.tab.h perly.h; then
+        rm -f y.tab.h
+    else
+        echo "perly.y -> perly.h" >&2
+        mv -f y.tab.h perly.h
+        xxx="$xxx perly.h"
+    fi
+    if cd x2p
+    then
+        rm -f y.tab.c y.tab.h
+        yacc a2p.y >/dev/null 2>&1
+        if cmp -s y.tab.c a2p.c
+        then
+            rm -f y.tab.c
+        else
+            echo "a2p.y -> a2p.c" >&2
+            mv -f y.tab.c a2p.c
+            chmod u+w a2p.c
+            sed -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
+                -e 's/y\.tab/a2p/g' a2p.c >a2p.tmp && mv a2p.tmp a2p.c
+            xxx="$xxx a2p.c"
+        fi
+        # In case somebody yacc -d:ed the a2p.y.
+        if test -f y.tab.h
+        then
+            if cmp -s y.tab.h a2p.h
+            then
+                rm -f y.tab.h
+            else
+                echo "a2p.h -> a2p.h" >&2
+                mv -f y.tab.h a2p.h
+                xxx="$xxx a2p.h"
+            fi
+        fi
+        cd ..
+    fi
+    ;;
+*)
+    echo "'$osname' is an EBCDIC system I don't know that well." >&4
+    ;;
+esac
+    case "$xxx" in
+    '') echo "No parser files were regenerated.  That's okay." >&2 ;;
+    esac
+    ;;
+esac
--- config_h.SH
+++ config_h.SH
@@ -1813,7 +1813,7 @@ sed <<!GROK!THIS! >config.h -e 's!^#undef\(.*/\)\*!/\*#define\1 \*!' -e 's!^#un-
  *	the sig_name list.
  */
 #define SIG_NAME $sig_name_init		/**/
-#define SIG_NUM  $sig_num			/**/
+#define SIG_NUM  $sig_num_init		/**/
 
 /* VOIDFLAGS:
  *	This symbol indicates how much support of the void type is given by this
@@ -1902,6 +1902,15 @@ sed <<!GROK!THIS! >config.h -e 's!^#undef\(.*/\)\*!/\*#define\1 \*!' -e 's!^#un-
 #define PRIVLIB "$privlib"		/**/
 #define PRIVLIB_EXP "$privlibexp"		/**/
 
+/* SELECT_MIN_BITS:
+ *	This symbol holds the minimum number of bits operated by select.
+ *	That is, if you do select(n, ...), how many bits at least will be
+ *	cleared in the masks if some activity is detected.  Usually this
+ *	is either n or 32*ceil(n/32), especially many little-endians do
+ *	the latter.  This is only useful if you have select(), naturally.
+ */
+#define SELECT_MIN_BITS 	$selectminbits	/**/
+
 /* SITEARCH:
  *	This symbol contains the name of the private library for this package.
  *	The library is private in the sense that it needn't be in anyone's
--- pp_sys.c
+++ pp_sys.c
@@ -56,7 +56,10 @@ extern "C" int syscall(unsigned long,...);
 
 /* XXX Configure test needed.
    h_errno might not be a simple 'int', especially for multi-threaded
-   applications.  HOST_NOT_FOUND is typically defined in <netdb.h>.
+   applications, see "extern int errno in perl.h".  Creating such
+   a test requires taking into account the differences between
+   compiling multithreaded and singlethreaded ($ccflags et al).
+   HOST_NOT_FOUND is typically defined in <netdb.h>.
 */
 #if defined(HOST_NOT_FOUND) && !defined(h_errno)
 extern int h_errno;
@@ -753,12 +756,17 @@ PP(pp_sselect)
 	    maxlen = j;
     }
 
+/* little endians can use vecs directly */
 #if BYTEORDER == 0x1234 || BYTEORDER == 0x12345678
-/* XXX Configure test needed. */
-#if defined(__linux__) || defined(OS2) || defined(NeXT) || defined(__osf__) || defined(sun)
-    growsize = sizeof(fd_set);
+#  if SELECT_MIN_BITS > 1
+    /* If SELECT_MIN_BITS is greater than one we most probably will want
+     * to align the sizes with SELECT_MIN_BITS/8 because for example
+     * in many little-endian (Intel, Alpha) systems (Linux, OS/2, Digital
+     * UNIX, Solaris, NeXT) the smallest quantum select() operates on
+     * (sets bit) is 32 bits.  */
+    growsize = maxlen + (SELECT_MIN_BITS/8 - (maxlen % (SELECT_MIN_BITS/8)));
 #else
-    growsize = maxlen;		/* little endians can use vecs directly */
+    growsize = sizeof(fd_set);
 #endif
 #else
 #ifdef NFDBITS
END
}  

sub _patch_5_005_01 {
  _patch(<<'END');
--- Configure
+++ Configure
@@ -21,7 +21,7 @@
 # $Id: Head.U,v 3.0.1.9 1997/02/28 15:02:09 ram Exp $
 #
 # Generated on Tue Jul  7 10:10:21 EDT 1998 [metaconfig 3.0 PL70]
-# (with additional metaconfig patches by doughera@lafayette.edu)
+# (with additional metaconfig patches by jhi@iki.fi)
 
 cat >/tmp/c1$$ <<EOF
 ARGGGHHHH!!!!!
@@ -56,33 +56,6 @@ case "$0" in
 	;;
 esac
 
-: the newline for tr
-if test X"$trnl" = X; then
-	case "`echo foo|tr '\n' x 2>/dev/null`" in
-	foox)
-		trnl='\n'
-		;;
-	esac
-fi
-if test X"$trnl" = X; then
-	case "`echo foo|tr '\012' x 2>/dev/null`" in
-	foox)
-		trnl='\012'
-		;;
-	esac
-fi
-if test -n "$DJGPP"; then
-	trnl='\012'
-fi
-if test X"$trnl" = X; then
-	cat <<EOM >&2
-
-$me: Fatal Error: cannot figure out how to translate newlines with 'tr'.
-
-EOM
-	exit 1
-fi
-
 : Proper separator for the PATH environment variable
 p_=:
 : On OS/2 this directory should exist if this is not floppy only system :-]
@@ -391,7 +364,6 @@ d_getservprotos=''
 d_getsbyname=''
 d_getsbyport=''
 d_gnulibc=''
-i_arpainet=''
 d_htonl=''
 d_inetaton=''
 d_isascii=''
@@ -540,6 +512,7 @@ dlsrc=''
 ld=''
 lddlflags=''
 usedl=''
+ebcdic=''
 doublesize=''
 fpostype=''
 gidtype=''
@@ -548,6 +521,7 @@ h_fcntl=''
 h_sysfile=''
 db_hashtype=''
 db_prefixtype=''
+i_arpainet=''
 i_db=''
 i_dbm=''
 i_rpcsvcdbm=''
@@ -633,6 +607,7 @@ libpth=''
 loclibpth=''
 plibpth=''
 xlibpth=''
+ignore_versioned_solibs=''
 libs=''
 lns=''
 lseektype=''
@@ -697,11 +672,13 @@ randbits=''
 installscript=''
 scriptdir=''
 scriptdirexp=''
+selectminbits=''
 selecttype=''
 sh=''
 sig_name=''
 sig_name_init=''
 sig_num=''
+sig_num_init=''
 installsitearch=''
 sitearch=''
 sitearchexp=''
@@ -719,6 +696,7 @@ startperl=''
 startsh=''
 stdchar=''
 sysman=''
+trnl=''
 uidtype=''
 nm_opt=''
 nm_so_opt=''
@@ -733,7 +711,6 @@ mips_type=''
 usrinc=''
 defvoidused=''
 voidflags=''
-ebcdic=''
 CONFIG=''
 
 define='define'
@@ -836,6 +813,8 @@ plibpth=''
 
 : default library list
 libswanted=''
+: some systems want only to use the non-versioned libso:s
+ignore_versioned_solibs=''
 : Possible local include directories to search.
 : Set locincpth to "" in a hint file to defeat local include searches.
 locincpth="/usr/local/include /opt/local/include /usr/gnu/include"
@@ -904,7 +883,7 @@ case "$sh" in
 $me:  Fatal Error:  I can't find a Bourne Shell anywhere.  
 
 Usually it's in /bin/sh.  How did you even get this far?
-Please contact me (Andy Dougherty) at doughera@lafayette.edu and 
+Please contact me (Jarkko Hietaniemi) at jhi@iki.fi and 
 we'll try to straighten this all out.
 EOM
 	exit 1
@@ -1240,7 +1219,7 @@ cat >extract <<'EOS'
 CONFIG=true
 echo "Doing variable substitutions on .SH files..."
 if test -f $src/MANIFEST; then
-	set x `awk '{print $1}' <$src/MANIFEST | grep '\.SH'`
+	set x `awk '{print $1}' <$src/MANIFEST | grep '\.SH$'`
 else
 	echo "(Looking for .SH files under the source directory.)"
 	set x `(cd $src; find . -name "*.SH" -print)`
@@ -1373,7 +1352,7 @@ THIS PACKAGE SEEMS TO BE INCOMPLETE.
 You have the option of continuing the configuration process, despite the
 distinct possibility that your kit is damaged, by typing 'y'es.  If you
 do, don't blame me if something goes wrong.  I advise you to type 'n'o
-and contact the author (doughera@lafayette.edu).
+and contact the author (jhi@iki.fi).
 
 EOM
 		echo $n "Continue? [n] $c" >&4
@@ -1396,6 +1375,30 @@ else
 fi
 rm -f missing x??
 
+echo " "
+: Find the appropriate value for a newline for tr
+if test -n "$DJGPP"; then
+       trnl='\012'
+fi
+if test X"$trnl" = X; then
+	case "`echo foo|tr '\n' x 2>/dev/null`" in
+	foox) trnl='\n' ;;
+	esac
+fi
+if test X"$trnl" = X; then
+	case "`echo foo|tr '\012' x 2>/dev/null`" in
+	foox) trnl='\012' ;;
+	esac
+fi
+if test X"$trnl" = X; then
+	cat <<EOM >&2
+
+$me: Fatal Error: cannot figure out how to translate newlines with 'tr'.
+
+EOM
+	exit 1
+fi
+
 : compute the number of columns on the terminal for proper question formatting
 case "$COLUMNS" in
 '') COLUMNS='80';;
@@ -1574,7 +1577,7 @@ Much effort has been expended to ensure that this shell script will run on any
 Unix system.  If despite that it blows up on yours, your best bet is to edit
 Configure and run it again.  If you can't run Configure for some reason,
 you'll have to generate a config.sh file by hand.  Whatever problems you
-have, let me (doughera@lafayette.edu) know how I blew it.
+have, let me (jhi@iki.fi) know how I blew it.
 
 This installation script affects things in two ways:
 
@@ -1841,14 +1844,14 @@ ABYZ)
 	    *C9D1*|*c9d1*)
 		echo "Hey, this might be EBCDIC." >&4
 		if test "X$up" = X -o "X$low" = X; then
-		    case "`echo IJ | tr '[A-IJ-RS-Z]' '[a-ij-rs-z]' 2>/dev/null`" in
+		    case "`echo IJ | $tr '[A-IJ-RS-Z]' '[a-ij-rs-z]' 2>/dev/null`" in
 		    ij) up='[A-IJ-RS-Z]'
 		        low='[a-ij-rs-z]'
 			;;
 		    esac
 		fi
 		if test "X$up" = X -o "X$low" = X; then
-		    case "`echo IJ | tr A-IJ-RS-Z a-ij-rs-z 2>/dev/null`" in
+		    case "`echo IJ | $tr A-IJ-RS-Z a-ij-rs-z 2>/dev/null`" in
 		    ij) up='A-IJ-RS-Z'
 		        low='a-ij-rs-z'
 			;;
@@ -1941,7 +1944,7 @@ EOM
 	(cd $src/hints; ls -C *.sh) | $sed 's/\.sh/   /g' >&4
 	dflt=''
 	: Half the following guesses are probably wrong... If you have better
-	: tests or hints, please send them to doughera@lafayette.edu
+	: tests or hints, please send them to jhi@iki.fi
 	: The metaconfig authors would also appreciate a copy...
 	$test -f /irix && osname=irix
 	$test -f /xenix && osname=sco_xenix
@@ -2025,7 +2028,7 @@ EOM
 			osvers="$3"
 			;;
 		dynixptx*) osname=dynixptx
-			osvers="$3"
+			osvers=`echo "$4" | $sed 's/^v//'`
 			;;
 		freebsd) osname=freebsd 
 			osvers="$3" ;;
@@ -3454,7 +3457,11 @@ cat <<'EOT' >testcpp.c
 ABC.XYZ
 EOT
 cd ..
+if test ! -f cppstdin; then
 echo 'cat >.$$.c; '"$cc"' -E ${1+"$@"} .$$.c; rm .$$.c' >cppstdin
+else
+	echo "Keeping your $hint cppstdin wrapper."
+fi
 chmod 755 cppstdin
 wrapper=`pwd`/cppstdin
 ok='false'
@@ -3705,7 +3712,8 @@ case "$libswanted" in
 esac
 for thislib in $libswanted; do
 	
-	if xxx=`./loc lib$thislib.$so.[0-9]'*' X $libpth`; $test -f "$xxx"; then
+	if xxx=`./loc lib$thislib.$so.[0-9]'*' X $libpth`;
+		$test -f "$xxx" -a "X$ignore_versioned_solibs" = "X"; then
 		echo "Found -l$thislib (shared)."
 		case " $dflt " in
 		*"-l$thislib "*);;
@@ -3992,10 +4000,21 @@ rmlist="$rmlist pdp11"
 : coherency check
 echo " "
 echo "Checking your choice of C compiler and flags for coherency..." >&4
+$cat > try.c <<'EOF'
+#include <stdio.h>
+main() { printf("Ok\n"); exit(0); }
+EOF
 set X $cc $optimize $ccflags -o try $ldflags try.c $libs
 shift
-$cat >try.msg <<EOM
-I've tried to compile and run a simple program with:
+$cat >try.msg <<'EOM'
+I've tried to compile and run the following simple program:
+
+EOM
+$cat try.c
+
+$cat >> try.msg <<EOM
+
+I used the command:
 
 	$*
 	./try
@@ -4003,10 +4022,6 @@ I've tried to compile and run a simple program with:
 and I got the following output:
 
 EOM
-$cat > try.c <<'EOF'
-#include <stdio.h>
-main() { printf("Ok\n"); exit(0); }
-EOF
 dflt=y
 if sh -c "$cc $optimize $ccflags -o try $ldflags try.c $libs" >>try.msg 2>&1; then
 	if sh -c './try' >>try.msg 2>&1; then
@@ -4043,7 +4058,7 @@ y)
 	$cat try.msg >&4
 	case "$knowitall" in
 	'')
-		echo "(The supplied flags might be incorrect with this C compiler.)"
+		echo "(The supplied flags or libraries might be incorrect.)"
 		;;
 	*) dflt=n;;
 	esac
@@ -4161,9 +4176,8 @@ eval $inhdr
 : determine which malloc to compile in
 echo " "
 case "$usemymalloc" in
-''|y*|true)	dflt='y' ;;
-n*|false)	dflt='n' ;;
-*)	dflt="$usemymalloc" ;;
+''|[yY]*|true|$define)	dflt='y' ;;
+*)	dflt='n' ;;
 esac
 rp="Do you wish to attempt to use the malloc that comes with $package?"
 . ./myread
@@ -4265,7 +4279,7 @@ understands function prototypes.  Unfortunately, your C compiler
 	$cc $ccflags
 doesn't seem to understand them.  Sorry about that.
 
-If GNU cc is avaiable for your system, perhaps you could try that instead.  
+If GNU cc is available for your system, perhaps you could try that instead.  
 
 Eventually, we hope to support building Perl with pre-ANSI compilers.
 If you would like to help in that effort, please contact <perlbug@perl.org>.
@@ -4320,32 +4334,6 @@ shift;
 $cc $optimize $ccflags $ldflags -o ${mc_file} $* ${mc_file}.c $libs;'
 
 echo " "
-echo "Determining whether or not we are on an EBCDIC system..." >&4
-cat >tebcdic.c <<EOM
-int main()
-{
-  if ('M'==0xd4) return 0;
-  return 1;
-}
-EOM
-val=$undef
-set tebcdic
-if eval $compile_ok; then
-	if ./tebcdic; then
-		echo "You have EBCDIC." >&4
-		val="$define"
-	else
-		echo "Nope, no EBCDIC.  Assuming ASCII or some ISO Latin." >&4
-	fi
-else
-	echo "I'm unable to compile the test program." >&4
-	echo "I'll asuume ASCII or some ISO Latin." >&4
-fi
-$rm -f tebcdic.c tebcdic
-set ebcdic
-eval $setvar
-
-echo " "
 echo "Checking for GNU C Library..." >&4
 cat >gnulibc.c <<EOM
 #include <stdio.h>
@@ -5159,7 +5147,7 @@ case "$shrpdir" in
 *)	$cat >&4 <<EOM
 WARNING:  Use of the shrpdir variable for the installation location of
 the shared $libperl is not supported.  It was never documented and
-will not work in this version.  Let me (doughera@lafayette.edu)
+will not work in this version.  Let me (jhi@iki.fi)
 know of any problems this may cause.
 
 EOM
@@ -6715,6 +6703,10 @@ eval $setvar
 set difftime d_difftime
 eval $inlibc
 
+: see if sys/stat.h is available
+set sys/stat.h i_sysstat
+eval $inhdr
+
 : see if this is a dirent system
 echo " "
 if xinc=`./findhdr dirent.h`; $test "$xinc"; then
@@ -6783,6 +6775,23 @@ set d_dirnamlen
 eval $setvar
 $rm -f try.c
 
+hasfield='varname=$1; struct=$2; field=$3; shift; shift; shift;
+while $test $# -ge 2; do
+	case "$1" in
+	$define) echo "#include <$2>";;
+	esac ;
+    shift 2;
+done > try.c;
+echo "int main () { struct $struct foo; foo.$field = 0; }" >> try.c;
+if eval $cc $optimize $ccflags -c try.c >/dev/null 2>&1; then
+	val="$define";
+else
+	val="$undef";
+fi;
+set $varname;
+eval $setvar;
+$rm -f try.c try.o'
+
 : see if dlerror exists
 xxx_runnm="$runnm"
 runnm=false
@@ -7317,7 +7326,7 @@ esac
 set netinet/in.h i_niin sys/in.h i_sysin
 eval $inhdr
 
-: see if this is an arpa/inet.h
+: see if arpa/inet.h has to be included
 set arpa/inet.h i_arpainet
 eval $inhdr
 
@@ -7643,6 +7652,27 @@ echo " "
 case "$d_msgctl$d_msgget$d_msgsnd$d_msgrcv" in
 *"$undef"*) h_msg=false;;
 esac
+
+case "$osname" in
+freebsd)
+    case "`ipcs 2>&1`" in
+    "SVID messages"*"not configured"*)
+	echo "But your $osname does not have the msg*(2) configured." >&4
+        h_msg=false
+	val="$undef"
+	set msgctl d_msgctl
+	eval $setvar
+	set msgget d_msgget
+	eval $setvar
+	set msgsnd d_msgsnd
+	eval $setvar
+	set msgrcv d_msgrcv
+	eval $setvar
+	;;
+    esac
+    ;;
+esac
+
 : we could also check for sys/ipc.h ...
 if $h_msg && $test `./findhdr sys/msg.h`; then
 	echo "You have the full msg*(2) library." >&4
@@ -7671,7 +7701,7 @@ set poll d_poll
 eval $inlibc
 
 
-: see whether the various POSIXish _yields exist within given cccmd
+: see whether the various POSIXish _yields exist
 $cat >try.c <<EOP
 #include <pthread.h>
 main() {
@@ -8125,6 +8155,25 @@ echo " "
 case "$d_semctl$d_semget$d_semop" in
 *"$undef"*) h_sem=false;;
 esac
+
+case "$osname" in
+freebsd)
+    case "`ipcs 2>&1`" in
+    "SVID messages"*"not configured"*)
+	echo "But your $osname does not have the sem*(2) configured." >&4
+        h_sem=false
+	val="$undef"
+	set semctl d_semctl
+	eval $setvar
+	set semget d_semget
+	eval $setvar
+	set semop d_semop
+	eval $setvar
+	;;
+    esac
+    ;;
+esac
+
 : we could also check for sys/ipc.h ...
 if $h_sem && $test `./findhdr sys/sem.h`; then
 	echo "You have the full sem*(2) library." >&4
@@ -8161,6 +8210,31 @@ case "$d_sem" in
 $define)
     : see whether semctl IPC_STAT can use union semun
     echo " "
+    $cat > try.h <<END
+#ifndef S_IRUSR
+#   ifdef S_IREAD
+#	define S_IRUSR S_IREAD
+#	define S_IWUSR S_IWRITE
+#	define S_IXUSR S_IEXEC
+#   else
+#	define S_IRUSR 0400
+#	define S_IWUSR 0200
+#	define S_IXUSR 0100
+#   endif
+#   define S_IRGRP (S_IRUSR>>3)
+#   define S_IWGRP (S_IWUSR>>3)
+#   define S_IXGRP (S_IXUSR>>3)
+#   define S_IROTH (S_IRUSR>>6)
+#   define S_IWOTH (S_IWUSR>>6)
+#   define S_IXOTH (S_IXUSR>>6)
+#endif
+#ifndef S_IRWXU
+#   define S_IRWXU (S_IRUSR|S_IWUSR|S_IXUSR)
+#   define S_IRWXG (S_IRGRP|S_IWGRP|S_IXGRP)
+#   define S_IRWXO (S_IROTH|S_IWOTH|S_IXOTH)
+#endif
+END
+
     $cat > try.c <<END
 #include <sys/types.h>
 #include <sys/ipc.h>
@@ -8235,6 +8309,7 @@ END
 #include <sys/stat.h>
 #include <stdio.h>
 #include <errno.h>
+#include "try.h"
 #ifndef errno
 extern int errno;
 #endif
@@ -8281,6 +8356,7 @@ END
     *)  echo "You cannot use struct semid_ds * for semctl IPC_STAT." >&4
         ;;
     esac
+    $rm -f try.h
     ;;
 *)  val="$undef"
 
@@ -8471,6 +8547,27 @@ echo " "
 case "$d_shmctl$d_shmget$d_shmat$d_shmdt" in
 *"$undef"*) h_shm=false;;
 esac
+
+case "$osname" in
+freebsd)
+    case "`ipcs 2>&1`" in
+    "SVID shared memory"*"not configured"*)
+	echo "But your $osname does not have the shm*(2) configured." >&4
+        h_shm=false
+	val="$undef"
+	set shmctl d_shmctl
+	evat $setvar
+	set shmget d_shmget
+	evat $setvar
+	set shmat d_shmat
+	evat $setvar
+	set shmdt d_shmdt
+	evat $setvar
+	;;
+    esac
+    ;;
+esac
+
 : we could also check for sys/ipc.h ...
 if $h_shm && $test `./findhdr sys/shm.h`; then
 	echo "You have the full shm*(2) library." >&4
@@ -8609,21 +8706,8 @@ eval $inlibc
 
 : see if stat knows about block sizes
 echo " "
-xxx=`./findhdr sys/stat.h`
-if $contains 'st_blocks;' "$xxx" >/dev/null 2>&1 ; then
-	if $contains 'st_blksize;' "$xxx" >/dev/null 2>&1 ; then
-		echo "Your stat() knows about block sizes." >&4
-		val="$define"
-	else
-		echo "Your stat() doesn't know about block sizes." >&4
-		val="$undef"
-	fi
-else
-	echo "Your stat() doesn't know about block sizes." >&4
-	val="$undef"
-fi
-set d_statblks
-eval $setvar
+set d_statblks stat st_blocks $i_sysstat sys/stat.h
+eval $hasfield
 
 : see if _ptr and _cnt from stdio act std
 echo " "
@@ -9567,6 +9651,32 @@ EOCP
 esac
 $rm -f try.c try
 
+echo " "
+echo "Determining whether or not we are on an EBCDIC system..." >&4
+$cat >tebcdic.c <<EOM
+int main()
+{
+  if ('M'==0xd4) return 0;
+  return 1;
+}
+EOM
+val=$undef
+set tebcdic
+if eval $compile_ok; then
+	if ./tebcdic; then
+		echo "You have EBCDIC." >&4
+		val="$define"
+	else
+		echo "Nope, no EBCDIC.  Assuming ASCII or some ISO Latin." >&4
+	fi
+else
+	echo "I'm unable to compile the test program." >&4
+	echo "I'll assume ASCII or some ISO Latin." >&4
+fi
+$rm -f tebcdic.c tebcdic
+set ebcdic
+eval $setvar
+
 : see what type file positions are declared as in the library
 rp="What is the type for file position used by fsetpos()?"
 set fpos_t fpostype long stdio.h sys/types.h
@@ -10174,8 +10284,10 @@ EOM
 		: The first arg can be int, unsigned, or size_t
 		: The last arg may or may not be 'const'
 		val=''
+		: void pointer has been seen but using that
+		: breaks the selectminbits test
 		for xxx in 'fd_set *' 'int *'; do
-			for nfd in 'int' 'size_t' 'unsigned' ; do
+			for nfd in 'int' 'size_t' 'unsigned' 'unsigned long'; do
 				for tmo in 'struct timeval *' 'const struct timeval *'; do
 					case "$val" in
 					'')	try="extern select _(($nfd, $xxx, $xxx, $xxx, $tmo));"
@@ -10207,6 +10319,100 @@ EOM
 	;;
 esac
 
+: check for the select 'width'
+case "$selectminbits" in
+'') case "$d_select" in
+	$define)
+		$cat <<EOM
+
+Checking to see on how many bits at a time your select() operates...
+EOM
+		$cat >try.c <<EOCP
+#include <sys/types.h>
+#$i_time I_TIME
+#$i_systime I_SYS_TIME
+#$i_systimek I_SYS_TIME_KERNEL
+#ifdef I_TIME
+#   include <time.h>
+#endif
+#ifdef I_SYS_TIME
+#   ifdef I_SYS_TIME_KERNEL
+#	define KERNEL
+#   endif
+#   include <sys/time.h>
+#   ifdef I_SYS_TIME_KERNEL
+#	undef KERNEL
+#   endif
+#endif
+#$i_sysselct I_SYS_SELECT
+#ifdef I_SYS_SELECT
+#include <sys/select.h>
+#endif
+#include <stdio.h>
+$selecttype b;
+#define S sizeof(*(b))
+#define MINBITS	64
+#define NBYTES (S * 8 > MINBITS ? S : MINBITS/8)
+#define NBITS  (NBYTES * 8)
+int main() {
+    char s[NBYTES];
+    struct timeval t;
+    int i;
+    FILE* fp;
+    int fd;
+
+    fclose(stdin);
+    fp = fopen("try.c", "r");
+    if (fp == 0)
+      exit(1);
+    fd = fileno(fp);
+    if (fd < 0)
+      exit(2);
+    b = ($selecttype)s;
+    for (i = 0; i < NBITS; i++)
+	FD_SET(i, b);
+    t.tv_sec  = 0;
+    t.tv_usec = 0;
+    select(fd + 1, b, 0, 0, &t);
+    for (i = NBITS - 1; i > fd && FD_ISSET(i, b); i--);
+    printf("%d\n", i + 1);
+    return 0;
+}
+EOCP
+		set try
+		if eval $compile_ok; then
+			selectminbits=`./try`
+			case "$selectminbits" in
+			'')	cat >&4 <<EOM
+Cannot figure out on how many bits at a time your select() operates.
+I'll play safe and guess it is 32 bits.
+EOM
+				selectminbits=32
+				bits="32 bits"
+				;;
+			1)	bits="1 bit" ;;
+			*)	bits="$selectminbits bits" ;;
+			esac
+			echo "Your select() operates on $bits at a time." >&4
+		else
+			rp='What is the minimum number of bits your select() operates on?'
+			case "$byteorder" in
+			1234|12345678)	dflt=32 ;;
+			*)		dflt=1	;;
+			esac
+			. ./myread
+			val=$ans
+			selectminbits="$val"
+		fi
+		$rm -f try.* try
+		;;
+	*)	: no select, so pick a harmless default
+		selectminbits='32'
+		;;
+	esac
+	;;
+esac
+
 : Trace out the files included by signal.h, then look for SIGxxx names.
 : Remove SIGARRAYSIZE used by HPUX.
 : Remove SIGTYP void lines used by OS2.
@@ -10415,7 +10621,13 @@ $eunicefix signal_cmd
 : generate list of signal names
 echo " "
 case "$sig_name_init" in
-'')
+'') doinit=yes ;;
+*)  case "$sig_num_init" in
+    ''|*,*) doinit=yes ;;
+    esac ;;
+esac
+case "$doinit" in
+yes)
 	echo "Generating a list of signal names and numbers..." >&4
 	. ./signal_cmd
 	sig_name=`$awk '{printf "%s ", $1}' signal.lst`
@@ -10423,7 +10635,9 @@ case "$sig_name_init" in
 	sig_name_init=`$awk 'BEGIN { printf "\"ZERO\", " }
 						{ printf "\"%s\", ", $1 }
 						END { printf "0\n" }' signal.lst`
-	sig_num=`$awk 'BEGIN { printf "0, " }
+	sig_num=`$awk '{printf "%d ", $2}' signal.lst`
+	sig_num="0 $sig_num"
+	sig_num_init=`$awk 'BEGIN { printf "0, " }
 					{ printf "%d, ", $2}
 					END { printf "0\n"}' signal.lst`
 	;;
@@ -10787,7 +11001,13 @@ $rm -f try.c
 EOS
 chmod +x ccsym
 $eunicefix ccsym
-./ccsym | $sort | $uniq >ccsym.raw
+./ccsym > ccsym1.raw
+if $test -s ccsym1.raw; then
+       $sort ccsym1.raw | $uniq >ccsym.raw
+else
+       mv ccsym1.raw ccsym.raw
+fi
+
 $awk '/\=/ { print $0; next }
 	{ print $0"=1" }' ccsym.raw >ccsym.list
 $awk '{ print $0"=1" }' Cppsym.true >ccsym.true
@@ -11012,10 +11232,6 @@ eval $inhdr
 set sys/resource.h i_sysresrc
 eval $inhdr
 
-: see if sys/stat.h is available
-set sys/stat.h i_sysstat
-eval $inhdr
-
 : see if this is a sys/un.h system
 set sys/un.h i_sysun
 eval $inhdr
@@ -11152,6 +11368,7 @@ for xxx in $known_extensions ; do
 		esac
 		;;
 	IPC/SysV|ipc/sysv)
+		: XXX Do we need a useipcsysv variable here
 		case "${d_msg}${d_sem}${d_shm}" in 
 		*"${define}"*) avail_ext="$avail_ext $xxx" ;;
 		esac
@@ -11731,6 +11948,7 @@ i_values='$i_values'
 i_varargs='$i_varargs'
 i_varhdr='$i_varhdr'
 i_vfork='$i_vfork'
+ignore_versioned_solibs='$ignore_versioned_solibs'
 incpath='$incpath'
 inews='$inews'
 installarchlib='$installarchlib'
@@ -11839,6 +12057,7 @@ runnm='$runnm'
 scriptdir='$scriptdir'
 scriptdirexp='$scriptdirexp'
 sed='$sed'
+selectminbits='$selectminbits'
 selecttype='$selecttype'
 sendmail='$sendmail'
 sh='$sh'
@@ -11851,6 +12070,7 @@ shsharp='$shsharp'
 sig_name='$sig_name'
 sig_name_init='$sig_name_init'
 sig_num='$sig_num'
+sig_num_init='$sig_num_init'
 signal_t='$signal_t'
 sitearch='$sitearch'
 sitearchexp='$sitearchexp'
--- Makefile.SH
+++ Makefile.SH
@@ -644,3 +644,83 @@ case `pwd` in
     ;;
 esac
 $rm -f $firstmakefile
+
+# Now do any special processing required before building.
+
+case "$ebcdic" in
+$define)
+    xxx=''
+    echo "This is an EBCDIC system, checking if any parser files need regenerating." >&4
+case "$osname" in
+os390)
+    rm -f y.tab.c y.tab.h
+    yacc -d perly.y >/dev/null 2>&1
+    if cmp -s y.tab.c perly.c; then
+        rm -f y.tab.c
+    else
+        echo "perly.y -> perly.c" >&2
+        mv -f y.tab.c perly.c
+        chmod u+w perly.c
+        sed -e '/^#include "perl\.h"/a\
+\
+#define yydebug    PL_yydebug\
+#define yynerrs    PL_yynerrs\
+#define yyerrflag  PL_yyerrflag\
+#define yychar     PL_yychar\
+#define yyval      PL_yyval\
+#define yylval     PL_yylval'				\
+            -e '/YYSTYPE *yyval;/D'			\
+            -e '/YYSTYPE *yylval;/D'			\
+            -e '/int  yychar,/,/yynerrs;/D'		\
+            -e 's/int yydebug = 0;/yydebug = 0;/'	\
+            -e 's/[^_]realloc(/PerlMem_realloc(/g'	\
+            -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
+            -e 's/y\.tab/perly/g' perly.c >perly.tmp && mv perly.tmp perly.c
+        xxx="$xxx perly.c"
+    fi
+    if cmp -s y.tab.h perly.h; then
+        rm -f y.tab.h
+    else
+        echo "perly.y -> perly.h" >&2
+        mv -f y.tab.h perly.h
+        xxx="$xxx perly.h"
+    fi
+    if cd x2p
+    then
+        rm -f y.tab.c y.tab.h
+        yacc a2p.y >/dev/null 2>&1
+        if cmp -s y.tab.c a2p.c
+        then
+            rm -f y.tab.c
+        else
+            echo "a2p.y -> a2p.c" >&2
+            mv -f y.tab.c a2p.c
+            chmod u+w a2p.c
+            sed -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
+                -e 's/y\.tab/a2p/g' a2p.c >a2p.tmp && mv a2p.tmp a2p.c
+            xxx="$xxx a2p.c"
+        fi
+        # In case somebody yacc -d:ed the a2p.y.
+        if test -f y.tab.h
+        then
+            if cmp -s y.tab.h a2p.h
+            then
+                rm -f y.tab.h
+            else
+                echo "a2p.h -> a2p.h" >&2
+                mv -f y.tab.h a2p.h
+                xxx="$xxx a2p.h"
+            fi
+        fi
+        cd ..
+    fi
+    ;;
+*)
+    echo "'$osname' is an EBCDIC system I don't know that well." >&4
+    ;;
+esac
+    case "$xxx" in
+    '') echo "No parser files were regenerated.  That's okay." >&2 ;;
+    esac
+    ;;
+esac
--- config_h.SH
+++ config_h.SH
@@ -1813,7 +1813,7 @@ sed <<!GROK!THIS! >config.h -e 's!^#undef\(.*/\)\*!/\*#define\1 \*!' -e 's!^#un-
  *	the sig_name list.
  */
 #define SIG_NAME $sig_name_init		/**/
-#define SIG_NUM  $sig_num			/**/
+#define SIG_NUM  $sig_num_init		/**/
 
 /* VOIDFLAGS:
  *	This symbol indicates how much support of the void type is given by this
@@ -1902,6 +1902,15 @@ sed <<!GROK!THIS! >config.h -e 's!^#undef\(.*/\)\*!/\*#define\1 \*!' -e 's!^#un-
 #define PRIVLIB "$privlib"		/**/
 #define PRIVLIB_EXP "$privlibexp"		/**/
 
+/* SELECT_MIN_BITS:
+ *	This symbol holds the minimum number of bits operated by select.
+ *	That is, if you do select(n, ...), how many bits at least will be
+ *	cleared in the masks if some activity is detected.  Usually this
+ *	is either n or 32*ceil(n/32), especially many little-endians do
+ *	the latter.  This is only useful if you have select(), naturally.
+ */
+#define SELECT_MIN_BITS 	$selectminbits	/**/
+
 /* SITEARCH:
  *	This symbol contains the name of the private library for this package.
  *	The library is private in the sense that it needn't be in anyone's
--- pp_sys.c
+++ pp_sys.c
@@ -56,7 +56,10 @@ extern "C" int syscall(unsigned long,...);
 
 /* XXX Configure test needed.
    h_errno might not be a simple 'int', especially for multi-threaded
-   applications.  HOST_NOT_FOUND is typically defined in <netdb.h>.
+   applications, see "extern int errno in perl.h".  Creating such
+   a test requires taking into account the differences between
+   compiling multithreaded and singlethreaded ($ccflags et al).
+   HOST_NOT_FOUND is typically defined in <netdb.h>.
 */
 #if defined(HOST_NOT_FOUND) && !defined(h_errno)
 extern int h_errno;
@@ -753,12 +756,17 @@ PP(pp_sselect)
 	    maxlen = j;
     }
 
+/* little endians can use vecs directly */
 #if BYTEORDER == 0x1234 || BYTEORDER == 0x12345678
-/* XXX Configure test needed. */
-#if defined(__linux__) || defined(OS2) || defined(NeXT) || defined(__osf__) || defined(sun)
-    growsize = sizeof(fd_set);
+#  if SELECT_MIN_BITS > 1
+    /* If SELECT_MIN_BITS is greater than one we most probably will want
+     * to align the sizes with SELECT_MIN_BITS/8 because for example
+     * in many little-endian (Intel, Alpha) systems (Linux, OS/2, Digital
+     * UNIX, Solaris, NeXT) the smallest quantum select() operates on
+     * (sets bit) is 32 bits.  */
+    growsize = maxlen + (SELECT_MIN_BITS/8 - (maxlen % (SELECT_MIN_BITS/8)));
 #else
-    growsize = maxlen;		/* little endians can use vecs directly */
+    growsize = sizeof(fd_set);
 #endif
 #else
 #ifdef NFDBITS
END
}

sub _patch_5_005 {
  _patch(<<'END');
--- Configure
+++ Configure
@@ -21,7 +21,7 @@
 # $Id: Head.U,v 3.0.1.9 1997/02/28 15:02:09 ram Exp $
 #
 # Generated on Tue Jul  7 10:10:21 EDT 1998 [metaconfig 3.0 PL70]
-# (with additional metaconfig patches by doughera@lafayette.edu)
+# (with additional metaconfig patches by jhi@iki.fi)
 
 cat >/tmp/c1$$ <<EOF
 ARGGGHHHH!!!!!
@@ -56,33 +56,6 @@ case "$0" in
 	;;
 esac
 
-: the newline for tr
-if test X"$trnl" = X; then
-	case "`echo foo|tr '\n' x 2>/dev/null`" in
-	foox)
-		trnl='\n'
-		;;
-	esac
-fi
-if test X"$trnl" = X; then
-	case "`echo foo|tr '\012' x 2>/dev/null`" in
-	foox)
-		trnl='\012'
-		;;
-	esac
-fi
-if test -n "$DJGPP"; then
-	trnl='\012'
-fi
-if test X"$trnl" = X; then
-	cat <<EOM >&2
-
-$me: Fatal Error: cannot figure out how to translate newlines with 'tr'.
-
-EOM
-	exit 1
-fi
-
 : Proper separator for the PATH environment variable
 p_=:
 : On OS/2 this directory should exist if this is not floppy only system :-]
@@ -391,7 +364,6 @@ d_getservprotos=''
 d_getsbyname=''
 d_getsbyport=''
 d_gnulibc=''
-i_arpainet=''
 d_htonl=''
 d_inetaton=''
 d_isascii=''
@@ -540,6 +512,7 @@ dlsrc=''
 ld=''
 lddlflags=''
 usedl=''
+ebcdic=''
 doublesize=''
 fpostype=''
 gidtype=''
@@ -548,6 +521,7 @@ h_fcntl=''
 h_sysfile=''
 db_hashtype=''
 db_prefixtype=''
+i_arpainet=''
 i_db=''
 i_dbm=''
 i_rpcsvcdbm=''
@@ -633,6 +607,7 @@ libpth=''
 loclibpth=''
 plibpth=''
 xlibpth=''
+ignore_versioned_solibs=''
 libs=''
 lns=''
 lseektype=''
@@ -697,11 +672,13 @@ randbits=''
 installscript=''
 scriptdir=''
 scriptdirexp=''
+selectminbits=''
 selecttype=''
 sh=''
 sig_name=''
 sig_name_init=''
 sig_num=''
+sig_num_init=''
 installsitearch=''
 sitearch=''
 sitearchexp=''
@@ -719,6 +696,7 @@ startperl=''
 startsh=''
 stdchar=''
 sysman=''
+trnl=''
 uidtype=''
 nm_opt=''
 nm_so_opt=''
@@ -733,7 +711,6 @@ mips_type=''
 usrinc=''
 defvoidused=''
 voidflags=''
-ebcdic=''
 CONFIG=''
 
 define='define'
@@ -836,6 +813,8 @@ plibpth=''
 
 : default library list
 libswanted=''
+: some systems want only to use the non-versioned libso:s
+ignore_versioned_solibs=''
 : Possible local include directories to search.
 : Set locincpth to "" in a hint file to defeat local include searches.
 locincpth="/usr/local/include /opt/local/include /usr/gnu/include"
@@ -904,7 +883,7 @@ case "$sh" in
 $me:  Fatal Error:  I can't find a Bourne Shell anywhere.  
 
 Usually it's in /bin/sh.  How did you even get this far?
-Please contact me (Andy Dougherty) at doughera@lafayette.edu and 
+Please contact me (Jarkko Hietaniemi) at jhi@iki.fi and 
 we'll try to straighten this all out.
 EOM
 	exit 1
@@ -1240,7 +1219,7 @@ cat >extract <<'EOS'
 CONFIG=true
 echo "Doing variable substitutions on .SH files..."
 if test -f $src/MANIFEST; then
-	set x `awk '{print $1}' <$src/MANIFEST | grep '\.SH'`
+	set x `awk '{print $1}' <$src/MANIFEST | grep '\.SH$'`
 else
 	echo "(Looking for .SH files under the source directory.)"
 	set x `(cd $src; find . -name "*.SH" -print)`
@@ -1373,7 +1352,7 @@ THIS PACKAGE SEEMS TO BE INCOMPLETE.
 You have the option of continuing the configuration process, despite the
 distinct possibility that your kit is damaged, by typing 'y'es.  If you
 do, don't blame me if something goes wrong.  I advise you to type 'n'o
-and contact the author (doughera@lafayette.edu).
+and contact the author (jhi@iki.fi).
 
 EOM
 		echo $n "Continue? [n] $c" >&4
@@ -1396,6 +1375,30 @@ else
 fi
 rm -f missing x??
 
+echo " "
+: Find the appropriate value for a newline for tr
+if test -n "$DJGPP"; then
+       trnl='\012'
+fi
+if test X"$trnl" = X; then
+	case "`echo foo|tr '\n' x 2>/dev/null`" in
+	foox) trnl='\n' ;;
+	esac
+fi
+if test X"$trnl" = X; then
+	case "`echo foo|tr '\012' x 2>/dev/null`" in
+	foox) trnl='\012' ;;
+	esac
+fi
+if test X"$trnl" = X; then
+	cat <<EOM >&2
+
+$me: Fatal Error: cannot figure out how to translate newlines with 'tr'.
+
+EOM
+	exit 1
+fi
+
 : compute the number of columns on the terminal for proper question formatting
 case "$COLUMNS" in
 '') COLUMNS='80';;
@@ -1574,7 +1577,7 @@ Much effort has been expended to ensure that this shell script will run on any
 Unix system.  If despite that it blows up on yours, your best bet is to edit
 Configure and run it again.  If you can't run Configure for some reason,
 you'll have to generate a config.sh file by hand.  Whatever problems you
-have, let me (doughera@lafayette.edu) know how I blew it.
+have, let me (jhi@iki.fi) know how I blew it.
 
 This installation script affects things in two ways:
 
@@ -1841,14 +1844,14 @@ ABYZ)
 	    *C9D1*|*c9d1*)
 		echo "Hey, this might be EBCDIC." >&4
 		if test "X$up" = X -o "X$low" = X; then
-		    case "`echo IJ | tr '[A-IJ-RS-Z]' '[a-ij-rs-z]' 2>/dev/null`" in
+		    case "`echo IJ | $tr '[A-IJ-RS-Z]' '[a-ij-rs-z]' 2>/dev/null`" in
 		    ij) up='[A-IJ-RS-Z]'
 		        low='[a-ij-rs-z]'
 			;;
 		    esac
 		fi
 		if test "X$up" = X -o "X$low" = X; then
-		    case "`echo IJ | tr A-IJ-RS-Z a-ij-rs-z 2>/dev/null`" in
+		    case "`echo IJ | $tr A-IJ-RS-Z a-ij-rs-z 2>/dev/null`" in
 		    ij) up='A-IJ-RS-Z'
 		        low='a-ij-rs-z'
 			;;
@@ -1941,7 +1944,7 @@ EOM
 	(cd $src/hints; ls -C *.sh) | $sed 's/\.sh/   /g' >&4
 	dflt=''
 	: Half the following guesses are probably wrong... If you have better
-	: tests or hints, please send them to doughera@lafayette.edu
+	: tests or hints, please send them to jhi@iki.fi
 	: The metaconfig authors would also appreciate a copy...
 	$test -f /irix && osname=irix
 	$test -f /xenix && osname=sco_xenix
@@ -2025,7 +2028,7 @@ EOM
 			osvers="$3"
 			;;
 		dynixptx*) osname=dynixptx
-			osvers="$3"
+			osvers=`echo "$4" | $sed 's/^v//'`
 			;;
 		freebsd) osname=freebsd 
 			osvers="$3" ;;
@@ -3454,7 +3457,11 @@ cat <<'EOT' >testcpp.c
 ABC.XYZ
 EOT
 cd ..
+if test ! -f cppstdin; then
 echo 'cat >.$$.c; '"$cc"' -E ${1+"$@"} .$$.c; rm .$$.c' >cppstdin
+else
+	echo "Keeping your $hint cppstdin wrapper."
+fi
 chmod 755 cppstdin
 wrapper=`pwd`/cppstdin
 ok='false'
@@ -3705,7 +3712,8 @@ case "$libswanted" in
 esac
 for thislib in $libswanted; do
 	
-	if xxx=`./loc lib$thislib.$so.[0-9]'*' X $libpth`; $test -f "$xxx"; then
+	if xxx=`./loc lib$thislib.$so.[0-9]'*' X $libpth`;
+		$test -f "$xxx" -a "X$ignore_versioned_solibs" = "X"; then
 		echo "Found -l$thislib (shared)."
 		case " $dflt " in
 		*"-l$thislib "*);;
@@ -3992,10 +4000,21 @@ rmlist="$rmlist pdp11"
 : coherency check
 echo " "
 echo "Checking your choice of C compiler and flags for coherency..." >&4
+$cat > try.c <<'EOF'
+#include <stdio.h>
+main() { printf("Ok\n"); exit(0); }
+EOF
 set X $cc $optimize $ccflags -o try $ldflags try.c $libs
 shift
-$cat >try.msg <<EOM
-I've tried to compile and run a simple program with:
+$cat >try.msg <<'EOM'
+I've tried to compile and run the following simple program:
+
+EOM
+$cat try.c
+
+$cat >> try.msg <<EOM
+
+I used the command:
 
 	$*
 	./try
@@ -4003,10 +4022,6 @@ I've tried to compile and run a simple program with:
 and I got the following output:
 
 EOM
-$cat > try.c <<'EOF'
-#include <stdio.h>
-main() { printf("Ok\n"); exit(0); }
-EOF
 dflt=y
 if sh -c "$cc $optimize $ccflags -o try $ldflags try.c $libs" >>try.msg 2>&1; then
 	if sh -c './try' >>try.msg 2>&1; then
@@ -4043,7 +4058,7 @@ y)
 	$cat try.msg >&4
 	case "$knowitall" in
 	'')
-		echo "(The supplied flags might be incorrect with this C compiler.)"
+		echo "(The supplied flags or libraries might be incorrect.)"
 		;;
 	*) dflt=n;;
 	esac
@@ -4161,9 +4176,8 @@ eval $inhdr
 : determine which malloc to compile in
 echo " "
 case "$usemymalloc" in
-''|y*|true)	dflt='y' ;;
-n*|false)	dflt='n' ;;
-*)	dflt="$usemymalloc" ;;
+''|[yY]*|true|$define)	dflt='y' ;;
+*)	dflt='n' ;;
 esac
 rp="Do you wish to attempt to use the malloc that comes with $package?"
 . ./myread
@@ -4265,7 +4279,7 @@ understands function prototypes.  Unfortunately, your C compiler
 	$cc $ccflags
 doesn't seem to understand them.  Sorry about that.
 
-If GNU cc is avaiable for your system, perhaps you could try that instead.  
+If GNU cc is available for your system, perhaps you could try that instead.  
 
 Eventually, we hope to support building Perl with pre-ANSI compilers.
 If you would like to help in that effort, please contact <perlbug@perl.org>.
@@ -4320,32 +4334,6 @@ shift;
 $cc $optimize $ccflags $ldflags -o ${mc_file} $* ${mc_file}.c $libs;'
 
 echo " "
-echo "Determining whether or not we are on an EBCDIC system..." >&4
-cat >tebcdic.c <<EOM
-int main()
-{
-  if ('M'==0xd4) return 0;
-  return 1;
-}
-EOM
-val=$undef
-set tebcdic
-if eval $compile_ok; then
-	if ./tebcdic; then
-		echo "You have EBCDIC." >&4
-		val="$define"
-	else
-		echo "Nope, no EBCDIC.  Assuming ASCII or some ISO Latin." >&4
-	fi
-else
-	echo "I'm unable to compile the test program." >&4
-	echo "I'll asuume ASCII or some ISO Latin." >&4
-fi
-$rm -f tebcdic.c tebcdic
-set ebcdic
-eval $setvar
-
-echo " "
 echo "Checking for GNU C Library..." >&4
 cat >gnulibc.c <<EOM
 #include <stdio.h>
@@ -5159,7 +5147,7 @@ case "$shrpdir" in
 *)	$cat >&4 <<EOM
 WARNING:  Use of the shrpdir variable for the installation location of
 the shared $libperl is not supported.  It was never documented and
-will not work in this version.  Let me (doughera@lafayette.edu)
+will not work in this version.  Let me (jhi@iki.fi)
 know of any problems this may cause.
 
 EOM
@@ -6715,6 +6703,10 @@ eval $setvar
 set difftime d_difftime
 eval $inlibc
 
+: see if sys/stat.h is available
+set sys/stat.h i_sysstat
+eval $inhdr
+
 : see if this is a dirent system
 echo " "
 if xinc=`./findhdr dirent.h`; $test "$xinc"; then
@@ -6783,6 +6775,23 @@ set d_dirnamlen
 eval $setvar
 $rm -f try.c
 
+hasfield='varname=$1; struct=$2; field=$3; shift; shift; shift;
+while $test $# -ge 2; do
+	case "$1" in
+	$define) echo "#include <$2>";;
+	esac ;
+    shift 2;
+done > try.c;
+echo "int main () { struct $struct foo; foo.$field = 0; }" >> try.c;
+if eval $cc $optimize $ccflags -c try.c >/dev/null 2>&1; then
+	val="$define";
+else
+	val="$undef";
+fi;
+set $varname;
+eval $setvar;
+$rm -f try.c try.o'
+
 : see if dlerror exists
 xxx_runnm="$runnm"
 runnm=false
@@ -7317,7 +7326,7 @@ esac
 set netinet/in.h i_niin sys/in.h i_sysin
 eval $inhdr
 
-: see if this is an arpa/inet.h
+: see if arpa/inet.h has to be included
 set arpa/inet.h i_arpainet
 eval $inhdr
 
@@ -7643,6 +7652,27 @@ echo " "
 case "$d_msgctl$d_msgget$d_msgsnd$d_msgrcv" in
 *"$undef"*) h_msg=false;;
 esac
+
+case "$osname" in
+freebsd)
+    case "`ipcs 2>&1`" in
+    "SVID messages"*"not configured"*)
+	echo "But your $osname does not have the msg*(2) configured." >&4
+        h_msg=false
+	val="$undef"
+	set msgctl d_msgctl
+	eval $setvar
+	set msgget d_msgget
+	eval $setvar
+	set msgsnd d_msgsnd
+	eval $setvar
+	set msgrcv d_msgrcv
+	eval $setvar
+	;;
+    esac
+    ;;
+esac
+
 : we could also check for sys/ipc.h ...
 if $h_msg && $test `./findhdr sys/msg.h`; then
 	echo "You have the full msg*(2) library." >&4
@@ -7671,7 +7701,7 @@ set poll d_poll
 eval $inlibc
 
 
-: see whether the various POSIXish _yields exist within given cccmd
+: see whether the various POSIXish _yields exist
 $cat >try.c <<EOP
 #include <pthread.h>
 main() {
@@ -8125,6 +8155,25 @@ echo " "
 case "$d_semctl$d_semget$d_semop" in
 *"$undef"*) h_sem=false;;
 esac
+
+case "$osname" in
+freebsd)
+    case "`ipcs 2>&1`" in
+    "SVID messages"*"not configured"*)
+	echo "But your $osname does not have the sem*(2) configured." >&4
+        h_sem=false
+	val="$undef"
+	set semctl d_semctl
+	eval $setvar
+	set semget d_semget
+	eval $setvar
+	set semop d_semop
+	eval $setvar
+	;;
+    esac
+    ;;
+esac
+
 : we could also check for sys/ipc.h ...
 if $h_sem && $test `./findhdr sys/sem.h`; then
 	echo "You have the full sem*(2) library." >&4
@@ -8161,6 +8210,31 @@ case "$d_sem" in
 $define)
     : see whether semctl IPC_STAT can use union semun
     echo " "
+    $cat > try.h <<END
+#ifndef S_IRUSR
+#   ifdef S_IREAD
+#	define S_IRUSR S_IREAD
+#	define S_IWUSR S_IWRITE
+#	define S_IXUSR S_IEXEC
+#   else
+#	define S_IRUSR 0400
+#	define S_IWUSR 0200
+#	define S_IXUSR 0100
+#   endif
+#   define S_IRGRP (S_IRUSR>>3)
+#   define S_IWGRP (S_IWUSR>>3)
+#   define S_IXGRP (S_IXUSR>>3)
+#   define S_IROTH (S_IRUSR>>6)
+#   define S_IWOTH (S_IWUSR>>6)
+#   define S_IXOTH (S_IXUSR>>6)
+#endif
+#ifndef S_IRWXU
+#   define S_IRWXU (S_IRUSR|S_IWUSR|S_IXUSR)
+#   define S_IRWXG (S_IRGRP|S_IWGRP|S_IXGRP)
+#   define S_IRWXO (S_IROTH|S_IWOTH|S_IXOTH)
+#endif
+END
+
     $cat > try.c <<END
 #include <sys/types.h>
 #include <sys/ipc.h>
@@ -8235,6 +8309,7 @@ END
 #include <sys/stat.h>
 #include <stdio.h>
 #include <errno.h>
+#include "try.h"
 #ifndef errno
 extern int errno;
 #endif
@@ -8281,6 +8356,7 @@ END
     *)  echo "You cannot use struct semid_ds * for semctl IPC_STAT." >&4
         ;;
     esac
+    $rm -f try.h
     ;;
 *)  val="$undef"
 
@@ -8471,6 +8547,27 @@ echo " "
 case "$d_shmctl$d_shmget$d_shmat$d_shmdt" in
 *"$undef"*) h_shm=false;;
 esac
+
+case "$osname" in
+freebsd)
+    case "`ipcs 2>&1`" in
+    "SVID shared memory"*"not configured"*)
+	echo "But your $osname does not have the shm*(2) configured." >&4
+        h_shm=false
+	val="$undef"
+	set shmctl d_shmctl
+	evat $setvar
+	set shmget d_shmget
+	evat $setvar
+	set shmat d_shmat
+	evat $setvar
+	set shmdt d_shmdt
+	evat $setvar
+	;;
+    esac
+    ;;
+esac
+
 : we could also check for sys/ipc.h ...
 if $h_shm && $test `./findhdr sys/shm.h`; then
 	echo "You have the full shm*(2) library." >&4
@@ -8609,21 +8706,8 @@ eval $inlibc
 
 : see if stat knows about block sizes
 echo " "
-xxx=`./findhdr sys/stat.h`
-if $contains 'st_blocks;' "$xxx" >/dev/null 2>&1 ; then
-	if $contains 'st_blksize;' "$xxx" >/dev/null 2>&1 ; then
-		echo "Your stat() knows about block sizes." >&4
-		val="$define"
-	else
-		echo "Your stat() doesn't know about block sizes." >&4
-		val="$undef"
-	fi
-else
-	echo "Your stat() doesn't know about block sizes." >&4
-	val="$undef"
-fi
-set d_statblks
-eval $setvar
+set d_statblks stat st_blocks $i_sysstat sys/stat.h
+eval $hasfield
 
 : see if _ptr and _cnt from stdio act std
 echo " "
@@ -9567,6 +9651,32 @@ EOCP
 esac
 $rm -f try.c try
 
+echo " "
+echo "Determining whether or not we are on an EBCDIC system..." >&4
+$cat >tebcdic.c <<EOM
+int main()
+{
+  if ('M'==0xd4) return 0;
+  return 1;
+}
+EOM
+val=$undef
+set tebcdic
+if eval $compile_ok; then
+	if ./tebcdic; then
+		echo "You have EBCDIC." >&4
+		val="$define"
+	else
+		echo "Nope, no EBCDIC.  Assuming ASCII or some ISO Latin." >&4
+	fi
+else
+	echo "I'm unable to compile the test program." >&4
+	echo "I'll assume ASCII or some ISO Latin." >&4
+fi
+$rm -f tebcdic.c tebcdic
+set ebcdic
+eval $setvar
+
 : see what type file positions are declared as in the library
 rp="What is the type for file position used by fsetpos()?"
 set fpos_t fpostype long stdio.h sys/types.h
@@ -10174,8 +10284,10 @@ EOM
 		: The first arg can be int, unsigned, or size_t
 		: The last arg may or may not be 'const'
 		val=''
+		: void pointer has been seen but using that
+		: breaks the selectminbits test
 		for xxx in 'fd_set *' 'int *'; do
-			for nfd in 'int' 'size_t' 'unsigned' ; do
+			for nfd in 'int' 'size_t' 'unsigned' 'unsigned long'; do
 				for tmo in 'struct timeval *' 'const struct timeval *'; do
 					case "$val" in
 					'')	try="extern select _(($nfd, $xxx, $xxx, $xxx, $tmo));"
@@ -10207,6 +10319,100 @@ EOM
 	;;
 esac
 
+: check for the select 'width'
+case "$selectminbits" in
+'') case "$d_select" in
+	$define)
+		$cat <<EOM
+
+Checking to see on how many bits at a time your select() operates...
+EOM
+		$cat >try.c <<EOCP
+#include <sys/types.h>
+#$i_time I_TIME
+#$i_systime I_SYS_TIME
+#$i_systimek I_SYS_TIME_KERNEL
+#ifdef I_TIME
+#   include <time.h>
+#endif
+#ifdef I_SYS_TIME
+#   ifdef I_SYS_TIME_KERNEL
+#	define KERNEL
+#   endif
+#   include <sys/time.h>
+#   ifdef I_SYS_TIME_KERNEL
+#	undef KERNEL
+#   endif
+#endif
+#$i_sysselct I_SYS_SELECT
+#ifdef I_SYS_SELECT
+#include <sys/select.h>
+#endif
+#include <stdio.h>
+$selecttype b;
+#define S sizeof(*(b))
+#define MINBITS	64
+#define NBYTES (S * 8 > MINBITS ? S : MINBITS/8)
+#define NBITS  (NBYTES * 8)
+int main() {
+    char s[NBYTES];
+    struct timeval t;
+    int i;
+    FILE* fp;
+    int fd;
+
+    fclose(stdin);
+    fp = fopen("try.c", "r");
+    if (fp == 0)
+      exit(1);
+    fd = fileno(fp);
+    if (fd < 0)
+      exit(2);
+    b = ($selecttype)s;
+    for (i = 0; i < NBITS; i++)
+	FD_SET(i, b);
+    t.tv_sec  = 0;
+    t.tv_usec = 0;
+    select(fd + 1, b, 0, 0, &t);
+    for (i = NBITS - 1; i > fd && FD_ISSET(i, b); i--);
+    printf("%d\n", i + 1);
+    return 0;
+}
+EOCP
+		set try
+		if eval $compile_ok; then
+			selectminbits=`./try`
+			case "$selectminbits" in
+			'')	cat >&4 <<EOM
+Cannot figure out on how many bits at a time your select() operates.
+I'll play safe and guess it is 32 bits.
+EOM
+				selectminbits=32
+				bits="32 bits"
+				;;
+			1)	bits="1 bit" ;;
+			*)	bits="$selectminbits bits" ;;
+			esac
+			echo "Your select() operates on $bits at a time." >&4
+		else
+			rp='What is the minimum number of bits your select() operates on?'
+			case "$byteorder" in
+			1234|12345678)	dflt=32 ;;
+			*)		dflt=1	;;
+			esac
+			. ./myread
+			val=$ans
+			selectminbits="$val"
+		fi
+		$rm -f try.* try
+		;;
+	*)	: no select, so pick a harmless default
+		selectminbits='32'
+		;;
+	esac
+	;;
+esac
+
 : Trace out the files included by signal.h, then look for SIGxxx names.
 : Remove SIGARRAYSIZE used by HPUX.
 : Remove SIGTYP void lines used by OS2.
@@ -10415,7 +10621,13 @@ $eunicefix signal_cmd
 : generate list of signal names
 echo " "
 case "$sig_name_init" in
-'')
+'') doinit=yes ;;
+*)  case "$sig_num_init" in
+    ''|*,*) doinit=yes ;;
+    esac ;;
+esac
+case "$doinit" in
+yes)
 	echo "Generating a list of signal names and numbers..." >&4
 	. ./signal_cmd
 	sig_name=`$awk '{printf "%s ", $1}' signal.lst`
@@ -10423,7 +10635,9 @@ case "$sig_name_init" in
 	sig_name_init=`$awk 'BEGIN { printf "\"ZERO\", " }
 						{ printf "\"%s\", ", $1 }
 						END { printf "0\n" }' signal.lst`
-	sig_num=`$awk 'BEGIN { printf "0, " }
+	sig_num=`$awk '{printf "%d ", $2}' signal.lst`
+	sig_num="0 $sig_num"
+	sig_num_init=`$awk 'BEGIN { printf "0, " }
 					{ printf "%d, ", $2}
 					END { printf "0\n"}' signal.lst`
 	;;
@@ -10787,7 +11001,13 @@ $rm -f try.c
 EOS
 chmod +x ccsym
 $eunicefix ccsym
-./ccsym | $sort | $uniq >ccsym.raw
+./ccsym > ccsym1.raw
+if $test -s ccsym1.raw; then
+       $sort ccsym1.raw | $uniq >ccsym.raw
+else
+       mv ccsym1.raw ccsym.raw
+fi
+
 $awk '/\=/ { print $0; next }
 	{ print $0"=1" }' ccsym.raw >ccsym.list
 $awk '{ print $0"=1" }' Cppsym.true >ccsym.true
@@ -11012,10 +11232,6 @@ eval $inhdr
 set sys/resource.h i_sysresrc
 eval $inhdr
 
-: see if sys/stat.h is available
-set sys/stat.h i_sysstat
-eval $inhdr
-
 : see if this is a sys/un.h system
 set sys/un.h i_sysun
 eval $inhdr
@@ -11152,6 +11368,7 @@ for xxx in $known_extensions ; do
 		esac
 		;;
 	IPC/SysV|ipc/sysv)
+		: XXX Do we need a useipcsysv variable here
 		case "${d_msg}${d_sem}${d_shm}" in 
 		*"${define}"*) avail_ext="$avail_ext $xxx" ;;
 		esac
@@ -11731,6 +11948,7 @@ i_values='$i_values'
 i_varargs='$i_varargs'
 i_varhdr='$i_varhdr'
 i_vfork='$i_vfork'
+ignore_versioned_solibs='$ignore_versioned_solibs'
 incpath='$incpath'
 inews='$inews'
 installarchlib='$installarchlib'
@@ -11839,6 +12057,7 @@ runnm='$runnm'
 scriptdir='$scriptdir'
 scriptdirexp='$scriptdirexp'
 sed='$sed'
+selectminbits='$selectminbits'
 selecttype='$selecttype'
 sendmail='$sendmail'
 sh='$sh'
@@ -11851,6 +12070,7 @@ shsharp='$shsharp'
 sig_name='$sig_name'
 sig_name_init='$sig_name_init'
 sig_num='$sig_num'
+sig_num_init='$sig_num_init'
 signal_t='$signal_t'
 sitearch='$sitearch'
 sitearchexp='$sitearchexp'
--- Makefile.SH
+++ Makefile.SH
@@ -644,3 +644,83 @@ case `pwd` in
     ;;
 esac
 $rm -f $firstmakefile
+
+# Now do any special processing required before building.
+
+case "$ebcdic" in
+$define)
+    xxx=''
+    echo "This is an EBCDIC system, checking if any parser files need regenerating." >&4
+case "$osname" in
+os390)
+    rm -f y.tab.c y.tab.h
+    yacc -d perly.y >/dev/null 2>&1
+    if cmp -s y.tab.c perly.c; then
+        rm -f y.tab.c
+    else
+        echo "perly.y -> perly.c" >&2
+        mv -f y.tab.c perly.c
+        chmod u+w perly.c
+        sed -e '/^#include "perl\.h"/a\
+\
+#define yydebug    PL_yydebug\
+#define yynerrs    PL_yynerrs\
+#define yyerrflag  PL_yyerrflag\
+#define yychar     PL_yychar\
+#define yyval      PL_yyval\
+#define yylval     PL_yylval'				\
+            -e '/YYSTYPE *yyval;/D'			\
+            -e '/YYSTYPE *yylval;/D'			\
+            -e '/int  yychar,/,/yynerrs;/D'		\
+            -e 's/int yydebug = 0;/yydebug = 0;/'	\
+            -e 's/[^_]realloc(/PerlMem_realloc(/g'	\
+            -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
+            -e 's/y\.tab/perly/g' perly.c >perly.tmp && mv perly.tmp perly.c
+        xxx="$xxx perly.c"
+    fi
+    if cmp -s y.tab.h perly.h; then
+        rm -f y.tab.h
+    else
+        echo "perly.y -> perly.h" >&2
+        mv -f y.tab.h perly.h
+        xxx="$xxx perly.h"
+    fi
+    if cd x2p
+    then
+        rm -f y.tab.c y.tab.h
+        yacc a2p.y >/dev/null 2>&1
+        if cmp -s y.tab.c a2p.c
+        then
+            rm -f y.tab.c
+        else
+            echo "a2p.y -> a2p.c" >&2
+            mv -f y.tab.c a2p.c
+            chmod u+w a2p.c
+            sed -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
+                -e 's/y\.tab/a2p/g' a2p.c >a2p.tmp && mv a2p.tmp a2p.c
+            xxx="$xxx a2p.c"
+        fi
+        # In case somebody yacc -d:ed the a2p.y.
+        if test -f y.tab.h
+        then
+            if cmp -s y.tab.h a2p.h
+            then
+                rm -f y.tab.h
+            else
+                echo "a2p.h -> a2p.h" >&2
+                mv -f y.tab.h a2p.h
+                xxx="$xxx a2p.h"
+            fi
+        fi
+        cd ..
+    fi
+    ;;
+*)
+    echo "'$osname' is an EBCDIC system I don't know that well." >&4
+    ;;
+esac
+    case "$xxx" in
+    '') echo "No parser files were regenerated.  That's okay." >&2 ;;
+    esac
+    ;;
+esac
--- config_h.SH
+++ config_h.SH
@@ -1813,7 +1813,7 @@ sed <<!GROK!THIS! >config.h -e 's!^#undef\(.*/\)\*!/\*#define\1 \*!' -e 's!^#un-
  *	the sig_name list.
  */
 #define SIG_NAME $sig_name_init		/**/
-#define SIG_NUM  $sig_num			/**/
+#define SIG_NUM  $sig_num_init		/**/
 
 /* VOIDFLAGS:
  *	This symbol indicates how much support of the void type is given by this
@@ -1902,6 +1902,15 @@ sed <<!GROK!THIS! >config.h -e 's!^#undef\(.*/\)\*!/\*#define\1 \*!' -e 's!^#un-
 #define PRIVLIB "$privlib"		/**/
 #define PRIVLIB_EXP "$privlibexp"		/**/
 
+/* SELECT_MIN_BITS:
+ *	This symbol holds the minimum number of bits operated by select.
+ *	That is, if you do select(n, ...), how many bits at least will be
+ *	cleared in the masks if some activity is detected.  Usually this
+ *	is either n or 32*ceil(n/32), especially many little-endians do
+ *	the latter.  This is only useful if you have select(), naturally.
+ */
+#define SELECT_MIN_BITS 	$selectminbits	/**/
+
 /* SITEARCH:
  *	This symbol contains the name of the private library for this package.
  *	The library is private in the sense that it needn't be in anyone's
--- pp_sys.c
+++ pp_sys.c
@@ -56,7 +56,10 @@ extern "C" int syscall(unsigned long,...);
 
 /* XXX Configure test needed.
    h_errno might not be a simple 'int', especially for multi-threaded
-   applications.  HOST_NOT_FOUND is typically defined in <netdb.h>.
+   applications, see "extern int errno in perl.h".  Creating such
+   a test requires taking into account the differences between
+   compiling multithreaded and singlethreaded ($ccflags et al).
+   HOST_NOT_FOUND is typically defined in <netdb.h>.
 */
 #if defined(HOST_NOT_FOUND) && !defined(h_errno)
 extern int h_errno;
@@ -753,12 +756,17 @@ PP(pp_sselect)
 	    maxlen = j;
     }
 
+/* little endians can use vecs directly */
 #if BYTEORDER == 0x1234 || BYTEORDER == 0x12345678
-/* XXX Configure test needed. */
-#if defined(__linux__) || defined(OS2) || defined(NeXT) || defined(__osf__)
-    growsize = sizeof(fd_set);
+#  if SELECT_MIN_BITS > 1
+    /* If SELECT_MIN_BITS is greater than one we most probably will want
+     * to align the sizes with SELECT_MIN_BITS/8 because for example
+     * in many little-endian (Intel, Alpha) systems (Linux, OS/2, Digital
+     * UNIX, Solaris, NeXT) the smallest quantum select() operates on
+     * (sets bit) is 32 bits.  */
+    growsize = maxlen + (SELECT_MIN_BITS/8 - (maxlen % (SELECT_MIN_BITS/8)));
 #else
-    growsize = maxlen;		/* little endians can use vecs directly */
+    growsize = sizeof(fd_set);
 #endif
 #else
 #ifdef NFDBITS
END
}

sub _patch_errno_gcc5 {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num < 5.021009;
  return if $num > 5.020002 && $num < 5.021;
  if ( $num < 5.006 ) {
    warn "The Errno GCC 5 patch only goes back as far as v5.6.0\n";
    warn "You will have to generate your own patch to go farther back\n";
    return;
  }
  elsif ( $num < 5.006001 ) {
    _patch(<<'END');
diff --git a/ext/Errno/Errno_pm.PL b/ext/Errno/Errno_pm.PL
index df68dc3..8385048 100644
--- ext/Errno/Errno_pm.PL
+++ ext/Errno/Errno_pm.PL
@@ -143,16 +143,26 @@ sub write_errno_pm {
 
     # invoke CPP and read the output
 
+    my $inhibit_linemarkers = '';
+    if ($Config{gccversion} =~ /\A(\d+)\./ and $1 >= 5) {
+        # GCC 5.0 interleaves expanded macros with line numbers breaking
+        # each line into multiple lines. RT#123784
+        $inhibit_linemarkers = ' -P';
+    }
+
     if ($^O eq 'VMS') {
-	my $cpp = "$Config{cppstdin} $Config{cppflags} $Config{cppminus}";
+  my $cpp = "$Config{cppstdin} $Config{cppflags}" .
+    $inhibit_linemarkers . " $Config{cppminus}";
 	$cpp =~ s/sys\$input//i;
 	open(CPPO,"$cpp  errno.c |") or
           die "Cannot exec $Config{cppstdin}";
     } elsif ($^O eq 'MSWin32') {
-	open(CPPO,"$Config{cpprun} $Config{cppflags} errno.c |") or
-	    die "Cannot run '$Config{cpprun} $Config{cppflags} errno.c'";
+       my $cpp = "$Config{cpprun} $Config{cppflags}" .
+         $inhibit_linemarkers;
+       open(CPPO,"$cpp errno.c |") or
+         die "Cannot run '$cpp errno.c'";
     } else {
-	my $cpp = default_cpp();
+	my $cpp = default_cpp() . $inhibit_linemarkers;
 	open(CPPO,"$cpp < errno.c |")
 	    or die "Cannot exec $cpp";
     }
END
  }
  elsif ( $num == 5.00700 ) {
    _patch_b64(<<'END');
ZGlmZiAtLWdpdCBhL2V4dC9FcnJuby9FcnJub19wbS5QTCBiL2V4dC9FcnJuby9FcnJub19wbS5Q
TAppbmRleCBkZjY4ZGMzYmRhLi4yNTFmMmJhNjYzIDEwMDY0NAotLS0gZXh0L0Vycm5vL0Vycm5v
X3BtLlBMCisrKyBleHQvRXJybm8vRXJybm9fcG0uUEwKQEAgLTIsOSArMiw3IEBAIHVzZSBFeHRV
dGlsczo6TWFrZU1ha2VyOwogdXNlIENvbmZpZzsKIHVzZSBzdHJpY3Q7CiAKLXVzZSB2YXJzIHF3
KCRWRVJTSU9OKTsKLQotJFZFUlNJT04gPSAiMS4xMTEiOworb3VyICRWRVJTSU9OID0gIjEuMTEx
IjsKIAogbXkgJWVyciA9ICgpOwogCkBAIC0yOSw2ICsyNywxMiBAQCBzdWIgcHJvY2Vzc19maWxl
IHsKICAgICAgICAgICAgIHdhcm4gIkNhbm5vdCBvcGVuICckZmlsZSciOwogICAgICAgICAgICAg
cmV0dXJuOwogCX0gICAgIAorICAgIH0gZWxzaWYgKCRDb25maWd7Z2NjdmVyc2lvbn0gbmUgJycp
IHsgCisJIyBXaXRoIHRoZSAtZE0gb3B0aW9uLCBnY2Mgb3V0cHV0cyBldmVyeSAjZGVmaW5lIGl0
IGZpbmRzCisJdW5sZXNzKG9wZW4oRkgsIiRDb25maWd7Y2N9IC1FIC1kTSAkQ29uZmlne2NwcGZs
YWdzfSAkZmlsZSB8IikpIHsKKyAgICAgICAgICAgIHdhcm4gIkNhbm5vdCBvcGVuICckZmlsZSci
OworICAgICAgICAgICAgcmV0dXJuOworCX0gICAgIAogICAgIH0gZWxzZSB7CiAJdW5sZXNzKG9w
ZW4oRkgsIjwgJGZpbGUiKSkgewogCSAgICAjIFRoaXMgZmlsZSBjb3VsZCBiZSBhIHRlbXBvcmFy
eSBmaWxlIGNyZWF0ZWQgYnkgY3Bwc3RkaW4KQEAgLTM3LDExICs0MSwxOSBAQCBzdWIgcHJvY2Vz
c19maWxlIHsKICAgICAgICAgICAgIHJldHVybjsKIAl9CiAgICAgfQotICAgIHdoaWxlKDxGSD4p
IHsKLQkkZXJyeyQxfSA9IDEKLQkgICAgaWYgL15ccyojXHMqZGVmaW5lXHMrKEVcdyspXHMrLzsK
LSAgIH0KLSAgIGNsb3NlKEZIKTsKKworICAgIGlmICgkXk8gZXEgJ01hY09TJykgeworCXdoaWxl
KDxGSD4pIHsKKwkgICAgJGVycnskMX0gPSAkMgorCQlpZiAvXlxzKiNccypkZWZpbmVccysoRVx3
KylccysoXGQrKS87CisJfQorICAgIH0gZWxzZSB7CisJd2hpbGUoPEZIPikgeworCSAgICAkZXJy
eyQxfSA9IDEKKwkJaWYgL15ccyojXHMqZGVmaW5lXHMrKEVcdyspXHMrLzsKKwl9CisgICAgfQor
ICAgIGNsb3NlKEZIKTsKIH0KIAogbXkgJGNwcHN0ZGluOwpAQCAtNzksNiArOTEsMTggQEAgc3Vi
IGdldF9maWxlcyB7CiAgICAgfSBlbHNpZiAoJF5PIGVxICd2bWVzYScpIHsKIAkjIE9TLzM5MCBD
IGNvbXBpbGVyIGRvZXNuJ3QgZ2VuZXJhdGUgI2ZpbGUgb3IgI2xpbmUgZGlyZWN0aXZlcwogCSRm
aWxleycuLi8uLi92bWVzYS9lcnJuby5oJ30gPSAxOworICAgIH0gZWxzaWYgKCRDb25maWd7YXJj
aG5hbWV9IGVxICdlcG9jJykgeworCSMgV2F0Y2ggb3V0IGZvciBjcm9zcyBjb21waWxpbmcgZm9y
IEVQT0MgKHVzdWFsbHkgZG9uZSBvbiBsaW51eCkKKwkkZmlsZXsnL3Vzci9sb2NhbC9lcG9jL2lu
Y2x1ZGUvbGliYy9zeXMvZXJybm8uaCd9ID0gMTsKKyAgICB9IGVsc2lmICgkXk8gZXEgJ2xpbnV4
JykgeworCSMgU29tZSBMaW51eGVzIGhhdmUgd2VpcmQgZXJybm8uaHMgd2hpY2ggZ2VuZXJhdGUK
KwkjIG5vICNmaWxlIG9yICNsaW5lIGRpcmVjdGl2ZXMKKwkkZmlsZXsnL3Vzci9pbmNsdWRlL2Vy
cm5vLmgnfSA9IDE7CisgICAgfSBlbHNpZiAoJF5PIGVxICdNYWNPUycpIHsKKwkjIG5vdGUgdGhh
dCB3ZSBhcmUgb25seSBnZXR0aW5nIHRoZSBHVVNJIGVycm5vJ3MgaGVyZSAuLi4KKwkjIHdlIG1p
Z2h0IG1pc3Mgb3V0IG9uIGNvbXBpbGVyLXNwZWNpZmljIG9uZXMKKwkkZmlsZXsiJEVOVntHVVNJ
fWluY2x1ZGU6c3lzOmVycm5vLmgifSA9IDE7CisKICAgICB9IGVsc2UgewogCW9wZW4oQ1BQSSwi
PiBlcnJuby5jIikgb3IKIAkgICAgZGllICJDYW5ub3Qgb3BlbiBlcnJuby5jIjsKQEAgLTEwMiw3
ICsxMjYsNyBAQCBzdWIgZ2V0X2ZpbGVzIHsKIAkgICAgJHBhdCA9ICdeL1wqXHMrKC4rKVxzK1xk
K1xzKjpccytcKi8nOwogCX0KIAllbHNlIHsKLQkgICAgJHBhdCA9ICdeIyg/OmxpbmUpP1xzKlxk
K1xzKyIoW14iXSspIic7CisJICAgICRwYXQgPSAnXiNccyooPzpsaW5lKT9ccypcZCtccysiKFte
Il0rKSInOwogCX0KIAl3aGlsZSg8Q1BQTz4pIHsKIAkgICAgaWYgKCReTyBlcSAnb3MyJyBvciAk
Xk8gZXEgJ01TV2luMzInKSB7CkBAIC0xNDEsMzEgKzE2NSw0MyBAQCBzdWIgd3JpdGVfZXJybm9f
cG0gewogCiAgICAgY2xvc2UoQ1BQSSk7CiAKKyAgICB1bmxlc3MgKCReTyBlcSAnTWFjT1MnKSB7
CSMgdHJ1c3Qgd2hhdCB3ZSBoYXZlCiAgICAgIyBpbnZva2UgQ1BQIGFuZCByZWFkIHRoZSBvdXRw
dXQKIAotICAgIGlmICgkXk8gZXEgJ1ZNUycpIHsKLQlteSAkY3BwID0gIiRDb25maWd7Y3Bwc3Rk
aW59ICRDb25maWd7Y3BwZmxhZ3N9ICRDb25maWd7Y3BwbWludXN9IjsKLQkkY3BwID1+IHMvc3lz
XCRpbnB1dC8vaTsKLQlvcGVuKENQUE8sIiRjcHAgIGVycm5vLmMgfCIpIG9yCi0gICAgICAgICAg
ZGllICJDYW5ub3QgZXhlYyAkQ29uZmlne2NwcHN0ZGlufSI7Ci0gICAgfSBlbHNpZiAoJF5PIGVx
ICdNU1dpbjMyJykgewotCW9wZW4oQ1BQTywiJENvbmZpZ3tjcHBydW59ICRDb25maWd7Y3BwZmxh
Z3N9IGVycm5vLmMgfCIpIG9yCi0JICAgIGRpZSAiQ2Fubm90IHJ1biAnJENvbmZpZ3tjcHBydW59
ICRDb25maWd7Y3BwZmxhZ3N9IGVycm5vLmMnIjsKLSAgICB9IGVsc2UgewotCW15ICRjcHAgPSBk
ZWZhdWx0X2NwcCgpOwotCW9wZW4oQ1BQTywiJGNwcCA8IGVycm5vLmMgfCIpCi0JICAgIG9yIGRp
ZSAiQ2Fubm90IGV4ZWMgJGNwcCI7Ci0gICAgfQorICAgICAgIG15ICRpbmhpYml0X2xpbmVtYXJr
ZXJzID0gJyc7CisgICAgICAgaWYgKCRDb25maWd7Z2NjdmVyc2lvbn0gPX4gL1xBKFxkKylcLi8g
YW5kICQxID49IDUpIHsKKyAgICAgICAgICAgIyBHQ0MgNS4wIGludGVybGVhdmVzIGV4cGFuZGVk
IG1hY3JvcyB3aXRoIGxpbmUgbnVtYmVycyBicmVha2luZworICAgICAgICAgICAjIGVhY2ggbGlu
ZSBpbnRvIG11bHRpcGxlIGxpbmVzLiBSVCMxMjM3ODQKKyAgICAgICAgICAgJGluaGliaXRfbGlu
ZW1hcmtlcnMgPSAnIC1QJzsKKyAgICAgICB9CisKKwlpZiAoJF5PIGVxICdWTVMnKSB7CisJICAg
IG15ICRjcHAgPSAiJENvbmZpZ3tjcHBzdGRpbn0gJENvbmZpZ3tjcHBmbGFnc30iIC4KKyAgICAg
ICAgJGluaGliaXRfbGluZW1hcmtlcnMgLiAiICRDb25maWd7Y3BwbWludXN9IjsKKwkgICAgJGNw
cCA9fiBzL3N5c1wkaW5wdXQvL2k7CisJICAgIG9wZW4oQ1BQTywiJGNwcCAgZXJybm8uYyB8Iikg
b3IKKwkJZGllICJDYW5ub3QgZXhlYyAkQ29uZmlne2NwcHN0ZGlufSI7CisJfSBlbHNpZiAoJF5P
IGVxICdNU1dpbjMyJykgeworICAgICAgICAgICBteSAkY3BwID0gIiRDb25maWd7Y3BwcnVufSAk
Q29uZmlne2NwcGZsYWdzfSIgLgorICAgICAgICAgICAgICAgJGluaGliaXRfbGluZW1hcmtlcnM7
CisgICAgICAgICAgIG9wZW4oQ1BQTywiJGNwcCBlcnJuby5jIHwiKSBvcgorICAgICAgICAgICAg
ICAgZGllICJDYW5ub3QgcnVuICckY3BwIGVycm5vLmMnIjsKKwl9IGVsc2UgeworCSAgICBteSAk
Y3BwID0gZGVmYXVsdF9jcHAoKSAuICRpbmhpYml0X2xpbmVtYXJrZXJzOworCSAgICBvcGVuKENQ
UE8sIiRjcHAgPCBlcnJuby5jIHwiKQorCQlvciBkaWUgIkNhbm5vdCBleGVjICRjcHAiOworCX0K
IAotICAgICVlcnIgPSAoKTsKKwklZXJyID0gKCk7CiAKLSAgICB3aGlsZSg8Q1BQTz4pIHsKLQlt
eSgkbmFtZSwkZXhwcik7Ci0JbmV4dCB1bmxlc3MgKCRuYW1lLCAkZXhwcikgPSAvIiguKj8pIlxz
KlxbXHMqXFtccyooLio/KVxzKlxdXHMqXF0vOwotCW5leHQgaWYgJG5hbWUgZXEgJGV4cHI7Ci0J
JGVycnskbmFtZX0gPSBldmFsICRleHByOworCXdoaWxlKDxDUFBPPikgeworCSAgICBteSgkbmFt
ZSwkZXhwcik7CisJICAgIG5leHQgdW5sZXNzICgkbmFtZSwgJGV4cHIpID0gLyIoLio/KSJccypc
W1xzKlxbXHMqKC4qPylccypcXVxzKlxdLzsKKwkgICAgbmV4dCBpZiAkbmFtZSBlcSAkZXhwcjsK
KwkgICAgJGVycnskbmFtZX0gPSBldmFsICRleHByOworCX0KKwljbG9zZShDUFBPKTsKICAgICB9
Ci0gICAgY2xvc2UoQ1BQTyk7CiAKICAgICAjIFdyaXRlIEVycm5vLnBtCiAKQEAgLTE3NSw3ICsy
MTEsNyBAQCBzdWIgd3JpdGVfZXJybm9fcG0gewogIwogCiBwYWNrYWdlIEVycm5vOwotdXNlIHZh
cnMgcXcoXEBFWFBPUlRfT0sgXCVFWFBPUlRfVEFHUyBcQElTQSBcJFZFUlNJT04gXCVlcnJubyBc
JEFVVE9MT0FEKTsKK291ciAoXEBFWFBPUlRfT0ssXCVFWFBPUlRfVEFHUyxcQElTQSxcJFZFUlNJ
T04sXCVlcnJubyxcJEFVVE9MT0FEKTsKIHVzZSBFeHBvcnRlciAoKTsKIHVzZSBDb25maWc7CiB1
c2Ugc3RyaWN0Owo=
END
  }
  elsif ( $num < 5.007002 ) { # v5.6.0 et al
    _patch(<<'END');
diff --git a/ext/Errno/Errno_pm.PL b/ext/Errno/Errno_pm.PL
index 3f2f3e0..d8fe44e 100644
--- ext/Errno/Errno_pm.PL
+++ ext/Errno/Errno_pm.PL
@@ -172,16 +172,26 @@ sub write_errno_pm {
     unless ($^O eq 'MacOS') {	# trust what we have
     # invoke CPP and read the output
 
+       my $inhibit_linemarkers = '';
+       if ($Config{gccversion} =~ /\A(\d+)\./ and $1 >= 5) {
+           # GCC 5.0 interleaves expanded macros with line numbers breaking
+           # each line into multiple lines. RT#123784
+           $inhibit_linemarkers = ' -P';
+       }
+
 	if ($^O eq 'VMS') {
-	    my $cpp = "$Config{cppstdin} $Config{cppflags} $Config{cppminus}";
+	    my $cpp = "$Config{cppstdin} $Config{cppflags}" .
+        $inhibit_linemarkers . " $Config{cppminus}";
 	    $cpp =~ s/sys\$input//i;
 	    open(CPPO,"$cpp  errno.c |") or
 		die "Cannot exec $Config{cppstdin}";
 	} elsif ($^O eq 'MSWin32') {
-	    open(CPPO,"$Config{cpprun} $Config{cppflags} errno.c |") or
-		die "Cannot run '$Config{cpprun} $Config{cppflags} errno.c'";
+           my $cpp = "$Config{cpprun} $Config{cppflags}" .
+               $inhibit_linemarkers;
+           open(CPPO,"$cpp errno.c |") or
+               die "Cannot run '$cpp errno.c'";
 	} else {
-	    my $cpp = default_cpp();
+	    my $cpp = default_cpp() . $inhibit_linemarkers;
 	    open(CPPO,"$cpp < errno.c |")
 		or die "Cannot exec $cpp";
 	}
END
  }
  elsif ( $num < 5.007003 ) { # v5.7.2
    _patch(<<'END');
diff --git a/ext/Errno/Errno_pm.PL b/ext/Errno/Errno_pm.PL
index 3f2f3e0..d8fe44e 100644
--- ext/Errno/Errno_pm.PL
+++ ext/Errno/Errno_pm.PL
@@ -172,16 +172,26 @@ sub write_errno_pm {
     unless ($^O eq 'MacOS') {	# trust what we have
     # invoke CPP and read the output
 
+       my $inhibit_linemarkers = '';
+       if ($Config{gccversion} =~ /\A(\d+)\./ and $1 >= 5) {
+           # GCC 5.0 interleaves expanded macros with line numbers breaking
+           # each line into multiple lines. RT#123784
+           $inhibit_linemarkers = ' -P';
+       }
+
 	if ($^O eq 'VMS') {
-	    my $cpp = "$Config{cppstdin} $Config{cppflags} $Config{cppminus}";
+	    my $cpp = "$Config{cppstdin} $Config{cppflags}" .
+        $inhibit_linemarkers . " $Config{cppminus}";
 	    $cpp =~ s/sys\$input//i;
 	    open(CPPO,"$cpp  errno.c |") or
 		die "Cannot exec $Config{cppstdin}";
 	} elsif ($^O eq 'MSWin32' || $^O eq 'NetWare') {
-	    open(CPPO,"$Config{cpprun} $Config{cppflags} errno.c |") or
-		die "Cannot run '$Config{cpprun} $Config{cppflags} errno.c'";
+           my $cpp = "$Config{cpprun} $Config{cppflags}" .
+               $inhibit_linemarkers;
+           open(CPPO,"$cpp errno.c |") or
+               die "Cannot run '$cpp errno.c'";
 	} else {
-	    my $cpp = default_cpp();
+	    my $cpp = default_cpp() . $inhibit_linemarkers;
 	    open(CPPO,"$cpp < errno.c |")
 		or die "Cannot exec $cpp";
 	}
END
  }
  elsif ( $num < 5.008009 ) {
    _patch(<<'END');
diff --git a/ext/Errno/Errno_pm.PL b/ext/Errno/Errno_pm.PL
index d8a0ab3..796e2f1 100644
--- ext/Errno/Errno_pm.PL
+++ ext/Errno/Errno_pm.PL
@@ -235,16 +235,26 @@ sub write_errno_pm {
     unless ($^O eq 'MacOS' || $^O eq 'beos') {	# trust what we have / get later
     # invoke CPP and read the output
 
+       my $inhibit_linemarkers = '';
+       if ($Config{gccversion} =~ /\A(\d+)\./ and $1 >= 5) {
+           # GCC 5.0 interleaves expanded macros with line numbers breaking
+           # each line into multiple lines. RT#123784
+           $inhibit_linemarkers = ' -P';
+       }
+
 	if ($^O eq 'VMS') {
-	    my $cpp = "$Config{cppstdin} $Config{cppflags} $Config{cppminus}";
+      my $cpp = "$Config{cppstdin} $Config{cppflags}" .
+      $inhibit_linemarkers . " $Config{cppminus}";
 	    $cpp =~ s/sys\$input//i;
 	    open(CPPO,"$cpp  errno.c |") or
 		die "Cannot exec $Config{cppstdin}";
 	} elsif ($^O eq 'MSWin32' || $^O eq 'NetWare') {
-	    open(CPPO,"$Config{cpprun} $Config{cppflags} errno.c |") or
-		die "Cannot run '$Config{cpprun} $Config{cppflags} errno.c'";
+           my $cpp = "$Config{cpprun} $Config{cppflags}" .
+               $inhibit_linemarkers;
+           open(CPPO,"$cpp errno.c |") or
+               die "Cannot run '$cpp errno.c'";
 	} else {
-	    my $cpp = default_cpp();
+	    my $cpp = default_cpp() . $inhibit_linemarkers;
 	    open(CPPO,"$cpp < errno.c |")
 		or die "Cannot exec $cpp";
 	}
END
  }
  elsif ( $num > 5.008009 and $num < 5.009003 ) {
    _patch_b64(<<'END');
LS0tIGV4dC9FcnJuby9FcnJub19wbS5QTAorKysgZXh0L0Vycm5vL0Vycm5vX3BtLlBMCkBAIC03
LDEyICs3LDM1IEBAIG91ciAkVkVSU0lPTiA9ICIxLjA5XzAxIjsKIG15ICVlcnIgPSAoKTsKIG15
ICV3c2EgPSAoKTsKIAorIyBTeW1iaWFuIGNyb3NzLWNvbXBpbGluZyBlbnZpcm9ubWVudC4KK215
ICRJc1N5bWJpYW4gPSBleGlzdHMgJEVOVntTREt9ICYmIC1kICIkRU5We1NES31cXGVwb2MzMiI7
CisKK215ICRJc01TV2luMzIgPSAkXk8gZXEgJ01TV2luMzInICYmICEkSXNTeW1iaWFuOworCiB1
bmxpbmsgIkVycm5vLnBtIiBpZiAtZiAiRXJybm8ucG0iOwogb3BlbiBPVVQsICI+RXJybm8ucG0i
IG9yIGRpZSAiQ2Fubm90IG9wZW4gRXJybm8ucG06ICQhIjsKIHNlbGVjdCBPVVQ7CiBteSAkZmls
ZTsKLWZvcmVhY2ggJGZpbGUgKGdldF9maWxlcygpKSB7Ci0gICAgcHJvY2Vzc19maWxlKCRmaWxl
KTsKK215IEBmaWxlcyA9IGdldF9maWxlcygpOworaWYgKCRDb25maWd7Z2NjdmVyc2lvbn0gbmUg
JycgJiYgJF5PIGVxICdNU1dpbjMyJykgeworICAgICMgTWluR1cgY29tcGxhaW5zICJ3YXJuaW5n
OiAjcHJhZ21hIHN5c3RlbV9oZWFkZXIgaWdub3JlZCBvdXRzaWRlIGluY2x1ZGUKKyAgICAjIGZp
bGUiIGlmIHRoZSBoZWFkZXIgZmlsZXMgYXJlIHByb2Nlc3NlZCBpbmRpdmlkdWFsbHksIHNvIGlu
Y2x1ZGUgdGhlbQorICAgICMgYWxsIGluIC5jIGZpbGUgYW5kIHByb2Nlc3MgdGhhdCBpbnN0ZWFk
LgorICAgIG9wZW4gSU5DUywgJz5pbmNsdWRlcy5jJyBvcgorCWRpZSAiQ2Fubm90IG9wZW4gaW5j
bHVkZXMuYyI7CisgICAgZm9yZWFjaCAkZmlsZSAoQGZpbGVzKSB7CisJbmV4dCBpZiAkZmlsZSBl
cSAnZXJybm8uYyc7CisJbmV4dCB1bmxlc3MgLWYgJGZpbGU7CisJcHJpbnQgSU5DUyBxcVsjaW5j
bHVkZSAiJGZpbGUiXG5dOworICAgIH0KKyAgICBjbG9zZSBJTkNTOworICAgIHByb2Nlc3NfZmls
ZSgnaW5jbHVkZXMuYycpOworICAgIHVubGluayAnaW5jbHVkZXMuYyc7Cit9CitlbHNlIHsKKyAg
ICBmb3JlYWNoICRmaWxlIChAZmlsZXMpIHsKKwlwcm9jZXNzX2ZpbGUoJGZpbGUpOworICAgIH0K
IH0KIHdyaXRlX2Vycm5vX3BtKCk7CiB1bmxpbmsgImVycm5vLmMiIGlmIC1mICJlcnJuby5jIjsK
QEAgLTI3LDcgKzUwLDcgQEAgc3ViIHByb2Nlc3NfZmlsZSB7CiAgICAgfQogCiAgICAgcmV0dXJu
IHVubGVzcyBkZWZpbmVkICRmaWxlIGFuZCAtZiAkZmlsZTsKLSMgICB3YXJuICJQcm9jZXNzaW5n
ICRmaWxlXG4iOworIyAgICB3YXJuICJQcm9jZXNzaW5nICRmaWxlXG4iOwogCiAgICAgbG9jYWwg
KkZIOwogICAgIGlmICgoJF5PIGVxICdWTVMnKSAmJiAoJENvbmZpZ3t2bXNfY2NfdHlwZX0gbmUg
J2dudWMnKSkgewpAQCAtNTMsNyArNzYsNyBAQCBzdWIgcHJvY2Vzc19maWxlIHsKICAgICAgICAg
ICAgIHJldHVybjsKIAl9CiAgICAgfQotCisgICAgCiAgICAgaWYgKCReTyBlcSAnTWFjT1MnKSB7
CiAJd2hpbGUoPEZIPikgewogCSAgICAkZXJyeyQxfSA9ICQyCkBAIC02MywxMiArODYsMTMgQEAg
c3ViIHByb2Nlc3NfZmlsZSB7CiAJd2hpbGUoPEZIPikgewogCSAgICAkZXJyeyQxfSA9IDEKIAkJ
aWYgL15ccyojXHMqZGVmaW5lXHMrKEVcdyspXHMrLzsKLSAgICAgICAgICAgIGlmICgkXk8gZXEg
J01TV2luMzInKSB7CisgICAgICAgICAgICBpZiAoJElzTVNXaW4zMikgewogCSAgICAgICAgJHdz
YXskMX0gPSAxCiAJICAgIAkgICAgaWYgL15ccyojXHMqZGVmaW5lXHMrV1NBKEVcdyspXHMrLzsK
ICAgICAgICAgICAgIH0KIAl9CiAgICAgfQorCiAgICAgY2xvc2UoRkgpOwogfQogCkBAIC0xMzAs
NiArMTU0LDEwIEBAIHN1YiBnZXRfZmlsZXMgewogICAgIH0gZWxzaWYgKCReTyBlcSAndm9zJykg
ewogCSMgYXZvaWQgcHJvYmxlbSB3aGVyZSBjcHAgcmV0dXJucyBub24tUE9TSVggcGF0aG5hbWVz
CiAJJGZpbGV7Jy9zeXN0ZW0vaW5jbHVkZV9saWJyYXJ5L2Vycm5vLmgnfSA9IDE7CisgICAgfSBl
bHNpZiAoJElzU3ltYmlhbikgeworICAgICAgICBteSAkU0RLID0gJEVOVntTREt9OworICAgICAg
ICAkU0RLID1+IHMhXFwhLyFnOworCSRmaWxleyIkU0RLL2Vwb2MzMi9pbmNsdWRlL2xpYmMvc3lz
L2Vycm5vLmgifSA9IDE7CiAgICAgfSBlbHNlIHsKIAlvcGVuKENQUEksIj4gZXJybm8uYyIpIG9y
CiAJICAgIGRpZSAiQ2Fubm90IG9wZW4gZXJybm8uYyI7CkBAIC0xMzgsNyArMTY2LDcgQEAgc3Vi
IGdldF9maWxlcyB7CiAJICAgIHByaW50IENQUEkgIiNpbmNsdWRlIDxud2Vycm5vLmg+XG4iOwog
CX0gZWxzZSB7CiAJICAgIHByaW50IENQUEkgIiNpbmNsdWRlIDxlcnJuby5oPlxuIjsKLQkgICAg
aWYgKCReTyBlcSAnTVNXaW4zMicpIHsKKwkgICAgaWYgKCRJc01TV2luMzIpIHsKIAkJcHJpbnQg
Q1BQSSAiI2RlZmluZSBfV0lOU09DS0FQSV9cbiI7ICMgZG9uJ3QgZHJhZyBpbiBldmVyeXRoaW5n
CiAJCXByaW50IENQUEkgIiNpbmNsdWRlIDx3aW5zb2NrLmg+XG4iOwogCSAgICB9CkBAIC0xNDcs
NyArMTc1LDcgQEAgc3ViIGdldF9maWxlcyB7CiAJY2xvc2UoQ1BQSSk7CiAKIAkjIGludm9rZSBD
UFAgYW5kIHJlYWQgdGhlIG91dHB1dAotCWlmICgkXk8gZXEgJ01TV2luMzInIHx8ICReTyBlcSAn
TmV0V2FyZScpIHsKKwlpZiAoJElzTVNXaW4zMiB8fCAkXk8gZXEgJ05ldFdhcmUnKSB7CiAJICAg
IG9wZW4oQ1BQTywiJENvbmZpZ3tjcHBydW59ICRDb25maWd7Y3BwZmxhZ3N9IGVycm5vLmMgfCIp
IG9yCiAJCWRpZSAiQ2Fubm90IHJ1biAnJENvbmZpZ3tjcHBydW59ICRDb25maWd7Y3BwZmxhZ3N9
IGVycm5vLmMnIjsKIAl9IGVsc2UgewpAQCAtMTU3LDE0ICsxODUsMTQgQEAgc3ViIGdldF9maWxl
cyB7CiAJfQogCiAJbXkgJHBhdDsKLQlpZiAoKCReTyBlcSAnTVNXaW4zMicgfHwgJF5PIGVxICdO
ZXRXYXJlJykgYW5kICRDb25maWd7Y2N9ID1+IC9eYmNjL2kpIHsKKwlpZiAoKCRJc01TV2luMzIg
fHwgJF5PIGVxICdOZXRXYXJlJykgYW5kICRDb25maWd7Y2N9ID1+IC9eYmNjL2kpIHsKIAkgICAg
JHBhdCA9ICdeL1wqXHMrKC4rKVxzK1xkK1xzKjpccytcKi8nOwogCX0KIAllbHNlIHsKIAkgICAg
JHBhdCA9ICdeI1xzKig/OmxpbmUpP1xzKlxkK1xzKyIoW14iXSspIic7CiAJfQogCXdoaWxlKDxD
UFBPPikgewotCSAgICBpZiAoJF5PIGVxICdvczInIG9yICReTyBlcSAnTVNXaW4zMicgb3IgJF5P
IGVxICdOZXRXYXJlJykgeworCSAgICBpZiAoJF5PIGVxICdvczInIG9yICRJc01TV2luMzIgb3Ig
JF5PIGVxICdOZXRXYXJlJykgewogCQlpZiAoLyRwYXQvbykgewogCQkgICBteSAkZiA9ICQxOwog
CQkgICAkZiA9fiBzLFxcXFwsLyxnOwpAQCAtMTk4LDcgKzIyNiw3IEBAIHN1YiB3cml0ZV9lcnJu
b19wbSB7CiAgICAgZWxzZSB7CiAJcHJpbnQgQ1BQSSAiI2luY2x1ZGUgPGVycm5vLmg+XG4iOwog
ICAgIH0KLSAgICBpZiAoJF5PIGVxICdNU1dpbjMyJykgeworICAgIGlmICgkSXNNU1dpbjMyKSB7
CiAJcHJpbnQgQ1BQSSAiI2luY2x1ZGUgPHdpbnNvY2suaD5cbiI7CiAJZm9yZWFjaCAkZXJyIChr
ZXlzICV3c2EpIHsKIAkgICAgcHJpbnQgQ1BQSSAiI2lmbmRlZiAkZXJyXG4iOwpAQCAtMjE3LDE2
ICsyNDUsMzEgQEAgc3ViIHdyaXRlX2Vycm5vX3BtIHsKICAgICB1bmxlc3MgKCReTyBlcSAnTWFj
T1MnIHx8ICReTyBlcSAnYmVvcycpIHsJIyB0cnVzdCB3aGF0IHdlIGhhdmUgLyBnZXQgbGF0ZXIK
ICAgICAjIGludm9rZSBDUFAgYW5kIHJlYWQgdGhlIG91dHB1dAogCisJbXkgJGluaGliaXRfbGlu
ZW1hcmtlcnMgPSAnJzsKKwlpZiAoJENvbmZpZ3tnY2N2ZXJzaW9ufSA9fiAvXEEoXGQrKVwuLyBh
bmQgJDEgPj0gNSkgeworCSAgICAjIEdDQyA1LjAgaW50ZXJsZWF2ZXMgZXhwYW5kZWQgbWFjcm9z
IHdpdGggbGluZSBudW1iZXJzIGJyZWFraW5nCisJICAgICMgZWFjaCBsaW5lIGludG8gbXVsdGlw
bGUgbGluZXMuIFJUIzEyMzc4NAorCSAgICAkaW5oaWJpdF9saW5lbWFya2VycyA9ICcgLVAnOwor
CX0KKwogCWlmICgkXk8gZXEgJ1ZNUycpIHsKLQkgICAgbXkgJGNwcCA9ICIkQ29uZmlne2NwcHN0
ZGlufSAkQ29uZmlne2NwcGZsYWdzfSAkQ29uZmlne2NwcG1pbnVzfSI7CisJICAgIG15ICRjcHAg
PSAiJENvbmZpZ3tjcHBzdGRpbn0gJENvbmZpZ3tjcHBmbGFnc30iIC4KKwkJJGluaGliaXRfbGlu
ZW1hcmtlcnMgLiAiICRDb25maWd7Y3BwbWludXN9IjsKIAkgICAgJGNwcCA9fiBzL3N5c1wkaW5w
dXQvL2k7CiAJICAgIG9wZW4oQ1BQTywiJGNwcCAgZXJybm8uYyB8Iikgb3IKIAkJZGllICJDYW5u
b3QgZXhlYyAkQ29uZmlne2NwcHN0ZGlufSI7Ci0JfSBlbHNpZiAoJF5PIGVxICdNU1dpbjMyJyB8
fCAkXk8gZXEgJ05ldFdhcmUnKSB7Ci0JICAgIG9wZW4oQ1BQTywiJENvbmZpZ3tjcHBydW59ICRD
b25maWd7Y3BwZmxhZ3N9IGVycm5vLmMgfCIpIG9yCi0JCWRpZSAiQ2Fubm90IHJ1biAnJENvbmZp
Z3tjcHBydW59ICRDb25maWd7Y3BwZmxhZ3N9IGVycm5vLmMnIjsKLQl9IGVsc2UgewotCSAgICBt
eSAkY3BwID0gZGVmYXVsdF9jcHAoKTsKKwl9IGVsc2lmICgkSXNNU1dpbjMyIHx8ICReTyBlcSAn
TmV0V2FyZScpIHsKKwkgICAgbXkgJGNwcCA9ICIkQ29uZmlne2NwcHJ1bn0gJENvbmZpZ3tjcHBm
bGFnc30iIC4KKwkJJGluaGliaXRfbGluZW1hcmtlcnM7CisJICAgIG9wZW4oQ1BQTywiJGNwcCBl
cnJuby5jIHwiKSBvcgorCQlkaWUgIkNhbm5vdCBydW4gJyRjcHAgZXJybm8uYyciOworCX0gZWxz
aWYgKCRJc1N5bWJpYW4pIHsKKyAgICAgICAgICAgIG15ICRjcHAgPSAiZ2NjIC1FIC1JJEVOVntT
REt9XFxlcG9jMzJcXGluY2x1ZGVcXGxpYmMiIC4KKwkJJGluaGliaXRfbGluZW1hcmtlcnMgLiIg
LSI7CisJICAgIG9wZW4oQ1BQTywiJGNwcCA8IGVycm5vLmMgfCIpCisJCW9yIGRpZSAiQ2Fubm90
IGV4ZWMgJGNwcCI7CisgICAgICAgIH0gZWxzZSB7CisJICAgIG15ICRjcHAgPSBkZWZhdWx0X2Nw
cCgpIC4gJGluaGliaXRfbGluZW1hcmtlcnM7CiAJICAgIG9wZW4oQ1BQTywiJGNwcCA8IGVycm5v
LmMgfCIpCiAJCW9yIGRpZSAiQ2Fubm90IGV4ZWMgJGNwcCI7CiAJfQo=
END
  }
  else {
    _patch(<<'END');
diff --git a/ext/Errno/Errno_pm.PL b/ext/Errno/Errno_pm.PL
index 3dadfce..c6bfa06 100644
--- ext/Errno/Errno_pm.PL
+++ ext/Errno/Errno_pm.PL
@@ -215,20 +215,31 @@ sub write_errno_pm {
     {	# BeOS (support now removed) did not enter this block
     # invoke CPP and read the output
 
+	my $inhibit_linemarkers = '';
+	if ($Config{gccversion} =~ /\A(\d+)\./ and $1 >= 5) {
+	    # GCC 5.0 interleaves expanded macros with line numbers breaking
+	    # each line into multiple lines. RT#123784
+	    $inhibit_linemarkers = ' -P';
+	}
+
 	if ($^O eq 'VMS') {
-	    my $cpp = "$Config{cppstdin} $Config{cppflags} $Config{cppminus}";
+	    my $cpp = "$Config{cppstdin} $Config{cppflags}" .
+		$inhibit_linemarkers . " $Config{cppminus}";
 	    $cpp =~ s/sys\$input//i;
 	    open(CPPO,"$cpp  errno.c |") or
 		die "Cannot exec $Config{cppstdin}";
 	} elsif ($IsMSWin32 || $^O eq 'NetWare') {
-	    open(CPPO,"$Config{cpprun} $Config{cppflags} errno.c |") or
-		die "Cannot run '$Config{cpprun} $Config{cppflags} errno.c'";
+	    my $cpp = "$Config{cpprun} $Config{cppflags}" .
+		$inhibit_linemarkers;
+	    open(CPPO,"$cpp errno.c |") or
+		die "Cannot run '$cpp errno.c'";
 	} elsif ($IsSymbian) {
-            my $cpp = "gcc -E -I$ENV{SDK}\\epoc32\\include\\libc -";
+            my $cpp = "gcc -E -I$ENV{SDK}\\epoc32\\include\\libc" .
+		$inhibit_linemarkers ." -";
 	    open(CPPO,"$cpp < errno.c |")
 		or die "Cannot exec $cpp";
         } else {
-	    my $cpp = default_cpp();
+	    my $cpp = default_cpp() . $inhibit_linemarkers;
 	    open(CPPO,"$cpp < errno.c |")
 		or die "Cannot exec $cpp";
 	}
END
  }
}

sub _patch_time_hires {
  _patch(<<'END');
diff --git a/dist/Time-HiRes/HiRes.pm b/dist/Time-HiRes/HiRes.pm
index ad9a65c99d..a3ddd595b7 100644
--- dist/Time-HiRes/HiRes.pm
+++ dist/Time-HiRes/HiRes.pm
@@ -23,12 +23,12 @@ our @EXPORT_OK = qw (usleep sleep ualarm alarm gettimeofday time tv_interval
 		 ITIMER_REAL ITIMER_VIRTUAL ITIMER_PROF ITIMER_REALPROF
 		 TIMER_ABSTIME
 		 d_usleep d_ualarm d_gettimeofday d_getitimer d_setitimer
-		 d_nanosleep d_clock_gettime d_clock_getres
+		 d_nanosleep d_clock_gettime d_clock_getres d_hires_utime
 		 d_clock d_clock_nanosleep
-		 stat lstat
+		 stat lstat utime
 		);
 
-our $VERSION = '1.9733';
+our $VERSION = '1.9741';
 our $XS_VERSION = $VERSION;
 $VERSION = eval $VERSION;
 
@@ -60,6 +60,7 @@ sub import {
 	    ($i eq 'clock'           && !&d_clock)           ||
 	    ($i eq 'nanosleep'       && !&d_nanosleep)       ||
 	    ($i eq 'usleep'          && !&d_usleep)          ||
+	    ($i eq 'utime'           && !&d_hires_utime)     ||
 	    ($i eq 'ualarm'          && !&d_ualarm)) {
 	    require Carp;
 	    Carp::croak("Time::HiRes::$i(): unimplemented in this platform");
@@ -92,7 +93,7 @@ Time::HiRes - High resolution alarm, sleep, gettimeofday, interval timers
 
   use Time::HiRes qw( usleep ualarm gettimeofday tv_interval nanosleep
 		      clock_gettime clock_getres clock_nanosleep clock
-                      stat lstat );
+                      stat lstat utime);
 
   usleep ($microseconds);
   nanosleep ($nanoseconds);
@@ -137,6 +138,9 @@ Time::HiRes - High resolution alarm, sleep, gettimeofday, interval timers
   my @stat = stat(FH);
   my @stat = lstat("file");
 
+  use Time::HiRes qw( utime );
+  utime $floating_seconds, $floating_seconds, file...;
+
 =head1 DESCRIPTION
 
 The C<Time::HiRes> module implements a Perl interface to the
@@ -446,6 +450,26 @@ if the operations are
 the access time stamp from t2 need not be greater-than the modify
 time stamp from t1: it may be equal or I<less>.
 
+=item utime LIST
+
+As L<perlfunc/utime>
+but with the ability to set the access/modify file timestamps
+in subsecond resolution, if the operating system and the filesystem
+both support such timestamps.  To override the standard utime():
+
+    use Time::HiRes qw(utime);
+
+Test for the value of &Time::HiRes::d_hires_utime to find out whether
+the operating system supports setting subsecond file timestamps.
+
+As with CORE::utime(), passing undef as both the atime and mtime will
+call the syscall with a NULL argument.
+
+The actual achievable subsecond resolution depends on the combination
+of the operating system and the filesystem.
+
+Returns the number of files successfully changed.
+
 =back
 
 =head1 EXAMPLES
@@ -535,7 +559,7 @@ VMS have emulations for it.)
 Here is an example of using C<NVtime> from C:
 
   NV (*myNVtime)(); /* Returns -1 on failure. */
-  SV **svp = hv_fetch(PL_modglobal, "Time::NVtime", 12, 0);
+  SV **svp = hv_fetchs(PL_modglobal, "Time::NVtime", 0);
   if (!svp)         croak("Time::HiRes is required");
   if (!SvIOK(*svp)) croak("Time::NVtime isn't a function pointer");
   myNVtime = INT2PTR(NV(*)(), SvIV(*svp));
@@ -586,9 +610,13 @@ might help in this (in case your system supports CLOCK_MONOTONIC).
 Some systems have APIs but not implementations: for example QNX and Haiku
 have the interval timer APIs but not the functionality.
 
-In OS X clock_getres(), clock_gettime() and clock_nanosleep() are
-emulated using the Mach timers; as a side effect of being emulated
-the CLOCK_REALTIME and CLOCK_MONOTONIC are the same timer.
+In pre-Sierra macOS (pre-10.12, OS X) clock_getres(), clock_gettime()
+and clock_nanosleep() are emulated using the Mach timers; as a side
+effect of being emulated the CLOCK_REALTIME and CLOCK_MONOTONIC are
+the same timer.
+
+gnukfreebsd seems to have non-functional futimens() and utimensat()
+(at least as of 10.1): therefore the hires utime() does not work.
 
 =head1 SEE ALSO
 
diff --git a/dist/Time-HiRes/HiRes.xs b/dist/Time-HiRes/HiRes.xs
index 38ca0dc320..6b0dba8e68 100644
--- dist/Time-HiRes/HiRes.xs
+++ dist/Time-HiRes/HiRes.xs
@@ -87,6 +87,10 @@ extern "C" {
 #   undef ITIMER_REALPROF
 #endif
 
+#ifndef TIME_HIRES_CLOCKID_T
+typedef int clockid_t;
+#endif
+
 #if defined(TIME_HIRES_CLOCK_GETTIME) && defined(_STRUCT_ITIMERSPEC)
 
 /* HP-UX has CLOCK_XXX values but as enums, not as defines.
@@ -747,21 +751,33 @@ hrstatns(UV *atime_nsec, UV *mtime_nsec, UV *ctime_nsec)
 #endif /* !TIME_HIRES_STAT */
 }
 
-/* Until Apple implements clock_gettime() (ditto clock_getres())
- * we will emulate it using Mach interfaces. */
-#if defined(PERL_DARWIN) && !defined(CLOCK_REALTIME)
-
-#  include <mach/mach_time.h>
+/* Until Apple implements clock_gettime()
+ * (ditto clock_getres() and clock_nanosleep())
+ * we will emulate them using the Mach kernel interfaces. */
+#if defined(PERL_DARWIN) && \
+  (defined(TIME_HIRES_CLOCK_GETTIME_EMULATION)   || \
+   defined(TIME_HIRES_CLOCK_GETRES_EMULATION)    || \
+   defined(TIME_HIRES_CLOCK_NANOSLEEP_EMULATION))
 
+#ifndef CLOCK_REALTIME
 #  define CLOCK_REALTIME  0x01
 #  define CLOCK_MONOTONIC 0x02
+#endif
 
+#ifndef TIMER_ABSTIME
 #  define TIMER_ABSTIME   0x01
+#endif
 
 #ifdef USE_ITHREADS
+#  define PERL_DARWIN_MUTEX
+#endif
+
+#ifdef PERL_DARWIN_MUTEX
 STATIC perl_mutex darwin_time_mutex;
 #endif
 
+#include <mach/mach_time.h>
+
 static uint64_t absolute_time_init;
 static mach_timebase_info_data_t timebase_info;
 static struct timespec timespec_init;
@@ -769,7 +785,7 @@ static struct timespec timespec_init;
 static int darwin_time_init() {
   struct timeval tv;
   int success = 1;
-#ifdef USE_ITHREADS
+#ifdef PERL_DARWIN_MUTEX
   MUTEX_LOCK(&darwin_time_mutex);
 #endif
   if (absolute_time_init == 0) {
@@ -784,13 +800,14 @@ static int darwin_time_init() {
       }
     }
   }
-#ifdef USE_ITHREADS
+#ifdef PERL_DARWIN_MUTEX
   MUTEX_UNLOCK(&darwin_time_mutex);
 #endif
   return success;
 }
 
-static int clock_gettime(int clock_id, struct timespec *ts) {
+#ifdef TIME_HIRES_CLOCK_GETTIME_EMULATION
+static int th_clock_gettime(clockid_t clock_id, struct timespec *ts) {
   if (darwin_time_init() && timebase_info.denom) {
     switch (clock_id) {
       case CLOCK_REALTIME:
@@ -822,7 +839,12 @@ static int clock_gettime(int clock_id, struct timespec *ts) {
   return -1;
 }
 
-static int clock_getres(int clock_id, struct timespec *ts) {
+#define clock_gettime(clock_id, ts) th_clock_gettime((clock_id), (ts))
+
+#endif /* TIME_HIRES_CLOCK_GETTIME_EMULATION */
+
+#ifdef TIME_HIRES_CLOCK_GETRES_EMULATION
+static int th_clock_getres(clockid_t clock_id, struct timespec *ts) {
   if (darwin_time_init() && timebase_info.denom) {
     switch (clock_id) {
       case CLOCK_REALTIME:
@@ -842,7 +864,11 @@ static int clock_getres(int clock_id, struct timespec *ts) {
   return -1;
 }
 
-static int clock_nanosleep(int clock_id, int flags,
+#define clock_getres(clock_id, ts) th_clock_getres((clock_id), (ts))
+#endif /* TIME_HIRES_CLOCK_GETRES_EMULATION */
+
+#ifdef TIME_HIRES_CLOCK_NANOSLEEP_EMULATION
+static int th_clock_nanosleep(clockid_t clock_id, int flags,
 			   const struct timespec *rqtp,
 			   struct timespec *rmtp) {
   if (darwin_time_init()) {
@@ -880,6 +906,11 @@ static int clock_nanosleep(int clock_id, int flags,
   return -1;
 }
 
+#define clock_nanosleep(clock_id, flags, rqtp, rmtp) \
+  th_clock_nanosleep((clock_id), (flags), (rqtp), (rmtp))
+
+#endif /* TIME_HIRES_CLOCK_NANOSLEEP_EMULATION */
+
 #endif /* PERL_DARWIN */
 
 #include "const-c.inc"
@@ -921,6 +952,22 @@ nsec_without_unslept(struct timespec *sleepfor,
 
 #endif
 
+/* In case Perl and/or Devel::PPPort are too old, minimally emulate
+ * IS_SAFE_PATHNAME() (which looks for zero bytes in the pathname). */
+#ifndef IS_SAFE_PATHNAME
+#if PERL_VERSION >= 12 /* Perl_ck_warner is 5.10.0 -> */
+#ifdef WARN_SYSCALLS
+#define WARNEMUCAT WARN_SYSCALLS /* 5.22.0 -> */
+#else
+#define WARNEMUCAT WARN_MISC
+#endif
+#define WARNEMU(opname) Perl_ck_warner(aTHX_ packWARN(WARNEMUCAT), "Invalid \\0 character in pathname for %s",opname)
+#else
+#define WARNEMU(opname) Perl_warn(aTHX_ "Invalid \\0 character in pathname for %s",opname)
+#endif
+#define IS_SAFE_PATHNAME(pv, len, opname) (((len)>1)&&memchr((pv), 0, (len)-1)?(SETERRNO(ENOENT, LIB_INVARG),WARNEMU(opname),FALSE):(TRUE))
+#endif
+
 MODULE = Time::HiRes            PACKAGE = Time::HiRes
 
 PROTOTYPES: ENABLE
@@ -941,7 +988,7 @@ BOOT:
 #   endif
 #endif
 #if defined(PERL_DARWIN)
-#  ifdef USE_ITHREADS
+#  if defined(USE_ITHREADS) && defined(PERL_DARWIN_MUTEX)
   MUTEX_INIT(&darwin_time_mutex);
 #  endif
 #endif
@@ -978,7 +1025,8 @@ usleep(useconds)
 		    useconds -= NV_1E6 * seconds;
 		}
 	    } else if (useconds < 0.0)
-	        croak("Time::HiRes::usleep(%"NVgf"): negative time not invented yet", useconds);
+	        croak("Time::HiRes::usleep(%" NVgf
+                      "): negative time not invented yet", useconds);
 	    usleep((U32)useconds);
 	} else
 	    PerlProc_pause();
@@ -1000,7 +1048,8 @@ nanosleep(nsec)
 	struct timespec sleepfor, unslept;
 	CODE:
 	if (nsec < 0.0)
-	    croak("Time::HiRes::nanosleep(%"NVgf"): negative time not invented yet", nsec);
+	    croak("Time::HiRes::nanosleep(%" NVgf
+                  "): negative time not invented yet", nsec);
         nanosleep_init(nsec, &sleepfor, &unslept);
 	if (nanosleep(&sleepfor, &unslept) == 0) {
 	    RETVAL = nsec;
@@ -1045,11 +1094,15 @@ sleep(...)
 		   useconds = -(IV)useconds;
 #endif /* #if defined(__sparc64__) && defined(__GNUC__) */
 		   if ((IV)useconds < 0)
-		     croak("Time::HiRes::sleep(%"NVgf"): internal error: useconds < 0 (unsigned %"UVuf" signed %"IVdf")", seconds, useconds, (IV)useconds);
+		     croak("Time::HiRes::sleep(%" NVgf
+                           "): internal error: useconds < 0 (unsigned %" UVuf
+                           " signed %" IVdf ")",
+                           seconds, useconds, (IV)useconds);
 		 }
 		 usleep(useconds);
 	    } else
-	        croak("Time::HiRes::sleep(%"NVgf"): negative time not invented yet", seconds);
+	        croak("Time::HiRes::sleep(%" NVgf
+                      "): negative time not invented yet", seconds);
 	} else
 	    PerlProc_pause();
 	gettimeofday(&Tb, NULL);
@@ -1097,7 +1150,9 @@ ualarm(useconds,uinterval=0)
 	  }
 #else
 	if (useconds >= IV_1E6 || uinterval >= IV_1E6) 
-		croak("Time::HiRes::ualarm(%d, %d): useconds or uinterval equal to or more than %"IVdf, useconds, uinterval, IV_1E6);
+		croak("Time::HiRes::ualarm(%d, %d): useconds or uinterval"
+                      " equal to or more than %" IVdf,
+                      useconds, uinterval, IV_1E6);
 	RETVAL = ualarm(useconds, uinterval);
 #endif
 
@@ -1110,7 +1165,8 @@ alarm(seconds,interval=0)
 	NV interval
 	CODE:
 	if (seconds < 0.0 || interval < 0.0)
-	    croak("Time::HiRes::alarm(%"NVgf", %"NVgf"): negative time not invented yet", seconds, interval);
+	    croak("Time::HiRes::alarm(%" NVgf ", %" NVgf
+                  "): negative time not invented yet", seconds, interval);
 	{
 	  IV iseconds = (IV)seconds;
 	  IV iinterval = (IV)interval;
@@ -1118,7 +1174,9 @@ alarm(seconds,interval=0)
 	  NV finterval = interval - iinterval;
 	  IV useconds, uinterval;
 	  if (fseconds >= 1.0 || finterval >= 1.0)
-		croak("Time::HiRes::alarm(%"NVgf", %"NVgf"): seconds or interval too large to split correctly", seconds, interval);
+		croak("Time::HiRes::alarm(%" NVgf ", %" NVgf
+                      "): seconds or interval too large to split correctly",
+                      seconds, interval);
 	  useconds = IV_1E6 * fseconds;
 	  uinterval = IV_1E6 * finterval;
 #if defined(HAS_SETITIMER) && defined(ITIMER_REAL)
@@ -1138,7 +1196,9 @@ alarm(seconds,interval=0)
 	  }
 #else
 	  if (iseconds || iinterval)
-		croak("Time::HiRes::alarm(%"NVgf", %"NVgf"): seconds or interval equal to or more than 1.0 ", seconds, interval);
+		croak("Time::HiRes::alarm(%" NVgf ", %" NVgf
+                      "): seconds or interval equal to or more than 1.0 ",
+                      seconds, interval);
 	    RETVAL = (NV)ualarm( useconds, uinterval ) / NV_1E6;
 #endif
 	}
@@ -1266,7 +1326,9 @@ setitimer(which, seconds, interval = 0)
 	struct itimerval oldit;
     PPCODE:
 	if (seconds < 0.0 || interval < 0.0)
-	    croak("Time::HiRes::setitimer(%"IVdf", %"NVgf", %"NVgf"): negative time not invented yet", (IV)which, seconds, interval);
+	    croak("Time::HiRes::setitimer(%" IVdf ", %" NVgf ", %" NVgf
+                  "): negative time not invented yet",
+                  (IV)which, seconds, interval);
 	newit.it_value.tv_sec  = (IV)seconds;
 	newit.it_value.tv_usec =
 	  (IV)((seconds  - (NV)newit.it_value.tv_sec)    * NV_1E6);
@@ -1317,11 +1379,89 @@ getitimer(which)
 
 #endif /* #if defined(HAS_GETITIMER) && defined(HAS_SETITIMER) */
 
+#if defined(TIME_HIRES_UTIME)
+
+I32
+utime(accessed, modified, ...)
+PROTOTYPE: $$@
+    PREINIT:
+	SV* accessed;
+	SV* modified;
+	SV* file;
+
+	struct timespec utbuf[2];
+	struct timespec *utbufp = utbuf;
+	int tot;
+
+    CODE:
+	accessed = ST(0);
+	modified = ST(1);
+	items -= 2;
+	tot = 0;
+
+	if ( accessed == &PL_sv_undef && modified == &PL_sv_undef )
+		utbufp = NULL;
+	else {
+		if (SvNV(accessed) < 0.0 || SvNV(modified) < 0.0)
+                    croak("Time::HiRes::utime(%" NVgf ", %" NVgf
+                          "): negative time not invented yet",
+                              SvNV(accessed), SvNV(modified));
+		Zero(&utbuf, sizeof utbuf, char);
+		utbuf[0].tv_sec = (Time_t)SvNV(accessed);  /* time accessed */
+		utbuf[0].tv_nsec = (long)( ( SvNV(accessed) - utbuf[0].tv_sec ) * 1e9 );
+		utbuf[1].tv_sec = (Time_t)SvNV(modified);  /* time modified */
+		utbuf[1].tv_nsec = (long)( ( SvNV(modified) - utbuf[1].tv_sec ) * 1e9 );
+	}
+
+	while (items > 0) {
+		file = POPs; items--;
+
+		if (SvROK(file) && GvIO(SvRV(file)) && IoIFP(sv_2io(SvRV(file)))) {
+			int fd =  PerlIO_fileno(IoIFP(sv_2io(file)));
+			if (fd < 0)
+				SETERRNO(EBADF,RMS_IFI);
+			else 
+#ifdef HAS_FUTIMENS
+			if (futimens(fd, utbufp) == 0)
+				tot++;
+#else  /* HAS_FUTIMES */
+				croak("futimens unimplemented in this platform");
+#endif /* HAS_FUTIMES */
+		}
+		else {
+#ifdef HAS_UTIMENSAT
+			STRLEN len;
+			char * name = SvPV(file, len);
+			if (IS_SAFE_PATHNAME(name, len, "utime") &&
+			    utimensat(AT_FDCWD, name, utbufp, 0) == 0)
+				tot++;
+#else  /* HAS_UTIMENSAT */
+			croak("utimensat unimplemented in this platform");
+#endif /* HAS_UTIMENSAT */
+		}
+	} /* while items */
+	RETVAL = tot;
+
+    OUTPUT:
+	RETVAL
+
+#else  /* #if defined(TIME_HIRES_UTIME) */
+
+I32
+utime(accessed, modified, ...)
+    CODE:
+        croak("Time::HiRes::utime(): unimplemented in this platform");
+        RETVAL = 0;
+    OUTPUT:
+	RETVAL
+
+#endif /* #if defined(TIME_HIRES_UTIME) */
+
 #if defined(TIME_HIRES_CLOCK_GETTIME)
 
 NV
 clock_gettime(clock_id = CLOCK_REALTIME)
-	int clock_id
+	clockid_t clock_id
     PREINIT:
 	struct timespec ts;
 	int status = -1;
@@ -1340,7 +1480,7 @@ clock_gettime(clock_id = CLOCK_REALTIME)
 
 NV
 clock_gettime(clock_id = 0)
-	int clock_id
+	clockid_t clock_id
     CODE:
 	PERL_UNUSED_ARG(clock_id);
         croak("Time::HiRes::clock_gettime(): unimplemented in this platform");
@@ -1354,7 +1494,7 @@ clock_gettime(clock_id = 0)
 
 NV
 clock_getres(clock_id = CLOCK_REALTIME)
-	int clock_id
+	clockid_t clock_id
     PREINIT:
 	int status = -1;
 	struct timespec ts;
@@ -1373,7 +1513,7 @@ clock_getres(clock_id = CLOCK_REALTIME)
 
 NV
 clock_getres(clock_id = 0)
-	int clock_id
+	clockid_t clock_id
     CODE:
 	PERL_UNUSED_ARG(clock_id);
         croak("Time::HiRes::clock_getres(): unimplemented in this platform");
@@ -1387,14 +1527,15 @@ clock_getres(clock_id = 0)
 
 NV
 clock_nanosleep(clock_id, nsec, flags = 0)
-	int clock_id
+	clockid_t clock_id
 	NV  nsec
 	int flags
     PREINIT:
 	struct timespec sleepfor, unslept;
     CODE:
 	if (nsec < 0.0)
-	    croak("Time::HiRes::clock_nanosleep(..., %"NVgf"): negative time not invented yet", nsec);
+	    croak("Time::HiRes::clock_nanosleep(..., %" NVgf
+                  "): negative time not invented yet", nsec);
         nanosleep_init(nsec, &sleepfor, &unslept);
 	if (clock_nanosleep(clock_id, flags, &sleepfor, &unslept) == 0) {
 	    RETVAL = nsec;
@@ -1408,7 +1549,7 @@ clock_nanosleep(clock_id, nsec, flags = 0)
 
 NV
 clock_nanosleep(clock_id, nsec, flags = 0)
-	int clock_id
+	clockid_t clock_id
 	NV  nsec
 	int flags
     CODE:
diff --git a/dist/Time-HiRes/Makefile.PL b/dist/Time-HiRes/Makefile.PL
index 087ab79871..ccad6a3e6f 100644
--- dist/Time-HiRes/Makefile.PL
+++ dist/Time-HiRes/Makefile.PL
@@ -88,7 +88,7 @@ sub try_compile_and_link {
     my $obj_ext = $Config{obj_ext} || ".o";
     unlink("$tmp.c", "$tmp$obj_ext");
 
-    if (open(TMPC, ">$tmp.c")) {
+    if (open(TMPC, '>', "$tmp.c")) {
 	print TMPC $c;
 	close(TMPC);
 
@@ -132,7 +132,7 @@ __EOD__
 	    unless defined $cccmd;
 
        if ($^O eq 'VMS') {
-	    open( CMDFILE, ">$tmp.com" );
+	    open( CMDFILE, '>', "$tmp.com" );
 	    print CMDFILE "\$ SET MESSAGE/NOFACILITY/NOSEVERITY/NOIDENT/NOTEXT\n";
 	    print CMDFILE "\$ $cccmd\n";
 	    print CMDFILE "\$ IF \$SEVERITY .NE. 1 THEN EXIT 44\n"; # escalate
@@ -290,6 +290,7 @@ sub has_clock_xxx_syscall {
 #include "EXTERN.h"
 #include "perl.h"
 #include "XSUB.h"
+#include <time.h>
 #include <$SYSCALL_H>
 int main(int argc, char** argv)
 {
@@ -309,6 +310,7 @@ sub has_clock_xxx {
 #include "EXTERN.h"
 #include "perl.h"
 #include "XSUB.h"
+#include <time.h>
 int main(int argc, char** argv)
 {
     struct timespec ts;
@@ -325,6 +327,7 @@ sub has_clock {
 #include "EXTERN.h"
 #include "perl.h"
 #include "XSUB.h"
+#include <time.h>
 int main(int argc, char** argv)
 {
     clock_t tictoc;
@@ -348,12 +351,63 @@ int main(int argc, char** argv)
     struct timespec ts2;
     ts1.tv_sec  = 0;
     ts1.tv_nsec = 750000000;;
-    ret = clock_nanosleep(CLOCK_MONOTONIC, 0, &ts1, &ts2);
+    /* All implementations are supposed to support CLOCK_REALTIME. */
+    ret = clock_nanosleep(CLOCK_REALTIME, 0, &ts1, &ts2);
     ret == 0 ? exit(0) : exit(errno ? errno : -1);
 }
 EOM
 }
 
+sub has_futimens {
+    return 1 if
+    try_compile_and_link(<<EOM);
+#include "EXTERN.h"
+#include "perl.h"
+#include "XSUB.h"
+#include <sys/stat.h>
+int main(int argc, char** argv)
+{
+    int ret;
+    struct timespec ts[2];
+    ret = futimens(0, ts);
+    ret == 0 ? exit(0) : exit(errno ? errno : -1);
+}
+EOM
+}
+
+sub has_utimensat{
+    return 1 if
+    try_compile_and_link(<<EOM);
+#include "EXTERN.h"
+#include "perl.h"
+#include "XSUB.h"
+#include <sys/stat.h>
+#include <fcntl.h>
+int main(int argc, char** argv)
+{
+    int ret;
+    struct timespec ts[2];
+    ret = utimensat(AT_FDCWD, 0, ts, 0);
+    ret == 0 ? exit(0) : exit(errno ? errno : -1);
+}
+EOM
+}
+
+sub has_clockid_t{
+    return 1 if
+    try_compile_and_link(<<EOM);
+#include "EXTERN.h"
+#include "perl.h"
+#include "XSUB.h"
+#include <time.h>
+int main(int argc, char** argv)
+{
+    clockid_t id = CLOCK_REALTIME;
+    exit(id == CLOCK_REALTIME ? 1 : 0);
+}
+EOM
+}
+
 sub DEFINE {
     my ($def, $val) = @_;
     my $define = defined $val ? "$def=$val" : $def ;
@@ -534,6 +588,16 @@ EOD
         print "(It would not be portable anyway.)\n";
     }
 
+    print "Looking for clockid_t... ";
+    my $has_clockid_t;
+    if (has_clockid_t()) {
+	print "found.\n";
+        $has_clockid_t++;
+	$DEFINE .= ' -DTIME_HIRES_CLOCKID_T';
+    } else {
+	print "NOT found, will use int.\n";
+    }
+
     print "Looking for clock_gettime()... ";
     my $has_clock_gettime;
     my $has_clock_gettime_emulation;
@@ -548,7 +612,7 @@ EOD
     } elsif ($^O eq 'darwin') {
        $has_clock_gettime_emulation++;
        $has_clock_gettime++;
-       $DEFINE .= ' -DTIME_HIRES_CLOCK_GETTIME';
+       $DEFINE .= ' -DTIME_HIRES_CLOCK_GETTIME -DTIME_HIRES_CLOCK_GETTIME_EMULATION';
     }
 
     if ($has_clock_gettime) {
@@ -577,7 +641,7 @@ EOD
     } elsif ($^O eq 'darwin') {
        $has_clock_getres_emulation++;
        $has_clock_getres++;
-       $DEFINE .= ' -DTIME_HIRES_CLOCK_GETRES';
+       $DEFINE .= ' -DTIME_HIRES_CLOCK_GETRES -DTIME_HIRES_CLOCK_GETRES_EMULATION';
     }
 
     if ($has_clock_getres) {
@@ -603,7 +667,7 @@ EOD
     } elsif ($^O eq 'darwin') {
         $has_clock_nanosleep++;
         $has_clock_nanosleep_emulation++;
-	$DEFINE .= ' -DTIME_HIRES_CLOCK_NANOSLEEP';
+	$DEFINE .= ' -DTIME_HIRES_CLOCK_NANOSLEEP -DTIME_HIRES_CLOCK_NANOSLEEP_EMULATION';
     }
 
     if ($has_clock_nanosleep) {
@@ -631,6 +695,36 @@ EOD
 	print "NOT found.\n";
     }
 
+    print "Looking for futimens()... ";
+    my $has_futimens;
+    if (has_futimens()) {
+        $has_futimens++;
+	$DEFINE .= ' -DHAS_FUTIMENS';
+    }
+
+    if ($has_futimens) {
+        print "found.\n";
+    } else {
+	print "NOT found.\n";
+    }
+
+    print "Looking for utimensat()... ";
+    my $has_utimensat;
+    if (has_utimensat()) {
+        $has_utimensat++;
+	$DEFINE .= ' -DHAS_UTIMENSAT';
+    }
+
+    if ($has_utimensat) {
+        print "found.\n";
+    } else {
+	print "NOT found.\n";
+    }
+
+    if ($has_futimens or $has_utimensat) {
+	$DEFINE .= ' -DTIME_HIRES_UTIME';
+    }
+
     print "Looking for stat() subsecond timestamps...\n";
 
     print "Trying struct stat st_atimespec.tv_nsec...";
@@ -644,7 +738,7 @@ int main(int argc, char** argv) {
 }
 EOM
       $has_stat_st_xtimespec++;
-      DEFINE('TIME_HIRES_STAT', 1);
+      DEFINE('TIME_HIRES_STAT_ST_XTIMESPEC');  # 1
     }
 
     if ($has_stat_st_xtimespec) {
@@ -664,7 +758,7 @@ int main(int argc, char** argv) {
 }
 EOM
       $has_stat_st_xtimensec++;
-      DEFINE('TIME_HIRES_STAT', 2);
+      DEFINE('TIME_HIRES_STAT_ST_XTIMENSEC');  # 2
     }
 
     if ($has_stat_st_xtimensec) {
@@ -684,7 +778,7 @@ int main(int argc, char** argv) {
 }
 EOM
       $has_stat_st_xtime_n++;
-      DEFINE('TIME_HIRES_STAT', 3);
+      DEFINE('TIME_HIRES_STAT_ST_XTIME_N');  # 3
     }
 
     if ($has_stat_st_xtime_n) {
@@ -704,7 +798,7 @@ int main(int argc, char** argv) {
 }
 EOM
       $has_stat_st_xtim++;
-      DEFINE('TIME_HIRES_STAT', 4);
+      DEFINE('TIME_HIRES_STAT_XTIM');  # 4
     }
 
     if ($has_stat_st_xtim) {
@@ -724,7 +818,7 @@ int main(int argc, char** argv) {
 }
 EOM
       $has_stat_st_uxtime++;
-      DEFINE('TIME_HIRES_STAT', 5);
+      DEFINE('TIME_HIRES_STAT_ST_UXTIME');  # 5
     }
 
     if ($has_stat_st_uxtime) {
@@ -733,6 +827,19 @@ EOM
 	print "NOT found.\n";
     }
 
+    # See HiRes.xs hrstatns()
+    if ($has_stat_st_xtimespec) {
+        DEFINE('TIME_HIRES_STAT', 1);
+    } elsif ($has_stat_st_xtimensec) {
+        DEFINE('TIME_HIRES_STAT', 2);
+    } elsif ($has_stat_st_xtime_n) {
+        DEFINE('TIME_HIRES_STAT', 3);
+    } elsif ($has_stat_st_xtim) {
+        DEFINE('TIME_HIRES_STAT', 4);
+    } elsif ($has_stat_st_uxtime) {
+        DEFINE('TIME_HIRES_STAT', 5);
+    }    
+
    if ($DEFINE =~ /-DTIME_HIRES_STAT=\d+/) {
     print "You seem to have stat() subsecond timestamps.\n";
     print "(Your struct stat has them, but the filesystems must help.)\n";
@@ -757,7 +864,7 @@ EOM
 
     if ($DEFINE) {
         $DEFINE =~ s/^\s+//;
-        if (open(XDEFINE, ">xdefine")) {
+        if (open(XDEFINE, '>', 'xdefine')) {
 	    print XDEFINE $DEFINE, "\n";
 	    close(XDEFINE);
         }
@@ -791,7 +898,7 @@ sub doMakefile {
 	    'DynaLoader' => 0,
 	    'Exporter' => 0,
 	    'ExtUtils::MakeMaker' => 0,
-	    'Test::More' => "0.82",
+	    'Test::More' => 0,
 	    'strict' => 0,
 	},
 	'dist'      => {
@@ -869,7 +976,8 @@ sub doConstants {
                       );
 	foreach (qw (d_usleep d_ualarm d_gettimeofday d_getitimer d_setitimer
 		     d_nanosleep d_clock_gettime d_clock_getres
-		     d_clock d_clock_nanosleep d_hires_stat)) {
+		     d_clock d_clock_nanosleep d_hires_stat
+                     d_futimens d_utimensat d_hires_utime)) {
 	    my $macro = $_;
 	    if ($macro =~ /^(d_nanosleep|d_clock)$/) {
 		$macro =~ s/^d_(.+)/TIME_HIRES_\U$1/;
@@ -879,6 +987,13 @@ sub doConstants {
 		push @names, {name => $_, macro => "TIME_HIRES_STAT", value => $d_hires_stat,
 			      default => ["IV", "0"]};
 		next;
+	    } elsif ($macro =~ /^(d_hires_utime)$/) {
+		my $d_hires_utime =
+                    ($DEFINE =~ /-DHAS_FUTIMENS/ ||
+                     $DEFINE =~ /-DHAS_UTIMENSAT/) ? 1 : 0;
+		push @names, {name => $_, macro => "TIME_HIRES_UTIME", value => $d_hires_utime,
+			      default => ["IV", "0"]};
+		next;
 	    } elsif ($macro =~ /^(d_clock_gettime|d_clock_getres|d_clock_nanosleep)$/) {
 		$macro =~ s/^d_(.+)/TIME_HIRES_\U$1/;
 		my $val = ($DEFINE =~ /-D$macro\b/) ? 1 : 0;
@@ -900,8 +1015,8 @@ sub doConstants {
 	foreach $file ('const-c.inc', 'const-xs.inc') {
 	    my $fallback = File::Spec->catfile('fallback', $file);
 	    local $/;
-	    open IN, "<$fallback" or die "Can't open $fallback: $!";
-	    open OUT, ">$file" or die "Can't open $file: $!";
+	    open IN, '<', $fallback or die "Can't open $fallback: $!";
+	    open OUT, '>', $file or die "Can't open $file: $!";
 	    print OUT <IN> or die $!;
 	    close OUT or die "Can't close $file: $!";
 	    close IN or die "Can't close $fallback: $!";
@@ -920,7 +1035,7 @@ sub main {
 	    DEFINE('SELECT_IS_BROKEN');
 	    $LIBS = [];
 	    print "System is $^O, skipping full configure...\n";
-	    open(XDEFINE, ">xdefine") or die "$0: Cannot create xdefine: $!\n";
+	    open(XDEFINE, '>', 'xdefine') or die "$0: Cannot create xdefine: $!\n";
 	    close(XDEFINE);
 	} else {
 	    init();
diff --git a/dist/Time-HiRes/fallback/const-c.inc b/dist/Time-HiRes/fallback/const-c.inc
index a8626172af..524db169a9 100644
--- dist/Time-HiRes/fallback/const-c.inc
+++ dist/Time-HiRes/fallback/const-c.inc
@@ -19,6 +19,7 @@ typedef double NV; /* 5.6 and later define NVTYPE, and typedef NV to it.  */
 #ifndef pTHX_
 #define pTHX_ /* 5.6 or later define this for threading support.  */
 #endif
+
 static int
 constant_11 (pTHX_ const char *name, IV *iv_return) {
   /* When generated this function returned values for the list of names given
@@ -86,6 +87,51 @@ constant_11 (pTHX_ const char *name, IV *iv_return) {
   return PERL_constant_NOTFOUND;
 }
 
+static int
+constant_13 (pTHX_ const char *name, IV *iv_return) {
+  /* When generated this function returned values for the list of names given
+     here.  However, subsequent manual editing may have added or removed some.
+     CLOCK_HIGHRES TIMER_ABSTIME d_hires_utime */
+  /* Offset 1 gives the best switch position.  */
+  switch (name[1]) {
+  case 'I':
+    if (memEQ(name, "TIMER_ABSTIME", 13)) {
+    /*                ^                  */
+#ifdef TIMER_ABSTIME
+      *iv_return = TIMER_ABSTIME;
+      return PERL_constant_ISIV;
+#else
+      return PERL_constant_NOTDEF;
+#endif
+    }
+    break;
+  case 'L':
+    if (memEQ(name, "CLOCK_HIGHRES", 13)) {
+    /*                ^                  */
+#ifdef CLOCK_HIGHRES
+      *iv_return = CLOCK_HIGHRES;
+      return PERL_constant_ISIV;
+#else
+      return PERL_constant_NOTDEF;
+#endif
+    }
+    break;
+  case '_':
+    if (memEQ(name, "d_hires_utime", 13)) {
+    /*                ^                  */
+#ifdef TIME_HIRES_UTIME
+      *iv_return = 1;
+      return PERL_constant_ISIV;
+#else
+      *iv_return = 0;
+      return PERL_constant_ISIV;
+#endif
+    }
+    break;
+  }
+  return PERL_constant_NOTFOUND;
+}
+
 static int
 constant_14 (pTHX_ const char *name, IV *iv_return) {
   /* When generated this function returned values for the list of names given
@@ -250,16 +296,17 @@ my @names = (qw(CLOCKS_PER_SEC CLOCK_HIGHRES CLOCK_MONOTONIC
             {name=>"d_getitimer", type=>"IV", macro=>"HAS_GETITIMER", value=>"1", default=>["IV", "0"]},
             {name=>"d_gettimeofday", type=>"IV", macro=>"HAS_GETTIMEOFDAY", value=>"1", default=>["IV", "0"]},
             {name=>"d_hires_stat", type=>"IV", macro=>"TIME_HIRES_STAT", value=>"1", default=>["IV", "0"]},
+            {name=>"d_hires_utime", type=>"IV", macro=>"TIME_HIRES_UTIME", value=>"1", default=>["IV", "0"]},
             {name=>"d_nanosleep", type=>"IV", macro=>"TIME_HIRES_NANOSLEEP", value=>"1", default=>["IV", "0"]},
             {name=>"d_setitimer", type=>"IV", macro=>"HAS_SETITIMER", value=>"1", default=>["IV", "0"]},
             {name=>"d_ualarm", type=>"IV", macro=>"HAS_UALARM", value=>"1", default=>["IV", "0"]},
             {name=>"d_usleep", type=>"IV", macro=>"HAS_USLEEP", value=>"1", default=>["IV", "0"]});
 
-print constant_types(); # macro defs
+print constant_types(), "\n"; # macro defs
 foreach (C_constant ("Time::HiRes", 'constant', 'IV', $types, undef, 3, @names) ) {
     print $_, "\n"; # C constant subs
 }
-print "#### XS Section:\n";
+print "\n#### XS Section:\n";
 print XS_constant ("Time::HiRes", $types);
 __END__
    */
@@ -322,33 +369,7 @@ __END__
     }
     break;
   case 13:
-    /* Names all of length 13.  */
-    /* CLOCK_HIGHRES TIMER_ABSTIME */
-    /* Offset 2 gives the best switch position.  */
-    switch (name[2]) {
-    case 'M':
-      if (memEQ(name, "TIMER_ABSTIME", 13)) {
-      /*                 ^                 */
-#ifdef TIMER_ABSTIME
-        *iv_return = TIMER_ABSTIME;
-        return PERL_constant_ISIV;
-#else
-        return PERL_constant_NOTDEF;
-#endif
-      }
-      break;
-    case 'O':
-      if (memEQ(name, "CLOCK_HIGHRES", 13)) {
-      /*                 ^                 */
-#ifdef CLOCK_HIGHRES
-        *iv_return = CLOCK_HIGHRES;
-        return PERL_constant_ISIV;
-#else
-        return PERL_constant_NOTDEF;
-#endif
-      }
-      break;
-    }
+    return constant_13 (aTHX_ name, iv_return);
     break;
   case 14:
     return constant_14 (aTHX_ name, iv_return);
diff --git a/dist/Time-HiRes/t/Watchdog.pm b/dist/Time-HiRes/t/Watchdog.pm
index 83e854396f..44ec8081de 100644
--- dist/Time-HiRes/t/Watchdog.pm
+++ dist/Time-HiRes/t/Watchdog.pm
@@ -10,44 +10,44 @@ my $watchdog_pid;
 my $TheEnd;
 
 if ($Config{d_fork}) {
-    note "I am the main process $$, starting the watchdog process...";
+    print("# I am the main process $$, starting the watchdog process...\n");
     $watchdog_pid = fork();
     if (defined $watchdog_pid) {
 	if ($watchdog_pid == 0) { # We are the kid, set up the watchdog.
 	    my $ppid = getppid();
-	    note "I am the watchdog process $$, sleeping for $waitfor seconds...";
+	    print("# I am the watchdog process $$, sleeping for $waitfor seconds...\n");
 	    sleep($waitfor - 2);    # Workaround for perlbug #49073
 	    sleep(2);               # Wait for parent to exit
 	    if (kill(0, $ppid)) {   # Check if parent still exists
 		warn "\n$0: overall time allowed for tests (${waitfor}s) exceeded!\n";
-		note "Terminating main process $ppid...";
+		print("Terminating main process $ppid...\n");
 		kill('KILL', $ppid);
-		note "This is the watchdog process $$, over and out.";
+		print("# This is the watchdog process $$, over and out.\n");
 	    }
 	    exit(0);
 	} else {
-	    note "The watchdog process $watchdog_pid launched, continuing testing...";
+	    print("# The watchdog process $watchdog_pid launched, continuing testing...\n");
 	    $TheEnd = time() + $waitfor;
 	}
     } else {
 	warn "$0: fork failed: $!\n";
     }
 } else {
-    note "No watchdog process (need fork)";
+    print("# No watchdog process (need fork)\n");
 }
 
 END {
     if ($watchdog_pid) { # Only in the main process.
 	my $left = $TheEnd - time();
-	note sprintf "I am the main process $$, terminating the watchdog process $watchdog_pid before it terminates me in %d seconds (testing took %d seconds).", $left, $waitfor - $left;
+	printf("# I am the main process $$, terminating the watchdog process $watchdog_pid before it terminates me in %d seconds (testing took %d seconds).\n", $left, $waitfor - $left);
 	if (kill(0, $watchdog_pid)) {
 	    local $? = 0;
 	    my $kill = kill('KILL', $watchdog_pid); # We are done, the watchdog can go.
 	    wait();
-	    note sprintf "kill KILL $watchdog_pid = %d", $kill;
+	    printf("# kill KILL $watchdog_pid = %d\n", $kill);
 	}
 	unlink("ktrace.out"); # Used in BSD system call tracing.
-	note "All done.";
+	print("# All done.\n");
     }
 }
 
diff --git a/dist/Time-HiRes/t/alarm.t b/dist/Time-HiRes/t/alarm.t
index 841694f67c..4935410d36 100644
--- dist/Time-HiRes/t/alarm.t
+++ dist/Time-HiRes/t/alarm.t
@@ -1,6 +1,6 @@
 use strict;
 
-use Test::More 0.82 tests => 10;
+use Test::More tests => 10;
 use t::Watchdog;
 
 BEGIN { require_ok "Time::HiRes"; }
@@ -10,7 +10,7 @@ use Config;
 my $limit = 0.25; # 25% is acceptable slosh for testing timers
 
 my $xdefine = ''; 
-if (open(XDEFINE, "xdefine")) {
+if (open(XDEFINE, "<", "xdefine")) {
     chomp($xdefine = <XDEFINE> || "");
     close(XDEFINE);
 }
@@ -29,12 +29,14 @@ SKIP: {
 
     my ($r, $i, $not, $ok);
 
+    $not = "";
+
     $r = [Time::HiRes::gettimeofday()];
     $i = 5;
     my $oldaction;
     if ($use_sigaction) {
 	$oldaction = new POSIX::SigAction;
-	note sprintf "sigaction tick, ALRM = %d", &POSIX::SIGALRM;
+	printf("# sigaction tick, ALRM = %d\n", &POSIX::SIGALRM);
 
 	# Perl's deferred signals may be too wimpy to break through
 	# a restartable select(), so use POSIX::sigaction if available.
@@ -44,7 +46,7 @@ SKIP: {
 			 $oldaction)
 	    or die "Error setting SIGALRM handler with sigaction: $!\n";
     } else {
-	note "SIG tick";
+	print("# SIG tick\n");
 	$SIG{ALRM} = "tick";
     }
 
@@ -56,8 +58,8 @@ SKIP: {
 	    Time::HiRes::alarm(0.3);
 	    select (undef, undef, undef, 3);
 	    my $ival = Time::HiRes::tv_interval ($r);
-	    note "Select returned! $i $ival";
-	    note abs($ival/3 - 1);
+	    print("# Select returned! $i $ival\n");
+	    printf("# %s\n", abs($ival/3 - 1));
 	    # Whether select() gets restarted after signals is
 	    # implementation dependent.  If it is restarted, we
 	    # will get about 3.3 seconds: 3 from the select, 0.3
@@ -86,7 +88,7 @@ SKIP: {
     sub tick {
 	$i--;
 	my $ival = Time::HiRes::tv_interval ($r);
-	note "Tick! $i $ival";
+	print("# Tick! $i $ival\n");
 	my $exp = 0.3 * (5 - $i);
 	if ($exp == 0) {
 	    $not = "tick: divisor became zero";
@@ -106,8 +108,8 @@ SKIP: {
 	Time::HiRes::alarm(0); # can't cancel usig %SIG
     }
 
+    print("# $not\n");
     ok !$not;
-    note $not || $ok;
 }
 
 SKIP: {
@@ -126,7 +128,7 @@ SKIP: {
     # http://groups.google.com/group/perl.perl5.porters/browse_thread/thread/adaffaaf939b042e/20dafc298df737f0%2320dafc298df737f0?sa=X&oi=groupsr&start=0&num=3
     # Perl changes [18765] and [18770], perl bug [perl #20920]
 
-    note "Finding delay loop...";
+    print("# Finding delay loop...\n");
 
     my $T = 0.01;
     my $DelayN = 1024;
@@ -137,7 +139,7 @@ SKIP: {
 	 for ($i = 0; $i < $DelayN; $i++) { }
 	 my $t1 = Time::HiRes::time();
 	 my $dt = $t1 - $t0;
-	 note "N = $DelayN, t1 = $t1, t0 = $t0, dt = $dt";
+	 print("# N = $DelayN, t1 = $t1, t0 = $t0, dt = $dt\n");
 	 last N if $dt > $T;
 	 $DelayN *= 2;
      } while (1);
@@ -169,7 +171,7 @@ SKIP: {
 
     $SIG{ALRM} = sub {
 	$a++;
-	note "Alarm $a - ", Time::HiRes::time();
+	printf("# Alarm $a - %s\n", Time::HiRes::time());
 	Time::HiRes::alarm(0) if $a >= $A; # Disarm the alarm.
 	$Delay->(2); # Try burning CPU at least for 2T seconds.
     }; 
@@ -204,18 +206,18 @@ SKIP: {
 	my $alrm = 0;
 	$SIG{ALRM} = sub { $alrm++ };
 	my $got = Time::HiRes::alarm(2.7);
-	ok $got == 0 or note $got;
+	ok $got == 0 or print("# $got\n");
 
 	my $t0 = Time::HiRes::time();
 	1 while Time::HiRes::time() - $t0 <= 1;
 
 	$got = Time::HiRes::alarm(0);
-	ok $got > 0 && $got < 1.8 or note $got;
+	ok $got > 0 && $got < 1.8 or print("# $got\n");
 
-	ok $alrm == 0 or note $alrm;
+	ok $alrm == 0 or print("# $alrm\n");
 
 	$got = Time::HiRes::alarm(0);
-	ok $got == 0 or note $got;
+	ok $got == 0 or print("# $got\n");
     }
 }
 
diff --git a/dist/Time-HiRes/t/clock.t b/dist/Time-HiRes/t/clock.t
index 6d11dd2ca0..346ca57fbf 100644
--- dist/Time-HiRes/t/clock.t
+++ dist/Time-HiRes/t/clock.t
@@ -1,6 +1,6 @@
 use strict;
 
-use Test::More 0.82 tests => 5;
+use Test::More tests => 5;
 use t::Watchdog;
 
 BEGIN { require_ok "Time::HiRes"; }
@@ -13,10 +13,10 @@ sub has_symbol {
     return $@ eq '';
 }
 
-note sprintf "have_clock_gettime   = %d", &Time::HiRes::d_clock_gettime;
-note sprintf "have_clock_getres    = %d", &Time::HiRes::d_clock_getres;
-note sprintf "have_clock_nanosleep = %d", &Time::HiRes::d_clock_nanosleep;
-note sprintf "have_clock           = %d", &Time::HiRes::d_clock;
+printf("# have_clock_gettime   = %d\n", &Time::HiRes::d_clock_gettime);
+printf("# have_clock_getres    = %d\n", &Time::HiRes::d_clock_getres);
+printf("# have_clock_nanosleep = %d\n", &Time::HiRes::d_clock_nanosleep);
+printf("# have_clock           = %d\n", &Time::HiRes::d_clock);
 
 # Ideally, we'd like to test that the timers are rather precise.
 # However, if the system is busy, there are no guarantees on how
@@ -36,25 +36,25 @@ SKIP: {
     my $ok = 0;
  TRY: {
 	for my $try (1..3) {
-	    note "CLOCK_REALTIME: try = $try";
+	    print("# CLOCK_REALTIME: try = $try\n");
 	    my $t0 = Time::HiRes::clock_gettime(&CLOCK_REALTIME);
 	    my $T = 1.5;
 	    Time::HiRes::sleep($T);
 	    my $t1 = Time::HiRes::clock_gettime(&CLOCK_REALTIME);
 	    if ($t0 > 0 && $t1 > $t0) {
-		note "t1 = $t1, t0 = $t0";
+		print("# t1 = $t1, t0 = $t0\n");
 		my $dt = $t1 - $t0;
 		my $rt = abs(1 - $dt / $T);
-		note "dt = $dt, rt = $rt";
+		print("# dt = $dt, rt = $rt\n");
 		if ($rt <= 2 * $limit) {
 		    $ok = 1;
 		    last TRY;
 		}
 	    } else {
-		note "Error: t0 = $t0, t1 = $t1";
+		print("# Error: t0 = $t0, t1 = $t1\n");
 	    }
 	    my $r = rand() + rand();
-	    note sprintf "Sleeping for %.6f seconds...\n", $r;
+	    printf("# Sleeping for %.6f seconds...\n", $r);
 	    Time::HiRes::sleep($r);
 	}
     }
@@ -64,7 +64,7 @@ SKIP: {
 SKIP: {
     skip "no clock_getres", 1 unless &Time::HiRes::d_clock_getres;
     my $tr = Time::HiRes::clock_getres();
-    ok $tr > 0 or note "tr = $tr";
+    ok $tr > 0 or print("# tr = $tr\n");
 }
 
 SKIP: {
@@ -73,17 +73,17 @@ SKIP: {
     my $s = 1.5e9;
     my $t = Time::HiRes::clock_nanosleep(&CLOCK_REALTIME, $s);
     my $r = abs(1 - $t / $s);
-    ok $r < 2 * $limit or note "t = $t, r = $r";
+    ok $r < 2 * $limit or print("# t = $t, r = $r\n");
 }
 
 SKIP: {
     skip "no clock", 1 unless &Time::HiRes::d_clock;
     my @clock = Time::HiRes::clock();
-    note "clock = @clock";
+    print("# clock = @clock\n");
     for my $i (1..3) {
 	for (my $j = 0; $j < 1e6; $j++) { }
 	push @clock, Time::HiRes::clock();
-	note "clock = @clock";
+	print("# clock = @clock\n");
     }
     ok $clock[0] >= 0 &&
 	$clock[1] > $clock[0] &&
diff --git a/dist/Time-HiRes/t/gettimeofday.t b/dist/Time-HiRes/t/gettimeofday.t
index 8f7c5f3039..69defe8672 100644
--- dist/Time-HiRes/t/gettimeofday.t
+++ dist/Time-HiRes/t/gettimeofday.t
@@ -8,26 +8,26 @@ BEGIN {
     }
 }
 
-use Test::More 0.82 tests => 6;
+use Test::More tests => 6;
 use t::Watchdog;
 
 my @one = Time::HiRes::gettimeofday();
-note 'gettimeofday returned ', 0+@one, ' args';
+printf("# gettimeofday returned %d args\n", 0+@one);
 ok @one == 2;
-ok $one[0] > 850_000_000 or note "@one too small";
+ok $one[0] > 850_000_000 or print("# @one too small\n");
 
 sleep 1;
 
 my @two = Time::HiRes::gettimeofday();
 ok $two[0] > $one[0] || ($two[0] == $one[0] && $two[1] > $one[1])
-	or note "@two is not greater than @one";
+	or print("# @two is not greater than @one\n");
 
 my $f = Time::HiRes::time();
-ok $f > 850_000_000 or note "$f too small";
-ok $f - $two[0] < 2 or note "$f - $two[0] >= 2";
+ok $f > 850_000_000 or print("# $f too small\n");
+ok $f - $two[0] < 2 or print("# $f - $two[0] >= 2\n");
 
 my $r = [Time::HiRes::gettimeofday()];
 my $g = Time::HiRes::tv_interval $r;
-ok $g < 2 or note $g;
+ok $g < 2 or print("# $g\n");
 
 1;
diff --git a/dist/Time-HiRes/t/itimer.t b/dist/Time-HiRes/t/itimer.t
index 9eb2b93f6f..31cdd674ae 100644
--- dist/Time-HiRes/t/itimer.t
+++ dist/Time-HiRes/t/itimer.t
@@ -25,7 +25,7 @@ BEGIN {
     }
 }
 
-use Test::More 0.82 tests => 2;
+use Test::More tests => 2;
 use t::Watchdog;
 
 my $limit = 0.25; # 25% is acceptable slosh for testing timers
@@ -35,11 +35,11 @@ my $r = [Time::HiRes::gettimeofday()];
 
 $SIG{VTALRM} = sub {
     $i ? $i-- : Time::HiRes::setitimer(&Time::HiRes::ITIMER_VIRTUAL, 0);
-    note "Tick! $i ", Time::HiRes::tv_interval($r);
+    printf("# Tick! $i %s\n", Time::HiRes::tv_interval($r));
 };	
 
-note "setitimer: ", join(" ",
-    Time::HiRes::setitimer(&Time::HiRes::ITIMER_VIRTUAL, 0.5, 0.4));
+printf("# setitimer: %s\n", join(" ",
+       Time::HiRes::setitimer(&Time::HiRes::ITIMER_VIRTUAL, 0.5, 0.4)));
 
 # Assume interval timer granularity of $limit * 0.5 seconds.  Too bold?
 my $virt = Time::HiRes::getitimer(&Time::HiRes::ITIMER_VIRTUAL);
@@ -47,19 +47,19 @@ ok(defined $virt && abs($virt / 0.5) - 1 < $limit,
    "ITIMER_VIRTUAL defined with sufficient granularity")
    or diag "virt=" . (defined $virt ? $virt : 'undef');
 
-note "getitimer: ", join(" ",
-    Time::HiRes::getitimer(&Time::HiRes::ITIMER_VIRTUAL));
+printf("# getitimer: %s\n", join(" ",
+       Time::HiRes::getitimer(&Time::HiRes::ITIMER_VIRTUAL)));
 
 while (Time::HiRes::getitimer(&Time::HiRes::ITIMER_VIRTUAL)) {
     my $j;
     for (1..1000) { $j++ } # Can't be unbreakable, must test getitimer().
 }
 
-note "getitimer: ", join(" ",
-    Time::HiRes::getitimer(&Time::HiRes::ITIMER_VIRTUAL));
+printf("# getitimer: %s\n", join(" ",
+       Time::HiRes::getitimer(&Time::HiRes::ITIMER_VIRTUAL)));
 
 $virt = Time::HiRes::getitimer(&Time::HiRes::ITIMER_VIRTUAL);
-note "at end, i=$i";
+print("# at end, i=$i\n");
 is($virt, 0, "time left should be zero");
 
 $SIG{VTALRM} = 'DEFAULT';
diff --git a/dist/Time-HiRes/t/nanosleep.t b/dist/Time-HiRes/t/nanosleep.t
index aef9db6163..c17a7e4790 100644
--- dist/Time-HiRes/t/nanosleep.t
+++ dist/Time-HiRes/t/nanosleep.t
@@ -8,7 +8,7 @@ BEGIN {
     }
 }
 
-use Test::More 0.82 tests => 3;
+use Test::More tests => 3;
 use t::Watchdog;
 
 eval { Time::HiRes::nanosleep(-5) };
@@ -21,7 +21,7 @@ my $two = CORE::time;
 Time::HiRes::nanosleep(10_000_000);
 my $three = CORE::time;
 ok $one == $two || $two == $three
-    or note "slept too long, $one $two $three";
+    or print("# slept too long, $one $two $three\n");
 
 SKIP: {
     skip "no gettimeofday", 1 unless &Time::HiRes::d_gettimeofday;
@@ -29,7 +29,7 @@ SKIP: {
     Time::HiRes::nanosleep(500_000_000);
     my $f2 = Time::HiRes::time();
     my $d = $f2 - $f;
-    ok $d > 0.4 && $d < 0.9 or note "slept $d secs $f to $f2";
+    ok $d > 0.4 && $d < 0.9 or print("# slept $d secs $f to $f2\n");
 }
 
 1;
diff --git a/dist/Time-HiRes/t/sleep.t b/dist/Time-HiRes/t/sleep.t
index e7cc6271a8..b84b4c6725 100644
--- dist/Time-HiRes/t/sleep.t
+++ dist/Time-HiRes/t/sleep.t
@@ -1,6 +1,6 @@
 use strict;
 
-use Test::More 0.82 tests => 4;
+use Test::More tests => 4;
 use t::Watchdog;
 
 BEGIN { require_ok "Time::HiRes"; }
@@ -8,7 +8,7 @@ BEGIN { require_ok "Time::HiRes"; }
 use Config;
 
 my $xdefine = ''; 
-if (open(XDEFINE, "xdefine")) {
+if (open(XDEFINE, "<", "xdefine")) {
     chomp($xdefine = <XDEFINE> || "");
     close(XDEFINE);
 }
@@ -26,12 +26,12 @@ like $@, qr/::sleep\(-1\): negative time not invented yet/,
 SKIP: {
     skip "no subsecond alarm", 2 unless $can_subsecond_alarm;
     my $f = Time::HiRes::time; 
-    note "time...$f";
+    print("# time...$f\n");
     ok 1;
 
     my $r = [Time::HiRes::gettimeofday()];
     Time::HiRes::sleep (0.5);
-    note "sleep...", Time::HiRes::tv_interval($r);
+    printf("# sleep...%s\n", Time::HiRes::tv_interval($r));
     ok 1;
 }
 
diff --git a/dist/Time-HiRes/t/stat.t b/dist/Time-HiRes/t/stat.t
index 68a6fb6bbd..a59a342e20 100644
--- dist/Time-HiRes/t/stat.t
+++ dist/Time-HiRes/t/stat.t
@@ -13,14 +13,14 @@ BEGIN {
     }
 }
 
-use Test::More 0.82 tests => 43;
+use Test::More tests => 43;
 use t::Watchdog;
 
 my @atime;
 my @mtime;
 for (1..5) {
     Time::HiRes::sleep(rand(0.1) + 0.1);
-    open(X, ">$$");
+    open(X, '>', $$);
     print X $$;
     close(X);
     my($a, $stat, $b) = ("a", [Time::HiRes::stat($$)], "b");
@@ -33,7 +33,7 @@ for (1..5) {
     is $b, "b";
     is_deeply $lstat, $stat;
     Time::HiRes::sleep(rand(0.1) + 0.1);
-    open(X, "<$$");
+    open(X, '<', $$);
     <X>;
     close(X);
     $stat = [Time::HiRes::stat($$)];
@@ -42,8 +42,8 @@ for (1..5) {
     is_deeply $lstat, $stat;
 }
 1 while unlink $$;
-note "mtime = @mtime";
-note "atime = @atime";
+print("# mtime = @mtime\n");
+print("# atime = @atime\n");
 my $ai = 0;
 my $mi = 0;
 my $ss = 0;
@@ -63,7 +63,7 @@ for (my $i = 1; $i < @mtime; $i++) {
 	$ss++;
     }
 }
-note "ai = $ai, mi = $mi, ss = $ss";
+print("# ai = $ai, mi = $mi, ss = $ss\n");
 # Need at least 75% of monotonical increase and
 # 20% of subsecond results. Yes, this is guessing.
 SKIP: {
@@ -75,7 +75,7 @@ SKIP: {
 my $targetname = "tgt$$";
 my $linkname = "link$$";
 SKIP: {
-    open(X, ">$targetname");
+    open(X, '>', $targetname);
     print X $$;
     close(X);
     eval { symlink $targetname, $linkname or die "can't symlink: $!"; };
diff --git a/dist/Time-HiRes/t/time.t b/dist/Time-HiRes/t/time.t
index feec4799d9..6f219f9e0c 100644
--- dist/Time-HiRes/t/time.t
+++ dist/Time-HiRes/t/time.t
@@ -1,6 +1,6 @@
 use strict;
 
-use Test::More 0.82 tests => 2;
+use Test::More tests => 2;
 use t::Watchdog;
 
 BEGIN { require_ok "Time::HiRes"; }
@@ -16,8 +16,8 @@ SKIP: {
     # (CORE::time() may be rounding down, up, or closest),
     # but allow 10% of slop.
     ok abs($s) / $n <= 1.10
-	or note "Time::HiRes::time() not close to CORE::time()";
-    note "s = $s, n = $n, s/n = ", abs($s)/$n;
+	or print("# Time::HiRes::time() not close to CORE::time()\n");
+    printf("# s = $s, n = $n, s/n = %s\n", abs($s)/$n);
 }
 
 1;
diff --git a/dist/Time-HiRes/t/tv_interval.t b/dist/Time-HiRes/t/tv_interval.t
index bffcf39ec1..8ac876daf3 100644
--- dist/Time-HiRes/t/tv_interval.t
+++ dist/Time-HiRes/t/tv_interval.t
@@ -1,10 +1,10 @@
 use strict;
 
-use Test::More 0.82 tests => 2;
+use Test::More tests => 2;
 
 BEGIN { require_ok "Time::HiRes"; }
 
 my $f = Time::HiRes::tv_interval [5, 100_000], [10, 500_000];
-ok abs($f - 5.4) < 0.001 or note $f;
+ok abs($f - 5.4) < 0.001 or print("# $f\n");
 
 1;
diff --git a/dist/Time-HiRes/t/ualarm.t b/dist/Time-HiRes/t/ualarm.t
index 12ef4b52cc..b50a175f44 100644
--- dist/Time-HiRes/t/ualarm.t
+++ dist/Time-HiRes/t/ualarm.t
@@ -8,7 +8,7 @@ BEGIN {
     }
 }
 
-use Test::More 0.82 tests => 12;
+use Test::More tests => 12;
 use t::Watchdog;
 
 use Config;
@@ -24,13 +24,13 @@ SKIP: {
     $tick = 0; Time::HiRes::ualarm(10_000); while ($tick == 0) { }
     my $three = CORE::time;
     ok $one == $two || $two == $three
-	or note "slept too long, $one $two $three";
-    note "tick = $tick, one = $one, two = $two, three = $three";
+	or print("# slept too long, $one $two $three\n");
+    print("# tick = $tick, one = $one, two = $two, three = $three\n");
 
     $tick = 0; Time::HiRes::ualarm(10_000, 10_000); while ($tick < 3) { }
     ok 1;
     Time::HiRes::ualarm(0);
-    note "tick = $tick, one = $one, two = $two, three = $three";
+    print("# tick = $tick, one = $one, two = $two, three = $three\n");
 }
 
 eval { Time::HiRes::ualarm(-4) };
@@ -59,24 +59,24 @@ for my $n (100_000, 1_100_000, 2_200_000, 4_300_000) {
 	my $alarmed = 0;
 	local $SIG{ ALRM } = sub { $alarmed++ };
 	my $t0 = Time::HiRes::time();
-	note "t0 = $t0";
-	note "ualarm($n)";
+	print("# t0 = $t0\n");
+	print("# ualarm($n)\n");
 	Time::HiRes::ualarm($n); 1 while $alarmed == 0;
 	my $t1 = Time::HiRes::time();
-	note "t1 = $t1";
+	print("# t1 = $t1\n");
 	my $dt = $t1 - $t0;
-	note "dt = $dt";
+	print("# dt = $dt\n");
 	my $r = $dt / ($n/1e6);
-	note "r = $r";
+	print("# r = $r\n");
 	$ok =
 	    ($n < 1_000_000 || # Too much noise.
 	     ($r >= 0.8 && $r <= 1.6));
 	last if $ok;
 	my $nap = bellish(3, 15);
-	note sprintf "Retrying in %.1f seconds...\n", $nap;
+	printf("# Retrying in %.1f seconds...\n", $nap);
 	Time::HiRes::sleep($nap);
     }
-    ok $ok or note "ualarm($n) close enough";
+    ok $ok or print("# ualarm($n) close enough\n");
 }
 
 {
@@ -93,12 +93,12 @@ for my $n (100_000, 1_100_000, 2_200_000, 4_300_000) {
     } while $t1 - $t0 <= 0.3;
     my $got1 = Time::HiRes::ualarm(0);
 
-    note "t0 = $t0";
-    note "got0 = $got0";
-    note "t1 = $t1";
-    note "t1 - t0 = ", ($t1 - $t0);
-    note "got1 = $got1";
-    ok $got0 == 0 or note $got0;
+    print("# t0 = $t0\n");
+    print("# got0 = $got0\n");
+    print("# t1 = $t1\n");
+    printf("# t1 - t0 = %s\n", ($t1 - $t0));
+    print("# got1 = $got1\n");
+    ok $got0 == 0 or print("# $got0\n");
     SKIP: {
 	skip "alarm interval exceeded", 2 if $t1 - $t0 >= 0.5;
 	ok $got1 > 0;
@@ -106,7 +106,7 @@ for my $n (100_000, 1_100_000, 2_200_000, 4_300_000) {
     }
     ok $got1 < 300_000;
     my $got2 = Time::HiRes::ualarm(0);
-    ok $got2 == 0 or note $got2;
+    ok $got2 == 0 or print("# $got2\n");
 }
 
 1;
diff --git a/dist/Time-HiRes/t/usleep.t b/dist/Time-HiRes/t/usleep.t
index 0d6bacfac3..bdf372bd16 100644
--- dist/Time-HiRes/t/usleep.t
+++ dist/Time-HiRes/t/usleep.t
@@ -8,7 +8,7 @@ BEGIN {
     }
 }
 
-use Test::More 0.82 tests => 6;
+use Test::More tests => 6;
 use t::Watchdog;
 
 eval { Time::HiRes::usleep(-2) };
@@ -23,7 +23,7 @@ my $two = CORE::time;
 Time::HiRes::usleep(10_000);
 my $three = CORE::time;
 ok $one == $two || $two == $three
-or note "slept too long, $one $two $three";
+or print("# slept too long, $one $two $three\n");
 
 SKIP: {
     skip "no gettimeofday", 1 unless &Time::HiRes::d_gettimeofday;
@@ -31,7 +31,7 @@ SKIP: {
     Time::HiRes::usleep(500_000);
     my $f2 = Time::HiRes::time();
     my $d = $f2 - $f;
-    ok $d > 0.4 && $d < 0.9 or note "slept $d secs $f to $f2";
+    ok $d > 0.4 && $d < 0.9 or print("# slept $d secs $f to $f2\n");
 }
 
 SKIP: {
@@ -39,7 +39,7 @@ SKIP: {
     my $r = [ Time::HiRes::gettimeofday() ];
     Time::HiRes::sleep( 0.5 );
     my $f = Time::HiRes::tv_interval $r;
-    ok $f > 0.4 && $f < 0.9 or note "slept $f instead of 0.5 secs.";
+    ok $f > 0.4 && $f < 0.9 or print("# slept $f instead of 0.5 secs.\n");
 }
 
 SKIP: {
@@ -59,7 +59,7 @@ SKIP: {
 
     SKIP: {
 	skip $msg, 1 unless $td < $sleep * (1 + $limit);
-	ok $a < $limit or note $msg;
+	ok $a < $limit or print("# $msg\n");
     }
 
     $t0 = Time::HiRes::gettimeofday();
@@ -71,7 +71,7 @@ SKIP: {
 
     SKIP: {
 	skip $msg, 1 unless $td < $sleep * (1 + $limit);
-	ok $a < $limit or note $msg;
+	ok $a < $limit or print("# $msg\n");
     }
 }
 
diff --git a/dist/Time-HiRes/typemap b/dist/Time-HiRes/typemap
index 1124eb6483..3fa91f3a0b 100644
--- dist/Time-HiRes/typemap
+++ dist/Time-HiRes/typemap
@@ -28,6 +28,8 @@ AV *			T_AVREF
 HV *			T_HVREF
 CV *			T_CVREF
 
+clockid_t               T_IV
+
 IV			T_IV
 UV			T_UV
 NV                      T_NV
END
}

sub _patch_fp_class_denorm {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );

  if ($num < 5.025004) {
    _patch(<<'END');
--- perl.h.orig
+++ perl.h
@@ -1585,6 +1585,26 @@ EXTERN_C char *crypt(const char *, const char *);
 #endif
 #endif
 
+/* We have somehow managed not to define the denormal/subnormal
+ * detection.
+ *
+ * This may happen if the compiler doesn't expose the C99 math like
+ * the fpclassify() without some special switches.  Perl tries to
+ * stay C89, so for example -std=c99 is not an option.
+ *
+ * The Perl_isinf() and Perl_isnan() should have been defined even if
+ * the C99 isinf() and isnan() are unavailable, and the NV_MIN becomes
+ * from the C89 DBL_MIN or moral equivalent. */
+#if !defined(Perl_fp_class_denorm) && defined(Perl_isinf) && defined(Perl_isnan) && defined(NV_MIN)
+#  define Perl_fp_class_denorm(x) ((x) != 0.0 && !Perl_isinf(x) && !Perl_isnan(x) && PERL_ABS(x) < NV_MIN)
+#endif
+
+/* This is not a great fallback: subnormals tests will fail,
+ * but at least Perl will link and 99.999% of tests will work. */
+#if !defined(Perl_fp_class_denorm)
+#  define Perl_fp_class_denorm(x) FALSE
+#endif
+
 /* There is no quadmath_vsnprintf, and therefore my_vsnprintf()
  * dies if called under USE_QUADMATH. */
 #if defined(HAS_VSNPRINTF) && defined(HAS_C99_VARIADIC_MACROS) && !(defined(DEBUGGING) && !defined(PERL_USE_GCC_BRACE_GROUPS)) && !defined(PERL_GCC_PEDANTIC)
END

  }
  else {
    _patch(<<'END');
--- perl.h.orig
+++ perl.h
@@ -6867,6 +6867,26 @@ extern void moncontrol(int);
 #  endif
 #endif
 
+/* We have somehow managed not to define the denormal/subnormal
+ * detection.
+ *
+ * This may happen if the compiler doesn't expose the C99 math like
+ * the fpclassify() without some special switches.  Perl tries to
+ * stay C89, so for example -std=c99 is not an option.
+ *
+ * The Perl_isinf() and Perl_isnan() should have been defined even if
+ * the C99 isinf() and isnan() are unavailable, and the NV_MIN becomes
+ * from the C89 DBL_MIN or moral equivalent. */
+#if !defined(Perl_fp_class_denorm) && defined(Perl_isinf) && defined(Perl_isnan) && defined(NV_MIN)
+#  define Perl_fp_class_denorm(x) ((x) != 0.0 && !Perl_isinf(x) && !Perl_isnan(x) && PERL_ABS(x) < NV_MIN)
+#endif
+
+/* This is not a great fallback: subnormals tests will fail,
+ * but at least Perl will link and 99.999% of tests will work. */
+#if !defined(Perl_fp_class_denorm)
+#  define Perl_fp_class_denorm(x) FALSE
+#endif
+
 #ifdef DOUBLE_IS_IEEE_FORMAT
 #  define DOUBLE_HAS_INF
 #  define DOUBLE_HAS_NAN
END
  }
}

sub _norm_ver {
  my $ver = shift;
  my @v = split(qr/[._]0*/, $ver);
  $v[2] ||= 0;
  return sprintf '%d.%03d%03d', @v;
}

sub _patch_develpatchperlversion {
  return if -d '.git';
  my $dpv = $Devel::PatchPerl::VERSION || "(unreleased)";
  _patch(<<"END");
diff --git a/Configure b/Configure
index e12c8bb..1a8088f 100755
--- Configure
+++ Configure
@@ -25151,6 +25151,8 @@ zcat='\$zcat'
 zip='\$zip'
 EOT
 
+echo "BuiltWithPatchPerl='$dpv'" >>config.sh
+
 : add special variables
 \$test -f \$src/patchlevel.h && \
 awk '/^#define[ 	]+PERL_/ {printf "\%s=\%s\\n",\$2,\$3}' \$src/patchlevel.h >>config.sh
END
}

sub _patch_conf_fwrapv {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num < 5.019011;
  _patch(<<'FWRAPV');
diff --git a/Configure b/Configure
index 15b3da1769..791889a2ab 100755
--- Configure
+++ Configure
@@ -4643,6 +4643,22 @@ case "$gccversion" in
     $rm -f try try.*
 esac
 
+# gcc 4.9 by default does some optimizations that break perl.
+# see ticket 121505.
+#
+# The -fwrapv disables those optimizations (and probably others,) so
+# for gcc 4.9 (and later, since the optimizations probably won't go
+# away), add -fwrapv unless the user requests -fno-wrapv, which
+# disables -fwrapv, or if the user requests -fsanitize=undefined,
+# which turns the overflows -fwrapv ignores into runtime errors.
+case "$gccversion" in
+4.[3-9].*|4.[1-9][0-9]*|[5-9].*|[1-9][0-9]*)
+    case "$ccflags" in
+    *-fno-wrapv*|*-fsanitize=undefined*|*-fwrapv*) ;;
+    *) ccflags="$ccflags -fwrapv" ;;
+    esac
+esac
+
 : What should the include directory be ?
 : Use sysroot if set, so findhdr looks in the right place.
 echo " "
FWRAPV
}

sub _patch_utils_h2ph {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num < 5.021009;
  return if    $num == 5.020003;
  if ( $num < 5.006001 ) {
    return _patch_b64(<<'UH2PH560');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTM2LDEzICszNiwyMSBAQCAk
Q29uZmlne3N0YXJ0cGVybH0KIAogcHJpbnQgT1VUIDw8JyFOTyFTVUJTISc7CiAKK3VzZSBzdHJp
Y3Q7CisKIHVzZSBDb25maWc7CiB1c2UgRmlsZTo6UGF0aCBxdyhta3BhdGgpOwogdXNlIEdldG9w
dDo6U3RkOwogCi1nZXRvcHRzKCdEZDpybGhhUScpOworIyBNYWtlIHN1cmUgcmVhZCBwZXJtaXNz
aW9ucyBmb3IgYWxsIGFyZSBzZXQ6CitpZiAoZGVmaW5lZCB1bWFzayAmJiAodW1hc2soKSAmIDA0
NDQpKSB7CisgICAgdW1hc2sgKHVtYXNrKCkgJiB+MDQ0NCk7Cit9CisKK2dldG9wdHMoJ0RkOnJs
aGFRZScpOwordXNlIHZhcnMgcXcoJG9wdF9EICRvcHRfZCAkb3B0X3IgJG9wdF9sICRvcHRfaCAk
b3B0X2EgJG9wdF9RICRvcHRfZSk7CiBkaWUgIi1yIGFuZCAtYSBvcHRpb25zIGFyZSBtdXR1YWxs
eSBleGNsdXNpdmVcbiIgaWYgKCRvcHRfciBhbmQgJG9wdF9hKTsKLUBpbmNfZGlycyA9IGluY19k
aXJzKCkgaWYgJG9wdF9hOworbXkgQGluY19kaXJzID0gaW5jX2RpcnMoKSBpZiAkb3B0X2E7CiAK
IG15ICRFeGl0ID0gMDsKIApAQCAtNTAsNyArNTgsNyBAQCBteSAkRGVzdF9kaXIgPSAkb3B0X2Qg
fHwgJENvbmZpZ3tpbnN0YWxsc2l0ZWFyY2h9OwogZGllICJEZXN0aW5hdGlvbiBkaXJlY3Rvcnkg
JERlc3RfZGlyIGRvZXNuJ3QgZXhpc3Qgb3IgaXNuJ3QgYSBkaXJlY3RvcnlcbiIKICAgICB1bmxl
c3MgLWQgJERlc3RfZGlyOwogCi1AaXNhdHlwZSA9IHNwbGl0KCcgJyw8PEVORCk7CitteSBAaXNh
dHlwZSA9IHNwbGl0KCcgJyw8PEVORCk7CiAJY2hhcgl1Y2hhcgl1X2NoYXIKIAlzaG9ydAl1c2hv
cnQJdV9zaG9ydAogCWludAl1aW50CXVfaW50CkBAIC01OCwxNCArNjYsMjYgQEAgZGllICJEZXN0
aW5hdGlvbiBkaXJlY3RvcnkgJERlc3RfZGlyIGRvZXNuJ3QgZXhpc3Qgb3IgaXNuJ3QgYSBkaXJl
Y3RvcnlcbiIKIAlGSUxFCWtleV90CWNhZGRyX3QKIEVORAogCitteSAlaXNhdHlwZTsKIEBpc2F0
eXBle0Bpc2F0eXBlfSA9ICgxKSB4IEBpc2F0eXBlOwotJGluaWYgPSAwOworbXkgJGluaWYgPSAw
OworbXkgJUlzX2NvbnZlcnRlZDsKK215ICViYWRfZmlsZSA9ICgpOwogCiBAQVJHViA9ICgnLScp
IHVubGVzcyBAQVJHVjsKIAogYnVpbGRfcHJlYW1ibGVfaWZfbmVjZXNzYXJ5KCk7CiAKLXdoaWxl
IChkZWZpbmVkICgkZmlsZSA9IG5leHRfZmlsZSgpKSkgeworc3ViIHJlaW5kZW50KCQpIHsKKyAg
ICBteSgkdGV4dCkgPSBzaGlmdDsKKyAgICAkdGV4dCA9fiBzL1xuL1xuICAgIC9nOworICAgICR0
ZXh0ID1+IHMvICAgICAgICAvXHQvZzsKKyAgICAkdGV4dDsKK30KKworbXkgKCR0LCAkdGFiLCAl
Y3VyYXJncywgJG5ldywgJGV2YWxfaW5kZXgsICRkaXIsICRuYW1lLCAkYXJncywgJG91dGZpbGUp
OworbXkgKCRpbmNsLCAkaW5jbF90eXBlLCAkaW5jbF9xdW90ZSwgJG5leHQpOword2hpbGUgKGRl
ZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxlKCkpKSB7CiAgICAgaWYgKC1sICRmaWxlIGFuZCAt
ZCAkZmlsZSkgewogICAgICAgICBsaW5rX2lmX3Bvc3NpYmxlKCRmaWxlKSBpZiAoJG9wdF9sKTsK
ICAgICAgICAgbmV4dDsKQEAgLTEwMCwzNiArMTIwLDIzIEBAIHdoaWxlIChkZWZpbmVkICgkZmls
ZSA9IG5leHRfZmlsZSgpKSkgewogCW9wZW4oT1VULCI+JERlc3RfZGlyLyRvdXRmaWxlIikgfHwg
ZGllICJDYW4ndCBjcmVhdGUgJG91dGZpbGU6ICQhXG4iOwogICAgIH0KIAotICAgIHByaW50IE9V
VCAicmVxdWlyZSAnX2gycGhfcHJlLnBoJztcblxuIjsKLSAgICB3aGlsZSAoPElOPikgewotCWNo
b3A7Ci0Jd2hpbGUgKC9cXCQvKSB7Ci0JICAgIGNob3A7Ci0JICAgICRfIC49IDxJTj47Ci0JICAg
IGNob3A7Ci0JfQotCXByaW50IE9VVCAiIyAkX1xuIiBpZiAkb3B0X0Q7Ci0KLQlpZiAoczovXCo6
XDIwMDpnKSB7Ci0JICAgIHM6XCovOlwyMDE6ZzsKLQkgICAgcy9cMjAwW15cMjAxXSpcMjAxLy9n
OwkjIGRlbGV0ZSBzaW5nbGUgbGluZSBjb21tZW50cwotCSAgICBpZiAocy9cMjAwLiovLykgewkJ
IyBiZWdpbiBtdWx0aS1saW5lIGNvbW1lbnQ/Ci0JCSRfIC49ICcvKic7Ci0JCSRfIC49IDxJTj47
Ci0JCXJlZG87Ci0JICAgIH0KLQl9CisgICAgcHJpbnQgT1VUCisgICAgICAgICJyZXF1aXJlICdf
aDJwaF9wcmUucGgnO1xuXG4iLAorICAgICAgICAibm8gd2FybmluZ3MgJ3JlZGVmaW5lJztcblxu
IjsKKworICAgIHdoaWxlIChkZWZpbmVkIChsb2NhbCAkXyA9IG5leHRfbGluZSgkZmlsZSkpKSB7
CiAJaWYgKHMvXlxzKlwjXHMqLy8pIHsKIAkgICAgaWYgKHMvXmRlZmluZVxzKyhcdyspLy8pIHsK
IAkJJG5hbWUgPSAkMTsKIAkJJG5ldyA9ICcnOwogCQlzL1xzKyQvLzsKKwkJcy9cKFx3K1xzKlwo
XCpcKVxzKlwoXHcqXClcKVxzKigtP1xkKykvJDEvOyAjIChpbnQgKCopKGZvb190KSkwCiAJCWlm
IChzL15cKChbXHcsXHNdKilcKS8vKSB7CiAJCSAgICAkYXJncyA9ICQxOwogICAgIAkgICAgCSAg
ICBteSAkcHJvdG8gPSAnKCkgJzsKIAkJICAgIGlmICgkYXJncyBuZSAnJykgewogICAgIAkgICAg
CSAgICAJJHByb3RvID0gJyc7Ci0JCQlmb3JlYWNoICRhcmcgKHNwbGl0KC8sXHMqLywkYXJncykp
IHsKKwkJCWZvcmVhY2ggbXkgJGFyZyAoc3BsaXQoLyxccyovLCRhcmdzKSkgewogCQkJICAgICRh
cmcgPX4gcy9eXHMqKFteXHNdLipbXlxzXSlccyokLyQxLzsKIAkJCSAgICAkY3VyYXJnc3skYXJn
fSA9IDE7CiAJCQl9CkBAIC0xNzcsMjIgKzE4NCwzMiBAQCB3aGlsZSAoZGVmaW5lZCAoJGZpbGUg
PSBuZXh0X2ZpbGUoKSkpIHsKICAgICAgICAgICAgICAgICAgICAgICBwcmludCBPVVQgJHQsInVu
bGVzcyhkZWZpbmVkKFwmJG5hbWUpKSB7XG4gICAgc3ViICRuYW1lICgpIHtcdCIsJG5ldywiO31c
bn1cbiI7CiAJCSAgICB9CiAJCX0KLQkgICAgfSBlbHNpZiAoL14oaW5jbHVkZXxpbXBvcnQpXHMq
WzwiXSguKilbPiJdLykgewotCQkoJGluY2wgPSAkMikgPX4gcy9cLmgkLy5waC87Ci0JCXByaW50
IE9VVCAkdCwicmVxdWlyZSAnJGluY2wnO1xuIjsKLQkgICAgfSBlbHNpZigvXmluY2x1ZGVfbmV4
dFxzKls8Il0oLiopWz4iXS8pIHsKLQkJKCRpbmNsID0gJDEpID1+IHMvXC5oJC8ucGgvOworCSAg
ICB9IGVsc2lmICgvXihpbmNsdWRlfGltcG9ydHxpbmNsdWRlX25leHQpXHMqKFs8XCJdKSguKilb
PlwiXS8pIHsKKyAgICAgICAgICAgICAgICAkaW5jbF90eXBlID0gJDE7CisgICAgICAgICAgICAg
ICAgJGluY2xfcXVvdGUgPSAkMjsKKyAgICAgICAgICAgICAgICAkaW5jbCA9ICQzOworICAgICAg
ICAgICAgICAgIGlmICgoJGluY2xfdHlwZSBlcSAnaW5jbHVkZV9uZXh0JykgfHwKKyAgICAgICAg
ICAgICAgICAgICAgKCRvcHRfZSAmJiBleGlzdHMoJGJhZF9maWxleyRpbmNsfSkpKSB7CisgICAg
ICAgICAgICAgICAgICAgICRpbmNsID1+IHMvXC5oJC8ucGgvOwogCQlwcmludCBPVVQgKCR0LAog
CQkJICAgImV2YWwge1xuIik7CiAgICAgICAgICAgICAgICAgJHRhYiArPSA0OwogICAgICAgICAg
ICAgICAgICR0ID0gIlx0IiB4ICgkdGFiIC8gOCkgLiAnICcgeCAoJHRhYiAlIDgpOworICAgICAg
ICAgICAgICAgICAgICBwcmludCBPVVQgKCR0LCAibXkoXEBSRU0pO1xuIik7CisgICAgICAgICAg
ICAgICAgICAgIGlmICgkaW5jbF90eXBlIGVxICdpbmNsdWRlX25leHQnKSB7CiAJCXByaW50IE9V
VCAoJHQsCiAJCQkgICAibXkoXCVJTkNEKSA9IG1hcCB7IFwkSU5De1wkX30gPT4gMSB9ICIsCi0J
CQkgICAiKGdyZXAgeyBcJF8gZXEgXCIkaW5jbFwiIH0ga2V5cyhcJUlOQykpO1xuIik7CisJCQkg
ICAgICAgICAgICIoZ3JlcCB7IFwkXyBlcSBcIiRpbmNsXCIgfSAiLAorICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAia2V5cyhcJUlOQykpO1xuIik7CiAJCXByaW50IE9VVCAoJHQs
Ci0JCQkgICAibXkoXEBSRU0pID0gbWFwIHsgXCJcJF8vJGluY2xcIiB9ICIsCisJCQkgICAgICAg
ICAgICJcQFJFTSA9IG1hcCB7IFwiXCRfLyRpbmNsXCIgfSAiLAogCQkJICAgIihncmVwIHsgbm90
IGV4aXN0cyhcJElOQ0R7XCJcJF8vJGluY2xcIn0pIiwKLQkJCSAgICJhbmQgLWYgXCJcJF8vJGlu
Y2xcIiB9IFxASU5DKTtcbiIpOworCQkJICAgICAgICAgICAiIGFuZCAtZiBcIlwkXy8kaW5jbFwi
IH0gXEBJTkMpO1xuIik7CisgICAgICAgICAgICAgICAgICAgIH0gZWxzZSB7CisgICAgICAgICAg
ICAgICAgICAgICAgICBwcmludCBPVVQgKCR0LAorICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAiXEBSRU0gPSBtYXAgeyBcIlwkXy8kaW5jbFwiIH0gIiwKKyAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgIihncmVwIHstciBcIlwkXy8kaW5jbFwiIH0gXEBJTkMpO1xu
Iik7CisgICAgICAgICAgICAgICAgICAgIH0KIAkJcHJpbnQgT1VUICgkdCwKIAkJCSAgICJyZXF1
aXJlIFwiXCRSRU1bMF1cIiBpZiBcQFJFTTtcbiIpOwogICAgICAgICAgICAgICAgICR0YWIgLT0g
NDsKQEAgLTIwMSw2ICsyMTgsMTQgQEAgd2hpbGUgKGRlZmluZWQgKCRmaWxlID0gbmV4dF9maWxl
KCkpKSB7CiAJCQkgICAifTtcbiIpOwogCQlwcmludCBPVVQgKCR0LAogCQkJICAgIndhcm4oXCRc
QCkgaWYgXCRcQDtcbiIpOworICAgICAgICAgICAgICAgIH0gZWxzZSB7CisgICAgICAgICAgICAg
ICAgICAgICRpbmNsID1+IHMvXC5oJC8ucGgvOworICAgICAgICAgICAgICAgICAgICAjIGNvcHkg
dGhlIHByZWZpeCBpbiB0aGUgcXVvdGUgc3ludGF4ICgjaW5jbHVkZSAieC5oIikgY2FzZQorICAg
ICAgICAgICAgICAgICAgICBpZiAoJGluY2wgIX4gbXwvfCAmJiAkaW5jbF9xdW90ZSBlcSBxeyJ9
ICYmICRmaWxlID1+IG18XiguKikvfCkgeworICAgICAgICAgICAgICAgICAgICAgICAgJGluY2wg
PSAiJDEvJGluY2wiOworICAgICAgICAgICAgICAgICAgICB9CisJCSAgICBwcmludCBPVVQgJHQs
InJlcXVpcmUgJyRpbmNsJztcbiI7CisgICAgICAgICAgICAgICAgfQogCSAgICB9IGVsc2lmICgv
XmlmZGVmXHMrKFx3KykvKSB7CiAJCXByaW50IE9VVCAkdCwiaWYoZGVmaW5lZCgmJDEpKSB7XG4i
OwogCQkkdGFiICs9IDQ7CkBAIC0yNDgsMjAgKzI3MywyNCBAQCB3aGlsZSAoZGVmaW5lZCAoJGZp
bGUgPSBuZXh0X2ZpbGUoKSkpIHsKIAkgICAgfSBlbHNpZigvXmlkZW50XHMrKC4qKS8pIHsKIAkJ
cHJpbnQgT1VUICR0LCAiIyAkMVxuIjsKIAkgICAgfQotIAl9IGVsc2lmKC9eXHMqKHR5cGVkZWZc
cyopP2VudW1ccyooXHMrW2EtekEtWl9dXHcqXHMqKT9cey8pIHsKLQkgICAgdW50aWwoL1x9Lio/
Oy8pIHsKLQkJY2hvbXAoJG5leHQgPSA8SU4+KTsKKwl9IGVsc2lmKC9eXHMqKHR5cGVkZWZccyop
P2VudW1ccyooXHMrW2EtekEtWl9dXHcqXHMqKT8vKSB7CisJICAgIHVudGlsKC9ce1tefV0qXH0u
KjsvIHx8IC87LykgeworCQlsYXN0IHVubGVzcyBkZWZpbmVkICgkbmV4dCA9IG5leHRfbGluZSgk
ZmlsZSkpOworCQljaG9tcCAkbmV4dDsKKwkJIyBkcm9wICIjZGVmaW5lIEZPTyBGT08iIGluIGVu
dW1zCisJCSRuZXh0ID1+IHMvXlxzKiNccypkZWZpbmVccysoXHcrKVxzK1wxXHMqJC8vOwogCQkk
XyAuPSAkbmV4dDsKIAkJcHJpbnQgT1VUICIjICRuZXh0XG4iIGlmICRvcHRfRDsKIAkgICAgfQor
CSAgICBzLyNccyppZi4qPyNccyplbmRpZi8vZzsgIyBkcm9wICNpZmRlZnMKIAkgICAgc0AvXCou
Kj9cKi9AQGc7CiAJICAgIHMvXHMrLyAvZzsKLQkgICAgL15ccz8odHlwZWRlZlxzPyk/ZW51bVxz
PyhbYS16QS1aX11cdyopP1xzP1x7KC4qKVx9XHM/KFthLXpBLVpfXVx3Kik/XHM/Oy87Ci0JICAg
ICgkZW51bV9zdWJzID0gJDMpID1+IHMvXHMvL2c7Ci0JICAgIEBlbnVtX3N1YnMgPSBzcGxpdCgv
LC8sICRlbnVtX3N1YnMpOwotCSAgICAkZW51bV92YWwgPSAtMTsKLQkgICAgZm9yICRlbnVtIChA
ZW51bV9zdWJzKSB7Ci0JCSgkZW51bV9uYW1lLCAkZW51bV92YWx1ZSkgPSAkZW51bSA9fiAvXihb
YS16QS1aX11cdyopKD0uKyk/JC87CisJICAgIG5leHQgdW5sZXNzIC9eXHM/KHR5cGVkZWZccz8p
P2VudW1ccz8oW2EtekEtWl9dXHcqKT9ccz9ceyguKilcfVxzPyhbYS16QS1aX11cdyopP1xzPzsv
OworCSAgICAobXkgJGVudW1fc3VicyA9ICQzKSA9fiBzL1xzLy9nOworCSAgICBteSBAZW51bV9z
dWJzID0gc3BsaXQoLywvLCAkZW51bV9zdWJzKTsKKwkgICAgbXkgJGVudW1fdmFsID0gLTE7CisJ
ICAgIGZvcmVhY2ggbXkgJGVudW0gKEBlbnVtX3N1YnMpIHsKKwkJbXkgKCRlbnVtX25hbWUsICRl
bnVtX3ZhbHVlKSA9ICRlbnVtID1+IC9eKFthLXpBLVpfXVx3KikoPS4rKT8kLzsKIAkJJGVudW1f
dmFsdWUgPX4gcy9ePS8vOwogCQkkZW51bV92YWwgPSAobGVuZ3RoKCRlbnVtX3ZhbHVlKSA/ICRl
bnVtX3ZhbHVlIDogJGVudW1fdmFsICsgMSk7CiAJCWlmICgkb3B0X2gpIHsKQEAgLTI3OCwzMSAr
MzA3LDQ3IEBAIHdoaWxlIChkZWZpbmVkICgkZmlsZSA9IG5leHRfZmlsZSgpKSkgewogCSAgICB9
CiAJfQogICAgIH0KLSAgICBwcmludCBPVVQgIjE7XG4iOwotCi0gICAgJGlzX2NvbnZlcnRlZHsk
ZmlsZX0gPSAxOworICAgICRJc19jb252ZXJ0ZWR7JGZpbGV9ID0gMTsKKyAgICBpZiAoJG9wdF9l
ICYmIGV4aXN0cygkYmFkX2ZpbGV7JGZpbGV9KSkgeworICAgICAgICB1bmxpbmsoJERlc3RfZGly
IC4gJy8nIC4gJG91dGZpbGUpOworICAgICAgICAkbmV4dCA9ICcnOworICAgIH0gZWxzZSB7Cisg
ICAgICAgIHByaW50IE9VVCAiMTtcbiI7CiAgICAgcXVldWVfaW5jbHVkZXNfZnJvbSgkZmlsZSkg
aWYgKCRvcHRfYSk7CisgICAgfQogfQogCi1leGl0ICRFeGl0OwotCi1zdWIgcmVpbmRlbnQoJCkg
ewotICAgIG15KCR0ZXh0KSA9IHNoaWZ0OwotICAgICR0ZXh0ID1+IHMvXG4vXG4gICAgL2c7Ci0g
ICAgJHRleHQgPX4gcy8gICAgICAgIC9cdC9nOwotICAgICR0ZXh0OworaWYgKCRvcHRfZSAmJiAo
c2NhbGFyKGtleXMgJWJhZF9maWxlKSA+IDApKSB7CisgICAgd2FybiAiV2FzIHVuYWJsZSB0byBj
b252ZXJ0IHRoZSBmb2xsb3dpbmcgZmlsZXM6XG4iOworICAgIHdhcm4gIlx0IiAuIGpvaW4oIlxu
XHQiLHNvcnQoa2V5cyAlYmFkX2ZpbGUpKSAuICJcbiI7CiB9CiAKK2V4aXQgJEV4aXQ7CisKIHN1
YiBleHByIHsKKyAgICBteSAkam9pbmVkX2FyZ3M7CiAgICAgaWYoa2V5cyglY3VyYXJncykpIHsK
LQlteSgkam9pbmVkX2FyZ3MpID0gam9pbignfCcsIGtleXMoJWN1cmFyZ3MpKTsKKwkkam9pbmVk
X2FyZ3MgPSBqb2luKCd8Jywga2V5cyglY3VyYXJncykpOwogICAgIH0KICAgICB3aGlsZSAoJF8g
bmUgJycpIHsKIAlzL15cJlwmLy8gJiYgZG8geyAkbmV3IC49ICIgJiYiOyBuZXh0O307ICMgaGFu
ZGxlICYmIG9wZXJhdG9yCiAJcy9eXCYoW1woYS16XCldKykvJDEvaTsJIyBoYWNrIGZvciB0aGlu
Z3MgdGhhdCB0YWtlIHRoZSBhZGRyZXNzIG9mCiAJcy9eKFxzKykvLwkJJiYgZG8geyRuZXcgLj0g
JyAnOyBuZXh0O307Ci0Jcy9eKDBYWzAtOUEtRl0rKVtVTF0qLy9pCSYmIGRvIHskbmV3IC49IGxj
KCQxKTsgbmV4dDt9OwotCXMvXigtP1xkK1wuXGQrRVstK11cZCspRj8vL2kJJiYgZG8geyRuZXcg
Lj0gJDE7IG5leHQ7fTsKKwlzL14wWChbMC05QS1GXSspW1VMXSovL2kgCisJICAgICYmIGRvIHtt
eSAkaGV4ID0gJDE7CisJCSAgICRoZXggPX4gcy9eMCsvLzsKKwkJICAgaWYgKGxlbmd0aCAkaGV4
ID4gOCAmJiAhJENvbmZpZ3t1c2U2NGJpdGludH0pIHsKKwkJICAgICAgICMgQ3JvYWsgaWYgbnZf
cHJlc2VydmVzX3V2X2JpdHMgPCA2NCA/CisJCSAgICAgICAkbmV3IC49ICAgICAgICAgaGV4KHN1
YnN0cigkaGV4LCAtOCkpICsKKwkJCSAgICAgICAyKiozMiAqIGhleChzdWJzdHIoJGhleCwgIDAs
IC04KSk7CisJCSAgICAgICAjIFRoZSBhYm92ZSB3aWxsIHByb2R1Y2UgImVycm9ybmV1cyIgY29k
ZQorCQkgICAgICAgIyBpZiB0aGUgaGV4IGNvbnN0YW50IHdhcyBlLmcuIGluc2lkZSBVSU5UNjRf
QworCQkgICAgICAgIyBtYWNybywgYnV0IHRoZW4gYWdhaW4sIGgycGggaXMgYW4gYXBwcm94aW1h
dGlvbi4KKwkJICAgfSBlbHNlIHsKKwkJICAgICAgICRuZXcgLj0gbGMoIjB4JGhleCIpOworCQkg
ICB9CisJCSAgIG5leHQ7fTsKKwlzL14oLT9cZCtcLlxkK0VbLStdP1xkKylbRkxdPy8vaQkmJiBk
byB7JG5ldyAuPSAkMTsgbmV4dDt9OwogCXMvXihcZCspXHMqW0xVXSovL2kJJiYgZG8geyRuZXcg
Lj0gJDE7IG5leHQ7fTsKIAlzL14oIihcXCJ8W14iXSkqIikvLwkmJiBkbyB7JG5ldyAuPSAkMTsg
bmV4dDt9OwogCXMvXicoKFxcInxbXiJdKSopJy8vCSYmIGRvIHsKQEAgLTM0MSwxMyArMzg2LDEz
IEBAIHN1YiBleHByIHsKIAkjIEVsaW1pbmF0ZSB0eXBlZGVmcwogCS9cKChbXHdcc10rKVtcKlxz
XSpcKVxzKltcd1woXS8gJiYgZG8gewogCSAgICBmb3JlYWNoIChzcGxpdCAvXHMrLywgJDEpIHsg
ICMgTWFrZSBzdXJlIGFsbCB0aGUgd29yZHMgYXJlIHR5cGVzLAotCQlsYXN0IHVubGVzcyAoJGlz
YXR5cGV7JF99IG9yICRfIGVxICdzdHJ1Y3QnKTsKKwkJbGFzdCB1bmxlc3MgKCRpc2F0eXBleyRf
fSBvciAkXyBlcSAnc3RydWN0JyBvciAkXyBlcSAndW5pb24nKTsKIAkgICAgfQogCSAgICBzL1wo
W1x3XHNdK1tcKlxzXSpcKS8vICYmIG5leHQ7ICAgICAgIyB0aGVuIGVsaW1pbmF0ZSB0aGVtLgog
CX07CiAJIyBzdHJ1Y3QvdW5pb24gbWVtYmVyLCBpbmNsdWRpbmcgYXJyYXlzOgogCXMvXihbX0Et
Wl1cdyooXFtbXlxdXStcXSk/KChcLnwtPilbX0EtWl1cdyooXFtbXlxdXStcXSk/KSspLy9pICYm
IGRvIHsKLQkgICAgJGlkID0gJDE7CisJICAgIG15ICRpZCA9ICQxOwogCSAgICAkaWQgPX4gcy8o
XC58KC0+KSkoW15cLlwtXSopLy0+XHskM1x9L2c7CiAJICAgICRpZCA9fiBzL1xiKFteXCRdKSgk
am9pbmVkX2FyZ3MpLyQxXCQkMi9nIGlmIGxlbmd0aCgkam9pbmVkX2FyZ3MpOwogCSAgICB3aGls
ZSgkaWQgPX4gL1xbXHMqKFteXCRcJlxkXF1dKylcXS8pIHsKQEAgLTM2Myw4ICs0MDgsOCBAQCBz
dWIgZXhwciB7CiAJICAgICRuZXcgLj0gIiAoXCQkaWQpIjsKIAl9OwogCXMvXihbX2EtekEtWl1c
dyopLy8JJiYgZG8gewotCSAgICAkaWQgPSAkMTsKLQkgICAgaWYgKCRpZCBlcSAnc3RydWN0Jykg
eworCSAgICBteSAkaWQgPSAkMTsKKwkgICAgaWYgKCRpZCBlcSAnc3RydWN0JyB8fCAkaWQgZXEg
J3VuaW9uJykgewogCQlzL15ccysoXHcrKS8vOwogCQkkaWQgLj0gJyAnIC4gJDE7CiAJCSRpc2F0
eXBleyRpZH0gPSAxOwpAQCAtMzc3LDggKzQyMiw4IEBAIHN1YiBleHByIHsKIAkJJG5ldyAuPSAn
LT4nIGlmIC9eW1xbXHtdLzsKIAkgICAgfSBlbHNpZiAoJGlkIGVxICdkZWZpbmVkJykgewogCQkk
bmV3IC49ICdkZWZpbmVkJzsKLQkgICAgfSBlbHNpZiAoL15cKC8pIHsKLQkJcy9eXCgoXHcpLC8o
IiQxIiwvIGlmICRpZCA9fiAvXl9JT1tXUl0qJC9pOwkjIGNoZWF0CisJICAgIH0gZWxzaWYgKC9e
XHMqXCgvKSB7CisJCXMvXlxzKlwoKFx3KSwvKCIkMSIsLyBpZiAkaWQgPX4gL15fSU9bV1JdKiQv
aTsJIyBjaGVhdAogCQkkbmV3IC49ICIgJiRpZCI7CiAJICAgIH0gZWxzaWYgKCRpc2F0eXBleyRp
ZH0pIHsKIAkJaWYgKCRuZXcgPX4gL3tccyokLykgewpAQCAtMzkxLDcgKzQzNiw3IEBAIHN1YiBl
eHByIHsKIAkJfQogCSAgICB9IGVsc2UgewogCQlpZiAoJGluaWYgJiYgJG5ldyAhfiAvZGVmaW5l
ZFxzKlwoJC8pIHsKLQkJICAgICRuZXcgLj0gJyhkZWZpbmVkKCYnIC4gJGlkIC4gJykgPyAmJyAu
ICRpZCAuICcgOiAwKSc7CisJCSAgICAkbmV3IC49ICcoZGVmaW5lZCgmJyAuICRpZCAuICcpID8g
JicgLiAkaWQgLiAnIDogdW5kZWYpJzsKIAkJfSBlbHNpZiAoL15cWy8pIHsKIAkJICAgICRuZXcg
Lj0gIiBcJCRpZCI7CiAJCX0gZWxzZSB7CkBAIC00MDUsNiArNDUwLDEwMSBAQCBzdWIgZXhwciB7
CiB9CiAKIAorc3ViIG5leHRfbGluZQoreworICAgIG15ICRmaWxlID0gc2hpZnQ7CisgICAgbXkg
KCRpbiwgJG91dCk7CisgICAgbXkgJHByZV9zdWJfdHJpX2dyYXBocyA9IDE7CisKKyAgICBSRUFE
OiB3aGlsZSAobm90IGVvZiBJTikgeworICAgICAgICAkaW4gIC49IDxJTj47CisgICAgICAgIGNo
b21wICRpbjsKKyAgICAgICAgbmV4dCB1bmxlc3MgbGVuZ3RoICRpbjsKKworICAgICAgICB3aGls
ZSAobGVuZ3RoICRpbikgeworICAgICAgICAgICAgaWYgKCRwcmVfc3ViX3RyaV9ncmFwaHMpIHsK
KyAgICAgICAgICAgICAgICAjIFByZXByb2Nlc3MgYWxsIHRyaS1ncmFwaHMgCisgICAgICAgICAg
ICAgICAgIyBpbmNsdWRpbmcgdGhpbmdzIHN0dWNrIGluIHF1b3RlZCBzdHJpbmcgY29uc3RhbnRz
LgorICAgICAgICAgICAgICAgICRpbiA9fiBzL1w/XD89LyMvZzsgICAgICAgICAgICAgICAgICAg
ICAgICAgIyB8ID8/PXwgICN8CisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cP1whL3wvZzsg
ICAgICAgICAgICAgICAgICAgICAgICAjIHwgPz8hfCAgfHwKKyAgICAgICAgICAgICAgICAkaW4g
PX4gcy9cP1w/Jy9eL2c7ICAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/Pyd8ICBefAorICAg
ICAgICAgICAgICAgICRpbiA9fiBzL1w/XD9cKC9bL2c7ICAgICAgICAgICAgICAgICAgICAgICAg
IyB8ID8/KHwgIFt8CisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cP1wpL10vZzsgICAgICAg
ICAgICAgICAgICAgICAgICAjIHwgPz8pfCAgXXwKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9c
P1w/XC0vfi9nOyAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/Py18ICB+fAorICAgICAgICAg
ICAgICAgICRpbiA9fiBzL1w/XD9cLy9cXC9nOyAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/
L3wgIFx8CisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cPzwvey9nOyAgICAgICAgICAgICAg
ICAgICAgICAgICAjIHwgPz88fCAge3wKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9cP1w/Pi99
L2c7ICAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/Pz58ICB9fAorICAgICAgICAgICAgfQor
CSAgICBpZiAoJGluID1+IC9eXCNpZmRlZiBfX0xBTkdVQUdFX1BBU0NBTF9fLykgeworICAgICAg
ICAgICAgICAgICMgVHJ1NjQgZGlzYXNzZW1ibGVyLmggZXZpbG5lc3M6IG1peGVkIEMgYW5kIFBh
c2NhbC4KKwkJd2hpbGUgKDxJTj4pIHsKKwkJICAgIGxhc3QgaWYgL15cI2VuZGlmLzsgCisJCX0K
KwkJbmV4dCBSRUFEOworCSAgICB9CisJICAgIGlmICgkaW4gPX4gL15leHRlcm4gaW5saW5lIC8g
JiYgIyBJbmxpbmVkIGFzc2VtYmxlci4KKwkJJF5PIGVxICdsaW51eCcgJiYgJGZpbGUgPX4gbSEo
PzpefC8pYXNtL1teL10rXC5oJCEpIHsKKyAJCXdoaWxlICg8SU4+KSB7CisJCSAgICBsYXN0IGlm
IC9efS87IAorCQl9CisJCW5leHQgUkVBRDsKKwkgICAgfQorICAgICAgICAgICAgaWYgKCRpbiA9
fiBzL1xcJC8vKSB7ICAgICAgICAgICAgICAgICAgICAgICAgICAgIyBcLW5ld2xpbmUKKyAgICAg
ICAgICAgICAgICAkb3V0ICAgIC49ICcgJzsKKyAgICAgICAgICAgICAgICBuZXh0IFJFQUQ7Cisg
ICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKFteIidcXFwvXSspLy8pIHsgICAgICAgICAg
ICAjIFBhc3N0aHJvdWdoCisgICAgICAgICAgICAgICAgJG91dCAgICAuPSAkMTsKKyAgICAgICAg
ICAgIH0gZWxzaWYgKCRpbiA9fiBzL14oXFwuKS8vKSB7ICAgICAgICAgICAgICAgICAgICMgXC4u
LgorICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7CisgICAgICAgICAgICB9IGVsc2lmICgk
aW4gPX4gL14nLykgeyAgICAgICAgICAgICAgICAgICAgICAgICAjICcuLi4KKyAgICAgICAgICAg
ICAgICBpZiAoJGluID1+IHMvXignKFxcLnxbXidcXF0pKicpLy8pIHsKKyAgICAgICAgICAgICAg
ICAgICAgJG91dCAgICAuPSAkMTsKKyAgICAgICAgICAgICAgICB9IGVsc2UgeworICAgICAgICAg
ICAgICAgICAgICBuZXh0IFJFQUQ7CisgICAgICAgICAgICAgICAgfQorICAgICAgICAgICAgfSBl
bHNpZiAoJGluID1+IC9eIi8pIHsgICAgICAgICAgICAgICAgICAgICAgICAgIyAiLi4uCisgICAg
ICAgICAgICAgICAgaWYgKCRpbiA9fiBzL14oIihcXC58W14iXFxdKSoiKS8vKSB7CisgICAgICAg
ICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7CisgICAgICAgICAgICAgICAgfSBlbHNlIHsKKyAg
ICAgICAgICAgICAgICAgICAgbmV4dCBSRUFEOworICAgICAgICAgICAgICAgIH0KKyAgICAgICAg
ICAgIH0gZWxzaWYgKCRpbiA9fiBzL15cL1wvLiovLykgeyAgICAgICAgICAgICAgICAgICMgLy8u
Li4KKyAgICAgICAgICAgICAgICAjIGZhbGwgdGhyb3VnaAorICAgICAgICAgICAgfSBlbHNpZiAo
JGluID1+IG0vXlwvXCovKSB7ICAgICAgICAgICAgICAgICAgICAgIyAvKi4uLgorICAgICAgICAg
ICAgICAgICMgQyBjb21tZW50IHJlbW92YWwgYWRhcHRlZCBmcm9tIHBlcmxmYXE2OgorICAgICAg
ICAgICAgICAgIGlmICgkaW4gPX4gcy9eXC9cKlteKl0qXCorKFteXC8qXVteKl0qXCorKSpcLy8v
KSB7CisgICAgICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJyAnOworICAgICAgICAgICAgICAg
IH0gZWxzZSB7ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIyBJbmNvbXBsZXRl
IC8qICovCisgICAgICAgICAgICAgICAgICAgIG5leHQgUkVBRDsKKyAgICAgICAgICAgICAgICB9
CisgICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKFwvKS8vKSB7ICAgICAgICAgICAgICAg
ICAgICAjIC8uLi4KKyAgICAgICAgICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAg
fSBlbHNpZiAoJGluID1+IHMvXihbXlwnXCJcXFwvXSspLy8pIHsKKyAgICAgICAgICAgICAgICAk
b3V0ICAgIC49ICQxOworICAgICAgICAgICAgfSBlbHNpZiAoJF5PIGVxICdsaW51eCcgJiYKKyAg
ICAgICAgICAgICAgICAgICAgICRmaWxlID1+IG0hKD86XnwvKWxpbnV4L2J5dGVvcmRlci9wZHBf
ZW5kaWFuXC5oJCEgJiYKKyAgICAgICAgICAgICAgICAgICAgICRpbiAgID1+IHMhXCdUIEtOT1ch
ISkgeworICAgICAgICAgICAgICAgICRvdXQgICAgPX4gcyFJIERPTiQhSV9ET19OT1RfS05PVyE7
CisgICAgICAgICAgICB9IGVsc2UgeworICAgICAgICAgICAgICAgIGlmICgkb3B0X2UpIHsKKyAg
ICAgICAgICAgICAgICAgICAgd2FybiAiQ2Fubm90IHBhcnNlICRmaWxlOlxuJGluXG4iOworICAg
ICAgICAgICAgICAgICAgICAkYmFkX2ZpbGV7JGZpbGV9ID0gMTsKKyAgICAgICAgICAgICAgICAg
ICAgJGluID0gJyc7CisgICAgICAgICAgICAgICAgICAgICRvdXQgPSB1bmRlZjsKKyAgICAgICAg
ICAgICAgICAgICAgbGFzdCBSRUFEOworICAgICAgICAgICAgICAgIH0gZWxzZSB7CisJCWRpZSAi
Q2Fubm90IHBhcnNlOlxuJGluXG4iOworICAgICAgICAgICAgICAgIH0KKyAgICAgICAgICAgIH0K
KyAgICAgICAgfQorCisgICAgICAgIGxhc3QgUkVBRCBpZiAkb3V0ID1+IC9cUy87CisgICAgfQor
CisgICAgcmV0dXJuICRvdXQ7Cit9CisKKwogIyBIYW5kbGUgcmVjdXJzaXZlIHN1YmRpcmVjdG9y
aWVzIHdpdGhvdXQgZ2V0dGluZyBhIGdyb3Rlc3F1ZWx5IGJpZyBzdGFjay4KICMgQ291bGQgdGhp
cyBiZSBpbXBsZW1lbnRlZCB1c2luZyBGaWxlOjpGaW5kPwogc3ViIG5leHRfZmlsZQpAQCAtNTA0
LDggKzY0NCwxMyBAQCBzdWIgcXVldWVfaW5jbHVkZXNfZnJvbQogICAgICAgICAgICAgICAgICRs
aW5lIC49IDxIRUFERVI+OwogICAgICAgICAgICAgfQogCi0gICAgICAgICAgICBpZiAoJGxpbmUg
PX4gL14jXHMqaW5jbHVkZVxzKzwoLio/KT4vKSB7Ci0gICAgICAgICAgICAgICAgcHVzaChAQVJH
ViwgJDEpIHVubGVzcyAkaXNfY29udmVydGVkeyQxfTsKKyAgICAgICAgICAgIGlmICgkbGluZSA9
fiAvXiNccyppbmNsdWRlXHMrKFs8Il0pKC4qPylbPiJdLykgeworICAgICAgICAgICAgICAgIG15
ICgkZGVsaW1pdGVyLCAkbmV3X2ZpbGUpID0gKCQxLCAkMik7CisgICAgICAgICAgICAgICAgIyBj
b3B5IHRoZSBwcmVmaXggaW4gdGhlIHF1b3RlIHN5bnRheCAoI2luY2x1ZGUgInguaCIpIGNhc2UK
KyAgICAgICAgICAgICAgICBpZiAoJGRlbGltaXRlciBlcSBxeyJ9ICYmICRmaWxlID1+IG18Xigu
KikvfCkgeworICAgICAgICAgICAgICAgICAgICAkbmV3X2ZpbGUgPSAiJDEvJG5ld19maWxlIjsK
KyAgICAgICAgICAgICAgICB9CisgICAgICAgICAgICAgICAgcHVzaChAQVJHViwgJG5ld19maWxl
KSB1bmxlc3MgJElzX2NvbnZlcnRlZHskbmV3X2ZpbGV9OwogICAgICAgICAgICAgfQogICAgICAg
ICB9CiAgICAgY2xvc2UgSEVBREVSOwpAQCAtNTQ2LDI1ICs2OTEsNTAgQEAgc3ViIGJ1aWxkX3By
ZWFtYmxlX2lmX25lY2Vzc2FyeQogICAgIG15ICglZGVmaW5lKSA9IF9leHRyYWN0X2NjX2RlZmlu
ZXMoKTsKIAogICAgIG9wZW4gIFBSRUFNQkxFLCAiPiRwcmVhbWJsZSIgb3IgZGllICJDYW5ub3Qg
b3BlbiAkcHJlYW1ibGU6ICAkISI7Ci0gICAgICAgIHByaW50IFBSRUFNQkxFICIjIFRoaXMgZmls
ZSB3YXMgY3JlYXRlZCBieSBoMnBoIHZlcnNpb24gJFZFUlNJT05cbiI7Ci0KLSAgICAgICAgZm9y
ZWFjaCAoc29ydCBrZXlzICVkZWZpbmUpIHsKLSAgICAgICAgICAgIGlmICgkb3B0X0QpIHsKLSAg
ICAgICAgICAgICAgICBwcmludCBQUkVBTUJMRSAiIyAkXz0kZGVmaW5leyRffVxuIjsKLSAgICAg
ICAgICAgIH0KLQotICAgICAgICAgICAgaWYgKCRkZWZpbmV7JF99ID1+IC9eXGQrJC8pIHsKLSAg
ICAgICAgICAgICAgICBwcmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNz
IChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7ICRkZWZpbmV7JF99IH0gfVxuXG4iOwotICAgICAg
ICAgICAgfSBlbHNpZiAoJGRlZmluZXskX30gPX4gL15cdyskLykgewotICAgICAgICAgICAgICAg
IHByaW50IFBSRUFNQkxFCi0gICAgICAgICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRf
KSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7JF99IH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNl
IHsKKwlwcmludCBQUkVBTUJMRSAiIyBUaGlzIGZpbGUgd2FzIGNyZWF0ZWQgYnkgaDJwaCB2ZXJz
aW9uICRWRVJTSU9OXG4iOworICAgICAgICAjIFByZXZlbnQgbm9uLXBvcnRhYmxlIGhleCBjb25z
dGFudHMgZnJvbSB3YXJuaW5nLgorICAgICAgICAjCisgICAgICAgICMgV2Ugc3RpbGwgcHJvZHVj
ZSBhbiBvdmVyZmxvdyB3YXJuaW5nIGlmIHdlIGNhbid0IHJlcHJlc2VudAorICAgICAgICAjIGEg
aGV4IGNvbnN0YW50IGFzIGFuIGludGVnZXIuCisgICAgICAgIHByaW50IFBSRUFNQkxFICJubyB3
YXJuaW5ncyBxdyhwb3J0YWJsZSk7XG4iOworCisJZm9yZWFjaCAoc29ydCBrZXlzICVkZWZpbmUp
IHsKKwkgICAgaWYgKCRvcHRfRCkgeworCQlwcmludCBQUkVBTUJMRSAiIyAkXz0kZGVmaW5leyRf
fVxuIjsKKwkgICAgfQorCSAgICBpZiAoJGRlZmluZXskX30gPX4gL15cKCguKilcKSQvKSB7CisJ
CSMgcGFyZW50aGVzaXplZCB2YWx1ZTogIGQ9KHYpCisJCSRkZWZpbmV7JF99ID0gJDE7CisJICAg
IH0KKwkgICAgaWYgKCRkZWZpbmV7JF99ID1+IC9eKFsrLV0/KFxkKyk/XC5cZCsoW2VFXVsrLV0/
XGQrKT8pW0ZMXT8kLykgeworCQkjIGZsb2F0OgorCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVu
bGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkgeyAkMSB9IH1cblxuIjsKKwkgICAgfSBlbHNp
ZiAoJGRlZmluZXskX30gPX4gL14oWystXT9cZCspVT9MezAsMn0kL2kpIHsKKwkJIyBpbnRlZ2Vy
OgorCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRf
KCkgeyAkMSB9IH1cblxuIjsKKyAgICAgICAgICAgIH0gZWxzaWYgKCRkZWZpbmV7JF99ID1+IC9e
KFsrLV0/MHhbXGRhLWZdKylVP0x7MCwyfSQvaSkgeworICAgICAgICAgICAgICAgICMgaGV4IGlu
dGVnZXIKKyAgICAgICAgICAgICAgICAjIFNwZWNpYWwgY2FzZWQsIHNpbmNlIHBlcmwgd2FybnMg
b24gaGV4IGludGVnZXJzCisgICAgICAgICAgICAgICAgIyB0aGF0IGNhbid0IGJlIHJlcHJlc2Vu
dGVkIGluIGEgVVYuCisgICAgICAgICAgICAgICAgIworICAgICAgICAgICAgICAgICMgVGhpcyB3
YXkgd2UgZ2V0IHRoZSB3YXJuaW5nIGF0IHRpbWUgb2YgdXNlLCBzbyB0aGUgdXNlcgorICAgICAg
ICAgICAgICAgICMgb25seSBnZXRzIHRoZSB3YXJuaW5nIGlmIHRoZXkgaGFwcGVuIHRvIHVzZSB0
aGlzCisgICAgICAgICAgICAgICAgIyBwbGF0Zm9ybS1zcGVjaWZpYyBkZWZpbml0aW9uLgorICAg
ICAgICAgICAgICAgIG15ICRjb2RlID0gJDE7CisgICAgICAgICAgICAgICAgJGNvZGUgPSAiaGV4
KCckY29kZScpIiBpZiBsZW5ndGggJGNvZGUgPiAxMDsKICAgICAgICAgICAgICAgICBwcmludCBQ
UkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIg
JF8oKSB7IFwiIiwKLSAgICAgICAgICAgICAgICAgICAgcXVvdGVtZXRhKCRkZWZpbmV7JF99KSwg
IlwiIH0gfVxuXG4iOwotICAgICAgICAgICAgfQotICAgICAgICB9CisgICAgICAgICAgICAgICAg
ICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgJGNvZGUgfSB9XG5cbiI7CisJ
ICAgIH0gZWxzaWYgKCRkZWZpbmV7JF99ID1+IC9eXHcrJC8pIHsKKwkJcHJpbnQgUFJFQU1CTEUK
KwkJICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7JF99IH0g
fVxuXG4iOworCSAgICB9IGVsc2UgeworCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAo
ZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkgeyBcIiIsCisJCSAgICBxdW90ZW1ldGEoJGRlZmluZXsk
X30pLCAiXCIgfSB9XG5cbiI7CisJICAgIH0KKwl9CiAgICAgY2xvc2UgUFJFQU1CTEUgICAgICAg
ICAgICAgICBvciBkaWUgIkNhbm5vdCBjbG9zZSAkcHJlYW1ibGU6ICAkISI7CiB9CiAKQEAgLTU3
NSwxNSArNzQ1LDE1IEBAIHN1YiBidWlsZF9wcmVhbWJsZV9pZl9uZWNlc3NhcnkKIHN1YiBfZXh0
cmFjdF9jY19kZWZpbmVzCiB7CiAgICAgbXkgJWRlZmluZTsKLSAgICBteSAkYWxsc3ltYm9scyA9
IGpvaW4gIiAiLCBAQ29uZmlne2Njc3ltYm9scywgY3Bwc3ltYm9scywgY3BwY2NzeW1ib2xzfTsK
KyAgICBteSAkYWxsc3ltYm9scyAgPSBqb2luICIgIiwKKwlAQ29uZmlneydjY3N5bWJvbHMnLCAn
Y3Bwc3ltYm9scycsICdjcHBjY3N5bWJvbHMnfTsKIAogICAgICMgU3BsaXQgY29tcGlsZXIgcHJl
LWRlZmluaXRpb25zIGludG8gYGtleT12YWx1ZScgcGFpcnM6Ci0gICAgZm9yZWFjaCAoc3BsaXQg
L1xzKy8sICRhbGxzeW1ib2xzKSB7Ci0gICAgICAgIC8oLis/KT0oLispLyBhbmQgJGRlZmluZXsk
MX0gPSAkMjsKLQotICAgICAgICBpZiAoJG9wdF9EKSB7Ci0gICAgICAgICAgICBwcmludCBTVERF
UlIgIiRfOiAgJDEgLT4gJDJcbiI7Ci0gICAgICAgIH0KKyAgICB3aGlsZSAoJGFsbHN5bWJvbHMg
PX4gLyhbXlxzXSspPSgoXFxcc3xbXlxzXSkrKS9nKSB7CisJJGRlZmluZXskMX0gPSAkMjsKKwlp
ZiAoJG9wdF9EKSB7CisJICAgIHByaW50IFNUREVSUiAiJF86ICAkMSAtPiAkMlxuIjsKKwl9CiAg
ICAgfQogCiAgICAgcmV0dXJuICVkZWZpbmU7CkBAIC02MTIsNiArNzgyLDEwIEBAIEl0IGlzIG1v
c3QgZWFzaWx5IHJ1biB3aGlsZSBpbiAvdXNyL2luY2x1ZGU6CiAKIAljZCAvdXNyL2luY2x1ZGU7
IGgycGggKiBzeXMvKgogCitvcgorCisJY2QgL3Vzci9pbmNsdWRlOyBoMnBoICogc3lzLyogYXJw
YS8qIG5ldGluZXQvKgorCiBvcgogCiAJY2QgL3Vzci9pbmNsdWRlOyBoMnBoIC1yIC1sIC4KQEAg
LTYyOSw3ICs4MDMsNyBAQCBJZiBydW4gd2l0aCBubyBhcmd1bWVudHMsIGZpbHRlcnMgc3RhbmRh
cmQgaW5wdXQgdG8gc3RhbmRhcmQgb3V0cHV0LgogPWl0ZW0gLWQgZGVzdGluYXRpb25fZGlyCiAK
IFB1dCB0aGUgcmVzdWx0aW5nIEI8LnBoPiBmaWxlcyBiZW5lYXRoIEI8ZGVzdGluYXRpb25fZGly
PiwgaW5zdGVhZCBvZgotYmVuZWF0aCB0aGUgZGVmYXVsdCBQZXJsIGxpYnJhcnkgbG9jYXRpb24g
KEM8JENvbmZpZ3snaW5zdGFsbHNpdHNlYXJjaCd9PikuCitiZW5lYXRoIHRoZSBkZWZhdWx0IFBl
cmwgbGlicmFyeSBsb2NhdGlvbiAoQzwkQ29uZmlneydpbnN0YWxsc2l0ZWFyY2gnfT4pLgogCiA9
aXRlbSAtcgogCkBAIC03MDgsMTggKzg4MiwxNiBAQCB0aGF0IGl0IGNhbiB0cmFuc2xhdGUuCiBJ
dCdzIG9ubHkgaW50ZW5kZWQgYXMgYSByb3VnaCB0b29sLgogWW91IG1heSBuZWVkIHRvIGRpY2tl
ciB3aXRoIHRoZSBmaWxlcyBwcm9kdWNlZC4KIAotRG9lc24ndCBydW4gd2l0aCBDPHVzZSBzdHJp
Y3Q+Ci0KIFlvdSBoYXZlIHRvIHJ1biB0aGlzIHByb2dyYW0gYnkgaGFuZDsgaXQncyBub3QgcnVu
IGFzIHBhcnQgb2YgdGhlIFBlcmwKIGluc3RhbGxhdGlvbi4KIAogRG9lc24ndCBoYW5kbGUgY29t
cGxpY2F0ZWQgZXhwcmVzc2lvbnMgYnVpbHQgcGllY2VtZWFsLCBhIGxhOgogCiAgICAgZW51bSB7
Ci0gICAgICAgIEZJUlNUX1ZBTFVFLAotICAgICAgICBTRUNPTkRfVkFMVUUsCisJRklSU1RfVkFM
VUUsCisJU0VDT05EX1ZBTFVFLAogICAgICNpZmRlZiBBQkMKLSAgICAgICAgVEhJUkRfVkFMVUUK
KwlUSElSRF9WQUxVRQogICAgICNlbmRpZgogICAgIH07CiAK
UH2PH560
  }
  if ( $num < 5.007000 ) {
    return _patch_b64(<<'UH2PH562');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTQyLDggKzQyLDEzIEBAIHVz
ZSBDb25maWc7CiB1c2UgRmlsZTo6UGF0aCBxdyhta3BhdGgpOwogdXNlIEdldG9wdDo6U3RkOwog
Ci1nZXRvcHRzKCdEZDpybGhhUScpOwotdXNlIHZhcnMgcXcoJG9wdF9EICRvcHRfZCAkb3B0X3Ig
JG9wdF9sICRvcHRfaCAkb3B0X2EgJG9wdF9RKTsKKyMgTWFrZSBzdXJlIHJlYWQgcGVybWlzc2lv
bnMgZm9yIGFsbCBhcmUgc2V0OgoraWYgKGRlZmluZWQgdW1hc2sgJiYgKHVtYXNrKCkgJiAwNDQ0
KSkgeworICAgIHVtYXNrICh1bWFzaygpICYgfjA0NDQpOworfQorCitnZXRvcHRzKCdEZDpybGhh
UWUnKTsKK3VzZSB2YXJzIHF3KCRvcHRfRCAkb3B0X2QgJG9wdF9yICRvcHRfbCAkb3B0X2ggJG9w
dF9hICRvcHRfUSAkb3B0X2UpOwogZGllICItciBhbmQgLWEgb3B0aW9ucyBhcmUgbXV0dWFsbHkg
ZXhjbHVzaXZlXG4iIGlmICgkb3B0X3IgYW5kICRvcHRfYSk7CiBteSBAaW5jX2RpcnMgPSBpbmNf
ZGlycygpIGlmICRvcHRfYTsKIApAQCAtNjUsMTMgKzcwLDIxIEBAIG15ICVpc2F0eXBlOwogQGlz
YXR5cGV7QGlzYXR5cGV9ID0gKDEpIHggQGlzYXR5cGU7CiBteSAkaW5pZiA9IDA7CiBteSAlSXNf
Y29udmVydGVkOworbXkgJWJhZF9maWxlID0gKCk7CiAKIEBBUkdWID0gKCctJykgdW5sZXNzIEBB
UkdWOwogCiBidWlsZF9wcmVhbWJsZV9pZl9uZWNlc3NhcnkoKTsKIAorc3ViIHJlaW5kZW50KCQp
IHsKKyAgICBteSgkdGV4dCkgPSBzaGlmdDsKKyAgICAkdGV4dCA9fiBzL1xuL1xuICAgIC9nOwor
ICAgICR0ZXh0ID1+IHMvICAgICAgICAvXHQvZzsKKyAgICAkdGV4dDsKK30KKwogbXkgKCR0LCAk
dGFiLCAlY3VyYXJncywgJG5ldywgJGV2YWxfaW5kZXgsICRkaXIsICRuYW1lLCAkYXJncywgJG91
dGZpbGUpOwotbXkgKCRpbmNsLCAkbmV4dCk7CitteSAoJGluY2wsICRpbmNsX3R5cGUsICRpbmNs
X3F1b3RlLCAkbmV4dCk7CiB3aGlsZSAoZGVmaW5lZCAobXkgJGZpbGUgPSBuZXh0X2ZpbGUoKSkp
IHsKICAgICBpZiAoLWwgJGZpbGUgYW5kIC1kICRmaWxlKSB7CiAgICAgICAgIGxpbmtfaWZfcG9z
c2libGUoJGZpbGUpIGlmICgkb3B0X2wpOwpAQCAtMTA3LDMwICsxMjAsMTcgQEAgd2hpbGUgKGRl
ZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxlKCkpKSB7CiAJb3BlbihPVVQsIj4kRGVzdF9kaXIv
JG91dGZpbGUiKSB8fCBkaWUgIkNhbid0IGNyZWF0ZSAkb3V0ZmlsZTogJCFcbiI7CiAgICAgfQog
Ci0gICAgcHJpbnQgT1VUICJyZXF1aXJlICdfaDJwaF9wcmUucGgnO1xuXG4iOwotICAgIHdoaWxl
ICg8SU4+KSB7Ci0JY2hvcDsKLQl3aGlsZSAoL1xcJC8pIHsKLQkgICAgY2hvcDsKLQkgICAgJF8g
Lj0gPElOPjsKLQkgICAgY2hvcDsKLQl9Ci0JcHJpbnQgT1VUICIjICRfXG4iIGlmICRvcHRfRDsK
LQotCWlmIChzOi9cKjpcMjAwOmcpIHsKLQkgICAgczpcKi86XDIwMTpnOwotCSAgICBzL1wyMDBb
XlwyMDFdKlwyMDEvL2c7CSMgZGVsZXRlIHNpbmdsZSBsaW5lIGNvbW1lbnRzCi0JICAgIGlmIChz
L1wyMDAuKi8vKSB7CQkjIGJlZ2luIG11bHRpLWxpbmUgY29tbWVudD8KLQkJJF8gLj0gJy8qJzsK
LQkJJF8gLj0gPElOPjsKLQkJcmVkbzsKLQkgICAgfQotCX0KKyAgICBwcmludCBPVVQKKyAgICAg
ICAgInJlcXVpcmUgJ19oMnBoX3ByZS5waCc7XG5cbiIsCisgICAgICAgICJubyB3YXJuaW5ncyAn
cmVkZWZpbmUnO1xuXG4iOworCisgICAgd2hpbGUgKGRlZmluZWQgKGxvY2FsICRfID0gbmV4dF9s
aW5lKCRmaWxlKSkpIHsKIAlpZiAocy9eXHMqXCNccyovLykgewogCSAgICBpZiAocy9eZGVmaW5l
XHMrKFx3KykvLykgewogCQkkbmFtZSA9ICQxOwogCQkkbmV3ID0gJyc7CiAJCXMvXHMrJC8vOwor
CQlzL1woXHcrXHMqXChcKlwpXHMqXChcdypcKVwpXHMqKC0/XGQrKS8kMS87ICMgKGludCAoKiko
Zm9vX3QpKTAKIAkJaWYgKHMvXlwoKFtcdyxcc10qKVwpLy8pIHsKIAkJICAgICRhcmdzID0gJDE7
CiAgICAgCSAgICAJICAgIG15ICRwcm90byA9ICcoKSAnOwpAQCAtMTg0LDIyICsxODQsMzIgQEAg
d2hpbGUgKGRlZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxlKCkpKSB7CiAgICAgICAgICAgICAg
ICAgICAgICAgcHJpbnQgT1VUICR0LCJ1bmxlc3MoZGVmaW5lZChcJiRuYW1lKSkge1xuICAgIHN1
YiAkbmFtZSAoKSB7XHQiLCRuZXcsIjt9XG59XG4iOwogCQkgICAgfQogCQl9Ci0JICAgIH0gZWxz
aWYgKC9eKGluY2x1ZGV8aW1wb3J0KVxzKls8Il0oLiopWz4iXS8pIHsKLQkJKCRpbmNsID0gJDIp
ID1+IHMvXC5oJC8ucGgvOwotCQlwcmludCBPVVQgJHQsInJlcXVpcmUgJyRpbmNsJztcbiI7Ci0J
ICAgIH0gZWxzaWYoL15pbmNsdWRlX25leHRccypbPCJdKC4qKVs+Il0vKSB7Ci0JCSgkaW5jbCA9
ICQxKSA9fiBzL1wuaCQvLnBoLzsKKwkgICAgfSBlbHNpZiAoL14oaW5jbHVkZXxpbXBvcnR8aW5j
bHVkZV9uZXh0KVxzKihbPFwiXSkoLiopWz5cIl0vKSB7CisgICAgICAgICAgICAgICAgJGluY2xf
dHlwZSA9ICQxOworICAgICAgICAgICAgICAgICRpbmNsX3F1b3RlID0gJDI7CisgICAgICAgICAg
ICAgICAgJGluY2wgPSAkMzsKKyAgICAgICAgICAgICAgICBpZiAoKCRpbmNsX3R5cGUgZXEgJ2lu
Y2x1ZGVfbmV4dCcpIHx8CisgICAgICAgICAgICAgICAgICAgICgkb3B0X2UgJiYgZXhpc3RzKCRi
YWRfZmlsZXskaW5jbH0pKSkgeworICAgICAgICAgICAgICAgICAgICAkaW5jbCA9fiBzL1wuaCQv
LnBoLzsKIAkJcHJpbnQgT1VUICgkdCwKIAkJCSAgICJldmFsIHtcbiIpOwogICAgICAgICAgICAg
ICAgICR0YWIgKz0gNDsKICAgICAgICAgICAgICAgICAkdCA9ICJcdCIgeCAoJHRhYiAvIDgpIC4g
JyAnIHggKCR0YWIgJSA4KTsKKyAgICAgICAgICAgICAgICAgICAgcHJpbnQgT1VUICgkdCwgIm15
KFxAUkVNKTtcbiIpOworICAgICAgICAgICAgICAgICAgICBpZiAoJGluY2xfdHlwZSBlcSAnaW5j
bHVkZV9uZXh0JykgewogCQlwcmludCBPVVQgKCR0LAogCQkJICAgIm15KFwlSU5DRCkgPSBtYXAg
eyBcJElOQ3tcJF99ID0+IDEgfSAiLAotCQkJICAgIihncmVwIHsgXCRfIGVxIFwiJGluY2xcIiB9
IGtleXMoXCVJTkMpKTtcbiIpOworCQkJICAgICAgICAgICAiKGdyZXAgeyBcJF8gZXEgXCIkaW5j
bFwiIH0gIiwKKyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgImtleXMoXCVJTkMp
KTtcbiIpOwogCQlwcmludCBPVVQgKCR0LAotCQkJICAgIm15KFxAUkVNKSA9IG1hcCB7IFwiXCRf
LyRpbmNsXCIgfSAiLAorCQkJICAgICAgICAgICAiXEBSRU0gPSBtYXAgeyBcIlwkXy8kaW5jbFwi
IH0gIiwKIAkJCSAgICIoZ3JlcCB7IG5vdCBleGlzdHMoXCRJTkNEe1wiXCRfLyRpbmNsXCJ9KSIs
Ci0JCQkgICAiYW5kIC1mIFwiXCRfLyRpbmNsXCIgfSBcQElOQyk7XG4iKTsKKwkJCSAgICAgICAg
ICAgIiBhbmQgLWYgXCJcJF8vJGluY2xcIiB9IFxASU5DKTtcbiIpOworICAgICAgICAgICAgICAg
ICAgICB9IGVsc2UgeworICAgICAgICAgICAgICAgICAgICAgICAgcHJpbnQgT1VUICgkdCwKKyAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIlxAUkVNID0gbWFwIHsgXCJcJF8vJGlu
Y2xcIiB9ICIsCisgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICIoZ3JlcCB7LXIg
XCJcJF8vJGluY2xcIiB9IFxASU5DKTtcbiIpOworICAgICAgICAgICAgICAgICAgICB9CiAJCXBy
aW50IE9VVCAoJHQsCiAJCQkgICAicmVxdWlyZSBcIlwkUkVNWzBdXCIgaWYgXEBSRU07XG4iKTsK
ICAgICAgICAgICAgICAgICAkdGFiIC09IDQ7CkBAIC0yMDgsNiArMjE4LDE0IEBAIHdoaWxlIChk
ZWZpbmVkIChteSAkZmlsZSA9IG5leHRfZmlsZSgpKSkgewogCQkJICAgIn07XG4iKTsKIAkJcHJp
bnQgT1VUICgkdCwKIAkJCSAgICJ3YXJuKFwkXEApIGlmIFwkXEA7XG4iKTsKKyAgICAgICAgICAg
ICAgICB9IGVsc2UgeworICAgICAgICAgICAgICAgICAgICAkaW5jbCA9fiBzL1wuaCQvLnBoLzsK
KyAgICAgICAgICAgICAgICAgICAgIyBjb3B5IHRoZSBwcmVmaXggaW4gdGhlIHF1b3RlIHN5bnRh
eCAoI2luY2x1ZGUgInguaCIpIGNhc2UKKyAgICAgICAgICAgICAgICAgICAgaWYgKCRpbmNsICF+
IG18L3wgJiYgJGluY2xfcXVvdGUgZXEgcXsifSAmJiAkZmlsZSA9fiBtfF4oLiopL3wpIHsKKyAg
ICAgICAgICAgICAgICAgICAgICAgICRpbmNsID0gIiQxLyRpbmNsIjsKKyAgICAgICAgICAgICAg
ICAgICAgfQorCQkgICAgcHJpbnQgT1VUICR0LCJyZXF1aXJlICckaW5jbCc7XG4iOworICAgICAg
ICAgICAgICAgIH0KIAkgICAgfSBlbHNpZiAoL15pZmRlZlxzKyhcdyspLykgewogCQlwcmludCBP
VVQgJHQsImlmKGRlZmluZWQoJiQxKSkge1xuIjsKIAkJJHRhYiArPSA0OwpAQCAtMjU1LDE1ICsy
NzMsMTkgQEAgd2hpbGUgKGRlZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxlKCkpKSB7CiAJICAg
IH0gZWxzaWYoL15pZGVudFxzKyguKikvKSB7CiAJCXByaW50IE9VVCAkdCwgIiMgJDFcbiI7CiAJ
ICAgIH0KLSAJfSBlbHNpZigvXlxzKih0eXBlZGVmXHMqKT9lbnVtXHMqKFxzK1thLXpBLVpfXVx3
KlxzKik/XHsvKSB7Ci0JICAgIHVudGlsKC9cfS4qPzsvKSB7Ci0JCWNob21wKCRuZXh0ID0gPElO
Pik7CisJfSBlbHNpZigvXlxzKih0eXBlZGVmXHMqKT9lbnVtXHMqKFxzK1thLXpBLVpfXVx3Klxz
Kik/LykgeworCSAgICB1bnRpbCgvXHtbXn1dKlx9Lio7LyB8fCAvOy8pIHsKKwkJbGFzdCB1bmxl
c3MgZGVmaW5lZCAoJG5leHQgPSBuZXh0X2xpbmUoJGZpbGUpKTsKKwkJY2hvbXAgJG5leHQ7CisJ
CSMgZHJvcCAiI2RlZmluZSBGT08gRk9PIiBpbiBlbnVtcworCQkkbmV4dCA9fiBzL15ccyojXHMq
ZGVmaW5lXHMrKFx3KylccytcMVxzKiQvLzsKIAkJJF8gLj0gJG5leHQ7CiAJCXByaW50IE9VVCAi
IyAkbmV4dFxuIiBpZiAkb3B0X0Q7CiAJICAgIH0KKwkgICAgcy8jXHMqaWYuKj8jXHMqZW5kaWYv
L2c7ICMgZHJvcCAjaWZkZWZzCiAJICAgIHNAL1wqLio/XCovQEBnOwogCSAgICBzL1xzKy8gL2c7
Ci0JICAgIC9eXHM/KHR5cGVkZWZccz8pP2VudW1ccz8oW2EtekEtWl9dXHcqKT9ccz9ceyguKilc
fVxzPyhbYS16QS1aX11cdyopP1xzPzsvOworCSAgICBuZXh0IHVubGVzcyAvXlxzPyh0eXBlZGVm
XHM/KT9lbnVtXHM/KFthLXpBLVpfXVx3Kik/XHM/XHsoLiopXH1ccz8oW2EtekEtWl9dXHcqKT9c
cz87LzsKIAkgICAgKG15ICRlbnVtX3N1YnMgPSAkMykgPX4gcy9ccy8vZzsKIAkgICAgbXkgQGVu
dW1fc3VicyA9IHNwbGl0KC8sLywgJGVudW1fc3Vicyk7CiAJICAgIG15ICRlbnVtX3ZhbCA9IC0x
OwpAQCAtMjg1LDIyICszMDcsMjIgQEAgd2hpbGUgKGRlZmluZWQgKG15ICRmaWxlID0gbmV4dF9m
aWxlKCkpKSB7CiAJICAgIH0KIAl9CiAgICAgfQotICAgIHByaW50IE9VVCAiMTtcbiI7Ci0KICAg
ICAkSXNfY29udmVydGVkeyRmaWxlfSA9IDE7CisgICAgaWYgKCRvcHRfZSAmJiBleGlzdHMoJGJh
ZF9maWxleyRmaWxlfSkpIHsKKyAgICAgICAgdW5saW5rKCREZXN0X2RpciAuICcvJyAuICRvdXRm
aWxlKTsKKyAgICAgICAgJG5leHQgPSAnJzsKKyAgICB9IGVsc2UgeworICAgICAgICBwcmludCBP
VVQgIjE7XG4iOwogICAgIHF1ZXVlX2luY2x1ZGVzX2Zyb20oJGZpbGUpIGlmICgkb3B0X2EpOwor
ICAgIH0KIH0KIAotZXhpdCAkRXhpdDsKLQotCi1zdWIgcmVpbmRlbnQoJCkgewotICAgIG15KCR0
ZXh0KSA9IHNoaWZ0OwotICAgICR0ZXh0ID1+IHMvXG4vXG4gICAgL2c7Ci0gICAgJHRleHQgPX4g
cy8gICAgICAgIC9cdC9nOwotICAgICR0ZXh0OworaWYgKCRvcHRfZSAmJiAoc2NhbGFyKGtleXMg
JWJhZF9maWxlKSA+IDApKSB7CisgICAgd2FybiAiV2FzIHVuYWJsZSB0byBjb252ZXJ0IHRoZSBm
b2xsb3dpbmcgZmlsZXM6XG4iOworICAgIHdhcm4gIlx0IiAuIGpvaW4oIlxuXHQiLHNvcnQoa2V5
cyAlYmFkX2ZpbGUpKSAuICJcbiI7CiB9CiAKK2V4aXQgJEV4aXQ7CiAKIHN1YiBleHByIHsKICAg
ICBteSAkam9pbmVkX2FyZ3M7CkBAIC0zMTEsOCArMzMzLDIxIEBAIHN1YiBleHByIHsKIAlzL15c
JlwmLy8gJiYgZG8geyAkbmV3IC49ICIgJiYiOyBuZXh0O307ICMgaGFuZGxlICYmIG9wZXJhdG9y
CiAJcy9eXCYoW1woYS16XCldKykvJDEvaTsJIyBoYWNrIGZvciB0aGluZ3MgdGhhdCB0YWtlIHRo
ZSBhZGRyZXNzIG9mCiAJcy9eKFxzKykvLwkJJiYgZG8geyRuZXcgLj0gJyAnOyBuZXh0O307Ci0J
cy9eKDBYWzAtOUEtRl0rKVtVTF0qLy9pCSYmIGRvIHskbmV3IC49IGxjKCQxKTsgbmV4dDt9Owot
CXMvXigtP1xkK1wuXGQrRVstK11cZCspRj8vL2kJJiYgZG8geyRuZXcgLj0gJDE7IG5leHQ7fTsK
KwlzL14wWChbMC05QS1GXSspW1VMXSovL2kgCisJICAgICYmIGRvIHtteSAkaGV4ID0gJDE7CisJ
CSAgICRoZXggPX4gcy9eMCsvLzsKKwkJICAgaWYgKGxlbmd0aCAkaGV4ID4gOCAmJiAhJENvbmZp
Z3t1c2U2NGJpdGludH0pIHsKKwkJICAgICAgICMgQ3JvYWsgaWYgbnZfcHJlc2VydmVzX3V2X2Jp
dHMgPCA2NCA/CisJCSAgICAgICAkbmV3IC49ICAgICAgICAgaGV4KHN1YnN0cigkaGV4LCAtOCkp
ICsKKwkJCSAgICAgICAyKiozMiAqIGhleChzdWJzdHIoJGhleCwgIDAsIC04KSk7CisJCSAgICAg
ICAjIFRoZSBhYm92ZSB3aWxsIHByb2R1Y2UgImVycm9ybmV1cyIgY29kZQorCQkgICAgICAgIyBp
ZiB0aGUgaGV4IGNvbnN0YW50IHdhcyBlLmcuIGluc2lkZSBVSU5UNjRfQworCQkgICAgICAgIyBt
YWNybywgYnV0IHRoZW4gYWdhaW4sIGgycGggaXMgYW4gYXBwcm94aW1hdGlvbi4KKwkJICAgfSBl
bHNlIHsKKwkJICAgICAgICRuZXcgLj0gbGMoIjB4JGhleCIpOworCQkgICB9CisJCSAgIG5leHQ7
fTsKKwlzL14oLT9cZCtcLlxkK0VbLStdP1xkKylbRkxdPy8vaQkmJiBkbyB7JG5ldyAuPSAkMTsg
bmV4dDt9OwogCXMvXihcZCspXHMqW0xVXSovL2kJJiYgZG8geyRuZXcgLj0gJDE7IG5leHQ7fTsK
IAlzL14oIihcXCJ8W14iXSkqIikvLwkmJiBkbyB7JG5ldyAuPSAkMTsgbmV4dDt9OwogCXMvXico
KFxcInxbXiJdKSopJy8vCSYmIGRvIHsKQEAgLTM1MSw3ICszODYsNyBAQCBzdWIgZXhwciB7CiAJ
IyBFbGltaW5hdGUgdHlwZWRlZnMKIAkvXCgoW1x3XHNdKylbXCpcc10qXClccypbXHdcKF0vICYm
IGRvIHsKIAkgICAgZm9yZWFjaCAoc3BsaXQgL1xzKy8sICQxKSB7ICAjIE1ha2Ugc3VyZSBhbGwg
dGhlIHdvcmRzIGFyZSB0eXBlcywKLQkJbGFzdCB1bmxlc3MgKCRpc2F0eXBleyRffSBvciAkXyBl
cSAnc3RydWN0Jyk7CisJCWxhc3QgdW5sZXNzICgkaXNhdHlwZXskX30gb3IgJF8gZXEgJ3N0cnVj
dCcgb3IgJF8gZXEgJ3VuaW9uJyk7CiAJICAgIH0KIAkgICAgcy9cKFtcd1xzXStbXCpcc10qXCkv
LyAmJiBuZXh0OyAgICAgICMgdGhlbiBlbGltaW5hdGUgdGhlbS4KIAl9OwpAQCAtMzc0LDcgKzQw
OSw3IEBAIHN1YiBleHByIHsKIAl9OwogCXMvXihbX2EtekEtWl1cdyopLy8JJiYgZG8gewogCSAg
ICBteSAkaWQgPSAkMTsKLQkgICAgaWYgKCRpZCBlcSAnc3RydWN0JykgeworCSAgICBpZiAoJGlk
IGVxICdzdHJ1Y3QnIHx8ICRpZCBlcSAndW5pb24nKSB7CiAJCXMvXlxzKyhcdyspLy87CiAJCSRp
ZCAuPSAnICcgLiAkMTsKIAkJJGlzYXR5cGV7JGlkfSA9IDE7CkBAIC0zODcsOCArNDIyLDggQEAg
c3ViIGV4cHIgewogCQkkbmV3IC49ICctPicgaWYgL15bXFtce10vOwogCSAgICB9IGVsc2lmICgk
aWQgZXEgJ2RlZmluZWQnKSB7CiAJCSRuZXcgLj0gJ2RlZmluZWQnOwotCSAgICB9IGVsc2lmICgv
XlwoLykgewotCQlzL15cKChcdyksLygiJDEiLC8gaWYgJGlkID1+IC9eX0lPW1dSXSokL2k7CSMg
Y2hlYXQKKwkgICAgfSBlbHNpZiAoL15ccypcKC8pIHsKKwkJcy9eXHMqXCgoXHcpLC8oIiQxIiwv
IGlmICRpZCA9fiAvXl9JT1tXUl0qJC9pOwkjIGNoZWF0CiAJCSRuZXcgLj0gIiAmJGlkIjsKIAkg
ICAgfSBlbHNpZiAoJGlzYXR5cGV7JGlkfSkgewogCQlpZiAoJG5ldyA9fiAve1xzKiQvKSB7CkBA
IC00MDEsNyArNDM2LDcgQEAgc3ViIGV4cHIgewogCQl9CiAJICAgIH0gZWxzZSB7CiAJCWlmICgk
aW5pZiAmJiAkbmV3ICF+IC9kZWZpbmVkXHMqXCgkLykgewotCQkgICAgJG5ldyAuPSAnKGRlZmlu
ZWQoJicgLiAkaWQgLiAnKSA/ICYnIC4gJGlkIC4gJyA6IDApJzsKKwkJICAgICRuZXcgLj0gJyhk
ZWZpbmVkKCYnIC4gJGlkIC4gJykgPyAmJyAuICRpZCAuICcgOiB1bmRlZiknOwogCQl9IGVsc2lm
ICgvXlxbLykgewogCQkgICAgJG5ldyAuPSAiIFwkJGlkIjsKIAkJfSBlbHNlIHsKQEAgLTQxNSw2
ICs0NTAsMTAxIEBAIHN1YiBleHByIHsKIH0KIAogCitzdWIgbmV4dF9saW5lCit7CisgICAgbXkg
JGZpbGUgPSBzaGlmdDsKKyAgICBteSAoJGluLCAkb3V0KTsKKyAgICBteSAkcHJlX3N1Yl90cmlf
Z3JhcGhzID0gMTsKKworICAgIFJFQUQ6IHdoaWxlIChub3QgZW9mIElOKSB7CisgICAgICAgICRp
biAgLj0gPElOPjsKKyAgICAgICAgY2hvbXAgJGluOworICAgICAgICBuZXh0IHVubGVzcyBsZW5n
dGggJGluOworCisgICAgICAgIHdoaWxlIChsZW5ndGggJGluKSB7CisgICAgICAgICAgICBpZiAo
JHByZV9zdWJfdHJpX2dyYXBocykgeworICAgICAgICAgICAgICAgICMgUHJlcHJvY2VzcyBhbGwg
dHJpLWdyYXBocyAKKyAgICAgICAgICAgICAgICAjIGluY2x1ZGluZyB0aGluZ3Mgc3R1Y2sgaW4g
cXVvdGVkIHN0cmluZyBjb25zdGFudHMuCisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cPz0v
Iy9nOyAgICAgICAgICAgICAgICAgICAgICAgICAjIHwgPz89fCAgI3wKKyAgICAgICAgICAgICAg
ICAkaW4gPX4gcy9cP1w/XCEvfC9nOyAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/PyF8ICB8
fAorICAgICAgICAgICAgICAgICRpbiA9fiBzL1w/XD8nL14vZzsgICAgICAgICAgICAgICAgICAg
ICAgICAgIyB8ID8/J3wgIF58CisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cP1woL1svZzsg
ICAgICAgICAgICAgICAgICAgICAgICAjIHwgPz8ofCAgW3wKKyAgICAgICAgICAgICAgICAkaW4g
PX4gcy9cP1w/XCkvXS9nOyAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/Pyl8ICBdfAorICAg
ICAgICAgICAgICAgICRpbiA9fiBzL1w/XD9cLS9+L2c7ICAgICAgICAgICAgICAgICAgICAgICAg
IyB8ID8/LXwgIH58CisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cP1wvL1xcL2c7ICAgICAg
ICAgICAgICAgICAgICAgICAjIHwgPz8vfCAgXHwKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9c
P1w/PC97L2c7ICAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/Pzx8ICB7fAorICAgICAgICAg
ICAgICAgICRpbiA9fiBzL1w/XD8+L30vZzsgICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/
PnwgIH18CisgICAgICAgICAgICB9CisJICAgIGlmICgkaW4gPX4gL15cI2lmZGVmIF9fTEFOR1VB
R0VfUEFTQ0FMX18vKSB7CisgICAgICAgICAgICAgICAgIyBUcnU2NCBkaXNhc3NlbWJsZXIuaCBl
dmlsbmVzczogbWl4ZWQgQyBhbmQgUGFzY2FsLgorCQl3aGlsZSAoPElOPikgeworCQkgICAgbGFz
dCBpZiAvXlwjZW5kaWYvOyAKKwkJfQorCQluZXh0IFJFQUQ7CisJICAgIH0KKwkgICAgaWYgKCRp
biA9fiAvXmV4dGVybiBpbmxpbmUgLyAmJiAjIElubGluZWQgYXNzZW1ibGVyLgorCQkkXk8gZXEg
J2xpbnV4JyAmJiAkZmlsZSA9fiBtISg/Ol58Lylhc20vW14vXStcLmgkISkgeworIAkJd2hpbGUg
KDxJTj4pIHsKKwkJICAgIGxhc3QgaWYgL159LzsgCisJCX0KKwkJbmV4dCBSRUFEOworCSAgICB9
CisgICAgICAgICAgICBpZiAoJGluID1+IHMvXFwkLy8pIHsgICAgICAgICAgICAgICAgICAgICAg
ICAgICAjIFwtbmV3bGluZQorICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJyAnOworICAgICAg
ICAgICAgICAgIG5leHQgUkVBRDsKKyAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiBzL14oW14i
J1xcXC9dKykvLykgeyAgICAgICAgICAgICMgUGFzc3Rocm91Z2gKKyAgICAgICAgICAgICAgICAk
b3V0ICAgIC49ICQxOworICAgICAgICAgICAgfSBlbHNpZiAoJGluID1+IHMvXihcXC4pLy8pIHsg
ICAgICAgICAgICAgICAgICAgIyBcLi4uCisgICAgICAgICAgICAgICAgJG91dCAgICAuPSAkMTsK
KyAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiAvXicvKSB7ICAgICAgICAgICAgICAgICAgICAg
ICAgICMgJy4uLgorICAgICAgICAgICAgICAgIGlmICgkaW4gPX4gcy9eKCcoXFwufFteJ1xcXSkq
JykvLykgeworICAgICAgICAgICAgICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAg
ICAgIH0gZWxzZSB7CisgICAgICAgICAgICAgICAgICAgIG5leHQgUkVBRDsKKyAgICAgICAgICAg
ICAgICB9CisgICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gL14iLykgeyAgICAgICAgICAgICAg
ICAgICAgICAgICAjICIuLi4KKyAgICAgICAgICAgICAgICBpZiAoJGluID1+IHMvXigiKFxcLnxb
XiJcXF0pKiIpLy8pIHsKKyAgICAgICAgICAgICAgICAgICAgJG91dCAgICAuPSAkMTsKKyAgICAg
ICAgICAgICAgICB9IGVsc2UgeworICAgICAgICAgICAgICAgICAgICBuZXh0IFJFQUQ7CisgICAg
ICAgICAgICAgICAgfQorICAgICAgICAgICAgfSBlbHNpZiAoJGluID1+IHMvXlwvXC8uKi8vKSB7
ICAgICAgICAgICAgICAgICAgIyAvLy4uLgorICAgICAgICAgICAgICAgICMgZmFsbCB0aHJvdWdo
CisgICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gbS9eXC9cKi8pIHsgICAgICAgICAgICAgICAg
ICAgICAjIC8qLi4uCisgICAgICAgICAgICAgICAgIyBDIGNvbW1lbnQgcmVtb3ZhbCBhZGFwdGVk
IGZyb20gcGVybGZhcTY6CisgICAgICAgICAgICAgICAgaWYgKCRpbiA9fiBzL15cL1wqW14qXSpc
KisoW15cLypdW14qXSpcKispKlwvLy8pIHsKKyAgICAgICAgICAgICAgICAgICAgJG91dCAgICAu
PSAnICc7CisgICAgICAgICAgICAgICAgfSBlbHNlIHsgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAjIEluY29tcGxldGUgLyogKi8KKyAgICAgICAgICAgICAgICAgICAgbmV4dCBS
RUFEOworICAgICAgICAgICAgICAgIH0KKyAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiBzL14o
XC8pLy8pIHsgICAgICAgICAgICAgICAgICAgICMgLy4uLgorICAgICAgICAgICAgICAgICRvdXQg
ICAgLj0gJDE7CisgICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKFteXCdcIlxcXC9dKykv
LykgeworICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7CisgICAgICAgICAgICB9IGVsc2lm
ICgkXk8gZXEgJ2xpbnV4JyAmJgorICAgICAgICAgICAgICAgICAgICAgJGZpbGUgPX4gbSEoPzpe
fC8pbGludXgvYnl0ZW9yZGVyL3BkcF9lbmRpYW5cLmgkISAmJgorICAgICAgICAgICAgICAgICAg
ICAgJGluICAgPX4gcyFcJ1QgS05PVyEhKSB7CisgICAgICAgICAgICAgICAgJG91dCAgICA9fiBz
IUkgRE9OJCFJX0RPX05PVF9LTk9XITsKKyAgICAgICAgICAgIH0gZWxzZSB7CisgICAgICAgICAg
ICAgICAgaWYgKCRvcHRfZSkgeworICAgICAgICAgICAgICAgICAgICB3YXJuICJDYW5ub3QgcGFy
c2UgJGZpbGU6XG4kaW5cbiI7CisgICAgICAgICAgICAgICAgICAgICRiYWRfZmlsZXskZmlsZX0g
PSAxOworICAgICAgICAgICAgICAgICAgICAkaW4gPSAnJzsKKyAgICAgICAgICAgICAgICAgICAg
JG91dCA9IHVuZGVmOworICAgICAgICAgICAgICAgICAgICBsYXN0IFJFQUQ7CisgICAgICAgICAg
ICAgICAgfSBlbHNlIHsKKwkJZGllICJDYW5ub3QgcGFyc2U6XG4kaW5cbiI7CisgICAgICAgICAg
ICAgICAgfQorICAgICAgICAgICAgfQorICAgICAgICB9CisKKyAgICAgICAgbGFzdCBSRUFEIGlm
ICRvdXQgPX4gL1xTLzsKKyAgICB9CisKKyAgICByZXR1cm4gJG91dDsKK30KKworCiAjIEhhbmRs
ZSByZWN1cnNpdmUgc3ViZGlyZWN0b3JpZXMgd2l0aG91dCBnZXR0aW5nIGEgZ3JvdGVzcXVlbHkg
YmlnIHN0YWNrLgogIyBDb3VsZCB0aGlzIGJlIGltcGxlbWVudGVkIHVzaW5nIEZpbGU6OkZpbmQ/
CiBzdWIgbmV4dF9maWxlCkBAIC01MTQsOCArNjQ0LDEzIEBAIHN1YiBxdWV1ZV9pbmNsdWRlc19m
cm9tCiAgICAgICAgICAgICAgICAgJGxpbmUgLj0gPEhFQURFUj47CiAgICAgICAgICAgICB9CiAK
LSAgICAgICAgICAgIGlmICgkbGluZSA9fiAvXiNccyppbmNsdWRlXHMrPCguKj8pPi8pIHsKLSAg
ICAgICAgICAgICAgICBwdXNoKEBBUkdWLCAkMSkgdW5sZXNzICRJc19jb252ZXJ0ZWR7JDF9Owor
ICAgICAgICAgICAgaWYgKCRsaW5lID1+IC9eI1xzKmluY2x1ZGVccysoWzwiXSkoLio/KVs+Il0v
KSB7CisgICAgICAgICAgICAgICAgbXkgKCRkZWxpbWl0ZXIsICRuZXdfZmlsZSkgPSAoJDEsICQy
KTsKKyAgICAgICAgICAgICAgICAjIGNvcHkgdGhlIHByZWZpeCBpbiB0aGUgcXVvdGUgc3ludGF4
ICgjaW5jbHVkZSAieC5oIikgY2FzZQorICAgICAgICAgICAgICAgIGlmICgkZGVsaW1pdGVyIGVx
IHF7In0gJiYgJGZpbGUgPX4gbXxeKC4qKS98KSB7CisgICAgICAgICAgICAgICAgICAgICRuZXdf
ZmlsZSA9ICIkMS8kbmV3X2ZpbGUiOworICAgICAgICAgICAgICAgIH0KKyAgICAgICAgICAgICAg
ICBwdXNoKEBBUkdWLCAkbmV3X2ZpbGUpIHVubGVzcyAkSXNfY29udmVydGVkeyRuZXdfZmlsZX07
CiAgICAgICAgICAgICB9CiAgICAgICAgIH0KICAgICBjbG9zZSBIRUFERVI7CkBAIC01NTYsMjUg
KzY5MSw1MCBAQCBzdWIgYnVpbGRfcHJlYW1ibGVfaWZfbmVjZXNzYXJ5CiAgICAgbXkgKCVkZWZp
bmUpID0gX2V4dHJhY3RfY2NfZGVmaW5lcygpOwogCiAgICAgb3BlbiAgUFJFQU1CTEUsICI+JHBy
ZWFtYmxlIiBvciBkaWUgIkNhbm5vdCBvcGVuICRwcmVhbWJsZTogICQhIjsKLSAgICAgICAgcHJp
bnQgUFJFQU1CTEUgIiMgVGhpcyBmaWxlIHdhcyBjcmVhdGVkIGJ5IGgycGggdmVyc2lvbiAkVkVS
U0lPTlxuIjsKLQotICAgICAgICBmb3JlYWNoIChzb3J0IGtleXMgJWRlZmluZSkgewotICAgICAg
ICAgICAgaWYgKCRvcHRfRCkgewotICAgICAgICAgICAgICAgIHByaW50IFBSRUFNQkxFICIjICRf
PSRkZWZpbmV7JF99XG4iOwotICAgICAgICAgICAgfQotCi0gICAgICAgICAgICBpZiAoJGRlZmlu
ZXskX30gPX4gL15cZCskLykgewotICAgICAgICAgICAgICAgIHByaW50IFBSRUFNQkxFCi0gICAg
ICAgICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgJGRlZmlu
ZXskX30gfSB9XG5cbiI7Ci0gICAgICAgICAgICB9IGVsc2lmICgkZGVmaW5leyRffSA9fiAvXlx3
KyQvKSB7Ci0gICAgICAgICAgICAgICAgcHJpbnQgUFJFQU1CTEUKLSAgICAgICAgICAgICAgICAg
ICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkgeyAmJGRlZmluZXskX30gfSB9XG5c
biI7Ci0gICAgICAgICAgICB9IGVsc2UgeworCXByaW50IFBSRUFNQkxFICIjIFRoaXMgZmlsZSB3
YXMgY3JlYXRlZCBieSBoMnBoIHZlcnNpb24gJFZFUlNJT05cbiI7CisgICAgICAgICMgUHJldmVu
dCBub24tcG9ydGFibGUgaGV4IGNvbnN0YW50cyBmcm9tIHdhcm5pbmcuCisgICAgICAgICMKKyAg
ICAgICAgIyBXZSBzdGlsbCBwcm9kdWNlIGFuIG92ZXJmbG93IHdhcm5pbmcgaWYgd2UgY2FuJ3Qg
cmVwcmVzZW50CisgICAgICAgICMgYSBoZXggY29uc3RhbnQgYXMgYW4gaW50ZWdlci4KKyAgICAg
ICAgcHJpbnQgUFJFQU1CTEUgIm5vIHdhcm5pbmdzIHF3KHBvcnRhYmxlKTtcbiI7CisKKwlmb3Jl
YWNoIChzb3J0IGtleXMgJWRlZmluZSkgeworCSAgICBpZiAoJG9wdF9EKSB7CisJCXByaW50IFBS
RUFNQkxFICIjICRfPSRkZWZpbmV7JF99XG4iOworCSAgICB9CisJICAgIGlmICgkZGVmaW5leyRf
fSA9fiAvXlwoKC4qKVwpJC8pIHsKKwkJIyBwYXJlbnRoZXNpemVkIHZhbHVlOiAgZD0odikKKwkJ
JGRlZmluZXskX30gPSAkMTsKKwkgICAgfQorCSAgICBpZiAoJGRlZmluZXskX30gPX4gL14oWyst
XT8oXGQrKT9cLlxkKyhbZUVdWystXT9cZCspPylbRkxdPyQvKSB7CisJCSMgZmxvYXQ6CisJCXBy
aW50IFBSRUFNQkxFCisJCSAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7ICQx
IH0gfVxuXG4iOworCSAgICB9IGVsc2lmICgkZGVmaW5leyRffSA9fiAvXihbKy1dP1xkKylVP0x7
MCwyfSQvaSkgeworCQkjIGludGVnZXI6CisJCXByaW50IFBSRUFNQkxFCisJCSAgICAidW5sZXNz
IChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7ICQxIH0gfVxuXG4iOworICAgICAgICAgICAgfSBl
bHNpZiAoJGRlZmluZXskX30gPX4gL14oWystXT8weFtcZGEtZl0rKVU/THswLDJ9JC9pKSB7Cisg
ICAgICAgICAgICAgICAgIyBoZXggaW50ZWdlcgorICAgICAgICAgICAgICAgICMgU3BlY2lhbCBj
YXNlZCwgc2luY2UgcGVybCB3YXJucyBvbiBoZXggaW50ZWdlcnMKKyAgICAgICAgICAgICAgICAj
IHRoYXQgY2FuJ3QgYmUgcmVwcmVzZW50ZWQgaW4gYSBVVi4KKyAgICAgICAgICAgICAgICAjCisg
ICAgICAgICAgICAgICAgIyBUaGlzIHdheSB3ZSBnZXQgdGhlIHdhcm5pbmcgYXQgdGltZSBvZiB1
c2UsIHNvIHRoZSB1c2VyCisgICAgICAgICAgICAgICAgIyBvbmx5IGdldHMgdGhlIHdhcm5pbmcg
aWYgdGhleSBoYXBwZW4gdG8gdXNlIHRoaXMKKyAgICAgICAgICAgICAgICAjIHBsYXRmb3JtLXNw
ZWNpZmljIGRlZmluaXRpb24uCisgICAgICAgICAgICAgICAgbXkgJGNvZGUgPSAkMTsKKyAgICAg
ICAgICAgICAgICAkY29kZSA9ICJoZXgoJyRjb2RlJykiIGlmIGxlbmd0aCAkY29kZSA+IDEwOwog
ICAgICAgICAgICAgICAgIHByaW50IFBSRUFNQkxFCi0gICAgICAgICAgICAgICAgICAgICJ1bmxl
c3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgXCIiLAotICAgICAgICAgICAgICAgICAgICBx
dW90ZW1ldGEoJGRlZmluZXskX30pLCAiXCIgfSB9XG5cbiI7Ci0gICAgICAgICAgICB9Ci0gICAg
ICAgIH0KKyAgICAgICAgICAgICAgICAgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRf
KCkgeyAkY29kZSB9IH1cblxuIjsKKwkgICAgfSBlbHNpZiAoJGRlZmluZXskX30gPX4gL15cdysk
LykgeworCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3Vi
ICRfKCkgeyAmJGRlZmluZXskX30gfSB9XG5cbiI7CisJICAgIH0gZWxzZSB7CisJCXByaW50IFBS
RUFNQkxFCisJCSAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7IFwiIiwKKwkJ
ICAgIHF1b3RlbWV0YSgkZGVmaW5leyRffSksICJcIiB9IH1cblxuIjsKKwkgICAgfQorCX0KICAg
ICBjbG9zZSBQUkVBTUJMRSAgICAgICAgICAgICAgIG9yIGRpZSAiQ2Fubm90IGNsb3NlICRwcmVh
bWJsZTogICQhIjsKIH0KIApAQCAtNTg2LDE1ICs3NDYsMTQgQEAgc3ViIF9leHRyYWN0X2NjX2Rl
ZmluZXMKIHsKICAgICBteSAlZGVmaW5lOwogICAgIG15ICRhbGxzeW1ib2xzICA9IGpvaW4gIiAi
LAotICAgICAgICBAQ29uZmlneydjY3N5bWJvbHMnLCAnY3Bwc3ltYm9scycsICdjcHBjY3N5bWJv
bHMnfTsKKwlAQ29uZmlneydjY3N5bWJvbHMnLCAnY3Bwc3ltYm9scycsICdjcHBjY3N5bWJvbHMn
fTsKIAogICAgICMgU3BsaXQgY29tcGlsZXIgcHJlLWRlZmluaXRpb25zIGludG8gYGtleT12YWx1
ZScgcGFpcnM6Ci0gICAgZm9yZWFjaCAoc3BsaXQgL1xzKy8sICRhbGxzeW1ib2xzKSB7Ci0gICAg
ICAgIC8oLis/KT0oLispLyBhbmQgJGRlZmluZXskMX0gPSAkMjsKLQotICAgICAgICBpZiAoJG9w
dF9EKSB7Ci0gICAgICAgICAgICBwcmludCBTVERFUlIgIiRfOiAgJDEgLT4gJDJcbiI7Ci0gICAg
ICAgIH0KKyAgICB3aGlsZSAoJGFsbHN5bWJvbHMgPX4gLyhbXlxzXSspPSgoXFxcc3xbXlxzXSkr
KS9nKSB7CisJJGRlZmluZXskMX0gPSAkMjsKKwlpZiAoJG9wdF9EKSB7CisJICAgIHByaW50IFNU
REVSUiAiJF86ICAkMSAtPiAkMlxuIjsKKwl9CiAgICAgfQogCiAgICAgcmV0dXJuICVkZWZpbmU7
CkBAIC02MjMsNiArNzgyLDEwIEBAIEl0IGlzIG1vc3QgZWFzaWx5IHJ1biB3aGlsZSBpbiAvdXNy
L2luY2x1ZGU6CiAKIAljZCAvdXNyL2luY2x1ZGU7IGgycGggKiBzeXMvKgogCitvcgorCisJY2Qg
L3Vzci9pbmNsdWRlOyBoMnBoICogc3lzLyogYXJwYS8qIG5ldGluZXQvKgorCiBvcgogCiAJY2Qg
L3Vzci9pbmNsdWRlOyBoMnBoIC1yIC1sIC4KQEAgLTY0MCw3ICs4MDMsNyBAQCBJZiBydW4gd2l0
aCBubyBhcmd1bWVudHMsIGZpbHRlcnMgc3RhbmRhcmQgaW5wdXQgdG8gc3RhbmRhcmQgb3V0cHV0
LgogPWl0ZW0gLWQgZGVzdGluYXRpb25fZGlyCiAKIFB1dCB0aGUgcmVzdWx0aW5nIEI8LnBoPiBm
aWxlcyBiZW5lYXRoIEI8ZGVzdGluYXRpb25fZGlyPiwgaW5zdGVhZCBvZgotYmVuZWF0aCB0aGUg
ZGVmYXVsdCBQZXJsIGxpYnJhcnkgbG9jYXRpb24gKEM8JENvbmZpZ3snaW5zdGFsbHNpdHNlYXJj
aCd9PikuCitiZW5lYXRoIHRoZSBkZWZhdWx0IFBlcmwgbGlicmFyeSBsb2NhdGlvbiAoQzwkQ29u
ZmlneydpbnN0YWxsc2l0ZWFyY2gnfT4pLgogCiA9aXRlbSAtcgogCkBAIC03MjUsMTAgKzg4OCwx
MCBAQCBpbnN0YWxsYXRpb24uCiBEb2Vzbid0IGhhbmRsZSBjb21wbGljYXRlZCBleHByZXNzaW9u
cyBidWlsdCBwaWVjZW1lYWwsIGEgbGE6CiAKICAgICBlbnVtIHsKLSAgICAgICAgRklSU1RfVkFM
VUUsCi0gICAgICAgIFNFQ09ORF9WQUxVRSwKKwlGSVJTVF9WQUxVRSwKKwlTRUNPTkRfVkFMVUUs
CiAgICAgI2lmZGVmIEFCQwotICAgICAgICBUSElSRF9WQUxVRQorCVRISVJEX1ZBTFVFCiAgICAg
I2VuZGlmCiAgICAgfTsKIAo=
UH2PH562
  }
  if ( $num < 5.007001 ) {
    _patch_b64(<<'UH2PH570');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTM2LDEzICszNiwxNiBAQAog
CiBwcmludCBPVVQgPDwnIU5PIVNVQlMhJzsKIAordXNlIHN0cmljdDsKKwogdXNlIENvbmZpZzsK
IHVzZSBGaWxlOjpQYXRoIHF3KG1rcGF0aCk7CiB1c2UgR2V0b3B0OjpTdGQ7CiAKIGdldG9wdHMo
J0RkOnJsaGFRJyk7Cit1c2UgdmFycyBxdygkb3B0X0QgJG9wdF9kICRvcHRfciAkb3B0X2wgJG9w
dF9oICRvcHRfYSAkb3B0X1EpOwogZGllICItciBhbmQgLWEgb3B0aW9ucyBhcmUgbXV0dWFsbHkg
ZXhjbHVzaXZlXG4iIGlmICgkb3B0X3IgYW5kICRvcHRfYSk7Ci1AaW5jX2RpcnMgPSBpbmNfZGly
cygpIGlmICRvcHRfYTsKK215IEBpbmNfZGlycyA9IGluY19kaXJzKCkgaWYgJG9wdF9hOwogCiBt
eSAkRXhpdCA9IDA7CiAKQEAgLTUwLDcgKzUzLDcgQEAKIGRpZSAiRGVzdGluYXRpb24gZGlyZWN0
b3J5ICREZXN0X2RpciBkb2Vzbid0IGV4aXN0IG9yIGlzbid0IGEgZGlyZWN0b3J5XG4iCiAgICAg
dW5sZXNzIC1kICREZXN0X2RpcjsKIAotQGlzYXR5cGUgPSBzcGxpdCgnICcsPDxFTkQpOworbXkg
QGlzYXR5cGUgPSBzcGxpdCgnICcsPDxFTkQpOwogCWNoYXIJdWNoYXIJdV9jaGFyCiAJc2hvcnQJ
dXNob3J0CXVfc2hvcnQKIAlpbnQJdWludAl1X2ludApAQCAtNTgsMTQgKzYxLDE4IEBACiAJRklM
RQlrZXlfdAljYWRkcl90CiBFTkQKIAorbXkgJWlzYXR5cGU7CiBAaXNhdHlwZXtAaXNhdHlwZX0g
PSAoMSkgeCBAaXNhdHlwZTsKLSRpbmlmID0gMDsKK215ICRpbmlmID0gMDsKK215ICVJc19jb252
ZXJ0ZWQ7CiAKIEBBUkdWID0gKCctJykgdW5sZXNzIEBBUkdWOwogCiBidWlsZF9wcmVhbWJsZV9p
Zl9uZWNlc3NhcnkoKTsKIAotd2hpbGUgKGRlZmluZWQgKCRmaWxlID0gbmV4dF9maWxlKCkpKSB7
CitteSAoJHQsICR0YWIsICVjdXJhcmdzLCAkbmV3LCAkZXZhbF9pbmRleCwgJGRpciwgJG5hbWUs
ICRhcmdzLCAkb3V0ZmlsZSk7CitteSAoJGluY2wsICRuZXh0KTsKK3doaWxlIChkZWZpbmVkICht
eSAkZmlsZSA9IG5leHRfZmlsZSgpKSkgewogICAgIGlmICgtbCAkZmlsZSBhbmQgLWQgJGZpbGUp
IHsKICAgICAgICAgbGlua19pZl9wb3NzaWJsZSgkZmlsZSkgaWYgKCRvcHRfbCk7CiAgICAgICAg
IG5leHQ7CkBAIC0xMDEsMjQgKzEwOCw3IEBACiAgICAgfQogCiAgICAgcHJpbnQgT1VUICJyZXF1
aXJlICdfaDJwaF9wcmUucGgnO1xuXG4iOwotICAgIHdoaWxlICg8SU4+KSB7Ci0JY2hvcDsKLQl3
aGlsZSAoL1xcJC8pIHsKLQkgICAgY2hvcDsKLQkgICAgJF8gLj0gPElOPjsKLQkgICAgY2hvcDsK
LQl9Ci0JcHJpbnQgT1VUICIjICRfXG4iIGlmICRvcHRfRDsKLQotCWlmIChzOi9cKjpcMjAwOmcp
IHsKLQkgICAgczpcKi86XDIwMTpnOwotCSAgICBzL1wyMDBbXlwyMDFdKlwyMDEvL2c7CSMgZGVs
ZXRlIHNpbmdsZSBsaW5lIGNvbW1lbnRzCi0JICAgIGlmIChzL1wyMDAuKi8vKSB7CQkjIGJlZ2lu
IG11bHRpLWxpbmUgY29tbWVudD8KLQkJJF8gLj0gJy8qJzsKLQkJJF8gLj0gPElOPjsKLQkJcmVk
bzsKLQkgICAgfQotCX0KKyAgICB3aGlsZSAoZGVmaW5lZCAobG9jYWwgJF8gPSBuZXh0X2xpbmUo
KSkpIHsKIAlpZiAocy9eXHMqXCNccyovLykgewogCSAgICBpZiAocy9eZGVmaW5lXHMrKFx3Kykv
LykgewogCQkkbmFtZSA9ICQxOwpAQCAtMTI5LDcgKzExOSw3IEBACiAgICAgCSAgICAJICAgIG15
ICRwcm90byA9ICcoKSAnOwogCQkgICAgaWYgKCRhcmdzIG5lICcnKSB7CiAgICAgCSAgICAJICAg
IAkkcHJvdG8gPSAnJzsKLQkJCWZvcmVhY2ggJGFyZyAoc3BsaXQoLyxccyovLCRhcmdzKSkgewor
CQkJZm9yZWFjaCBteSAkYXJnIChzcGxpdCgvLFxzKi8sJGFyZ3MpKSB7CiAJCQkgICAgJGFyZyA9
fiBzL15ccyooW15cc10uKlteXHNdKVxzKiQvJDEvOwogCQkJICAgICRjdXJhcmdzeyRhcmd9ID0g
MTsKIAkJCX0KQEAgLTI0OCwyMCArMjM4LDI0IEBACiAJICAgIH0gZWxzaWYoL15pZGVudFxzKygu
KikvKSB7CiAJCXByaW50IE9VVCAkdCwgIiMgJDFcbiI7CiAJICAgIH0KLSAJfSBlbHNpZigvXlxz
Kih0eXBlZGVmXHMqKT9lbnVtXHMqKFxzK1thLXpBLVpfXVx3KlxzKik/XHsvKSB7Ci0JICAgIHVu
dGlsKC9cfS4qPzsvKSB7Ci0JCWNob21wKCRuZXh0ID0gPElOPik7CisJfSBlbHNpZigvXlxzKih0
eXBlZGVmXHMqKT9lbnVtXHMqKFxzK1thLXpBLVpfXVx3KlxzKik/LykgeworCSAgICB1bnRpbCgv
XHtbXn1dKlx9Lio7LyB8fCAvOy8pIHsKKwkJbGFzdCB1bmxlc3MgZGVmaW5lZCAoJG5leHQgPSBu
ZXh0X2xpbmUoKSk7CisJCWNob21wICRuZXh0OworCQkjIGRyb3AgIiNkZWZpbmUgRk9PIEZPTyIg
aW4gZW51bXMKKwkJJG5leHQgPX4gcy9eXHMqI1xzKmRlZmluZVxzKyhcdyspXHMrXDFccyokLy87
CiAJCSRfIC49ICRuZXh0OwogCQlwcmludCBPVVQgIiMgJG5leHRcbiIgaWYgJG9wdF9EOwogCSAg
ICB9CisJICAgIHMvI1xzKmlmLio/I1xzKmVuZGlmLy9nOyAjIGRyb3AgI2lmZGVmcwogCSAgICBz
QC9cKi4qP1wqL0BAZzsKIAkgICAgcy9ccysvIC9nOwotCSAgICAvXlxzPyh0eXBlZGVmXHM/KT9l
bnVtXHM/KFthLXpBLVpfXVx3Kik/XHM/XHsoLiopXH1ccz8oW2EtekEtWl9dXHcqKT9ccz87LzsK
LQkgICAgKCRlbnVtX3N1YnMgPSAkMykgPX4gcy9ccy8vZzsKLQkgICAgQGVudW1fc3VicyA9IHNw
bGl0KC8sLywgJGVudW1fc3Vicyk7Ci0JICAgICRlbnVtX3ZhbCA9IC0xOwotCSAgICBmb3IgJGVu
dW0gKEBlbnVtX3N1YnMpIHsKLQkJKCRlbnVtX25hbWUsICRlbnVtX3ZhbHVlKSA9ICRlbnVtID1+
IC9eKFthLXpBLVpfXVx3KikoPS4rKT8kLzsKKwkgICAgbmV4dCB1bmxlc3MgL15ccz8odHlwZWRl
ZlxzPyk/ZW51bVxzPyhbYS16QS1aX11cdyopP1xzP1x7KC4qKVx9XHM/KFthLXpBLVpfXVx3Kik/
XHM/Oy87CisJICAgIChteSAkZW51bV9zdWJzID0gJDMpID1+IHMvXHMvL2c7CisJICAgIG15IEBl
bnVtX3N1YnMgPSBzcGxpdCgvLC8sICRlbnVtX3N1YnMpOworCSAgICBteSAkZW51bV92YWwgPSAt
MTsKKwkgICAgZm9yZWFjaCBteSAkZW51bSAoQGVudW1fc3VicykgeworCQlteSAoJGVudW1fbmFt
ZSwgJGVudW1fdmFsdWUpID0gJGVudW0gPX4gL14oW2EtekEtWl9dXHcqKSg9LispPyQvOwogCQkk
ZW51bV92YWx1ZSA9fiBzL149Ly87CiAJCSRlbnVtX3ZhbCA9IChsZW5ndGgoJGVudW1fdmFsdWUp
ID8gJGVudW1fdmFsdWUgOiAkZW51bV92YWwgKyAxKTsKIAkJaWYgKCRvcHRfaCkgewpAQCAtMjgw
LDEyICsyNzQsMTMgQEAKICAgICB9CiAgICAgcHJpbnQgT1VUICIxO1xuIjsKIAotICAgICRpc19j
b252ZXJ0ZWR7JGZpbGV9ID0gMTsKKyAgICAkSXNfY29udmVydGVkeyRmaWxlfSA9IDE7CiAgICAg
cXVldWVfaW5jbHVkZXNfZnJvbSgkZmlsZSkgaWYgKCRvcHRfYSk7CiB9CiAKIGV4aXQgJEV4aXQ7
CiAKKwogc3ViIHJlaW5kZW50KCQpIHsKICAgICBteSgkdGV4dCkgPSBzaGlmdDsKICAgICAkdGV4
dCA9fiBzL1xuL1xuICAgIC9nOwpAQCAtMjkzLDkgKzI4OCwxMSBAQAogICAgICR0ZXh0OwogfQog
CisKIHN1YiBleHByIHsKKyAgICBteSAkam9pbmVkX2FyZ3M7CiAgICAgaWYoa2V5cyglY3VyYXJn
cykpIHsKLQlteSgkam9pbmVkX2FyZ3MpID0gam9pbignfCcsIGtleXMoJWN1cmFyZ3MpKTsKKwkk
am9pbmVkX2FyZ3MgPSBqb2luKCd8Jywga2V5cyglY3VyYXJncykpOwogICAgIH0KICAgICB3aGls
ZSAoJF8gbmUgJycpIHsKIAlzL15cJlwmLy8gJiYgZG8geyAkbmV3IC49ICIgJiYiOyBuZXh0O307
ICMgaGFuZGxlICYmIG9wZXJhdG9yCkBAIC0zNDEsMTMgKzMzOCwxMyBAQAogCSMgRWxpbWluYXRl
IHR5cGVkZWZzCiAJL1woKFtcd1xzXSspW1wqXHNdKlwpXHMqW1x3XChdLyAmJiBkbyB7CiAJICAg
IGZvcmVhY2ggKHNwbGl0IC9ccysvLCAkMSkgeyAgIyBNYWtlIHN1cmUgYWxsIHRoZSB3b3JkcyBh
cmUgdHlwZXMsCi0JCWxhc3QgdW5sZXNzICgkaXNhdHlwZXskX30gb3IgJF8gZXEgJ3N0cnVjdCcp
OworCQlsYXN0IHVubGVzcyAoJGlzYXR5cGV7JF99IG9yICRfIGVxICdzdHJ1Y3QnIG9yICRfIGVx
ICd1bmlvbicpOwogCSAgICB9CiAJICAgIHMvXChbXHdcc10rW1wqXHNdKlwpLy8gJiYgbmV4dDsg
ICAgICAjIHRoZW4gZWxpbWluYXRlIHRoZW0uCiAJfTsKIAkjIHN0cnVjdC91bmlvbiBtZW1iZXIs
IGluY2x1ZGluZyBhcnJheXM6CiAJcy9eKFtfQS1aXVx3KihcW1teXF1dK1xdKT8oKFwufC0+KVtf
QS1aXVx3KihcW1teXF1dK1xdKT8pKykvL2kgJiYgZG8gewotCSAgICAkaWQgPSAkMTsKKwkgICAg
bXkgJGlkID0gJDE7CiAJICAgICRpZCA9fiBzLyhcLnwoLT4pKShbXlwuXC1dKikvLT5ceyQzXH0v
ZzsKIAkgICAgJGlkID1+IHMvXGIoW15cJF0pKCRqb2luZWRfYXJncykvJDFcJCQyL2cgaWYgbGVu
Z3RoKCRqb2luZWRfYXJncyk7CiAJICAgIHdoaWxlKCRpZCA9fiAvXFtccyooW15cJFwmXGRcXV0r
KVxdLykgewpAQCAtMzYzLDggKzM2MCw4IEBACiAJICAgICRuZXcgLj0gIiAoXCQkaWQpIjsKIAl9
OwogCXMvXihbX2EtekEtWl1cdyopLy8JJiYgZG8gewotCSAgICAkaWQgPSAkMTsKLQkgICAgaWYg
KCRpZCBlcSAnc3RydWN0JykgeworCSAgICBteSAkaWQgPSAkMTsKKwkgICAgaWYgKCRpZCBlcSAn
c3RydWN0JyB8fCAkaWQgZXEgJ3VuaW9uJykgewogCQlzL15ccysoXHcrKS8vOwogCQkkaWQgLj0g
JyAnIC4gJDE7CiAJCSRpc2F0eXBleyRpZH0gPSAxOwpAQCAtMzc3LDggKzM3NCw4IEBACiAJCSRu
ZXcgLj0gJy0+JyBpZiAvXltcW1x7XS87CiAJICAgIH0gZWxzaWYgKCRpZCBlcSAnZGVmaW5lZCcp
IHsKIAkJJG5ldyAuPSAnZGVmaW5lZCc7Ci0JICAgIH0gZWxzaWYgKC9eXCgvKSB7Ci0JCXMvXlwo
KFx3KSwvKCIkMSIsLyBpZiAkaWQgPX4gL15fSU9bV1JdKiQvaTsJIyBjaGVhdAorCSAgICB9IGVs
c2lmICgvXlxzKlwoLykgeworCQlzL15ccypcKChcdyksLygiJDEiLC8gaWYgJGlkID1+IC9eX0lP
W1dSXSokL2k7CSMgY2hlYXQKIAkJJG5ldyAuPSAiICYkaWQiOwogCSAgICB9IGVsc2lmICgkaXNh
dHlwZXskaWR9KSB7CiAJCWlmICgkbmV3ID1+IC97XHMqJC8pIHsKQEAgLTQwNSw2ICs0MDIsNjYg
QEAKIH0KIAogCitzdWIgbmV4dF9saW5lCit7CisgICAgbXkgKCRpbiwgJG91dCk7CisgICAgbXkg
JHByZV9zdWJfdHJpX2dyYXBocyA9IDE7CisKKyAgICBSRUFEOiB3aGlsZSAobm90IGVvZiBJTikg
eworICAgICAgICAkaW4gIC49IDxJTj47CisgICAgICAgIGNob21wICRpbjsKKyAgICAgICAgbmV4
dCB1bmxlc3MgbGVuZ3RoICRpbjsKKworICAgICAgICB3aGlsZSAobGVuZ3RoICRpbikgeworICAg
ICAgICAgICAgaWYgKCRwcmVfc3ViX3RyaV9ncmFwaHMpIHsKKyAgICAgICAgICAgICAgICAjIFBy
ZXByb2Nlc3MgYWxsIHRyaS1ncmFwaHMgCisgICAgICAgICAgICAgICAgIyBpbmNsdWRpbmcgdGhp
bmdzIHN0dWNrIGluIHF1b3RlZCBzdHJpbmcgY29uc3RhbnRzLgorICAgICAgICAgICAgICAgICRp
biA9fiBzL1w/XD89LyMvZzsgICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/PXwgICN8Cisg
ICAgICAgICAgICAgICAgJGluID1+IHMvXD9cP1whL3wvZzsgICAgICAgICAgICAgICAgICAgICAg
ICAjIHwgPz8hfCAgfHwKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9cP1w/Jy9eL2c7ICAgICAg
ICAgICAgICAgICAgICAgICAgICMgfCA/Pyd8ICBefAorICAgICAgICAgICAgICAgICRpbiA9fiBz
L1w/XD9cKC9bL2c7ICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/KHwgIFt8CisgICAgICAg
ICAgICAgICAgJGluID1+IHMvXD9cP1wpL10vZzsgICAgICAgICAgICAgICAgICAgICAgICAjIHwg
Pz8pfCAgXXwKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9cP1w/XC0vfi9nOyAgICAgICAgICAg
ICAgICAgICAgICAgICMgfCA/Py18ICB+fAorICAgICAgICAgICAgICAgICRpbiA9fiBzL1w/XD9c
Ly9cXC9nOyAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/L3wgIFx8CisgICAgICAgICAgICAg
ICAgJGluID1+IHMvXD9cPzwvey9nOyAgICAgICAgICAgICAgICAgICAgICAgICAjIHwgPz88fCAg
e3wKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9cP1w/Pi99L2c7ICAgICAgICAgICAgICAgICAg
ICAgICAgICMgfCA/Pz58ICB9fAorICAgICAgICAgICAgfQorICAgICAgICAgICAgaWYgKCRpbiA9
fiBzL1xcJC8vKSB7ICAgICAgICAgICAgICAgICAgICAgICAgICAgIyBcLW5ld2xpbmUKKyAgICAg
ICAgICAgICAgICAkb3V0ICAgIC49ICcgJzsKKyAgICAgICAgICAgICAgICBuZXh0IFJFQUQ7Cisg
ICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKFteIidcXFwvXSspLy8pIHsgICAgICAgICAg
ICAjIFBhc3N0aHJvdWdoCisgICAgICAgICAgICAgICAgJG91dCAgICAuPSAkMTsKKyAgICAgICAg
ICAgIH0gZWxzaWYgKCRpbiA9fiBzL14oXFwuKS8vKSB7ICAgICAgICAgICAgICAgICAgICMgXC4u
LgorICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7CisgICAgICAgICAgICB9IGVsc2lmICgk
aW4gPX4gcy9eKCcoXFwufFteJ1xcXSkqJykvLykgeyAgICAgICAjICcuLi4KKyAgICAgICAgICAg
ICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAgfSBlbHNpZiAoJGluID1+IHMvXigiKFxc
LnxbXiJcXF0pKiIpLy8pIHsgICAgICAgIyAiLi4uCisgICAgICAgICAgICAgICAgJG91dCAgICAu
PSAkMTsKKyAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiBzL15cL1wvLiovLykgeyAgICAgICAg
ICAgICAgICAgICMgLy8uLi4KKyAgICAgICAgICAgICAgICAjIGZhbGwgdGhyb3VnaAorICAgICAg
ICAgICAgfSBlbHNpZiAoJGluID1+IG0vXlwvXCovKSB7ICAgICAgICAgICAgICAgICAgICAgIyAv
Ki4uLgorICAgICAgICAgICAgICAgICMgQyBjb21tZW50IHJlbW92YWwgYWRhcHRlZCBmcm9tIHBl
cmxmYXE2OgorICAgICAgICAgICAgICAgIGlmICgkaW4gPX4gcy9eXC9cKlteKl0qXCorKFteXC8q
XVteKl0qXCorKSpcLy8vKSB7CisgICAgICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJyAnOwor
ICAgICAgICAgICAgICAgIH0gZWxzZSB7ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgIyBJbmNvbXBsZXRlIC8qICovCisgICAgICAgICAgICAgICAgICAgIG5leHQgUkVBRDsKKyAg
ICAgICAgICAgICAgICB9CisgICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKFwvKS8vKSB7
ICAgICAgICAgICAgICAgICAgICAjIC8uLi4KKyAgICAgICAgICAgICAgICAkb3V0ICAgIC49ICQx
OworICAgICAgICAgICAgfSBlbHNpZiAoJGluID1+IHMvXihbXlwnXCJcXFwvXSspLy8pIHsKKyAg
ICAgICAgICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAgfSBlbHNlIHsKKyAgICAg
ICAgICAgICAgICBkaWUgIkNhbm5vdCBwYXJzZTpcbiRpblxuIjsKKyAgICAgICAgICAgIH0KKyAg
ICAgICAgfQorCisgICAgICAgIGxhc3QgUkVBRCBpZiAkb3V0ID1+IC9cUy87CisgICAgfQorCisg
ICAgcmV0dXJuICRvdXQ7Cit9CisKKwogIyBIYW5kbGUgcmVjdXJzaXZlIHN1YmRpcmVjdG9yaWVz
IHdpdGhvdXQgZ2V0dGluZyBhIGdyb3Rlc3F1ZWx5IGJpZyBzdGFjay4KICMgQ291bGQgdGhpcyBi
ZSBpbXBsZW1lbnRlZCB1c2luZyBGaWxlOjpGaW5kPwogc3ViIG5leHRfZmlsZQpAQCAtNTA1LDcg
KzU2Miw3IEBACiAgICAgICAgICAgICB9CiAKICAgICAgICAgICAgIGlmICgkbGluZSA9fiAvXiNc
cyppbmNsdWRlXHMrPCguKj8pPi8pIHsKLSAgICAgICAgICAgICAgICBwdXNoKEBBUkdWLCAkMSkg
dW5sZXNzICRpc19jb252ZXJ0ZWR7JDF9OworICAgICAgICAgICAgICAgIHB1c2goQEFSR1YsICQx
KSB1bmxlc3MgJElzX2NvbnZlcnRlZHskMX07CiAgICAgICAgICAgICB9CiAgICAgICAgIH0KICAg
ICBjbG9zZSBIRUFERVI7CkBAIC01NTMsOSArNjEwLDkgQEAKICAgICAgICAgICAgICAgICBwcmlu
dCBQUkVBTUJMRSAiIyAkXz0kZGVmaW5leyRffVxuIjsKICAgICAgICAgICAgIH0KIAotICAgICAg
ICAgICAgaWYgKCRkZWZpbmV7JF99ID1+IC9eXGQrJC8pIHsKKyAgICAgICAgICAgIGlmICgkZGVm
aW5leyRffSA9fiAvXihcZCspVT9MezAsMn0kL2kpIHsKICAgICAgICAgICAgICAgICBwcmludCBQ
UkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIg
JF8oKSB7ICRkZWZpbmV7JF99IH0gfVxuXG4iOworICAgICAgICAgICAgICAgICAgICAidW5sZXNz
IChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7ICQxIH0gfVxuXG4iOwogICAgICAgICAgICAgfSBl
bHNpZiAoJGRlZmluZXskX30gPX4gL15cdyskLykgewogICAgICAgICAgICAgICAgIHByaW50IFBS
RUFNQkxFCiAgICAgICAgICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAk
XygpIHsgJiRkZWZpbmV7JF99IH0gfVxuXG4iOwpAQCAtNTc1LDcgKzYzMiw4IEBACiBzdWIgX2V4
dHJhY3RfY2NfZGVmaW5lcwogewogICAgIG15ICVkZWZpbmU7Ci0gICAgbXkgJGFsbHN5bWJvbHMg
PSBqb2luICIgIiwgQENvbmZpZ3tjY3N5bWJvbHMsIGNwcHN5bWJvbHMsIGNwcGNjc3ltYm9sc307
CisgICAgbXkgJGFsbHN5bWJvbHMgID0gam9pbiAiICIsCisgICAgICAgIEBDb25maWd7J2Njc3lt
Ym9scycsICdjcHBzeW1ib2xzJywgJ2NwcGNjc3ltYm9scyd9OwogCiAgICAgIyBTcGxpdCBjb21w
aWxlciBwcmUtZGVmaW5pdGlvbnMgaW50byBga2V5PXZhbHVlJyBwYWlyczoKICAgICBmb3JlYWNo
IChzcGxpdCAvXHMrLywgJGFsbHN5bWJvbHMpIHsKQEAgLTcwOCw4ICs3NjYsNiBAQAogSXQncyBv
bmx5IGludGVuZGVkIGFzIGEgcm91Z2ggdG9vbC4KIFlvdSBtYXkgbmVlZCB0byBkaWNrZXIgd2l0
aCB0aGUgZmlsZXMgcHJvZHVjZWQuCiAKLURvZXNuJ3QgcnVuIHdpdGggQzx1c2Ugc3RyaWN0Pgot
CiBZb3UgaGF2ZSB0byBydW4gdGhpcyBwcm9ncmFtIGJ5IGhhbmQ7IGl0J3Mgbm90IHJ1biBhcyBw
YXJ0IG9mIHRoZSBQZXJsCiBpbnN0YWxsYXRpb24uCiAK
UH2PH570
  }
  elsif ( $num < 5.007002 ) {
    _patch_b64(<<'UH2PH571');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTEwOCwyNCArMTA4LDcgQEAK
ICAgICB9CiAKICAgICBwcmludCBPVVQgInJlcXVpcmUgJ19oMnBoX3ByZS5waCc7XG5cbiI7Ci0g
ICAgd2hpbGUgKDxJTj4pIHsKLQljaG9wOwotCXdoaWxlICgvXFwkLykgewotCSAgICBjaG9wOwot
CSAgICAkXyAuPSA8SU4+OwotCSAgICBjaG9wOwotCX0KLQlwcmludCBPVVQgIiMgJF9cbiIgaWYg
JG9wdF9EOwotCi0JaWYgKHM6L1wqOlwyMDA6ZykgewotCSAgICBzOlwqLzpcMjAxOmc7Ci0JICAg
IHMvXDIwMFteXDIwMV0qXDIwMS8vZzsJIyBkZWxldGUgc2luZ2xlIGxpbmUgY29tbWVudHMKLQkg
ICAgaWYgKHMvXDIwMC4qLy8pIHsJCSMgYmVnaW4gbXVsdGktbGluZSBjb21tZW50PwotCQkkXyAu
PSAnLyonOwotCQkkXyAuPSA8SU4+OwotCQlyZWRvOwotCSAgICB9Ci0JfQorICAgIHdoaWxlIChk
ZWZpbmVkIChsb2NhbCAkXyA9IG5leHRfbGluZSgpKSkgewogCWlmIChzL15ccypcI1xzKi8vKSB7
CiAJICAgIGlmIChzL15kZWZpbmVccysoXHcrKS8vKSB7CiAJCSRuYW1lID0gJDE7CkBAIC0yNTUs
MTUgKzIzOCwxOSBAQAogCSAgICB9IGVsc2lmKC9eaWRlbnRccysoLiopLykgewogCQlwcmludCBP
VVQgJHQsICIjICQxXG4iOwogCSAgICB9Ci0gCX0gZWxzaWYoL15ccyoodHlwZWRlZlxzKik/ZW51
bVxzKihccytbYS16QS1aX11cdypccyopP1x7LykgewotCSAgICB1bnRpbCgvXH0uKj87Lykgewot
CQljaG9tcCgkbmV4dCA9IDxJTj4pOworCX0gZWxzaWYoL15ccyoodHlwZWRlZlxzKik/ZW51bVxz
KihccytbYS16QS1aX11cdypccyopPy8pIHsKKwkgICAgdW50aWwoL1x7W159XSpcfS4qOy8gfHwg
LzsvKSB7CisJCWxhc3QgdW5sZXNzIGRlZmluZWQgKCRuZXh0ID0gbmV4dF9saW5lKCkpOworCQlj
aG9tcCAkbmV4dDsKKwkJIyBkcm9wICIjZGVmaW5lIEZPTyBGT08iIGluIGVudW1zCisJCSRuZXh0
ID1+IHMvXlxzKiNccypkZWZpbmVccysoXHcrKVxzK1wxXHMqJC8vOwogCQkkXyAuPSAkbmV4dDsK
IAkJcHJpbnQgT1VUICIjICRuZXh0XG4iIGlmICRvcHRfRDsKIAkgICAgfQorCSAgICBzLyNccypp
Zi4qPyNccyplbmRpZi8vZzsgIyBkcm9wICNpZmRlZnMKIAkgICAgc0AvXCouKj9cKi9AQGc7CiAJ
ICAgIHMvXHMrLyAvZzsKLQkgICAgL15ccz8odHlwZWRlZlxzPyk/ZW51bVxzPyhbYS16QS1aX11c
dyopP1xzP1x7KC4qKVx9XHM/KFthLXpBLVpfXVx3Kik/XHM/Oy87CisJICAgIG5leHQgdW5sZXNz
IC9eXHM/KHR5cGVkZWZccz8pP2VudW1ccz8oW2EtekEtWl9dXHcqKT9ccz9ceyguKilcfVxzPyhb
YS16QS1aX11cdyopP1xzPzsvOwogCSAgICAobXkgJGVudW1fc3VicyA9ICQzKSA9fiBzL1xzLy9n
OwogCSAgICBteSBAZW51bV9zdWJzID0gc3BsaXQoLywvLCAkZW51bV9zdWJzKTsKIAkgICAgbXkg
JGVudW1fdmFsID0gLTE7CkBAIC0zNTEsNyArMzM4LDcgQEAKIAkjIEVsaW1pbmF0ZSB0eXBlZGVm
cwogCS9cKChbXHdcc10rKVtcKlxzXSpcKVxzKltcd1woXS8gJiYgZG8gewogCSAgICBmb3JlYWNo
IChzcGxpdCAvXHMrLywgJDEpIHsgICMgTWFrZSBzdXJlIGFsbCB0aGUgd29yZHMgYXJlIHR5cGVz
LAotCQlsYXN0IHVubGVzcyAoJGlzYXR5cGV7JF99IG9yICRfIGVxICdzdHJ1Y3QnKTsKKwkJbGFz
dCB1bmxlc3MgKCRpc2F0eXBleyRffSBvciAkXyBlcSAnc3RydWN0JyBvciAkXyBlcSAndW5pb24n
KTsKIAkgICAgfQogCSAgICBzL1woW1x3XHNdK1tcKlxzXSpcKS8vICYmIG5leHQ7ICAgICAgIyB0
aGVuIGVsaW1pbmF0ZSB0aGVtLgogCX07CkBAIC0zNzQsNyArMzYxLDcgQEAKIAl9OwogCXMvXihb
X2EtekEtWl1cdyopLy8JJiYgZG8gewogCSAgICBteSAkaWQgPSAkMTsKLQkgICAgaWYgKCRpZCBl
cSAnc3RydWN0JykgeworCSAgICBpZiAoJGlkIGVxICdzdHJ1Y3QnIHx8ICRpZCBlcSAndW5pb24n
KSB7CiAJCXMvXlxzKyhcdyspLy87CiAJCSRpZCAuPSAnICcgLiAkMTsKIAkJJGlzYXR5cGV7JGlk
fSA9IDE7CkBAIC0zODcsOCArMzc0LDggQEAKIAkJJG5ldyAuPSAnLT4nIGlmIC9eW1xbXHtdLzsK
IAkgICAgfSBlbHNpZiAoJGlkIGVxICdkZWZpbmVkJykgewogCQkkbmV3IC49ICdkZWZpbmVkJzsK
LQkgICAgfSBlbHNpZiAoL15cKC8pIHsKLQkJcy9eXCgoXHcpLC8oIiQxIiwvIGlmICRpZCA9fiAv
Xl9JT1tXUl0qJC9pOwkjIGNoZWF0CisJICAgIH0gZWxzaWYgKC9eXHMqXCgvKSB7CisJCXMvXlxz
KlwoKFx3KSwvKCIkMSIsLyBpZiAkaWQgPX4gL15fSU9bV1JdKiQvaTsJIyBjaGVhdAogCQkkbmV3
IC49ICIgJiRpZCI7CiAJICAgIH0gZWxzaWYgKCRpc2F0eXBleyRpZH0pIHsKIAkJaWYgKCRuZXcg
PX4gL3tccyokLykgewpAQCAtNDE1LDYgKzQwMiw2NiBAQAogfQogCiAKK3N1YiBuZXh0X2xpbmUK
K3sKKyAgICBteSAoJGluLCAkb3V0KTsKKyAgICBteSAkcHJlX3N1Yl90cmlfZ3JhcGhzID0gMTsK
KworICAgIFJFQUQ6IHdoaWxlIChub3QgZW9mIElOKSB7CisgICAgICAgICRpbiAgLj0gPElOPjsK
KyAgICAgICAgY2hvbXAgJGluOworICAgICAgICBuZXh0IHVubGVzcyBsZW5ndGggJGluOworCisg
ICAgICAgIHdoaWxlIChsZW5ndGggJGluKSB7CisgICAgICAgICAgICBpZiAoJHByZV9zdWJfdHJp
X2dyYXBocykgeworICAgICAgICAgICAgICAgICMgUHJlcHJvY2VzcyBhbGwgdHJpLWdyYXBocyAK
KyAgICAgICAgICAgICAgICAjIGluY2x1ZGluZyB0aGluZ3Mgc3R1Y2sgaW4gcXVvdGVkIHN0cmlu
ZyBjb25zdGFudHMuCisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cPz0vIy9nOyAgICAgICAg
ICAgICAgICAgICAgICAgICAjIHwgPz89fCAgI3wKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9c
P1w/XCEvfC9nOyAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/PyF8ICB8fAorICAgICAgICAg
ICAgICAgICRpbiA9fiBzL1w/XD8nL14vZzsgICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/
J3wgIF58CisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cP1woL1svZzsgICAgICAgICAgICAg
ICAgICAgICAgICAjIHwgPz8ofCAgW3wKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9cP1w/XCkv
XS9nOyAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/Pyl8ICBdfAorICAgICAgICAgICAgICAg
ICRpbiA9fiBzL1w/XD9cLS9+L2c7ICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/LXwgIH58
CisgICAgICAgICAgICAgICAgJGluID1+IHMvXD9cP1wvL1xcL2c7ICAgICAgICAgICAgICAgICAg
ICAgICAjIHwgPz8vfCAgXHwKKyAgICAgICAgICAgICAgICAkaW4gPX4gcy9cP1w/PC97L2c7ICAg
ICAgICAgICAgICAgICAgICAgICAgICMgfCA/Pzx8ICB7fAorICAgICAgICAgICAgICAgICRpbiA9
fiBzL1w/XD8+L30vZzsgICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/PnwgIH18CisgICAg
ICAgICAgICB9CisgICAgICAgICAgICBpZiAoJGluID1+IHMvXFwkLy8pIHsgICAgICAgICAgICAg
ICAgICAgICAgICAgICAjIFwtbmV3bGluZQorICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJyAn
OworICAgICAgICAgICAgICAgIG5leHQgUkVBRDsKKyAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9
fiBzL14oW14iJ1xcXC9dKykvLykgeyAgICAgICAgICAgICMgUGFzc3Rocm91Z2gKKyAgICAgICAg
ICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAgfSBlbHNpZiAoJGluID1+IHMvXihc
XC4pLy8pIHsgICAgICAgICAgICAgICAgICAgIyBcLi4uCisgICAgICAgICAgICAgICAgJG91dCAg
ICAuPSAkMTsKKyAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiBzL14oJyhcXC58W14nXFxdKSon
KS8vKSB7ICAgICAgICMgJy4uLgorICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7CisgICAg
ICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKCIoXFwufFteIlxcXSkqIikvLykgeyAgICAgICAj
ICIuLi4KKyAgICAgICAgICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAgfSBlbHNp
ZiAoJGluID1+IHMvXlwvXC8uKi8vKSB7ICAgICAgICAgICAgICAgICAgIyAvLy4uLgorICAgICAg
ICAgICAgICAgICMgZmFsbCB0aHJvdWdoCisgICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gbS9e
XC9cKi8pIHsgICAgICAgICAgICAgICAgICAgICAjIC8qLi4uCisgICAgICAgICAgICAgICAgIyBD
IGNvbW1lbnQgcmVtb3ZhbCBhZGFwdGVkIGZyb20gcGVybGZhcTY6CisgICAgICAgICAgICAgICAg
aWYgKCRpbiA9fiBzL15cL1wqW14qXSpcKisoW15cLypdW14qXSpcKispKlwvLy8pIHsKKyAgICAg
ICAgICAgICAgICAgICAgJG91dCAgICAuPSAnICc7CisgICAgICAgICAgICAgICAgfSBlbHNlIHsg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAjIEluY29tcGxldGUgLyogKi8KKyAg
ICAgICAgICAgICAgICAgICAgbmV4dCBSRUFEOworICAgICAgICAgICAgICAgIH0KKyAgICAgICAg
ICAgIH0gZWxzaWYgKCRpbiA9fiBzL14oXC8pLy8pIHsgICAgICAgICAgICAgICAgICAgICMgLy4u
LgorICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7CisgICAgICAgICAgICB9IGVsc2lmICgk
aW4gPX4gcy9eKFteXCdcIlxcXC9dKykvLykgeworICAgICAgICAgICAgICAgICRvdXQgICAgLj0g
JDE7CisgICAgICAgICAgICB9IGVsc2UgeworICAgICAgICAgICAgICAgIGRpZSAiQ2Fubm90IHBh
cnNlOlxuJGluXG4iOworICAgICAgICAgICAgfQorICAgICAgICB9CisKKyAgICAgICAgbGFzdCBS
RUFEIGlmICRvdXQgPX4gL1xTLzsKKyAgICB9CisKKyAgICByZXR1cm4gJG91dDsKK30KKworCiAj
IEhhbmRsZSByZWN1cnNpdmUgc3ViZGlyZWN0b3JpZXMgd2l0aG91dCBnZXR0aW5nIGEgZ3JvdGVz
cXVlbHkgYmlnIHN0YWNrLgogIyBDb3VsZCB0aGlzIGJlIGltcGxlbWVudGVkIHVzaW5nIEZpbGU6
OkZpbmQ/CiBzdWIgbmV4dF9maWxlCkBAIC01NjMsOSArNjEwLDkgQEAKICAgICAgICAgICAgICAg
ICBwcmludCBQUkVBTUJMRSAiIyAkXz0kZGVmaW5leyRffVxuIjsKICAgICAgICAgICAgIH0KIAot
ICAgICAgICAgICAgaWYgKCRkZWZpbmV7JF99ID1+IC9eXGQrJC8pIHsKKyAgICAgICAgICAgIGlm
ICgkZGVmaW5leyRffSA9fiAvXihcZCspVT9MezAsMn0kL2kpIHsKICAgICAgICAgICAgICAgICBw
cmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykg
eyBzdWIgJF8oKSB7ICRkZWZpbmV7JF99IH0gfVxuXG4iOworICAgICAgICAgICAgICAgICAgICAi
dW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7ICQxIH0gfVxuXG4iOwogICAgICAgICAg
ICAgfSBlbHNpZiAoJGRlZmluZXskX30gPX4gL15cdyskLykgewogICAgICAgICAgICAgICAgIHBy
aW50IFBSRUFNQkxFCiAgICAgICAgICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7
IHN1YiAkXygpIHsgJiRkZWZpbmV7JF99IH0gfVxuXG4iOwo=
UH2PH571
  }
  elsif ( $num < 5.007003 ) {
    _patch_b64(<<'UH2PH572');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTIzOCwxNSArMjM4LDE5IEBA
CiAJICAgIH0gZWxzaWYoL15pZGVudFxzKyguKikvKSB7CiAJCXByaW50IE9VVCAkdCwgIiMgJDFc
biI7CiAJICAgIH0KLSAJfSBlbHNpZigvXlxzKih0eXBlZGVmXHMqKT9lbnVtXHMqKFxzK1thLXpB
LVpfXVx3KlxzKik/XHsvKSB7Ci0JICAgIHVudGlsKC9cfS4qPzsvKSB7Ci0JCWNob21wKCRuZXh0
ID0gPElOPik7CisJfSBlbHNpZigvXlxzKih0eXBlZGVmXHMqKT9lbnVtXHMqKFxzK1thLXpBLVpf
XVx3KlxzKik/LykgeworCSAgICB1bnRpbCgvXHtbXn1dKlx9Lio7LyB8fCAvOy8pIHsKKwkJbGFz
dCB1bmxlc3MgZGVmaW5lZCAoJG5leHQgPSBuZXh0X2xpbmUoKSk7CisJCWNob21wICRuZXh0Owor
CQkjIGRyb3AgIiNkZWZpbmUgRk9PIEZPTyIgaW4gZW51bXMKKwkJJG5leHQgPX4gcy9eXHMqI1xz
KmRlZmluZVxzKyhcdyspXHMrXDFccyokLy87CiAJCSRfIC49ICRuZXh0OwogCQlwcmludCBPVVQg
IiMgJG5leHRcbiIgaWYgJG9wdF9EOwogCSAgICB9CisJICAgIHMvI1xzKmlmLio/I1xzKmVuZGlm
Ly9nOyAjIGRyb3AgI2lmZGVmcwogCSAgICBzQC9cKi4qP1wqL0BAZzsKIAkgICAgcy9ccysvIC9n
OwotCSAgICAvXlxzPyh0eXBlZGVmXHM/KT9lbnVtXHM/KFthLXpBLVpfXVx3Kik/XHM/XHsoLiop
XH1ccz8oW2EtekEtWl9dXHcqKT9ccz87LzsKKwkgICAgbmV4dCB1bmxlc3MgL15ccz8odHlwZWRl
ZlxzPyk/ZW51bVxzPyhbYS16QS1aX11cdyopP1xzP1x7KC4qKVx9XHM/KFthLXpBLVpfXVx3Kik/
XHM/Oy87CiAJICAgIChteSAkZW51bV9zdWJzID0gJDMpID1+IHMvXHMvL2c7CiAJICAgIG15IEBl
bnVtX3N1YnMgPSBzcGxpdCgvLC8sICRlbnVtX3N1YnMpOwogCSAgICBteSAkZW51bV92YWwgPSAt
MTsKQEAgLTMzNCw3ICszMzgsNyBAQAogCSMgRWxpbWluYXRlIHR5cGVkZWZzCiAJL1woKFtcd1xz
XSspW1wqXHNdKlwpXHMqW1x3XChdLyAmJiBkbyB7CiAJICAgIGZvcmVhY2ggKHNwbGl0IC9ccysv
LCAkMSkgeyAgIyBNYWtlIHN1cmUgYWxsIHRoZSB3b3JkcyBhcmUgdHlwZXMsCi0JCWxhc3QgdW5s
ZXNzICgkaXNhdHlwZXskX30gb3IgJF8gZXEgJ3N0cnVjdCcpOworCQlsYXN0IHVubGVzcyAoJGlz
YXR5cGV7JF99IG9yICRfIGVxICdzdHJ1Y3QnIG9yICRfIGVxICd1bmlvbicpOwogCSAgICB9CiAJ
ICAgIHMvXChbXHdcc10rW1wqXHNdKlwpLy8gJiYgbmV4dDsgICAgICAjIHRoZW4gZWxpbWluYXRl
IHRoZW0uCiAJfTsKQEAgLTM1Nyw3ICszNjEsNyBAQAogCX07CiAJcy9eKFtfYS16QS1aXVx3Kikv
LwkmJiBkbyB7CiAJICAgIG15ICRpZCA9ICQxOwotCSAgICBpZiAoJGlkIGVxICdzdHJ1Y3QnKSB7
CisJICAgIGlmICgkaWQgZXEgJ3N0cnVjdCcgfHwgJGlkIGVxICd1bmlvbicpIHsKIAkJcy9eXHMr
KFx3KykvLzsKIAkJJGlkIC49ICcgJyAuICQxOwogCQkkaXNhdHlwZXskaWR9ID0gMTsKQEAgLTQz
NCw3ICs0MzgsNyBAQAogICAgICAgICAgICAgfSBlbHNpZiAoJGluID1+IHMvXigiKFxcLnxbXiJc
XF0pKiIpLy8pIHsgICAgICAgIyAiLi4uCiAgICAgICAgICAgICAgICAgJG91dCAgICAuPSAkMTsK
ICAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiBzL15cL1wvLiovLykgeyAgICAgICAgICAgICAg
ICAgICMgLy8uLi4KLSAgICAgICAgICAgICAgICBsYXN0IFJFQUQ7CisgICAgICAgICAgICAgICAg
IyBmYWxsIHRocm91Z2gKICAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiBtL15cL1wqLykgeyAg
ICAgICAgICAgICAgICAgICAgICMgLyouLi4KICAgICAgICAgICAgICAgICAjIEMgY29tbWVudCBy
ZW1vdmFsIGFkYXB0ZWQgZnJvbSBwZXJsZmFxNjoKICAgICAgICAgICAgICAgICBpZiAoJGluID1+
IHMvXlwvXCpbXipdKlwqKyhbXlwvKl1bXipdKlwqKykqXC8vLykgewpAQCAtNDUxLDcgKzQ1NSw3
IEBACiAgICAgICAgICAgICB9CiAgICAgICAgIH0KIAotICAgICAgICBsYXN0IFJFQUQ7CisgICAg
ICAgIGxhc3QgUkVBRCBpZiAkb3V0ID1+IC9cUy87CiAgICAgfQogCiAgICAgcmV0dXJuICRvdXQ7
CkBAIC02MDYsOSArNjEwLDkgQEAKICAgICAgICAgICAgICAgICBwcmludCBQUkVBTUJMRSAiIyAk
Xz0kZGVmaW5leyRffVxuIjsKICAgICAgICAgICAgIH0KIAotICAgICAgICAgICAgaWYgKCRkZWZp
bmV7JF99ID1+IC9eXGQrJC8pIHsKKyAgICAgICAgICAgIGlmICgkZGVmaW5leyRffSA9fiAvXihc
ZCspVT9MezAsMn0kL2kpIHsKICAgICAgICAgICAgICAgICBwcmludCBQUkVBTUJMRQotICAgICAg
ICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7ICRkZWZpbmV7
JF99IH0gfVxuXG4iOworICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykg
eyBzdWIgJF8oKSB7ICQxIH0gfVxuXG4iOwogICAgICAgICAgICAgfSBlbHNpZiAoJGRlZmluZXsk
X30gPX4gL15cdyskLykgewogICAgICAgICAgICAgICAgIHByaW50IFBSRUFNQkxFCiAgICAgICAg
ICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7
JF99IH0gfVxuXG4iOwo=
UH2PH572
  }
  if ( $num < 5.008000 ) {
    return _patch_b64(<<'UH2PH573');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTQyLDggKzQyLDEzIEBAIHVz
ZSBDb25maWc7CiB1c2UgRmlsZTo6UGF0aCBxdyhta3BhdGgpOwogdXNlIEdldG9wdDo6U3RkOwog
Ci1nZXRvcHRzKCdEZDpybGhhUScpOwotdXNlIHZhcnMgcXcoJG9wdF9EICRvcHRfZCAkb3B0X3Ig
JG9wdF9sICRvcHRfaCAkb3B0X2EgJG9wdF9RKTsKKyMgTWFrZSBzdXJlIHJlYWQgcGVybWlzc2lv
bnMgZm9yIGFsbCBhcmUgc2V0OgoraWYgKGRlZmluZWQgdW1hc2sgJiYgKHVtYXNrKCkgJiAwNDQ0
KSkgeworICAgIHVtYXNrICh1bWFzaygpICYgfjA0NDQpOworfQorCitnZXRvcHRzKCdEZDpybGhh
UWUnKTsKK3VzZSB2YXJzIHF3KCRvcHRfRCAkb3B0X2QgJG9wdF9yICRvcHRfbCAkb3B0X2ggJG9w
dF9hICRvcHRfUSAkb3B0X2UpOwogZGllICItciBhbmQgLWEgb3B0aW9ucyBhcmUgbXV0dWFsbHkg
ZXhjbHVzaXZlXG4iIGlmICgkb3B0X3IgYW5kICRvcHRfYSk7CiBteSBAaW5jX2RpcnMgPSBpbmNf
ZGlycygpIGlmICRvcHRfYTsKIApAQCAtNjUsMTMgKzcwLDIxIEBAIG15ICVpc2F0eXBlOwogQGlz
YXR5cGV7QGlzYXR5cGV9ID0gKDEpIHggQGlzYXR5cGU7CiBteSAkaW5pZiA9IDA7CiBteSAlSXNf
Y29udmVydGVkOworbXkgJWJhZF9maWxlID0gKCk7CiAKIEBBUkdWID0gKCctJykgdW5sZXNzIEBB
UkdWOwogCiBidWlsZF9wcmVhbWJsZV9pZl9uZWNlc3NhcnkoKTsKIAorc3ViIHJlaW5kZW50KCQp
IHsKKyAgICBteSgkdGV4dCkgPSBzaGlmdDsKKyAgICAkdGV4dCA9fiBzL1xuL1xuICAgIC9nOwor
ICAgICR0ZXh0ID1+IHMvICAgICAgICAvXHQvZzsKKyAgICAkdGV4dDsKK30KKwogbXkgKCR0LCAk
dGFiLCAlY3VyYXJncywgJG5ldywgJGV2YWxfaW5kZXgsICRkaXIsICRuYW1lLCAkYXJncywgJG91
dGZpbGUpOwotbXkgKCRpbmNsLCAkbmV4dCk7CitteSAoJGluY2wsICRpbmNsX3R5cGUsICRpbmNs
X3F1b3RlLCAkbmV4dCk7CiB3aGlsZSAoZGVmaW5lZCAobXkgJGZpbGUgPSBuZXh0X2ZpbGUoKSkp
IHsKICAgICBpZiAoLWwgJGZpbGUgYW5kIC1kICRmaWxlKSB7CiAgICAgICAgIGxpbmtfaWZfcG9z
c2libGUoJGZpbGUpIGlmICgkb3B0X2wpOwpAQCAtMTA3LDEzICsxMjAsMTcgQEAgd2hpbGUgKGRl
ZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxlKCkpKSB7CiAJb3BlbihPVVQsIj4kRGVzdF9kaXIv
JG91dGZpbGUiKSB8fCBkaWUgIkNhbid0IGNyZWF0ZSAkb3V0ZmlsZTogJCFcbiI7CiAgICAgfQog
Ci0gICAgcHJpbnQgT1VUICJyZXF1aXJlICdfaDJwaF9wcmUucGgnO1xuXG4iOwotICAgIHdoaWxl
IChkZWZpbmVkIChsb2NhbCAkXyA9IG5leHRfbGluZSgpKSkgeworICAgIHByaW50IE9VVAorICAg
ICAgICAicmVxdWlyZSAnX2gycGhfcHJlLnBoJztcblxuIiwKKyAgICAgICAgIm5vIHdhcm5pbmdz
ICdyZWRlZmluZSc7XG5cbiI7CisKKyAgICB3aGlsZSAoZGVmaW5lZCAobG9jYWwgJF8gPSBuZXh0
X2xpbmUoJGZpbGUpKSkgewogCWlmIChzL15ccypcI1xzKi8vKSB7CiAJICAgIGlmIChzL15kZWZp
bmVccysoXHcrKS8vKSB7CiAJCSRuYW1lID0gJDE7CiAJCSRuZXcgPSAnJzsKIAkJcy9ccyskLy87
CisJCXMvXChcdytccypcKFwqXClccypcKFx3KlwpXClccyooLT9cZCspLyQxLzsgIyAoaW50ICgq
KShmb29fdCkpMAogCQlpZiAocy9eXCgoW1x3LFxzXSopXCkvLykgewogCQkgICAgJGFyZ3MgPSAk
MTsKICAgICAJICAgIAkgICAgbXkgJHByb3RvID0gJygpICc7CkBAIC0xNjcsMjIgKzE4NCwzMiBA
QCB3aGlsZSAoZGVmaW5lZCAobXkgJGZpbGUgPSBuZXh0X2ZpbGUoKSkpIHsKICAgICAgICAgICAg
ICAgICAgICAgICBwcmludCBPVVQgJHQsInVubGVzcyhkZWZpbmVkKFwmJG5hbWUpKSB7XG4gICAg
c3ViICRuYW1lICgpIHtcdCIsJG5ldywiO31cbn1cbiI7CiAJCSAgICB9CiAJCX0KLQkgICAgfSBl
bHNpZiAoL14oaW5jbHVkZXxpbXBvcnQpXHMqWzwiXSguKilbPiJdLykgewotCQkoJGluY2wgPSAk
MikgPX4gcy9cLmgkLy5waC87Ci0JCXByaW50IE9VVCAkdCwicmVxdWlyZSAnJGluY2wnO1xuIjsK
LQkgICAgfSBlbHNpZigvXmluY2x1ZGVfbmV4dFxzKls8Il0oLiopWz4iXS8pIHsKLQkJKCRpbmNs
ID0gJDEpID1+IHMvXC5oJC8ucGgvOworCSAgICB9IGVsc2lmICgvXihpbmNsdWRlfGltcG9ydHxp
bmNsdWRlX25leHQpXHMqKFs8XCJdKSguKilbPlwiXS8pIHsKKyAgICAgICAgICAgICAgICAkaW5j
bF90eXBlID0gJDE7CisgICAgICAgICAgICAgICAgJGluY2xfcXVvdGUgPSAkMjsKKyAgICAgICAg
ICAgICAgICAkaW5jbCA9ICQzOworICAgICAgICAgICAgICAgIGlmICgoJGluY2xfdHlwZSBlcSAn
aW5jbHVkZV9uZXh0JykgfHwKKyAgICAgICAgICAgICAgICAgICAgKCRvcHRfZSAmJiBleGlzdHMo
JGJhZF9maWxleyRpbmNsfSkpKSB7CisgICAgICAgICAgICAgICAgICAgICRpbmNsID1+IHMvXC5o
JC8ucGgvOwogCQlwcmludCBPVVQgKCR0LAogCQkJICAgImV2YWwge1xuIik7CiAgICAgICAgICAg
ICAgICAgJHRhYiArPSA0OwogICAgICAgICAgICAgICAgICR0ID0gIlx0IiB4ICgkdGFiIC8gOCkg
LiAnICcgeCAoJHRhYiAlIDgpOworICAgICAgICAgICAgICAgICAgICBwcmludCBPVVQgKCR0LCAi
bXkoXEBSRU0pO1xuIik7CisgICAgICAgICAgICAgICAgICAgIGlmICgkaW5jbF90eXBlIGVxICdp
bmNsdWRlX25leHQnKSB7CiAJCXByaW50IE9VVCAoJHQsCiAJCQkgICAibXkoXCVJTkNEKSA9IG1h
cCB7IFwkSU5De1wkX30gPT4gMSB9ICIsCi0JCQkgICAiKGdyZXAgeyBcJF8gZXEgXCIkaW5jbFwi
IH0ga2V5cyhcJUlOQykpO1xuIik7CisJCQkgICAgICAgICAgICIoZ3JlcCB7IFwkXyBlcSBcIiRp
bmNsXCIgfSAiLAorICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAia2V5cyhcJUlO
QykpO1xuIik7CiAJCXByaW50IE9VVCAoJHQsCi0JCQkgICAibXkoXEBSRU0pID0gbWFwIHsgXCJc
JF8vJGluY2xcIiB9ICIsCisJCQkgICAgICAgICAgICJcQFJFTSA9IG1hcCB7IFwiXCRfLyRpbmNs
XCIgfSAiLAogCQkJICAgIihncmVwIHsgbm90IGV4aXN0cyhcJElOQ0R7XCJcJF8vJGluY2xcIn0p
IiwKLQkJCSAgICJhbmQgLWYgXCJcJF8vJGluY2xcIiB9IFxASU5DKTtcbiIpOworCQkJICAgICAg
ICAgICAiIGFuZCAtZiBcIlwkXy8kaW5jbFwiIH0gXEBJTkMpO1xuIik7CisgICAgICAgICAgICAg
ICAgICAgIH0gZWxzZSB7CisgICAgICAgICAgICAgICAgICAgICAgICBwcmludCBPVVQgKCR0LAor
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAiXEBSRU0gPSBtYXAgeyBcIlwkXy8k
aW5jbFwiIH0gIiwKKyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIihncmVwIHst
ciBcIlwkXy8kaW5jbFwiIH0gXEBJTkMpO1xuIik7CisgICAgICAgICAgICAgICAgICAgIH0KIAkJ
cHJpbnQgT1VUICgkdCwKIAkJCSAgICJyZXF1aXJlIFwiXCRSRU1bMF1cIiBpZiBcQFJFTTtcbiIp
OwogICAgICAgICAgICAgICAgICR0YWIgLT0gNDsKQEAgLTE5MSw2ICsyMTgsMTQgQEAgd2hpbGUg
KGRlZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxlKCkpKSB7CiAJCQkgICAifTtcbiIpOwogCQlw
cmludCBPVVQgKCR0LAogCQkJICAgIndhcm4oXCRcQCkgaWYgXCRcQDtcbiIpOworICAgICAgICAg
ICAgICAgIH0gZWxzZSB7CisgICAgICAgICAgICAgICAgICAgICRpbmNsID1+IHMvXC5oJC8ucGgv
OworICAgICAgICAgICAgICAgICAgICAjIGNvcHkgdGhlIHByZWZpeCBpbiB0aGUgcXVvdGUgc3lu
dGF4ICgjaW5jbHVkZSAieC5oIikgY2FzZQorICAgICAgICAgICAgICAgICAgICBpZiAoJGluY2wg
IX4gbXwvfCAmJiAkaW5jbF9xdW90ZSBlcSBxeyJ9ICYmICRmaWxlID1+IG18XiguKikvfCkgewor
ICAgICAgICAgICAgICAgICAgICAgICAgJGluY2wgPSAiJDEvJGluY2wiOworICAgICAgICAgICAg
ICAgICAgICB9CisJCSAgICBwcmludCBPVVQgJHQsInJlcXVpcmUgJyRpbmNsJztcbiI7CisgICAg
ICAgICAgICAgICAgfQogCSAgICB9IGVsc2lmICgvXmlmZGVmXHMrKFx3KykvKSB7CiAJCXByaW50
IE9VVCAkdCwiaWYoZGVmaW5lZCgmJDEpKSB7XG4iOwogCQkkdGFiICs9IDQ7CkBAIC0yNDAsNyAr
Mjc1LDcgQEAgd2hpbGUgKGRlZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxlKCkpKSB7CiAJICAg
IH0KIAl9IGVsc2lmKC9eXHMqKHR5cGVkZWZccyopP2VudW1ccyooXHMrW2EtekEtWl9dXHcqXHMq
KT8vKSB7CiAJICAgIHVudGlsKC9ce1tefV0qXH0uKjsvIHx8IC87LykgewotCQlsYXN0IHVubGVz
cyBkZWZpbmVkICgkbmV4dCA9IG5leHRfbGluZSgpKTsKKwkJbGFzdCB1bmxlc3MgZGVmaW5lZCAo
JG5leHQgPSBuZXh0X2xpbmUoJGZpbGUpKTsKIAkJY2hvbXAgJG5leHQ7CiAJCSMgZHJvcCAiI2Rl
ZmluZSBGT08gRk9PIiBpbiBlbnVtcwogCQkkbmV4dCA9fiBzL15ccyojXHMqZGVmaW5lXHMrKFx3
KylccytcMVxzKiQvLzsKQEAgLTI3MiwyMiArMzA3LDIyIEBAIHdoaWxlIChkZWZpbmVkIChteSAk
ZmlsZSA9IG5leHRfZmlsZSgpKSkgewogCSAgICB9CiAJfQogICAgIH0KLSAgICBwcmludCBPVVQg
IjE7XG4iOwotCiAgICAgJElzX2NvbnZlcnRlZHskZmlsZX0gPSAxOworICAgIGlmICgkb3B0X2Ug
JiYgZXhpc3RzKCRiYWRfZmlsZXskZmlsZX0pKSB7CisgICAgICAgIHVubGluaygkRGVzdF9kaXIg
LiAnLycgLiAkb3V0ZmlsZSk7CisgICAgICAgICRuZXh0ID0gJyc7CisgICAgfSBlbHNlIHsKKyAg
ICAgICAgcHJpbnQgT1VUICIxO1xuIjsKICAgICBxdWV1ZV9pbmNsdWRlc19mcm9tKCRmaWxlKSBp
ZiAoJG9wdF9hKTsKKyAgICB9CiB9CiAKLWV4aXQgJEV4aXQ7Ci0KLQotc3ViIHJlaW5kZW50KCQp
IHsKLSAgICBteSgkdGV4dCkgPSBzaGlmdDsKLSAgICAkdGV4dCA9fiBzL1xuL1xuICAgIC9nOwot
ICAgICR0ZXh0ID1+IHMvICAgICAgICAvXHQvZzsKLSAgICAkdGV4dDsKK2lmICgkb3B0X2UgJiYg
KHNjYWxhcihrZXlzICViYWRfZmlsZSkgPiAwKSkgeworICAgIHdhcm4gIldhcyB1bmFibGUgdG8g
Y29udmVydCB0aGUgZm9sbG93aW5nIGZpbGVzOlxuIjsKKyAgICB3YXJuICJcdCIgLiBqb2luKCJc
blx0Iixzb3J0KGtleXMgJWJhZF9maWxlKSkgLiAiXG4iOwogfQogCitleGl0ICRFeGl0OwogCiBz
dWIgZXhwciB7CiAgICAgbXkgJGpvaW5lZF9hcmdzOwpAQCAtMjk4LDggKzMzMywyMSBAQCBzdWIg
ZXhwciB7CiAJcy9eXCZcJi8vICYmIGRvIHsgJG5ldyAuPSAiICYmIjsgbmV4dDt9OyAjIGhhbmRs
ZSAmJiBvcGVyYXRvcgogCXMvXlwmKFtcKGEtelwpXSspLyQxL2k7CSMgaGFjayBmb3IgdGhpbmdz
IHRoYXQgdGFrZSB0aGUgYWRkcmVzcyBvZgogCXMvXihccyspLy8JCSYmIGRvIHskbmV3IC49ICcg
JzsgbmV4dDt9OwotCXMvXigwWFswLTlBLUZdKylbVUxdKi8vaQkmJiBkbyB7JG5ldyAuPSBsYygk
MSk7IG5leHQ7fTsKLQlzL14oLT9cZCtcLlxkK0VbLStdXGQrKUY/Ly9pCSYmIGRvIHskbmV3IC49
ICQxOyBuZXh0O307CisJcy9eMFgoWzAtOUEtRl0rKVtVTF0qLy9pIAorCSAgICAmJiBkbyB7bXkg
JGhleCA9ICQxOworCQkgICAkaGV4ID1+IHMvXjArLy87CisJCSAgIGlmIChsZW5ndGggJGhleCA+
IDggJiYgISRDb25maWd7dXNlNjRiaXRpbnR9KSB7CisJCSAgICAgICAjIENyb2FrIGlmIG52X3By
ZXNlcnZlc191dl9iaXRzIDwgNjQgPworCQkgICAgICAgJG5ldyAuPSAgICAgICAgIGhleChzdWJz
dHIoJGhleCwgLTgpKSArCisJCQkgICAgICAgMioqMzIgKiBoZXgoc3Vic3RyKCRoZXgsICAwLCAt
OCkpOworCQkgICAgICAgIyBUaGUgYWJvdmUgd2lsbCBwcm9kdWNlICJlcnJvcm5ldXMiIGNvZGUK
KwkJICAgICAgICMgaWYgdGhlIGhleCBjb25zdGFudCB3YXMgZS5nLiBpbnNpZGUgVUlOVDY0X0MK
KwkJICAgICAgICMgbWFjcm8sIGJ1dCB0aGVuIGFnYWluLCBoMnBoIGlzIGFuIGFwcHJveGltYXRp
b24uCisJCSAgIH0gZWxzZSB7CisJCSAgICAgICAkbmV3IC49IGxjKCIweCRoZXgiKTsKKwkJICAg
fQorCQkgICBuZXh0O307CisJcy9eKC0/XGQrXC5cZCtFWy0rXT9cZCspW0ZMXT8vL2kJJiYgZG8g
eyRuZXcgLj0gJDE7IG5leHQ7fTsKIAlzL14oXGQrKVxzKltMVV0qLy9pCSYmIGRvIHskbmV3IC49
ICQxOyBuZXh0O307CiAJcy9eKCIoXFwifFteIl0pKiIpLy8JJiYgZG8geyRuZXcgLj0gJDE7IG5l
eHQ7fTsKIAlzL14nKChcXCJ8W14iXSkqKScvLwkmJiBkbyB7CkBAIC0zODgsNyArNDM2LDcgQEAg
c3ViIGV4cHIgewogCQl9CiAJICAgIH0gZWxzZSB7CiAJCWlmICgkaW5pZiAmJiAkbmV3ICF+IC9k
ZWZpbmVkXHMqXCgkLykgewotCQkgICAgJG5ldyAuPSAnKGRlZmluZWQoJicgLiAkaWQgLiAnKSA/
ICYnIC4gJGlkIC4gJyA6IDApJzsKKwkJICAgICRuZXcgLj0gJyhkZWZpbmVkKCYnIC4gJGlkIC4g
JykgPyAmJyAuICRpZCAuICcgOiB1bmRlZiknOwogCQl9IGVsc2lmICgvXlxbLykgewogCQkgICAg
JG5ldyAuPSAiIFwkJGlkIjsKIAkJfSBlbHNlIHsKQEAgLTQwNCw2ICs0NTIsNyBAQCBzdWIgZXhw
ciB7CiAKIHN1YiBuZXh0X2xpbmUKIHsKKyAgICBteSAkZmlsZSA9IHNoaWZ0OwogICAgIG15ICgk
aW4sICRvdXQpOwogICAgIG15ICRwcmVfc3ViX3RyaV9ncmFwaHMgPSAxOwogCkBAIC00MjYsNiAr
NDc1LDIwIEBAIHN1YiBuZXh0X2xpbmUKICAgICAgICAgICAgICAgICAkaW4gPX4gcy9cP1w/PC97
L2c7ICAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/Pzx8ICB7fAogICAgICAgICAgICAgICAg
ICRpbiA9fiBzL1w/XD8+L30vZzsgICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/PnwgIH18
CiAgICAgICAgICAgICB9CisJICAgIGlmICgkaW4gPX4gL15cI2lmZGVmIF9fTEFOR1VBR0VfUEFT
Q0FMX18vKSB7CisgICAgICAgICAgICAgICAgIyBUcnU2NCBkaXNhc3NlbWJsZXIuaCBldmlsbmVz
czogbWl4ZWQgQyBhbmQgUGFzY2FsLgorCQl3aGlsZSAoPElOPikgeworCQkgICAgbGFzdCBpZiAv
XlwjZW5kaWYvOyAKKwkJfQorCQluZXh0IFJFQUQ7CisJICAgIH0KKwkgICAgaWYgKCRpbiA9fiAv
XmV4dGVybiBpbmxpbmUgLyAmJiAjIElubGluZWQgYXNzZW1ibGVyLgorCQkkXk8gZXEgJ2xpbnV4
JyAmJiAkZmlsZSA9fiBtISg/Ol58Lylhc20vW14vXStcLmgkISkgeworIAkJd2hpbGUgKDxJTj4p
IHsKKwkJICAgIGxhc3QgaWYgL159LzsgCisJCX0KKwkJbmV4dCBSRUFEOworCSAgICB9CiAgICAg
ICAgICAgICBpZiAoJGluID1+IHMvXFwkLy8pIHsgICAgICAgICAgICAgICAgICAgICAgICAgICAj
IFwtbmV3bGluZQogICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJyAnOwogICAgICAgICAgICAg
ICAgIG5leHQgUkVBRDsKQEAgLTQzMywxMCArNDk2LDE4IEBAIHN1YiBuZXh0X2xpbmUKICAgICAg
ICAgICAgICAgICAkb3V0ICAgIC49ICQxOwogICAgICAgICAgICAgfSBlbHNpZiAoJGluID1+IHMv
XihcXC4pLy8pIHsgICAgICAgICAgICAgICAgICAgIyBcLi4uCiAgICAgICAgICAgICAgICAgJG91
dCAgICAuPSAkMTsKLSAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiBzL14oJyhcXC58W14nXFxd
KSonKS8vKSB7ICAgICAgICMgJy4uLgotICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7Ci0g
ICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKCIoXFwufFteIlxcXSkqIikvLykgeyAgICAg
ICAjICIuLi4KLSAgICAgICAgICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAgfSBl
bHNpZiAoJGluID1+IC9eJy8pIHsgICAgICAgICAgICAgICAgICAgICAgICAgIyAnLi4uCisgICAg
ICAgICAgICAgICAgaWYgKCRpbiA9fiBzL14oJyhcXC58W14nXFxdKSonKS8vKSB7CisgICAgICAg
ICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7CisgICAgICAgICAgICAgICAgfSBlbHNlIHsKKyAg
ICAgICAgICAgICAgICAgICAgbmV4dCBSRUFEOworICAgICAgICAgICAgICAgIH0KKyAgICAgICAg
ICAgIH0gZWxzaWYgKCRpbiA9fiAvXiIvKSB7ICAgICAgICAgICAgICAgICAgICAgICAgICMgIi4u
LgorICAgICAgICAgICAgICAgIGlmICgkaW4gPX4gcy9eKCIoXFwufFteIlxcXSkqIikvLykgewor
ICAgICAgICAgICAgICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAgICAgIH0gZWxz
ZSB7CisgICAgICAgICAgICAgICAgICAgIG5leHQgUkVBRDsKKyAgICAgICAgICAgICAgICB9CiAg
ICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eXC9cLy4qLy8pIHsgICAgICAgICAgICAgICAg
ICAjIC8vLi4uCiAgICAgICAgICAgICAgICAgIyBmYWxsIHRocm91Z2gKICAgICAgICAgICAgIH0g
ZWxzaWYgKCRpbiA9fiBtL15cL1wqLykgeyAgICAgICAgICAgICAgICAgICAgICMgLyouLi4KQEAg
LTQ1MCw4ICs1MjEsMjAgQEAgc3ViIG5leHRfbGluZQogICAgICAgICAgICAgICAgICRvdXQgICAg
Lj0gJDE7CiAgICAgICAgICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKFteXCdcIlxcXC9dKykvLykg
ewogICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7CisgICAgICAgICAgICB9IGVsc2lmICgk
Xk8gZXEgJ2xpbnV4JyAmJgorICAgICAgICAgICAgICAgICAgICAgJGZpbGUgPX4gbSEoPzpefC8p
bGludXgvYnl0ZW9yZGVyL3BkcF9lbmRpYW5cLmgkISAmJgorICAgICAgICAgICAgICAgICAgICAg
JGluICAgPX4gcyFcJ1QgS05PVyEhKSB7CisgICAgICAgICAgICAgICAgJG91dCAgICA9fiBzIUkg
RE9OJCFJX0RPX05PVF9LTk9XITsKICAgICAgICAgICAgIH0gZWxzZSB7Ci0gICAgICAgICAgICAg
ICAgZGllICJDYW5ub3QgcGFyc2U6XG4kaW5cbiI7CisgICAgICAgICAgICAgICAgaWYgKCRvcHRf
ZSkgeworICAgICAgICAgICAgICAgICAgICB3YXJuICJDYW5ub3QgcGFyc2UgJGZpbGU6XG4kaW5c
biI7CisgICAgICAgICAgICAgICAgICAgICRiYWRfZmlsZXskZmlsZX0gPSAxOworICAgICAgICAg
ICAgICAgICAgICAkaW4gPSAnJzsKKyAgICAgICAgICAgICAgICAgICAgJG91dCA9IHVuZGVmOwor
ICAgICAgICAgICAgICAgICAgICBsYXN0IFJFQUQ7CisgICAgICAgICAgICAgICAgfSBlbHNlIHsK
KwkJZGllICJDYW5ub3QgcGFyc2U6XG4kaW5cbiI7CisgICAgICAgICAgICAgICAgfQogICAgICAg
ICAgICAgfQogICAgICAgICB9CiAKQEAgLTU2MSw4ICs2NDQsMTMgQEAgc3ViIHF1ZXVlX2luY2x1
ZGVzX2Zyb20KICAgICAgICAgICAgICAgICAkbGluZSAuPSA8SEVBREVSPjsKICAgICAgICAgICAg
IH0KIAotICAgICAgICAgICAgaWYgKCRsaW5lID1+IC9eI1xzKmluY2x1ZGVccys8KC4qPyk+Lykg
ewotICAgICAgICAgICAgICAgIHB1c2goQEFSR1YsICQxKSB1bmxlc3MgJElzX2NvbnZlcnRlZHsk
MX07CisgICAgICAgICAgICBpZiAoJGxpbmUgPX4gL14jXHMqaW5jbHVkZVxzKyhbPCJdKSguKj8p
Wz4iXS8pIHsKKyAgICAgICAgICAgICAgICBteSAoJGRlbGltaXRlciwgJG5ld19maWxlKSA9ICgk
MSwgJDIpOworICAgICAgICAgICAgICAgICMgY29weSB0aGUgcHJlZml4IGluIHRoZSBxdW90ZSBz
eW50YXggKCNpbmNsdWRlICJ4LmgiKSBjYXNlCisgICAgICAgICAgICAgICAgaWYgKCRkZWxpbWl0
ZXIgZXEgcXsifSAmJiAkZmlsZSA9fiBtfF4oLiopL3wpIHsKKyAgICAgICAgICAgICAgICAgICAg
JG5ld19maWxlID0gIiQxLyRuZXdfZmlsZSI7CisgICAgICAgICAgICAgICAgfQorICAgICAgICAg
ICAgICAgIHB1c2goQEFSR1YsICRuZXdfZmlsZSkgdW5sZXNzICRJc19jb252ZXJ0ZWR7JG5ld19m
aWxlfTsKICAgICAgICAgICAgIH0KICAgICAgICAgfQogICAgIGNsb3NlIEhFQURFUjsKQEAgLTYw
MywyNSArNjkxLDUwIEBAIHN1YiBidWlsZF9wcmVhbWJsZV9pZl9uZWNlc3NhcnkKICAgICBteSAo
JWRlZmluZSkgPSBfZXh0cmFjdF9jY19kZWZpbmVzKCk7CiAKICAgICBvcGVuICBQUkVBTUJMRSwg
Ij4kcHJlYW1ibGUiIG9yIGRpZSAiQ2Fubm90IG9wZW4gJHByZWFtYmxlOiAgJCEiOwotICAgICAg
ICBwcmludCBQUkVBTUJMRSAiIyBUaGlzIGZpbGUgd2FzIGNyZWF0ZWQgYnkgaDJwaCB2ZXJzaW9u
ICRWRVJTSU9OXG4iOwotCi0gICAgICAgIGZvcmVhY2ggKHNvcnQga2V5cyAlZGVmaW5lKSB7Ci0g
ICAgICAgICAgICBpZiAoJG9wdF9EKSB7Ci0gICAgICAgICAgICAgICAgcHJpbnQgUFJFQU1CTEUg
IiMgJF89JGRlZmluZXskX31cbiI7Ci0gICAgICAgICAgICB9Ci0KLSAgICAgICAgICAgIGlmICgk
ZGVmaW5leyRffSA9fiAvXihcZCspVT9MezAsMn0kL2kpIHsKLSAgICAgICAgICAgICAgICBwcmlu
dCBQUkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBz
dWIgJF8oKSB7ICQxIH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNpZiAoJGRlZmluZXskX30g
PX4gL15cdyskLykgewotICAgICAgICAgICAgICAgIHByaW50IFBSRUFNQkxFCi0gICAgICAgICAg
ICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7JF99
IH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNlIHsKKwlwcmludCBQUkVBTUJMRSAiIyBUaGlz
IGZpbGUgd2FzIGNyZWF0ZWQgYnkgaDJwaCB2ZXJzaW9uICRWRVJTSU9OXG4iOworICAgICAgICAj
IFByZXZlbnQgbm9uLXBvcnRhYmxlIGhleCBjb25zdGFudHMgZnJvbSB3YXJuaW5nLgorICAgICAg
ICAjCisgICAgICAgICMgV2Ugc3RpbGwgcHJvZHVjZSBhbiBvdmVyZmxvdyB3YXJuaW5nIGlmIHdl
IGNhbid0IHJlcHJlc2VudAorICAgICAgICAjIGEgaGV4IGNvbnN0YW50IGFzIGFuIGludGVnZXIu
CisgICAgICAgIHByaW50IFBSRUFNQkxFICJubyB3YXJuaW5ncyBxdyhwb3J0YWJsZSk7XG4iOwor
CisJZm9yZWFjaCAoc29ydCBrZXlzICVkZWZpbmUpIHsKKwkgICAgaWYgKCRvcHRfRCkgeworCQlw
cmludCBQUkVBTUJMRSAiIyAkXz0kZGVmaW5leyRffVxuIjsKKwkgICAgfQorCSAgICBpZiAoJGRl
ZmluZXskX30gPX4gL15cKCguKilcKSQvKSB7CisJCSMgcGFyZW50aGVzaXplZCB2YWx1ZTogIGQ9
KHYpCisJCSRkZWZpbmV7JF99ID0gJDE7CisJICAgIH0KKwkgICAgaWYgKCRkZWZpbmV7JF99ID1+
IC9eKFsrLV0/KFxkKyk/XC5cZCsoW2VFXVsrLV0/XGQrKT8pW0ZMXT8kLykgeworCQkjIGZsb2F0
OgorCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRf
KCkgeyAkMSB9IH1cblxuIjsKKwkgICAgfSBlbHNpZiAoJGRlZmluZXskX30gPX4gL14oWystXT9c
ZCspVT9MezAsMn0kL2kpIHsKKwkJIyBpbnRlZ2VyOgorCQlwcmludCBQUkVBTUJMRQorCQkgICAg
InVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkgeyAkMSB9IH1cblxuIjsKKyAgICAgICAg
ICAgIH0gZWxzaWYgKCRkZWZpbmV7JF99ID1+IC9eKFsrLV0/MHhbXGRhLWZdKylVP0x7MCwyfSQv
aSkgeworICAgICAgICAgICAgICAgICMgaGV4IGludGVnZXIKKyAgICAgICAgICAgICAgICAjIFNw
ZWNpYWwgY2FzZWQsIHNpbmNlIHBlcmwgd2FybnMgb24gaGV4IGludGVnZXJzCisgICAgICAgICAg
ICAgICAgIyB0aGF0IGNhbid0IGJlIHJlcHJlc2VudGVkIGluIGEgVVYuCisgICAgICAgICAgICAg
ICAgIworICAgICAgICAgICAgICAgICMgVGhpcyB3YXkgd2UgZ2V0IHRoZSB3YXJuaW5nIGF0IHRp
bWUgb2YgdXNlLCBzbyB0aGUgdXNlcgorICAgICAgICAgICAgICAgICMgb25seSBnZXRzIHRoZSB3
YXJuaW5nIGlmIHRoZXkgaGFwcGVuIHRvIHVzZSB0aGlzCisgICAgICAgICAgICAgICAgIyBwbGF0
Zm9ybS1zcGVjaWZpYyBkZWZpbml0aW9uLgorICAgICAgICAgICAgICAgIG15ICRjb2RlID0gJDE7
CisgICAgICAgICAgICAgICAgJGNvZGUgPSAiaGV4KCckY29kZScpIiBpZiBsZW5ndGggJGNvZGUg
PiAxMDsKICAgICAgICAgICAgICAgICBwcmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAgICAg
ICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7IFwiIiwKLSAgICAgICAgICAgICAg
ICAgICAgcXVvdGVtZXRhKCRkZWZpbmV7JF99KSwgIlwiIH0gfVxuXG4iOwotICAgICAgICAgICAg
fQotICAgICAgICB9CisgICAgICAgICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7
IHN1YiAkXygpIHsgJGNvZGUgfSB9XG5cbiI7CisJICAgIH0gZWxzaWYgKCRkZWZpbmV7JF99ID1+
IC9eXHcrJC8pIHsKKwkJcHJpbnQgUFJFQU1CTEUKKwkJICAgICJ1bmxlc3MgKGRlZmluZWQgJiRf
KSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7JF99IH0gfVxuXG4iOworCSAgICB9IGVsc2UgeworCQlw
cmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkgeyBc
IiIsCisJCSAgICBxdW90ZW1ldGEoJGRlZmluZXskX30pLCAiXCIgfSB9XG5cbiI7CisJICAgIH0K
Kwl9CiAgICAgY2xvc2UgUFJFQU1CTEUgICAgICAgICAgICAgICBvciBkaWUgIkNhbm5vdCBjbG9z
ZSAkcHJlYW1ibGU6ICAkISI7CiB9CiAKQEAgLTYzMywxNSArNzQ2LDE0IEBAIHN1YiBfZXh0cmFj
dF9jY19kZWZpbmVzCiB7CiAgICAgbXkgJWRlZmluZTsKICAgICBteSAkYWxsc3ltYm9scyAgPSBq
b2luICIgIiwKLSAgICAgICAgQENvbmZpZ3snY2NzeW1ib2xzJywgJ2NwcHN5bWJvbHMnLCAnY3Bw
Y2NzeW1ib2xzJ307CisJQENvbmZpZ3snY2NzeW1ib2xzJywgJ2NwcHN5bWJvbHMnLCAnY3BwY2Nz
eW1ib2xzJ307CiAKICAgICAjIFNwbGl0IGNvbXBpbGVyIHByZS1kZWZpbml0aW9ucyBpbnRvIGBr
ZXk9dmFsdWUnIHBhaXJzOgotICAgIGZvcmVhY2ggKHNwbGl0IC9ccysvLCAkYWxsc3ltYm9scykg
ewotICAgICAgICAvKC4rPyk9KC4rKS8gYW5kICRkZWZpbmV7JDF9ID0gJDI7Ci0KLSAgICAgICAg
aWYgKCRvcHRfRCkgewotICAgICAgICAgICAgcHJpbnQgU1RERVJSICIkXzogICQxIC0+ICQyXG4i
OwotICAgICAgICB9CisgICAgd2hpbGUgKCRhbGxzeW1ib2xzID1+IC8oW15cc10rKT0oKFxcXHN8
W15cc10pKykvZykgeworCSRkZWZpbmV7JDF9ID0gJDI7CisJaWYgKCRvcHRfRCkgeworCSAgICBw
cmludCBTVERFUlIgIiRfOiAgJDEgLT4gJDJcbiI7CisJfQogICAgIH0KIAogICAgIHJldHVybiAl
ZGVmaW5lOwpAQCAtNjcwLDYgKzc4MiwxMCBAQCBJdCBpcyBtb3N0IGVhc2lseSBydW4gd2hpbGUg
aW4gL3Vzci9pbmNsdWRlOgogCiAJY2QgL3Vzci9pbmNsdWRlOyBoMnBoICogc3lzLyoKIAorb3IK
KworCWNkIC91c3IvaW5jbHVkZTsgaDJwaCAqIHN5cy8qIGFycGEvKiBuZXRpbmV0LyoKKwogb3IK
IAogCWNkIC91c3IvaW5jbHVkZTsgaDJwaCAtciAtbCAuCkBAIC02ODcsNyArODAzLDcgQEAgSWYg
cnVuIHdpdGggbm8gYXJndW1lbnRzLCBmaWx0ZXJzIHN0YW5kYXJkIGlucHV0IHRvIHN0YW5kYXJk
IG91dHB1dC4KID1pdGVtIC1kIGRlc3RpbmF0aW9uX2RpcgogCiBQdXQgdGhlIHJlc3VsdGluZyBC
PC5waD4gZmlsZXMgYmVuZWF0aCBCPGRlc3RpbmF0aW9uX2Rpcj4sIGluc3RlYWQgb2YKLWJlbmVh
dGggdGhlIGRlZmF1bHQgUGVybCBsaWJyYXJ5IGxvY2F0aW9uIChDPCRDb25maWd7J2luc3RhbGxz
aXRzZWFyY2gnfT4pLgorYmVuZWF0aCB0aGUgZGVmYXVsdCBQZXJsIGxpYnJhcnkgbG9jYXRpb24g
KEM8JENvbmZpZ3snaW5zdGFsbHNpdGVhcmNoJ30+KS4KIAogPWl0ZW0gLXIKIApAQCAtNzcyLDEw
ICs4ODgsMTAgQEAgaW5zdGFsbGF0aW9uLgogRG9lc24ndCBoYW5kbGUgY29tcGxpY2F0ZWQgZXhw
cmVzc2lvbnMgYnVpbHQgcGllY2VtZWFsLCBhIGxhOgogCiAgICAgZW51bSB7Ci0gICAgICAgIEZJ
UlNUX1ZBTFVFLAotICAgICAgICBTRUNPTkRfVkFMVUUsCisJRklSU1RfVkFMVUUsCisJU0VDT05E
X1ZBTFVFLAogICAgICNpZmRlZiBBQkMKLSAgICAgICAgVEhJUkRfVkFMVUUKKwlUSElSRF9WQUxV
RQogICAgICNlbmRpZgogICAgIH07CiAK
UH2PH573
  }
  if ( $num < 5.008001 ) {
    return _patch_b64(<<'UH2PH580');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTQyLDggKzQyLDEzIEBAIHVz
ZSBDb25maWc7CiB1c2UgRmlsZTo6UGF0aCBxdyhta3BhdGgpOwogdXNlIEdldG9wdDo6U3RkOwog
Ci1nZXRvcHRzKCdEZDpybGhhUScpOwotdXNlIHZhcnMgcXcoJG9wdF9EICRvcHRfZCAkb3B0X3Ig
JG9wdF9sICRvcHRfaCAkb3B0X2EgJG9wdF9RKTsKKyMgTWFrZSBzdXJlIHJlYWQgcGVybWlzc2lv
bnMgZm9yIGFsbCBhcmUgc2V0OgoraWYgKGRlZmluZWQgdW1hc2sgJiYgKHVtYXNrKCkgJiAwNDQ0
KSkgeworICAgIHVtYXNrICh1bWFzaygpICYgfjA0NDQpOworfQorCitnZXRvcHRzKCdEZDpybGhh
UWUnKTsKK3VzZSB2YXJzIHF3KCRvcHRfRCAkb3B0X2QgJG9wdF9yICRvcHRfbCAkb3B0X2ggJG9w
dF9hICRvcHRfUSAkb3B0X2UpOwogZGllICItciBhbmQgLWEgb3B0aW9ucyBhcmUgbXV0dWFsbHkg
ZXhjbHVzaXZlXG4iIGlmICgkb3B0X3IgYW5kICRvcHRfYSk7CiBteSBAaW5jX2RpcnMgPSBpbmNf
ZGlycygpIGlmICRvcHRfYTsKIApAQCAtNjUsMTMgKzcwLDIxIEBAIG15ICVpc2F0eXBlOwogQGlz
YXR5cGV7QGlzYXR5cGV9ID0gKDEpIHggQGlzYXR5cGU7CiBteSAkaW5pZiA9IDA7CiBteSAlSXNf
Y29udmVydGVkOworbXkgJWJhZF9maWxlID0gKCk7CiAKIEBBUkdWID0gKCctJykgdW5sZXNzIEBB
UkdWOwogCiBidWlsZF9wcmVhbWJsZV9pZl9uZWNlc3NhcnkoKTsKIAorc3ViIHJlaW5kZW50KCQp
IHsKKyAgICBteSgkdGV4dCkgPSBzaGlmdDsKKyAgICAkdGV4dCA9fiBzL1xuL1xuICAgIC9nOwor
ICAgICR0ZXh0ID1+IHMvICAgICAgICAvXHQvZzsKKyAgICAkdGV4dDsKK30KKwogbXkgKCR0LCAk
dGFiLCAlY3VyYXJncywgJG5ldywgJGV2YWxfaW5kZXgsICRkaXIsICRuYW1lLCAkYXJncywgJG91
dGZpbGUpOwotbXkgKCRpbmNsLCAkbmV4dCk7CitteSAoJGluY2wsICRpbmNsX3R5cGUsICRpbmNs
X3F1b3RlLCAkbmV4dCk7CiB3aGlsZSAoZGVmaW5lZCAobXkgJGZpbGUgPSBuZXh0X2ZpbGUoKSkp
IHsKICAgICBpZiAoLWwgJGZpbGUgYW5kIC1kICRmaWxlKSB7CiAgICAgICAgIGxpbmtfaWZfcG9z
c2libGUoJGZpbGUpIGlmICgkb3B0X2wpOwpAQCAtMTA3LDcgKzEyMCw5IEBAIHdoaWxlIChkZWZp
bmVkIChteSAkZmlsZSA9IG5leHRfZmlsZSgpKSkgewogCW9wZW4oT1VULCI+JERlc3RfZGlyLyRv
dXRmaWxlIikgfHwgZGllICJDYW4ndCBjcmVhdGUgJG91dGZpbGU6ICQhXG4iOwogICAgIH0KIAot
ICAgIHByaW50IE9VVCAicmVxdWlyZSAnX2gycGhfcHJlLnBoJztcblxuIjsKKyAgICBwcmludCBP
VVQKKyAgICAgICAgInJlcXVpcmUgJ19oMnBoX3ByZS5waCc7XG5cbiIsCisgICAgICAgICJubyB3
YXJuaW5ncyAncmVkZWZpbmUnO1xuXG4iOwogCiAgICAgd2hpbGUgKGRlZmluZWQgKGxvY2FsICRf
ID0gbmV4dF9saW5lKCRmaWxlKSkpIHsKIAlpZiAocy9eXHMqXCNccyovLykgewpAQCAtMTY5LDIy
ICsxODQsMzIgQEAgd2hpbGUgKGRlZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxlKCkpKSB7CiAg
ICAgICAgICAgICAgICAgICAgICAgcHJpbnQgT1VUICR0LCJ1bmxlc3MoZGVmaW5lZChcJiRuYW1l
KSkge1xuICAgIHN1YiAkbmFtZSAoKSB7XHQiLCRuZXcsIjt9XG59XG4iOwogCQkgICAgfQogCQl9
Ci0JICAgIH0gZWxzaWYgKC9eKGluY2x1ZGV8aW1wb3J0KVxzKls8Il0oLiopWz4iXS8pIHsKLQkJ
KCRpbmNsID0gJDIpID1+IHMvXC5oJC8ucGgvOwotCQlwcmludCBPVVQgJHQsInJlcXVpcmUgJyRp
bmNsJztcbiI7Ci0JICAgIH0gZWxzaWYoL15pbmNsdWRlX25leHRccypbPCJdKC4qKVs+Il0vKSB7
Ci0JCSgkaW5jbCA9ICQxKSA9fiBzL1wuaCQvLnBoLzsKKwkgICAgfSBlbHNpZiAoL14oaW5jbHVk
ZXxpbXBvcnR8aW5jbHVkZV9uZXh0KVxzKihbPFwiXSkoLiopWz5cIl0vKSB7CisgICAgICAgICAg
ICAgICAgJGluY2xfdHlwZSA9ICQxOworICAgICAgICAgICAgICAgICRpbmNsX3F1b3RlID0gJDI7
CisgICAgICAgICAgICAgICAgJGluY2wgPSAkMzsKKyAgICAgICAgICAgICAgICBpZiAoKCRpbmNs
X3R5cGUgZXEgJ2luY2x1ZGVfbmV4dCcpIHx8CisgICAgICAgICAgICAgICAgICAgICgkb3B0X2Ug
JiYgZXhpc3RzKCRiYWRfZmlsZXskaW5jbH0pKSkgeworICAgICAgICAgICAgICAgICAgICAkaW5j
bCA9fiBzL1wuaCQvLnBoLzsKIAkJcHJpbnQgT1VUICgkdCwKIAkJCSAgICJldmFsIHtcbiIpOwog
ICAgICAgICAgICAgICAgICR0YWIgKz0gNDsKICAgICAgICAgICAgICAgICAkdCA9ICJcdCIgeCAo
JHRhYiAvIDgpIC4gJyAnIHggKCR0YWIgJSA4KTsKKyAgICAgICAgICAgICAgICAgICAgcHJpbnQg
T1VUICgkdCwgIm15KFxAUkVNKTtcbiIpOworICAgICAgICAgICAgICAgICAgICBpZiAoJGluY2xf
dHlwZSBlcSAnaW5jbHVkZV9uZXh0JykgewogCQlwcmludCBPVVQgKCR0LAogCQkJICAgIm15KFwl
SU5DRCkgPSBtYXAgeyBcJElOQ3tcJF99ID0+IDEgfSAiLAotCQkJICAgIihncmVwIHsgXCRfIGVx
IFwiJGluY2xcIiB9IGtleXMoXCVJTkMpKTtcbiIpOworCQkJICAgICAgICAgICAiKGdyZXAgeyBc
JF8gZXEgXCIkaW5jbFwiIH0gIiwKKyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ImtleXMoXCVJTkMpKTtcbiIpOwogCQlwcmludCBPVVQgKCR0LAotCQkJICAgIm15KFxAUkVNKSA9
IG1hcCB7IFwiXCRfLyRpbmNsXCIgfSAiLAorCQkJICAgICAgICAgICAiXEBSRU0gPSBtYXAgeyBc
IlwkXy8kaW5jbFwiIH0gIiwKIAkJCSAgICIoZ3JlcCB7IG5vdCBleGlzdHMoXCRJTkNEe1wiXCRf
LyRpbmNsXCJ9KSIsCi0JCQkgICAiYW5kIC1mIFwiXCRfLyRpbmNsXCIgfSBcQElOQyk7XG4iKTsK
KwkJCSAgICAgICAgICAgIiBhbmQgLWYgXCJcJF8vJGluY2xcIiB9IFxASU5DKTtcbiIpOworICAg
ICAgICAgICAgICAgICAgICB9IGVsc2UgeworICAgICAgICAgICAgICAgICAgICAgICAgcHJpbnQg
T1VUICgkdCwKKyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIlxAUkVNID0gbWFw
IHsgXCJcJF8vJGluY2xcIiB9ICIsCisgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICIoZ3JlcCB7LXIgXCJcJF8vJGluY2xcIiB9IFxASU5DKTtcbiIpOworICAgICAgICAgICAgICAg
ICAgICB9CiAJCXByaW50IE9VVCAoJHQsCiAJCQkgICAicmVxdWlyZSBcIlwkUkVNWzBdXCIgaWYg
XEBSRU07XG4iKTsKICAgICAgICAgICAgICAgICAkdGFiIC09IDQ7CkBAIC0xOTMsNiArMjE4LDE0
IEBAIHdoaWxlIChkZWZpbmVkIChteSAkZmlsZSA9IG5leHRfZmlsZSgpKSkgewogCQkJICAgIn07
XG4iKTsKIAkJcHJpbnQgT1VUICgkdCwKIAkJCSAgICJ3YXJuKFwkXEApIGlmIFwkXEA7XG4iKTsK
KyAgICAgICAgICAgICAgICB9IGVsc2UgeworICAgICAgICAgICAgICAgICAgICAkaW5jbCA9fiBz
L1wuaCQvLnBoLzsKKyAgICAgICAgICAgICAgICAgICAgIyBjb3B5IHRoZSBwcmVmaXggaW4gdGhl
IHF1b3RlIHN5bnRheCAoI2luY2x1ZGUgInguaCIpIGNhc2UKKyAgICAgICAgICAgICAgICAgICAg
aWYgKCRpbmNsICF+IG18L3wgJiYgJGluY2xfcXVvdGUgZXEgcXsifSAmJiAkZmlsZSA9fiBtfF4o
LiopL3wpIHsKKyAgICAgICAgICAgICAgICAgICAgICAgICRpbmNsID0gIiQxLyRpbmNsIjsKKyAg
ICAgICAgICAgICAgICAgICAgfQorCQkgICAgcHJpbnQgT1VUICR0LCJyZXF1aXJlICckaW5jbCc7
XG4iOworICAgICAgICAgICAgICAgIH0KIAkgICAgfSBlbHNpZiAoL15pZmRlZlxzKyhcdyspLykg
ewogCQlwcmludCBPVVQgJHQsImlmKGRlZmluZWQoJiQxKSkge1xuIjsKIAkJJHRhYiArPSA0OwpA
QCAtMjc0LDIyICszMDcsMjIgQEAgd2hpbGUgKGRlZmluZWQgKG15ICRmaWxlID0gbmV4dF9maWxl
KCkpKSB7CiAJICAgIH0KIAl9CiAgICAgfQotICAgIHByaW50IE9VVCAiMTtcbiI7Ci0KICAgICAk
SXNfY29udmVydGVkeyRmaWxlfSA9IDE7CisgICAgaWYgKCRvcHRfZSAmJiBleGlzdHMoJGJhZF9m
aWxleyRmaWxlfSkpIHsKKyAgICAgICAgdW5saW5rKCREZXN0X2RpciAuICcvJyAuICRvdXRmaWxl
KTsKKyAgICAgICAgJG5leHQgPSAnJzsKKyAgICB9IGVsc2UgeworICAgICAgICBwcmludCBPVVQg
IjE7XG4iOwogICAgIHF1ZXVlX2luY2x1ZGVzX2Zyb20oJGZpbGUpIGlmICgkb3B0X2EpOworICAg
IH0KIH0KIAotZXhpdCAkRXhpdDsKLQotCi1zdWIgcmVpbmRlbnQoJCkgewotICAgIG15KCR0ZXh0
KSA9IHNoaWZ0OwotICAgICR0ZXh0ID1+IHMvXG4vXG4gICAgL2c7Ci0gICAgJHRleHQgPX4gcy8g
ICAgICAgIC9cdC9nOwotICAgICR0ZXh0OworaWYgKCRvcHRfZSAmJiAoc2NhbGFyKGtleXMgJWJh
ZF9maWxlKSA+IDApKSB7CisgICAgd2FybiAiV2FzIHVuYWJsZSB0byBjb252ZXJ0IHRoZSBmb2xs
b3dpbmcgZmlsZXM6XG4iOworICAgIHdhcm4gIlx0IiAuIGpvaW4oIlxuXHQiLHNvcnQoa2V5cyAl
YmFkX2ZpbGUpKSAuICJcbiI7CiB9CiAKK2V4aXQgJEV4aXQ7CiAKIHN1YiBleHByIHsKICAgICBt
eSAkam9pbmVkX2FyZ3M7CkBAIC00MDMsNyArNDM2LDcgQEAgc3ViIGV4cHIgewogCQl9CiAJICAg
IH0gZWxzZSB7CiAJCWlmICgkaW5pZiAmJiAkbmV3ICF+IC9kZWZpbmVkXHMqXCgkLykgewotCQkg
ICAgJG5ldyAuPSAnKGRlZmluZWQoJicgLiAkaWQgLiAnKSA/ICYnIC4gJGlkIC4gJyA6IDApJzsK
KwkJICAgICRuZXcgLj0gJyhkZWZpbmVkKCYnIC4gJGlkIC4gJykgPyAmJyAuICRpZCAuICcgOiB1
bmRlZiknOwogCQl9IGVsc2lmICgvXlxbLykgewogCQkgICAgJG5ldyAuPSAiIFwkJGlkIjsKIAkJ
fSBlbHNlIHsKQEAgLTQ2MywxMCArNDk2LDE4IEBAIHN1YiBuZXh0X2xpbmUKICAgICAgICAgICAg
ICAgICAkb3V0ICAgIC49ICQxOwogICAgICAgICAgICAgfSBlbHNpZiAoJGluID1+IHMvXihcXC4p
Ly8pIHsgICAgICAgICAgICAgICAgICAgIyBcLi4uCiAgICAgICAgICAgICAgICAgJG91dCAgICAu
PSAkMTsKLSAgICAgICAgICAgIH0gZWxzaWYgKCRpbiA9fiBzL14oJyhcXC58W14nXFxdKSonKS8v
KSB7ICAgICAgICMgJy4uLgotICAgICAgICAgICAgICAgICRvdXQgICAgLj0gJDE7Ci0gICAgICAg
ICAgICB9IGVsc2lmICgkaW4gPX4gcy9eKCIoXFwufFteIlxcXSkqIikvLykgeyAgICAgICAjICIu
Li4KLSAgICAgICAgICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAgfSBlbHNpZiAo
JGluID1+IC9eJy8pIHsgICAgICAgICAgICAgICAgICAgICAgICAgIyAnLi4uCisgICAgICAgICAg
ICAgICAgaWYgKCRpbiA9fiBzL14oJyhcXC58W14nXFxdKSonKS8vKSB7CisgICAgICAgICAgICAg
ICAgICAgICRvdXQgICAgLj0gJDE7CisgICAgICAgICAgICAgICAgfSBlbHNlIHsKKyAgICAgICAg
ICAgICAgICAgICAgbmV4dCBSRUFEOworICAgICAgICAgICAgICAgIH0KKyAgICAgICAgICAgIH0g
ZWxzaWYgKCRpbiA9fiAvXiIvKSB7ICAgICAgICAgICAgICAgICAgICAgICAgICMgIi4uLgorICAg
ICAgICAgICAgICAgIGlmICgkaW4gPX4gcy9eKCIoXFwufFteIlxcXSkqIikvLykgeworICAgICAg
ICAgICAgICAgICAgICAkb3V0ICAgIC49ICQxOworICAgICAgICAgICAgICAgIH0gZWxzZSB7Cisg
ICAgICAgICAgICAgICAgICAgIG5leHQgUkVBRDsKKyAgICAgICAgICAgICAgICB9CiAgICAgICAg
ICAgICB9IGVsc2lmICgkaW4gPX4gcy9eXC9cLy4qLy8pIHsgICAgICAgICAgICAgICAgICAjIC8v
Li4uCiAgICAgICAgICAgICAgICAgIyBmYWxsIHRocm91Z2gKICAgICAgICAgICAgIH0gZWxzaWYg
KCRpbiA9fiBtL15cL1wqLykgeyAgICAgICAgICAgICAgICAgICAgICMgLyouLi4KQEAgLTQ4NSw3
ICs1MjYsMTUgQEAgc3ViIG5leHRfbGluZQogICAgICAgICAgICAgICAgICAgICAgJGluICAgPX4g
cyFcJ1QgS05PVyEhKSB7CiAgICAgICAgICAgICAgICAgJG91dCAgICA9fiBzIUkgRE9OJCFJX0RP
X05PVF9LTk9XITsKICAgICAgICAgICAgIH0gZWxzZSB7CisgICAgICAgICAgICAgICAgaWYgKCRv
cHRfZSkgeworICAgICAgICAgICAgICAgICAgICB3YXJuICJDYW5ub3QgcGFyc2UgJGZpbGU6XG4k
aW5cbiI7CisgICAgICAgICAgICAgICAgICAgICRiYWRfZmlsZXskZmlsZX0gPSAxOworICAgICAg
ICAgICAgICAgICAgICAkaW4gPSAnJzsKKyAgICAgICAgICAgICAgICAgICAgJG91dCA9IHVuZGVm
OworICAgICAgICAgICAgICAgICAgICBsYXN0IFJFQUQ7CisgICAgICAgICAgICAgICAgfSBlbHNl
IHsKIAkJZGllICJDYW5ub3QgcGFyc2U6XG4kaW5cbiI7CisgICAgICAgICAgICAgICAgfQogICAg
ICAgICAgICAgfQogICAgICAgICB9CiAKQEAgLTU5NSw4ICs2NDQsMTMgQEAgc3ViIHF1ZXVlX2lu
Y2x1ZGVzX2Zyb20KICAgICAgICAgICAgICAgICAkbGluZSAuPSA8SEVBREVSPjsKICAgICAgICAg
ICAgIH0KIAotICAgICAgICAgICAgaWYgKCRsaW5lID1+IC9eI1xzKmluY2x1ZGVccys8KC4qPyk+
LykgewotICAgICAgICAgICAgICAgIHB1c2goQEFSR1YsICQxKSB1bmxlc3MgJElzX2NvbnZlcnRl
ZHskMX07CisgICAgICAgICAgICBpZiAoJGxpbmUgPX4gL14jXHMqaW5jbHVkZVxzKyhbPCJdKSgu
Kj8pWz4iXS8pIHsKKyAgICAgICAgICAgICAgICBteSAoJGRlbGltaXRlciwgJG5ld19maWxlKSA9
ICgkMSwgJDIpOworICAgICAgICAgICAgICAgICMgY29weSB0aGUgcHJlZml4IGluIHRoZSBxdW90
ZSBzeW50YXggKCNpbmNsdWRlICJ4LmgiKSBjYXNlCisgICAgICAgICAgICAgICAgaWYgKCRkZWxp
bWl0ZXIgZXEgcXsifSAmJiAkZmlsZSA9fiBtfF4oLiopL3wpIHsKKyAgICAgICAgICAgICAgICAg
ICAgJG5ld19maWxlID0gIiQxLyRuZXdfZmlsZSI7CisgICAgICAgICAgICAgICAgfQorICAgICAg
ICAgICAgICAgIHB1c2goQEFSR1YsICRuZXdfZmlsZSkgdW5sZXNzICRJc19jb252ZXJ0ZWR7JG5l
d19maWxlfTsKICAgICAgICAgICAgIH0KICAgICAgICAgfQogICAgIGNsb3NlIEhFQURFUjsKQEAg
LTYzNywyNSArNjkxLDUwIEBAIHN1YiBidWlsZF9wcmVhbWJsZV9pZl9uZWNlc3NhcnkKICAgICBt
eSAoJWRlZmluZSkgPSBfZXh0cmFjdF9jY19kZWZpbmVzKCk7CiAKICAgICBvcGVuICBQUkVBTUJM
RSwgIj4kcHJlYW1ibGUiIG9yIGRpZSAiQ2Fubm90IG9wZW4gJHByZWFtYmxlOiAgJCEiOwotICAg
ICAgICBwcmludCBQUkVBTUJMRSAiIyBUaGlzIGZpbGUgd2FzIGNyZWF0ZWQgYnkgaDJwaCB2ZXJz
aW9uICRWRVJTSU9OXG4iOwotCi0gICAgICAgIGZvcmVhY2ggKHNvcnQga2V5cyAlZGVmaW5lKSB7
Ci0gICAgICAgICAgICBpZiAoJG9wdF9EKSB7Ci0gICAgICAgICAgICAgICAgcHJpbnQgUFJFQU1C
TEUgIiMgJF89JGRlZmluZXskX31cbiI7Ci0gICAgICAgICAgICB9Ci0KLSAgICAgICAgICAgIGlm
ICgkZGVmaW5leyRffSA9fiAvXihcZCspVT9MezAsMn0kL2kpIHsKLSAgICAgICAgICAgICAgICBw
cmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykg
eyBzdWIgJF8oKSB7ICQxIH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNpZiAoJGRlZmluZXsk
X30gPX4gL15cdyskLykgewotICAgICAgICAgICAgICAgIHByaW50IFBSRUFNQkxFCi0gICAgICAg
ICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7
JF99IH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNlIHsKKwlwcmludCBQUkVBTUJMRSAiIyBU
aGlzIGZpbGUgd2FzIGNyZWF0ZWQgYnkgaDJwaCB2ZXJzaW9uICRWRVJTSU9OXG4iOworICAgICAg
ICAjIFByZXZlbnQgbm9uLXBvcnRhYmxlIGhleCBjb25zdGFudHMgZnJvbSB3YXJuaW5nLgorICAg
ICAgICAjCisgICAgICAgICMgV2Ugc3RpbGwgcHJvZHVjZSBhbiBvdmVyZmxvdyB3YXJuaW5nIGlm
IHdlIGNhbid0IHJlcHJlc2VudAorICAgICAgICAjIGEgaGV4IGNvbnN0YW50IGFzIGFuIGludGVn
ZXIuCisgICAgICAgIHByaW50IFBSRUFNQkxFICJubyB3YXJuaW5ncyBxdyhwb3J0YWJsZSk7XG4i
OworCisJZm9yZWFjaCAoc29ydCBrZXlzICVkZWZpbmUpIHsKKwkgICAgaWYgKCRvcHRfRCkgewor
CQlwcmludCBQUkVBTUJMRSAiIyAkXz0kZGVmaW5leyRffVxuIjsKKwkgICAgfQorCSAgICBpZiAo
JGRlZmluZXskX30gPX4gL15cKCguKilcKSQvKSB7CisJCSMgcGFyZW50aGVzaXplZCB2YWx1ZTog
IGQ9KHYpCisJCSRkZWZpbmV7JF99ID0gJDE7CisJICAgIH0KKwkgICAgaWYgKCRkZWZpbmV7JF99
ID1+IC9eKFsrLV0/KFxkKyk/XC5cZCsoW2VFXVsrLV0/XGQrKT8pW0ZMXT8kLykgeworCQkjIGZs
b2F0OgorCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3Vi
ICRfKCkgeyAkMSB9IH1cblxuIjsKKwkgICAgfSBlbHNpZiAoJGRlZmluZXskX30gPX4gL14oWyst
XT9cZCspVT9MezAsMn0kL2kpIHsKKwkJIyBpbnRlZ2VyOgorCQlwcmludCBQUkVBTUJMRQorCQkg
ICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkgeyAkMSB9IH1cblxuIjsKKyAgICAg
ICAgICAgIH0gZWxzaWYgKCRkZWZpbmV7JF99ID1+IC9eKFsrLV0/MHhbXGRhLWZdKylVP0x7MCwy
fSQvaSkgeworICAgICAgICAgICAgICAgICMgaGV4IGludGVnZXIKKyAgICAgICAgICAgICAgICAj
IFNwZWNpYWwgY2FzZWQsIHNpbmNlIHBlcmwgd2FybnMgb24gaGV4IGludGVnZXJzCisgICAgICAg
ICAgICAgICAgIyB0aGF0IGNhbid0IGJlIHJlcHJlc2VudGVkIGluIGEgVVYuCisgICAgICAgICAg
ICAgICAgIworICAgICAgICAgICAgICAgICMgVGhpcyB3YXkgd2UgZ2V0IHRoZSB3YXJuaW5nIGF0
IHRpbWUgb2YgdXNlLCBzbyB0aGUgdXNlcgorICAgICAgICAgICAgICAgICMgb25seSBnZXRzIHRo
ZSB3YXJuaW5nIGlmIHRoZXkgaGFwcGVuIHRvIHVzZSB0aGlzCisgICAgICAgICAgICAgICAgIyBw
bGF0Zm9ybS1zcGVjaWZpYyBkZWZpbml0aW9uLgorICAgICAgICAgICAgICAgIG15ICRjb2RlID0g
JDE7CisgICAgICAgICAgICAgICAgJGNvZGUgPSAiaGV4KCckY29kZScpIiBpZiBsZW5ndGggJGNv
ZGUgPiAxMDsKICAgICAgICAgICAgICAgICBwcmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAg
ICAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7IFwiIiwKLSAgICAgICAgICAg
ICAgICAgICAgcXVvdGVtZXRhKCRkZWZpbmV7JF99KSwgIlwiIH0gfVxuXG4iOwotICAgICAgICAg
ICAgfQotICAgICAgICB9CisgICAgICAgICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRf
KSB7IHN1YiAkXygpIHsgJGNvZGUgfSB9XG5cbiI7CisJICAgIH0gZWxzaWYgKCRkZWZpbmV7JF99
ID1+IC9eXHcrJC8pIHsKKwkJcHJpbnQgUFJFQU1CTEUKKwkJICAgICJ1bmxlc3MgKGRlZmluZWQg
JiRfKSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7JF99IH0gfVxuXG4iOworCSAgICB9IGVsc2Ugewor
CQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkg
eyBcIiIsCisJCSAgICBxdW90ZW1ldGEoJGRlZmluZXskX30pLCAiXCIgfSB9XG5cbiI7CisJICAg
IH0KKwl9CiAgICAgY2xvc2UgUFJFQU1CTEUgICAgICAgICAgICAgICBvciBkaWUgIkNhbm5vdCBj
bG9zZSAkcHJlYW1ibGU6ICAkISI7CiB9CiAKQEAgLTY2NywxNSArNzQ2LDE0IEBAIHN1YiBfZXh0
cmFjdF9jY19kZWZpbmVzCiB7CiAgICAgbXkgJWRlZmluZTsKICAgICBteSAkYWxsc3ltYm9scyAg
PSBqb2luICIgIiwKLSAgICAgICAgQENvbmZpZ3snY2NzeW1ib2xzJywgJ2NwcHN5bWJvbHMnLCAn
Y3BwY2NzeW1ib2xzJ307CisJQENvbmZpZ3snY2NzeW1ib2xzJywgJ2NwcHN5bWJvbHMnLCAnY3Bw
Y2NzeW1ib2xzJ307CiAKICAgICAjIFNwbGl0IGNvbXBpbGVyIHByZS1kZWZpbml0aW9ucyBpbnRv
IGBrZXk9dmFsdWUnIHBhaXJzOgotICAgIGZvcmVhY2ggKHNwbGl0IC9ccysvLCAkYWxsc3ltYm9s
cykgewotICAgICAgICAvKC4rPyk9KC4rKS8gYW5kICRkZWZpbmV7JDF9ID0gJDI7Ci0KLSAgICAg
ICAgaWYgKCRvcHRfRCkgewotICAgICAgICAgICAgcHJpbnQgU1RERVJSICIkXzogICQxIC0+ICQy
XG4iOwotICAgICAgICB9CisgICAgd2hpbGUgKCRhbGxzeW1ib2xzID1+IC8oW15cc10rKT0oKFxc
XHN8W15cc10pKykvZykgeworCSRkZWZpbmV7JDF9ID0gJDI7CisJaWYgKCRvcHRfRCkgeworCSAg
ICBwcmludCBTVERFUlIgIiRfOiAgJDEgLT4gJDJcbiI7CisJfQogICAgIH0KIAogICAgIHJldHVy
biAlZGVmaW5lOwpAQCAtNzI1LDcgKzgwMyw3IEBAIElmIHJ1biB3aXRoIG5vIGFyZ3VtZW50cywg
ZmlsdGVycyBzdGFuZGFyZCBpbnB1dCB0byBzdGFuZGFyZCBvdXRwdXQuCiA9aXRlbSAtZCBkZXN0
aW5hdGlvbl9kaXIKIAogUHV0IHRoZSByZXN1bHRpbmcgQjwucGg+IGZpbGVzIGJlbmVhdGggQjxk
ZXN0aW5hdGlvbl9kaXI+LCBpbnN0ZWFkIG9mCi1iZW5lYXRoIHRoZSBkZWZhdWx0IFBlcmwgbGli
cmFyeSBsb2NhdGlvbiAoQzwkQ29uZmlneydpbnN0YWxsc2l0c2VhcmNoJ30+KS4KK2JlbmVhdGgg
dGhlIGRlZmF1bHQgUGVybCBsaWJyYXJ5IGxvY2F0aW9uIChDPCRDb25maWd7J2luc3RhbGxzaXRl
YXJjaCd9PikuCiAKID1pdGVtIC1yCiAKQEAgLTgxMCwxMCArODg4LDEwIEBAIGluc3RhbGxhdGlv
bi4KIERvZXNuJ3QgaGFuZGxlIGNvbXBsaWNhdGVkIGV4cHJlc3Npb25zIGJ1aWx0IHBpZWNlbWVh
bCwgYSBsYToKIAogICAgIGVudW0gewotICAgICAgICBGSVJTVF9WQUxVRSwKLSAgICAgICAgU0VD
T05EX1ZBTFVFLAorCUZJUlNUX1ZBTFVFLAorCVNFQ09ORF9WQUxVRSwKICAgICAjaWZkZWYgQUJD
Ci0gICAgICAgIFRISVJEX1ZBTFVFCisJVEhJUkRfVkFMVUUKICAgICAjZW5kaWYKICAgICB9Owog
Cg==
UH2PH580
  }
  if ( $num < 5.008009 ) {
    return _patch_b64(<<'UH2PH588');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTg0LDcgKzg0LDcgQEAgc3Vi
IHJlaW5kZW50KCQpIHsKIH0KIAogbXkgKCR0LCAkdGFiLCAlY3VyYXJncywgJG5ldywgJGV2YWxf
aW5kZXgsICRkaXIsICRuYW1lLCAkYXJncywgJG91dGZpbGUpOwotbXkgKCRpbmNsLCAkaW5jbF90
eXBlLCAkbmV4dCk7CitteSAoJGluY2wsICRpbmNsX3R5cGUsICRpbmNsX3F1b3RlLCAkbmV4dCk7
CiB3aGlsZSAoZGVmaW5lZCAobXkgJGZpbGUgPSBuZXh0X2ZpbGUoKSkpIHsKICAgICBpZiAoLWwg
JGZpbGUgYW5kIC1kICRmaWxlKSB7CiAgICAgICAgIGxpbmtfaWZfcG9zc2libGUoJGZpbGUpIGlm
ICgkb3B0X2wpOwpAQCAtMTg0LDkgKzE4NCwxMCBAQCB3aGlsZSAoZGVmaW5lZCAobXkgJGZpbGUg
PSBuZXh0X2ZpbGUoKSkpIHsKICAgICAgICAgICAgICAgICAgICAgICBwcmludCBPVVQgJHQsInVu
bGVzcyhkZWZpbmVkKFwmJG5hbWUpKSB7XG4gICAgc3ViICRuYW1lICgpIHtcdCIsJG5ldywiO31c
bn1cbiI7CiAJCSAgICB9CiAJCX0KLQkgICAgfSBlbHNpZiAoL14oaW5jbHVkZXxpbXBvcnR8aW5j
bHVkZV9uZXh0KVxzKls8XCJdKC4qKVs+XCJdLykgeworCSAgICB9IGVsc2lmICgvXihpbmNsdWRl
fGltcG9ydHxpbmNsdWRlX25leHQpXHMqKFs8XCJdKSguKilbPlwiXS8pIHsKICAgICAgICAgICAg
ICAgICAkaW5jbF90eXBlID0gJDE7Ci0gICAgICAgICAgICAgICAgJGluY2wgPSAkMjsKKyAgICAg
ICAgICAgICAgICAkaW5jbF9xdW90ZSA9ICQyOworICAgICAgICAgICAgICAgICRpbmNsID0gJDM7
CiAgICAgICAgICAgICAgICAgaWYgKCgkaW5jbF90eXBlIGVxICdpbmNsdWRlX25leHQnKSB8fAog
ICAgICAgICAgICAgICAgICAgICAoJG9wdF9lICYmIGV4aXN0cygkYmFkX2ZpbGV7JGluY2x9KSkp
IHsKICAgICAgICAgICAgICAgICAgICAgJGluY2wgPX4gcy9cLmgkLy5waC87CkBAIC0yMTksNiAr
MjIwLDEwIEBAIHdoaWxlIChkZWZpbmVkIChteSAkZmlsZSA9IG5leHRfZmlsZSgpKSkgewogCQkJ
ICAgIndhcm4oXCRcQCkgaWYgXCRcQDtcbiIpOwogICAgICAgICAgICAgICAgIH0gZWxzZSB7CiAg
ICAgICAgICAgICAgICAgICAgICRpbmNsID1+IHMvXC5oJC8ucGgvOworICAgICAgICAgICAgICAg
ICAgICAjIGNvcHkgdGhlIHByZWZpeCBpbiB0aGUgcXVvdGUgc3ludGF4ICgjaW5jbHVkZSAieC5o
IikgY2FzZQorICAgICAgICAgICAgICAgICAgICBpZiAoJGluY2wgIX4gbXwvfCAmJiAkaW5jbF9x
dW90ZSBlcSBxeyJ9ICYmICRmaWxlID1+IG18XiguKikvfCkgeworICAgICAgICAgICAgICAgICAg
ICAgICAgJGluY2wgPSAiJDEvJGluY2wiOworICAgICAgICAgICAgICAgICAgICB9CiAJCSAgICBw
cmludCBPVVQgJHQsInJlcXVpcmUgJyRpbmNsJztcbiI7CiAgICAgICAgICAgICAgICAgfQogCSAg
ICB9IGVsc2lmICgvXmlmZGVmXHMrKFx3KykvKSB7CkBAIC00MzEsNyArNDM2LDcgQEAgc3ViIGV4
cHIgewogCQl9CiAJICAgIH0gZWxzZSB7CiAJCWlmICgkaW5pZiAmJiAkbmV3ICF+IC9kZWZpbmVk
XHMqXCgkLykgewotCQkgICAgJG5ldyAuPSAnKGRlZmluZWQoJicgLiAkaWQgLiAnKSA/ICYnIC4g
JGlkIC4gJyA6IDApJzsKKwkJICAgICRuZXcgLj0gJyhkZWZpbmVkKCYnIC4gJGlkIC4gJykgPyAm
JyAuICRpZCAuICcgOiB1bmRlZiknOwogCQl9IGVsc2lmICgvXlxbLykgewogCQkgICAgJG5ldyAu
PSAiIFwkJGlkIjsKIAkJfSBlbHNlIHsKQEAgLTYzOSw4ICs2NDQsMTMgQEAgc3ViIHF1ZXVlX2lu
Y2x1ZGVzX2Zyb20KICAgICAgICAgICAgICAgICAkbGluZSAuPSA8SEVBREVSPjsKICAgICAgICAg
ICAgIH0KIAotICAgICAgICAgICAgaWYgKCRsaW5lID1+IC9eI1xzKmluY2x1ZGVccys8KC4qPyk+
LykgewotICAgICAgICAgICAgICAgIHB1c2goQEFSR1YsICQxKSB1bmxlc3MgJElzX2NvbnZlcnRl
ZHskMX07CisgICAgICAgICAgICBpZiAoJGxpbmUgPX4gL14jXHMqaW5jbHVkZVxzKyhbPCJdKSgu
Kj8pWz4iXS8pIHsKKyAgICAgICAgICAgICAgICBteSAoJGRlbGltaXRlciwgJG5ld19maWxlKSA9
ICgkMSwgJDIpOworICAgICAgICAgICAgICAgICMgY29weSB0aGUgcHJlZml4IGluIHRoZSBxdW90
ZSBzeW50YXggKCNpbmNsdWRlICJ4LmgiKSBjYXNlCisgICAgICAgICAgICAgICAgaWYgKCRkZWxp
bWl0ZXIgZXEgcXsifSAmJiAkZmlsZSA9fiBtfF4oLiopL3wpIHsKKyAgICAgICAgICAgICAgICAg
ICAgJG5ld19maWxlID0gIiQxLyRuZXdfZmlsZSI7CisgICAgICAgICAgICAgICAgfQorICAgICAg
ICAgICAgICAgIHB1c2goQEFSR1YsICRuZXdfZmlsZSkgdW5sZXNzICRJc19jb252ZXJ0ZWR7JG5l
d19maWxlfTsKICAgICAgICAgICAgIH0KICAgICAgICAgfQogICAgIGNsb3NlIEhFQURFUjsKQEAg
LTY4MSwyNSArNjkxLDUwIEBAIHN1YiBidWlsZF9wcmVhbWJsZV9pZl9uZWNlc3NhcnkKICAgICBt
eSAoJWRlZmluZSkgPSBfZXh0cmFjdF9jY19kZWZpbmVzKCk7CiAKICAgICBvcGVuICBQUkVBTUJM
RSwgIj4kcHJlYW1ibGUiIG9yIGRpZSAiQ2Fubm90IG9wZW4gJHByZWFtYmxlOiAgJCEiOwotICAg
ICAgICBwcmludCBQUkVBTUJMRSAiIyBUaGlzIGZpbGUgd2FzIGNyZWF0ZWQgYnkgaDJwaCB2ZXJz
aW9uICRWRVJTSU9OXG4iOwotCi0gICAgICAgIGZvcmVhY2ggKHNvcnQga2V5cyAlZGVmaW5lKSB7
Ci0gICAgICAgICAgICBpZiAoJG9wdF9EKSB7Ci0gICAgICAgICAgICAgICAgcHJpbnQgUFJFQU1C
TEUgIiMgJF89JGRlZmluZXskX31cbiI7Ci0gICAgICAgICAgICB9Ci0KLSAgICAgICAgICAgIGlm
ICgkZGVmaW5leyRffSA9fiAvXihcZCspVT9MezAsMn0kL2kpIHsKLSAgICAgICAgICAgICAgICBw
cmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykg
eyBzdWIgJF8oKSB7ICQxIH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNpZiAoJGRlZmluZXsk
X30gPX4gL15cdyskLykgewotICAgICAgICAgICAgICAgIHByaW50IFBSRUFNQkxFCi0gICAgICAg
ICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7
JF99IH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNlIHsKKwlwcmludCBQUkVBTUJMRSAiIyBU
aGlzIGZpbGUgd2FzIGNyZWF0ZWQgYnkgaDJwaCB2ZXJzaW9uICRWRVJTSU9OXG4iOworICAgICAg
ICAjIFByZXZlbnQgbm9uLXBvcnRhYmxlIGhleCBjb25zdGFudHMgZnJvbSB3YXJuaW5nLgorICAg
ICAgICAjCisgICAgICAgICMgV2Ugc3RpbGwgcHJvZHVjZSBhbiBvdmVyZmxvdyB3YXJuaW5nIGlm
IHdlIGNhbid0IHJlcHJlc2VudAorICAgICAgICAjIGEgaGV4IGNvbnN0YW50IGFzIGFuIGludGVn
ZXIuCisgICAgICAgIHByaW50IFBSRUFNQkxFICJubyB3YXJuaW5ncyBxdyhwb3J0YWJsZSk7XG4i
OworCisJZm9yZWFjaCAoc29ydCBrZXlzICVkZWZpbmUpIHsKKwkgICAgaWYgKCRvcHRfRCkgewor
CQlwcmludCBQUkVBTUJMRSAiIyAkXz0kZGVmaW5leyRffVxuIjsKKwkgICAgfQorCSAgICBpZiAo
JGRlZmluZXskX30gPX4gL15cKCguKilcKSQvKSB7CisJCSMgcGFyZW50aGVzaXplZCB2YWx1ZTog
IGQ9KHYpCisJCSRkZWZpbmV7JF99ID0gJDE7CisJICAgIH0KKwkgICAgaWYgKCRkZWZpbmV7JF99
ID1+IC9eKFsrLV0/KFxkKyk/XC5cZCsoW2VFXVsrLV0/XGQrKT8pW0ZMXT8kLykgeworCQkjIGZs
b2F0OgorCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3Vi
ICRfKCkgeyAkMSB9IH1cblxuIjsKKwkgICAgfSBlbHNpZiAoJGRlZmluZXskX30gPX4gL14oWyst
XT9cZCspVT9MezAsMn0kL2kpIHsKKwkJIyBpbnRlZ2VyOgorCQlwcmludCBQUkVBTUJMRQorCQkg
ICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkgeyAkMSB9IH1cblxuIjsKKyAgICAg
ICAgICAgIH0gZWxzaWYgKCRkZWZpbmV7JF99ID1+IC9eKFsrLV0/MHhbXGRhLWZdKylVP0x7MCwy
fSQvaSkgeworICAgICAgICAgICAgICAgICMgaGV4IGludGVnZXIKKyAgICAgICAgICAgICAgICAj
IFNwZWNpYWwgY2FzZWQsIHNpbmNlIHBlcmwgd2FybnMgb24gaGV4IGludGVnZXJzCisgICAgICAg
ICAgICAgICAgIyB0aGF0IGNhbid0IGJlIHJlcHJlc2VudGVkIGluIGEgVVYuCisgICAgICAgICAg
ICAgICAgIworICAgICAgICAgICAgICAgICMgVGhpcyB3YXkgd2UgZ2V0IHRoZSB3YXJuaW5nIGF0
IHRpbWUgb2YgdXNlLCBzbyB0aGUgdXNlcgorICAgICAgICAgICAgICAgICMgb25seSBnZXRzIHRo
ZSB3YXJuaW5nIGlmIHRoZXkgaGFwcGVuIHRvIHVzZSB0aGlzCisgICAgICAgICAgICAgICAgIyBw
bGF0Zm9ybS1zcGVjaWZpYyBkZWZpbml0aW9uLgorICAgICAgICAgICAgICAgIG15ICRjb2RlID0g
JDE7CisgICAgICAgICAgICAgICAgJGNvZGUgPSAiaGV4KCckY29kZScpIiBpZiBsZW5ndGggJGNv
ZGUgPiAxMDsKICAgICAgICAgICAgICAgICBwcmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAg
ICAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIgJF8oKSB7IFwiIiwKLSAgICAgICAgICAg
ICAgICAgICAgcXVvdGVtZXRhKCRkZWZpbmV7JF99KSwgIlwiIH0gfVxuXG4iOwotICAgICAgICAg
ICAgfQotICAgICAgICB9CisgICAgICAgICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRf
KSB7IHN1YiAkXygpIHsgJGNvZGUgfSB9XG5cbiI7CisJICAgIH0gZWxzaWYgKCRkZWZpbmV7JF99
ID1+IC9eXHcrJC8pIHsKKwkJcHJpbnQgUFJFQU1CTEUKKwkJICAgICJ1bmxlc3MgKGRlZmluZWQg
JiRfKSB7IHN1YiAkXygpIHsgJiRkZWZpbmV7JF99IH0gfVxuXG4iOworCSAgICB9IGVsc2Ugewor
CQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkg
eyBcIiIsCisJCSAgICBxdW90ZW1ldGEoJGRlZmluZXskX30pLCAiXCIgfSB9XG5cbiI7CisJICAg
IH0KKwl9CiAgICAgY2xvc2UgUFJFQU1CTEUgICAgICAgICAgICAgICBvciBkaWUgIkNhbm5vdCBj
bG9zZSAkcHJlYW1ibGU6ICAkISI7CiB9CiAKQEAgLTcxMSwxNSArNzQ2LDE0IEBAIHN1YiBfZXh0
cmFjdF9jY19kZWZpbmVzCiB7CiAgICAgbXkgJWRlZmluZTsKICAgICBteSAkYWxsc3ltYm9scyAg
PSBqb2luICIgIiwKLSAgICAgICAgQENvbmZpZ3snY2NzeW1ib2xzJywgJ2NwcHN5bWJvbHMnLCAn
Y3BwY2NzeW1ib2xzJ307CisJQENvbmZpZ3snY2NzeW1ib2xzJywgJ2NwcHN5bWJvbHMnLCAnY3Bw
Y2NzeW1ib2xzJ307CiAKICAgICAjIFNwbGl0IGNvbXBpbGVyIHByZS1kZWZpbml0aW9ucyBpbnRv
IGBrZXk9dmFsdWUnIHBhaXJzOgotICAgIGZvcmVhY2ggKHNwbGl0IC9ccysvLCAkYWxsc3ltYm9s
cykgewotICAgICAgICAvKC4rPyk9KC4rKS8gYW5kICRkZWZpbmV7JDF9ID0gJDI7Ci0KLSAgICAg
ICAgaWYgKCRvcHRfRCkgewotICAgICAgICAgICAgcHJpbnQgU1RERVJSICIkXzogICQxIC0+ICQy
XG4iOwotICAgICAgICB9CisgICAgd2hpbGUgKCRhbGxzeW1ib2xzID1+IC8oW15cc10rKT0oKFxc
XHN8W15cc10pKykvZykgeworCSRkZWZpbmV7JDF9ID0gJDI7CisJaWYgKCRvcHRfRCkgeworCSAg
ICBwcmludCBTVERFUlIgIiRfOiAgJDEgLT4gJDJcbiI7CisJfQogICAgIH0KIAogICAgIHJldHVy
biAlZGVmaW5lOwpAQCAtNzY5LDcgKzgwMyw3IEBAIElmIHJ1biB3aXRoIG5vIGFyZ3VtZW50cywg
ZmlsdGVycyBzdGFuZGFyZCBpbnB1dCB0byBzdGFuZGFyZCBvdXRwdXQuCiA9aXRlbSAtZCBkZXN0
aW5hdGlvbl9kaXIKIAogUHV0IHRoZSByZXN1bHRpbmcgQjwucGg+IGZpbGVzIGJlbmVhdGggQjxk
ZXN0aW5hdGlvbl9kaXI+LCBpbnN0ZWFkIG9mCi1iZW5lYXRoIHRoZSBkZWZhdWx0IFBlcmwgbGli
cmFyeSBsb2NhdGlvbiAoQzwkQ29uZmlneydpbnN0YWxsc2l0c2VhcmNoJ30+KS4KK2JlbmVhdGgg
dGhlIGRlZmF1bHQgUGVybCBsaWJyYXJ5IGxvY2F0aW9uIChDPCRDb25maWd7J2luc3RhbGxzaXRl
YXJjaCd9PikuCiAKID1pdGVtIC1yCiAKQEAgLTg1NCwxMCArODg4LDEwIEBAIGluc3RhbGxhdGlv
bi4KIERvZXNuJ3QgaGFuZGxlIGNvbXBsaWNhdGVkIGV4cHJlc3Npb25zIGJ1aWx0IHBpZWNlbWVh
bCwgYSBsYToKIAogICAgIGVudW0gewotICAgICAgICBGSVJTVF9WQUxVRSwKLSAgICAgICAgU0VD
T05EX1ZBTFVFLAorCUZJUlNUX1ZBTFVFLAorCVNFQ09ORF9WQUxVRSwKICAgICAjaWZkZWYgQUJD
Ci0gICAgICAgIFRISVJEX1ZBTFVFCisJVEhJUkRfVkFMVUUKICAgICAjZW5kaWYKICAgICB9Owog
Cg==
UH2PH588
  }
  if ( $num > 5.008009 and $num < 5.009002 ) {
    _patch_b64(<<'UH2PH592');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTU4LDEzICs1OCwxNCBAQAog
ZGllICJEZXN0aW5hdGlvbiBkaXJlY3RvcnkgJERlc3RfZGlyIGRvZXNuJ3QgZXhpc3Qgb3IgaXNu
J3QgYSBkaXJlY3RvcnlcbiIKICAgICB1bmxlc3MgLWQgJERlc3RfZGlyOwogCi1teSBAaXNhdHlw
ZSA9IHNwbGl0KCcgJyw8PEVORCk7CitteSBAaXNhdHlwZSA9IHF3KAogCWNoYXIJdWNoYXIJdV9j
aGFyCiAJc2hvcnQJdXNob3J0CXVfc2hvcnQKIAlpbnQJdWludAl1X2ludAogCWxvbmcJdWxvbmcJ
dV9sb25nCiAJRklMRQlrZXlfdAljYWRkcl90Ci1FTkQKKwlmbG9hdAlkb3VibGUJc2l6ZV90Cisp
OwogCiBteSAlaXNhdHlwZTsKIEBpc2F0eXBle0Bpc2F0eXBlfSA9ICgxKSB4IEBpc2F0eXBlOwpA
QCAtMTMzLDE5ICsxMzQsMjAgQEAKIAkJcy9cKFx3K1xzKlwoXCpcKVxzKlwoXHcqXClcKVxzKigt
P1xkKykvJDEvOyAjIChpbnQgKCopKGZvb190KSkwCiAJCWlmIChzL15cKChbXHcsXHNdKilcKS8v
KSB7CiAJCSAgICAkYXJncyA9ICQxOwotICAgIAkgICAgCSAgICBteSAkcHJvdG8gPSAnKCkgJzsK
KwkJICAgIG15ICRwcm90byA9ICcoKSAnOwogCQkgICAgaWYgKCRhcmdzIG5lICcnKSB7Ci0gICAg
CSAgICAJICAgIAkkcHJvdG8gPSAnJzsKKwkJCSRwcm90byA9ICcnOwogCQkJZm9yZWFjaCBteSAk
YXJnIChzcGxpdCgvLFxzKi8sJGFyZ3MpKSB7CiAJCQkgICAgJGFyZyA9fiBzL15ccyooW15cc10u
KlteXHNdKVxzKiQvJDEvOwogCQkJICAgICRjdXJhcmdzeyRhcmd9ID0gMTsKIAkJCX0KIAkJCSRh
cmdzID1+IHMvXGIoXHcpL1wkJDEvZzsKLQkJCSRhcmdzID0gImxvY2FsKCRhcmdzKSA9IFxAXztc
biR0ICAgICI7CisJCQkkYXJncyA9ICJteSgkYXJncykgPSBcQF87XG4kdCAgICAiOwogCQkgICAg
fQogCQkgICAgcy9eXHMrLy87CiAJCSAgICBleHByKCk7CiAJCSAgICAkbmV3ID1+IHMvKFsiXFxd
KS9cXCQxL2c7ICAgICAgICMiXSk7CisJCSAgRU1JVDoKIAkJICAgICRuZXcgPSByZWluZGVudCgk
bmV3KTsKIAkJICAgICRhcmdzID0gcmVpbmRlbnQoJGFyZ3MpOwogCQkgICAgaWYgKCR0IG5lICcn
KSB7CkBAIC0yNjgsMTIgKzI3MCwxNCBAQAogCSAgICB9IGVsc2lmKC9eaWRlbnRccysoLiopLykg
ewogCQlwcmludCBPVVQgJHQsICIjICQxXG4iOwogCSAgICB9Ci0JfSBlbHNpZigvXlxzKih0eXBl
ZGVmXHMqKT9lbnVtXHMqKFxzK1thLXpBLVpfXVx3KlxzKik/LykgeworCX0gZWxzaWYgKC9eXHMq
KHR5cGVkZWZccyopP2VudW1ccyooXHMrW2EtekEtWl9dXHcqXHMqKT8vKSB7ICMgeyBmb3IgdmkK
IAkgICAgdW50aWwoL1x7W159XSpcfS4qOy8gfHwgLzsvKSB7CiAJCWxhc3QgdW5sZXNzIGRlZmlu
ZWQgKCRuZXh0ID0gbmV4dF9saW5lKCRmaWxlKSk7CiAJCWNob21wICRuZXh0OwogCQkjIGRyb3Ag
IiNkZWZpbmUgRk9PIEZPTyIgaW4gZW51bXMKIAkJJG5leHQgPX4gcy9eXHMqI1xzKmRlZmluZVxz
KyhcdyspXHMrXDFccyokLy87CisJCSMgI2RlZmluZXMgaW4gZW51bXMgKGFsaWFzZXMpCisJCSRu
ZXh0ID1+IHMvXlxzKiNccypkZWZpbmVccysoXHcrKVxzKyhcdyspXHMqJC8kMSA9ICQyLC87CiAJ
CSRfIC49ICRuZXh0OwogCQlwcmludCBPVVQgIiMgJG5leHRcbiIgaWYgJG9wdF9EOwogCSAgICB9
CkBAIC0yODYsNiArMjkwLDcgQEAKIAkgICAgbXkgJGVudW1fdmFsID0gLTE7CiAJICAgIGZvcmVh
Y2ggbXkgJGVudW0gKEBlbnVtX3N1YnMpIHsKIAkJbXkgKCRlbnVtX25hbWUsICRlbnVtX3ZhbHVl
KSA9ICRlbnVtID1+IC9eKFthLXpBLVpfXVx3KikoPS4rKT8kLzsKKwkJJGVudW1fbmFtZSBvciBu
ZXh0OwogCQkkZW51bV92YWx1ZSA9fiBzL149Ly87CiAJCSRlbnVtX3ZhbCA9IChsZW5ndGgoJGVu
dW1fdmFsdWUpID8gJGVudW1fdmFsdWUgOiAkZW51bV92YWwgKyAxKTsKIAkJaWYgKCRvcHRfaCkg
ewpAQCAtMzAwLDYgKzMwNSw3NSBAQAogCQkJICAgICAgICJ1bmxlc3MgZGVmaW5lZChcJiRlbnVt
X25hbWUpO1xuIik7CiAJCX0KIAkgICAgfQorCX0gZWxzaWYgKC9eKD86X19leHRlbnNpb25fX1xz
Kyk/KD86ZXh0ZXJufHN0YXRpYylccysoPzpfXyk/aW5saW5lKD86X18pP1xzKy8KKwkgICAgYW5k
ICEvO1xzKiQvIGFuZCAhL3tccyp9XHMqJC8pCisJeyAjIHsgZm9yIHZpCisJICAgICMgVGhpcyBp
cyBhIGhhY2sgdG8gcGFyc2UgdGhlIGlubGluZSBmdW5jdGlvbnMgaW4gdGhlIGdsaWJjIGhlYWRl
cnMuCisJICAgICMgV2FybmluZzogbWFzc2l2ZSBrbHVkZ2UgYWhlYWQuIFdlIHN1cHBvc2UgaW5s
aW5lIGZ1bmN0aW9ucworCSAgICAjIGFyZSBtYWlubHkgY29uc3RydWN0ZWQgbGlrZSBtYWNyb3Mu
CisJICAgIHdoaWxlICgxKSB7CisJCWxhc3QgdW5sZXNzIGRlZmluZWQgKCRuZXh0ID0gbmV4dF9s
aW5lKCRmaWxlKSk7CisJCWNob21wICRuZXh0OworCQl1bmRlZiAkXywgbGFzdCBpZiAkbmV4dCA9
fiAvX19USFJPV1xzKjsvCisJCQkgICAgICAgb3IgJG5leHQgPX4gL14oX19leHRlbnNpb25fX3xl
eHRlcm58c3RhdGljKVxiLzsKKwkJJF8gLj0gIiAkbmV4dCI7CisJCXByaW50IE9VVCAiIyAkbmV4
dFxuIiBpZiAkb3B0X0Q7CisJCWxhc3QgaWYgJG5leHQgPX4gL159fF57Lip9XHMqJC87CisJICAg
IH0KKwkgICAgbmV4dCBpZiBub3QgZGVmaW5lZDsgIyBiZWNhdXNlIGl0J3Mgb25seSBhIHByb3Rv
dHlwZQorCSAgICBzL1xiKF9fZXh0ZW5zaW9uX198ZXh0ZXJufHN0YXRpY3woPzpfXyk/aW5saW5l
KD86X18pPylcYi8vZzsKKwkgICAgIyB2aW9sZW50bHkgZHJvcCAjaWZkZWZzCisJICAgIHMvI1xz
KmlmLio/I1xzKmVuZGlmLy9nCisJCWFuZCBwcmludCBPVVQgIiMgc29tZSAjaWZkZWYgd2VyZSBk
cm9wcGVkIGhlcmUgLS0gZmlsbCBpbiB0aGUgYmxhbmtzXG4iOworCSAgICBpZiAocy9eKD86XHd8
XHN8XCopKlxzKFx3KylccyovLykgeworCQkkbmFtZSA9ICQxOworCSAgICB9IGVsc2UgeworCQl3
YXJuICJuYW1lIG5vdCBmb3VuZCI7IG5leHQ7ICMgc2hvdWxkbid0IG9jY3VyLi4uCisJICAgIH0K
KwkgICAgbXkgQGFyZ3M7CisJICAgIGlmIChzL15cKChbXigpXSopXClccyooXHcrXHMqKSovLykg
eworCQlmb3IgbXkgJGFyZyAoc3BsaXQgLywvLCAkMSkgeworCQkgICAgaWYgKCRhcmcgPX4gLyhc
dyspXHMqJC8pIHsKKwkJCSRjdXJhcmdzeyQxfSA9IDE7CisJCQlwdXNoIEBhcmdzLCAkMTsKKwkJ
ICAgIH0KKwkJfQorCSAgICB9CisJICAgICRhcmdzID0gKAorCQlAYXJncworCQk/ICJteSgiIC4g
KGpvaW4gJywnLCBtYXAgIlwkJF8iLCBAYXJncykgLiAiKSA9IFxAXztcbiR0ICAgICIKKwkJOiAi
IgorCSAgICApOworCSAgICBteSAkcHJvdG8gPSBAYXJncyA/ICcnIDogJygpICc7CisJICAgICRu
ZXcgPSAnJzsKKwkgICAgcy9cYnJldHVyblxiLy9nOyAjICJyZXR1cm4iIGRvZXNuJ3Qgb2NjdXIg
aW4gbWFjcm9zIHVzdWFsbHkuLi4KKwkgICAgZXhwcigpOworCSAgICAjIHRyeSB0byBmaW5kIGFu
ZCBwZXJsaWZ5IGxvY2FsIEMgdmFyaWFibGVzCisJICAgIG91ciBAbG9jYWxfdmFyaWFibGVzID0g
KCk7ICMgbmVlZHMgdG8gYmUgYSBvdXIoKTogKD97Li4ufSkgYnVnIHdvcmthcm91bmQKKwkgICAg
eworCQl1c2UgcmUgImV2YWwiOworCQlteSAkdHlwZWxpc3QgPSBqb2luICd8Jywga2V5cyAlaXNh
dHlwZTsKKwkJJG5ldyA9fiBzWycKKwkJICAoPzooPzp1bik/c2lnbmVkXHMrKT8KKwkJICAoPzps
b25nXHMrKT8KKwkJICAoPzokdHlwZWxpc3QpXHMrCisJCSAgKFx3KykKKwkJICAoP3sgcHVzaCBA
bG9jYWxfdmFyaWFibGVzLCAkMSB9KQorCQkgICddCisJCSBbbXkgXCQkMV1neDsKKwkJJG5ldyA9
fiBzWycKKwkJICAoPzooPzp1bik/c2lnbmVkXHMrKT8KKwkJICAoPzpsb25nXHMrKT8KKwkJICAo
PzokdHlwZWxpc3QpXHMrCisJCSAgJyBccysgJihcdyspIFxzKiA7CisJCSAgKD97IHB1c2ggQGxv
Y2FsX3ZhcmlhYmxlcywgJDEgfSkKKwkJICBdCisJCSBbbXkgXCQkMTtdZ3g7CisJICAgICB9CisJ
ICAgICRuZXcgPX4gcy8mJF9cYi9cJCRfL2cgZm9yIEBsb2NhbF92YXJpYWJsZXM7CisJICAgICRu
ZXcgPX4gcy8oWyJcXF0pL1xcJDEvZzsgICAgICAgIyJdKTsKKwkgICAgIyBub3cgdGhhdCdzIGFs
bW9zdCBsaWtlIGEgbWFjcm8gKHdlIGhvcGUpCisJICAgIGdvdG8gRU1JVDsKIAl9CiAgICAgfQog
ICAgICRJc19jb252ZXJ0ZWR7JGZpbGV9ID0gMTsKQEAgLTMwOCw3ICszODIsNyBAQAogICAgICAg
ICAkbmV4dCA9ICcnOwogICAgIH0gZWxzZSB7CiAgICAgICAgIHByaW50IE9VVCAiMTtcbiI7Ci0g
ICAgcXVldWVfaW5jbHVkZXNfZnJvbSgkZmlsZSkgaWYgKCRvcHRfYSk7CisJcXVldWVfaW5jbHVk
ZXNfZnJvbSgkZmlsZSkgaWYgJG9wdF9hOwogICAgIH0KIH0KIApAQCAtMzIwLDYgKzM5NCw3IEBA
CiBleGl0ICRFeGl0OwogCiBzdWIgZXhwciB7CisgICAgJG5ldyA9ICciKGFzc2VtYmx5IGNvZGUp
IicgYW5kIHJldHVybiBpZiAvXGJfX2FzbV9fXGIvOyAjIGZyZWFrIG91dC4KICAgICBteSAkam9p
bmVkX2FyZ3M7CiAgICAgaWYoa2V5cyglY3VyYXJncykpIHsKIAkkam9pbmVkX2FyZ3MgPSBqb2lu
KCd8Jywga2V5cyglY3VyYXJncykpOwpAQCAtMzI4LDcgKzQwMyw3IEBACiAJcy9eXCZcJi8vICYm
IGRvIHsgJG5ldyAuPSAiICYmIjsgbmV4dDt9OyAjIGhhbmRsZSAmJiBvcGVyYXRvcgogCXMvXlwm
KFtcKGEtelwpXSspLyQxL2k7CSMgaGFjayBmb3IgdGhpbmdzIHRoYXQgdGFrZSB0aGUgYWRkcmVz
cyBvZgogCXMvXihccyspLy8JCSYmIGRvIHskbmV3IC49ICcgJzsgbmV4dDt9OwotCXMvXjBYKFsw
LTlBLUZdKylbVUxdKi8vaSAKKwlzL14wWChbMC05QS1GXSspW1VMXSovL2kKIAkgICAgJiYgZG8g
e215ICRoZXggPSAkMTsKIAkJICAgJGhleCA9fiBzL14wKy8vOwogCQkgICBpZiAobGVuZ3RoICRo
ZXggPiA4ICYmICEkQ29uZmlne3VzZTY0Yml0aW50fSkgewpAQCAtMzgwLDEwICs0NTUsMTYgQEAK
ICAgICAgICAgfTsKIAkjIEVsaW1pbmF0ZSB0eXBlZGVmcwogCS9cKChbXHdcc10rKVtcKlxzXSpc
KVxzKltcd1woXS8gJiYgZG8geworCSAgICBteSAkZG9pdCA9IDE7CiAJICAgIGZvcmVhY2ggKHNw
bGl0IC9ccysvLCAkMSkgeyAgIyBNYWtlIHN1cmUgYWxsIHRoZSB3b3JkcyBhcmUgdHlwZXMsCi0J
CWxhc3QgdW5sZXNzICgkaXNhdHlwZXskX30gb3IgJF8gZXEgJ3N0cnVjdCcgb3IgJF8gZXEgJ3Vu
aW9uJyk7CisJICAgICAgICB1bmxlc3MoJGlzYXR5cGV7JF99IG9yICRfIGVxICdzdHJ1Y3QnIG9y
ICRfIGVxICd1bmlvbicpeworCQkgICAgJGRvaXQgPSAwOworCQkgICAgbGFzdDsKKwkJfQorCSAg
ICB9CisJICAgIGlmKCAkZG9pdCApeworCQlzL1woW1x3XHNdK1tcKlxzXSpcKS8vICYmIG5leHQ7
ICAgICAgIyB0aGVuIGVsaW1pbmF0ZSB0aGVtLgogCSAgICB9Ci0JICAgIHMvXChbXHdcc10rW1wq
XHNdKlwpLy8gJiYgbmV4dDsgICAgICAjIHRoZW4gZWxpbWluYXRlIHRoZW0uCiAJfTsKIAkjIHN0
cnVjdC91bmlvbiBtZW1iZXIsIGluY2x1ZGluZyBhcnJheXM6CiAJcy9eKFtfQS1aXVx3KihcW1te
XF1dK1xdKT8oKFwufC0+KVtfQS1aXVx3KihcW1teXF1dK1xdKT8pKykvL2kgJiYgZG8gewpAQCAt
NDU4LDcgKzUzOSw3IEBACiAKICAgICAgICAgd2hpbGUgKGxlbmd0aCAkaW4pIHsKICAgICAgICAg
ICAgIGlmICgkcHJlX3N1Yl90cmlfZ3JhcGhzKSB7Ci0gICAgICAgICAgICAgICAgIyBQcmVwcm9j
ZXNzIGFsbCB0cmktZ3JhcGhzIAorICAgICAgICAgICAgICAgICMgUHJlcHJvY2VzcyBhbGwgdHJp
LWdyYXBocwogICAgICAgICAgICAgICAgICMgaW5jbHVkaW5nIHRoaW5ncyBzdHVjayBpbiBxdW90
ZWQgc3RyaW5nIGNvbnN0YW50cy4KICAgICAgICAgICAgICAgICAkaW4gPX4gcy9cP1w/PS8jL2c7
ICAgICAgICAgICAgICAgICAgICAgICAgICMgfCA/Pz18ICAjfAogICAgICAgICAgICAgICAgICRp
biA9fiBzL1w/XD9cIS98L2c7ICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/IXwgIHx8CkBA
IC00NzEsMTcgKzU1MiwxOSBAQAogICAgICAgICAgICAgICAgICRpbiA9fiBzL1w/XD8+L30vZzsg
ICAgICAgICAgICAgICAgICAgICAgICAgIyB8ID8/PnwgIH18CiAgICAgICAgICAgICB9CiAJICAg
IGlmICgkaW4gPX4gL15cI2lmZGVmIF9fTEFOR1VBR0VfUEFTQ0FMX18vKSB7Ci0gICAgICAgICAg
ICAgICAgIyBUcnU2NCBkaXNhc3NlbWJsZXIuaCBldmlsbmVzczogbWl4ZWQgQyBhbmQgUGFzY2Fs
LgorCQkjIFRydTY0IGRpc2Fzc2VtYmxlci5oIGV2aWxuZXNzOiBtaXhlZCBDIGFuZCBQYXNjYWwu
CiAJCXdoaWxlICg8SU4+KSB7Ci0JCSAgICBsYXN0IGlmIC9eXCNlbmRpZi87IAorCQkgICAgbGFz
dCBpZiAvXlwjZW5kaWYvOwogCQl9CisJCSRpbiA9ICIiOwogCQluZXh0IFJFQUQ7CiAJICAgIH0K
IAkgICAgaWYgKCRpbiA9fiAvXmV4dGVybiBpbmxpbmUgLyAmJiAjIElubGluZWQgYXNzZW1ibGVy
LgogCQkkXk8gZXEgJ2xpbnV4JyAmJiAkZmlsZSA9fiBtISg/Ol58Lylhc20vW14vXStcLmgkISkg
ewotIAkJd2hpbGUgKDxJTj4pIHsKLQkJICAgIGxhc3QgaWYgL159LzsgCisJCXdoaWxlICg8SU4+
KSB7CisJCSAgICBsYXN0IGlmIC9efS87CiAJCX0KKwkJJGluID0gIiI7CiAJCW5leHQgUkVBRDsK
IAkgICAgfQogICAgICAgICAgICAgaWYgKCRpbiA9fiBzL1xcJC8vKSB7ICAgICAgICAgICAgICAg
ICAgICAgICAgICAgIyBcLW5ld2xpbmUK
UH2PH592
  }
  if ( $num > 5.008009 and $num < 5.009003 ) {
    _patch_b64(<<'UH2PH593');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTM1NCw2ICszNTQsNyBAQAog
CQl1c2UgcmUgImV2YWwiOwogCQlteSAkdHlwZWxpc3QgPSBqb2luICd8Jywga2V5cyAlaXNhdHlw
ZTsKIAkJJG5ldyA9fiBzWycKKwkJICAoPzooPzpfXyk/Y29uc3QoPzpfXyk/XHMrKT8KIAkJICAo
PzooPzp1bik/c2lnbmVkXHMrKT8KIAkJICAoPzpsb25nXHMrKT8KIAkJICAoPzokdHlwZWxpc3Qp
XHMrCkBAIC0zNjIsNiArMzYzLDcgQEAKIAkJICAnXQogCQkgW215IFwkJDFdZ3g7CiAJCSRuZXcg
PX4gc1snCisJCSAgKD86KD86X18pP2NvbnN0KD86X18pP1xzKyk/CiAJCSAgKD86KD86dW4pP3Np
Z25lZFxzKyk/CiAJCSAgKD86bG9uZ1xzKyk/CiAJCSAgKD86JHR5cGVsaXN0KVxzKwpAQCAtNzM0
LDkgKzczNiwxNSBAQAogIyBub24tR0NDPykgQyBjb21waWxlcnMsIGJ1dCBnY2MgdXNlcyBhbiBh
ZGRpdGlvbmFsIGluY2x1ZGUgZGlyZWN0b3J5Lgogc3ViIGluY19kaXJzCiB7Ci0gICAgbXkgJGZy
b21fZ2NjICAgID0gYCRDb25maWd7Y2N9IC12IDI+JjFgOwotICAgICRmcm9tX2djYyAgICAgICA9
fiBzOl5SZWFkaW5nIHNwZWNzIGZyb20gKC4qPykvc3BlY3NcYi4qOiQxL2luY2x1ZGU6czsKLQor
ICAgIG15ICRmcm9tX2djYyAgICA9IGBMQ19BTEw9QyAkQ29uZmlne2NjfSAtdiAyPiYxYDsKKyAg
ICBpZiggISggJGZyb21fZ2NjID1+IHM6XlJlYWRpbmcgc3BlY3MgZnJvbSAoLio/KS9zcGVjc1xi
Lio6JDEvaW5jbHVkZTpzICkgKQorICAgIHsgIyBnY2MtNCsgOgorICAgICAgICRmcm9tX2djYyAg
ID0gYExDX0FMTD1DICRDb25maWd7Y2N9IC1wcmludC1zZWFyY2gtZGlycyAyPiYxYDsKKyAgICAg
ICBpZiAoICEoJGZyb21fZ2NjID1+IHMvXmluc3RhbGw6XHMqKFteXHNdK1teXHNcL10pKFtcc1wv
XSopLiokLyQxXC9pbmNsdWRlL3MpICkKKyAgICAgICB7CisgICAgICAgICAgICRmcm9tX2djYyA9
ICcnOworICAgICAgIH07CisgICAgfTsKICAgICBsZW5ndGgoJGZyb21fZ2NjKSA/ICgkZnJvbV9n
Y2MsICRDb25maWd7dXNyaW5jfSkgOiAoJENvbmZpZ3t1c3JpbmN9KTsKIH0KIAo=
UH2PH593
  }
  if ( $num > 5.008009 and $num < 5.009004 ) {
    _patch_b64(<<'UH2PH594');
LS0tIHV0aWxzL2gycGguUEwKKysrIHV0aWxzL2gycGguUEwKQEAgLTUxNCw3ICs1MTQsNyBAQAog
CQl9CiAJICAgIH0gZWxzZSB7CiAJCWlmICgkaW5pZiAmJiAkbmV3ICF+IC9kZWZpbmVkXHMqXCgk
LykgewotCQkgICAgJG5ldyAuPSAnKGRlZmluZWQoJicgLiAkaWQgLiAnKSA/ICYnIC4gJGlkIC4g
JyA6IDApJzsKKwkJICAgICRuZXcgLj0gJyhkZWZpbmVkKCYnIC4gJGlkIC4gJykgPyAmJyAuICRp
ZCAuICcgOiB1bmRlZiknOwogCQl9IGVsc2lmICgvXlxbLykgewogCQkgICAgJG5ldyAuPSAiIFwk
JGlkIjsKIAkJfSBlbHNlIHsKQEAgLTc3MiwyNSArNzcyLDMzIEBACiAgICAgbXkgKCVkZWZpbmUp
ID0gX2V4dHJhY3RfY2NfZGVmaW5lcygpOwogCiAgICAgb3BlbiAgUFJFQU1CTEUsICI+JHByZWFt
YmxlIiBvciBkaWUgIkNhbm5vdCBvcGVuICRwcmVhbWJsZTogICQhIjsKLSAgICAgICAgcHJpbnQg
UFJFQU1CTEUgIiMgVGhpcyBmaWxlIHdhcyBjcmVhdGVkIGJ5IGgycGggdmVyc2lvbiAkVkVSU0lP
TlxuIjsKKwlwcmludCBQUkVBTUJMRSAiIyBUaGlzIGZpbGUgd2FzIGNyZWF0ZWQgYnkgaDJwaCB2
ZXJzaW9uICRWRVJTSU9OXG4iOwogCi0gICAgICAgIGZvcmVhY2ggKHNvcnQga2V5cyAlZGVmaW5l
KSB7Ci0gICAgICAgICAgICBpZiAoJG9wdF9EKSB7Ci0gICAgICAgICAgICAgICAgcHJpbnQgUFJF
QU1CTEUgIiMgJF89JGRlZmluZXskX31cbiI7Ci0gICAgICAgICAgICB9Ci0KLSAgICAgICAgICAg
IGlmICgkZGVmaW5leyRffSA9fiAvXihcZCspVT9MezAsMn0kL2kpIHsKLSAgICAgICAgICAgICAg
ICBwcmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYk
XykgeyBzdWIgJF8oKSB7ICQxIH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNpZiAoJGRlZmlu
ZXskX30gPX4gL15cdyskLykgewotICAgICAgICAgICAgICAgIHByaW50IFBSRUFNQkxFCi0gICAg
ICAgICAgICAgICAgICAgICJ1bmxlc3MgKGRlZmluZWQgJiRfKSB7IHN1YiAkXygpIHsgJiRkZWZp
bmV7JF99IH0gfVxuXG4iOwotICAgICAgICAgICAgfSBlbHNlIHsKLSAgICAgICAgICAgICAgICBw
cmludCBQUkVBTUJMRQotICAgICAgICAgICAgICAgICAgICAidW5sZXNzIChkZWZpbmVkICYkXykg
eyBzdWIgJF8oKSB7IFwiIiwKLSAgICAgICAgICAgICAgICAgICAgcXVvdGVtZXRhKCRkZWZpbmV7
JF99KSwgIlwiIH0gfVxuXG4iOwotICAgICAgICAgICAgfQotICAgICAgICB9CisJZm9yZWFjaCAo
c29ydCBrZXlzICVkZWZpbmUpIHsKKwkgICAgaWYgKCRvcHRfRCkgeworCQlwcmludCBQUkVBTUJM
RSAiIyAkXz0kZGVmaW5leyRffVxuIjsKKwkgICAgfQorCSAgICBpZiAoJGRlZmluZXskX30gPX4g
L15cKCguKilcKSQvKSB7CisJCSMgcGFyZW50aGVzaXplZCB2YWx1ZTogIGQ9KHYpCisJCSRkZWZp
bmV7JF99ID0gJDE7CisJICAgIH0KKwkgICAgaWYgKCRkZWZpbmV7JF99ID1+IC9eKFsrLV0/KFxk
Kyk/XC5cZCsoW2VFXVsrLV0/XGQrKT8pW0ZMXT8kLykgeworCQkjIGZsb2F0OgorCQlwcmludCBQ
UkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVmaW5lZCAmJF8pIHsgc3ViICRfKCkgeyAkMSB9IH1c
blxuIjsKKwkgICAgfSBlbHNpZiAoJGRlZmluZXskX30gPX4gL14oWystXT9cZCspVT9MezAsMn0k
L2kpIHsKKwkJIyBpbnRlZ2VyOgorCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVm
aW5lZCAmJF8pIHsgc3ViICRfKCkgeyAkMSB9IH1cblxuIjsKKwkgICAgfSBlbHNpZiAoJGRlZmlu
ZXskX30gPX4gL15cdyskLykgeworCQlwcmludCBQUkVBTUJMRQorCQkgICAgInVubGVzcyAoZGVm
aW5lZCAmJF8pIHsgc3ViICRfKCkgeyAmJGRlZmluZXskX30gfSB9XG5cbiI7CisJICAgIH0gZWxz
ZSB7CisJCXByaW50IFBSRUFNQkxFCisJCSAgICAidW5sZXNzIChkZWZpbmVkICYkXykgeyBzdWIg
JF8oKSB7IFwiIiwKKwkJICAgIHF1b3RlbWV0YSgkZGVmaW5leyRffSksICJcIiB9IH1cblxuIjsK
KwkgICAgfQorCX0KICAgICBjbG9zZSBQUkVBTUJMRSAgICAgICAgICAgICAgIG9yIGRpZSAiQ2Fu
bm90IGNsb3NlICRwcmVhbWJsZTogICQhIjsKIH0KIApAQCAtODAyLDE1ICs4MTAsMTQgQEAKIHsK
ICAgICBteSAlZGVmaW5lOwogICAgIG15ICRhbGxzeW1ib2xzICA9IGpvaW4gIiAiLAotICAgICAg
ICBAQ29uZmlneydjY3N5bWJvbHMnLCAnY3Bwc3ltYm9scycsICdjcHBjY3N5bWJvbHMnfTsKKwlA
Q29uZmlneydjY3N5bWJvbHMnLCAnY3Bwc3ltYm9scycsICdjcHBjY3N5bWJvbHMnfTsKIAogICAg
ICMgU3BsaXQgY29tcGlsZXIgcHJlLWRlZmluaXRpb25zIGludG8gYGtleT12YWx1ZScgcGFpcnM6
Ci0gICAgZm9yZWFjaCAoc3BsaXQgL1xzKy8sICRhbGxzeW1ib2xzKSB7Ci0gICAgICAgIC8oLis/
KT0oLispLyBhbmQgJGRlZmluZXskMX0gPSAkMjsKLQotICAgICAgICBpZiAoJG9wdF9EKSB7Ci0g
ICAgICAgICAgICBwcmludCBTVERFUlIgIiRfOiAgJDEgLT4gJDJcbiI7Ci0gICAgICAgIH0KKyAg
ICB3aGlsZSAoJGFsbHN5bWJvbHMgPX4gLyhbXlxzXSspPSgoXFxcc3xbXlxzXSkrKS9nKSB7CisJ
JGRlZmluZXskMX0gPSAkMjsKKwlpZiAoJG9wdF9EKSB7CisJICAgIHByaW50IFNUREVSUiAiJF86
ICAkMSAtPiAkMlxuIjsKKwl9CiAgICAgfQogCiAgICAgcmV0dXJuICVkZWZpbmU7CkBAIC05NDUs
MTAgKzk1MiwxMCBAQAogRG9lc24ndCBoYW5kbGUgY29tcGxpY2F0ZWQgZXhwcmVzc2lvbnMgYnVp
bHQgcGllY2VtZWFsLCBhIGxhOgogCiAgICAgZW51bSB7Ci0gICAgICAgIEZJUlNUX1ZBTFVFLAot
ICAgICAgICBTRUNPTkRfVkFMVUUsCisJRklSU1RfVkFMVUUsCisJU0VDT05EX1ZBTFVFLAogICAg
ICNpZmRlZiBBQkMKLSAgICAgICAgVEhJUkRfVkFMVUUKKwlUSElSRF9WQUxVRQogICAgICNlbmRp
ZgogICAgIH07CiAK
UH2PH594
  }
  # All the rest
  _patch(<<'UH2PH');
--- utils/h2ph.PL
+++ utils/h2ph.PL
@@ -788,6 +788,11 @@ sub build_preamble_if_necessary
 
     open  PREAMBLE, ">$preamble" or die "Cannot open $preamble:  $!";
 	print PREAMBLE "# This file was created by h2ph version $VERSION\n";
+        # Prevent non-portable hex constants from warning.
+        #
+        # We still produce an overflow warning if we can't represent
+        # a hex constant as an integer.
+        print PREAMBLE "no warnings qw(portable);\n";
 
 	foreach (sort keys %define) {
 	    if ($opt_D) {
@@ -814,6 +819,18 @@ DEFINE
 		# integer:
 		print PREAMBLE
 		    "unless (defined &$_) { sub $_() { $1 } }\n\n";
+            } elsif ($define{$_} =~ /^([+-]?0x[\da-f]+)U?L{0,2}$/i) {
+                # hex integer
+                # Special cased, since perl warns on hex integers
+                # that can't be represented in a UV.
+                #
+                # This way we get the warning at time of use, so the user
+                # only gets the warning if they happen to use this
+                # platform-specific definition.
+                my $code = $1;
+                $code = "hex('$code')" if length $code > 10;
+                print PREAMBLE
+                    "unless (defined &$_) { sub $_() { $code } }\n\n";
 	    } elsif ($define{$_} =~ /^\w+$/) {
 		my $def = $define{$_};
 		if ($isatype{$def}) {
UH2PH
}

sub _patch_lib_h2ph {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num < 5.021010;
  return if    $num == 5.020003;
  if ( $num >= 5.013005 ) {
    _patch(<<'LH2PH1');
--- lib/h2ph.t
+++ lib/h2ph.t
@@ -48,7 +48,7 @@ $result = runperl( progfile => '_h2ph_pre.ph',
                    stderr => 1 );
 like( $result, qr/syntax OK$/, "preamble compiles");
 
-$result = runperl( switches => ["-w"],
+$result = runperl( switches => ['-I.', "-w"],
                    stderr => 1,
                    prog => <<'PROG' );
 $SIG{__WARN__} = sub { die $_[0] }; require q(lib/h2ph.pht);
LH2PH1
  }
  elsif ( $num >= 5.013001 ) {
    _patch(<<'LH2PH2');
--- lib/h2ph.t
+++ lib/h2ph.t
@@ -48,7 +48,7 @@ $result = runperl( progfile => '_h2ph_pre.ph',
                    stderr => 1 );
 like( $result, qr/syntax OK$/, "preamble compiles");
 
-$result = runperl( switches => ["-w"], 
+$result = runperl( switches => ['-I.', "-w"], 
                    stderr => 1,
                    prog => <<'PROG' );
 $SIG{__WARN__} = sub { die $_[0] }; require q(lib/h2ph.pht);
LH2PH2
  }
  elsif ( $num >= 5.010001 ) {
    _patch(<<'LH2PH3');
--- lib/h2ph.t
+++ lib/h2ph.t
@@ -41,7 +41,7 @@ $result = runperl( progfile => 'lib/h2ph.pht',
                    stderr => 1 );
 like( $result, qr/syntax OK$/, "output compiles");
 
-$result = runperl( switches => ["-w"], 
+$result = runperl( switches => ['-I.',"-w"], 
                    prog => '$SIG{__WARN__} = sub { die $_[0] }; require q(lib/h2ph.pht);');
 is( $result, '', "output free of warnings" );
 
LH2PH3
  }
}

sub _patch_sdbm_file_c {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num > 5.010000;
  return unless $num < 5.014004;
  _patch_b64(<<'SDBMFILEC');
LS0tIGV4dC9TREJNX0ZpbGUvc2RibS9zZGJtLmMKKysrIGV4dC9TREJNX0ZpbGUvc2RibS9zZGJt
LmMKQEAgLTc4LDggKzc4LDggQEAgc2RibV9vcGVuKHJlZ2lzdGVyIGNoYXIgKmZpbGUsIHJlZ2lz
dGVyIGludCBmbGFncywgcmVnaXN0ZXIgaW50IG1vZGUpCiAJcmVnaXN0ZXIgY2hhciAqZGlybmFt
ZTsKIAlyZWdpc3RlciBjaGFyICpwYWduYW1lOwogCXNpemVfdCBmaWxlbGVuOwotCWNvbnN0IHNp
emVfdCBkaXJmZXh0X2xlbiA9IHNpemVvZihESVJGRVhUICIiKTsKLQljb25zdCBzaXplX3QgcGFn
ZmV4dF9sZW4gPSBzaXplb2YoUEFHRkVYVCAiIik7CisJY29uc3Qgc2l6ZV90IGRpcmZleHRfc2l6
ZSA9IHNpemVvZihESVJGRVhUICIiKTsKKwljb25zdCBzaXplX3QgcGFnZmV4dF9zaXplID0gc2l6
ZW9mKFBBR0ZFWFQgIiIpOwogCiAJaWYgKGZpbGUgPT0gTlVMTCB8fCAhKmZpbGUpCiAJCXJldHVy
biBlcnJubyA9IEVJTlZBTCwgKERCTSAqKSBOVUxMOwpAQCAtODgsMTcgKzg4LDE3IEBAIHNkYm1f
b3BlbihyZWdpc3RlciBjaGFyICpmaWxlLCByZWdpc3RlciBpbnQgZmxhZ3MsIHJlZ2lzdGVyIGlu
dCBtb2RlKQogICovCiAJZmlsZWxlbiA9IHN0cmxlbihmaWxlKTsKIAotCWlmICgoZGlybmFtZSA9
IChjaGFyICopIG1hbGxvYyhmaWxlbGVuICsgZGlyZmV4dF9sZW4gKyAxCi0JCQkJICAgICAgICsg
ZmlsZWxlbiArIHBhZ2ZleHRfbGVuICsgMSkpID09IE5VTEwpCisJaWYgKChkaXJuYW1lID0gKGNo
YXIgKikgbWFsbG9jKGZpbGVsZW4gKyBkaXJmZXh0X3NpemUKKwkJCQkgICAgICAgKyBmaWxlbGVu
ICsgcGFnZmV4dF9zaXplKSkgPT0gTlVMTCkKIAkJcmV0dXJuIGVycm5vID0gRU5PTUVNLCAoREJN
ICopIE5VTEw7CiAvKgogICogYnVpbGQgdGhlIGZpbGUgbmFtZXMKICAqLwogCW1lbWNweShkaXJu
YW1lLCBmaWxlLCBmaWxlbGVuKTsKLQltZW1jcHkoZGlybmFtZSArIGZpbGVsZW4sIERJUkZFWFQs
IGRpcmZleHRfbGVuICsgMSk7Ci0JcGFnbmFtZSA9IGRpcm5hbWUgKyBmaWxlbGVuICsgZGlyZmV4
dF9sZW4gKyAxOworCW1lbWNweShkaXJuYW1lICsgZmlsZWxlbiwgRElSRkVYVCwgZGlyZmV4dF9z
aXplKTsKKwlwYWduYW1lID0gZGlybmFtZSArIGZpbGVsZW4gKyBkaXJmZXh0X3NpemU7CiAJbWVt
Y3B5KHBhZ25hbWUsIGZpbGUsIGZpbGVsZW4pOwotCW1lbWNweShwYWduYW1lICsgZmlsZWxlbiwg
UEFHRkVYVCwgcGFnZmV4dF9sZW4gKyAxKTsKKwltZW1jcHkocGFnbmFtZSArIGZpbGVsZW4sIFBB
R0ZFWFQsIHBhZ2ZleHRfc2l6ZSk7CiAKIAlkYiA9IHNkYm1fcHJlcChkaXJuYW1lLCBwYWduYW1l
LCBmbGFncywgbW9kZSk7CiAJZnJlZSgoY2hhciAqKSBkaXJuYW1lKTsK
SDBMFILEC
}

sub _patch_mmaix_pm {
  my $perlver = shift;
  return unless $^O eq 'aix';
  my $num = _norm_ver( $perlver );
  return unless $num > 5.027000;
  return unless $num < 5.031001;
  _patch_b64(<<'MMAIXPM');
LS0tIGNwYW4vRXh0VXRpbHMtTWFrZU1ha2VyL2xpYi9FeHRVdGlscy9NTV9BSVgucG0KKysrIGNw
YW4vRXh0VXRpbHMtTWFrZU1ha2VyL2xpYi9FeHRVdGlscy9NTV9BSVgucG0KQEAgLTUwLDcgKzUw
LDkgQEAgc3ViIHhzX2Rsc3ltc19leHQgewogCiBzdWIgeHNfZGxzeW1zX2FyZyB7CiAgICAgbXko
JHNlbGYsICRmaWxlKSA9IEBfOwotICAgIHJldHVybiBxcXstYkU6JHtmaWxlfX07CisgICAgbXkg
JGFyZyA9IHFxey1iRToke2ZpbGV9fTsKKyAgICAkYXJnID0gJy1XbCwnLiRhcmcgaWYgJENvbmZp
Z3tsZGRsZmxhZ3N9ID1+IC8tV2wsLWJFOi87CisgICAgcmV0dXJuICRhcmc7CiB9CiAKIHN1YiBp
bml0X290aGVycyB7Cg==
MMAIXPM
}

sub _patch_time_local_t {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  if ( $num < 5.029000 && $num > 5.025003 ) {
    return _patch_b64(<<'TIMELOCALT1');
LS0tIGNwYW4vVGltZS1Mb2NhbC90L0xvY2FsLnQKKysrIGNwYW4vVGltZS1Mb2NhbC90L0xvY2Fs
LnQKQEAgLTg1LDE5ICs4NSwxNyBAQCBteSAkZXBvY2hfaXNfNjQKIAogZm9yICggQHRpbWUsIEBu
ZWdfdGltZSApIHsKICAgICBteSAoICR5ZWFyLCAkbW9uLCAkbWRheSwgJGhvdXIsICRtaW4sICRz
ZWMgKSA9IEAkXzsKLSAgICAkeWVhciAtPSAxOTAwOwogICAgICRtb24tLTsKIAogU0tJUDogewog
ICAgICAgICBza2lwICcxOTcwIHRlc3Qgb24gVk9TIGZhaWxzLicsIDEyCi0gICAgICAgICAgICBp
ZiAkXk8gZXEgJ3ZvcycgJiYgJHllYXIgPT0gNzA7CisgICAgICAgICAgICBpZiAkXk8gZXEgJ3Zv
cycgJiYgJHllYXIgPT0gMTk3MDsKICAgICAgICAgc2tpcCAndGhpcyBwbGF0Zm9ybSBkb2VzIG5v
dCBzdXBwb3J0IG5lZ2F0aXZlIGVwb2Nocy4nLCAxMgotICAgICAgICAgICAgaWYgJHllYXIgPCA3
MCAmJiAhJG5lZ19lcG9jaF9vazsKKyAgICAgICAgICAgIGlmICR5ZWFyIDwgMTk3MCAmJiAhJG5l
Z19lcG9jaF9vazsKIAogICAgICAgICAjIFRlc3QgdGltZWxvY2FsKCkKICAgICAgICAgewotICAg
ICAgICAgICAgbXkgJHllYXJfaW4gPSAkeWVhciA8IDcwID8gJHllYXIgKyAxOTAwIDogJHllYXI7
Ci0gICAgICAgICAgICBteSAkdGltZSA9IHRpbWVsb2NhbCggJHNlYywgJG1pbiwgJGhvdXIsICRt
ZGF5LCAkbW9uLCAkeWVhcl9pbiApOworICAgICAgICAgICAgbXkgJHRpbWUgPSB0aW1lbG9jYWwo
ICRzZWMsICRtaW4sICRob3VyLCAkbWRheSwgJG1vbiwgJHllYXIgKTsKIAogICAgICAgICAgICAg
bXkgKCAkcywgJG0sICRoLCAkRCwgJE0sICRZICkgPSBsb2NhbHRpbWUoJHRpbWUpOwogCkBAIC0x
MDYsMTMgKzEwNCwxMiBAQCBTS0lQOiB7CiAgICAgICAgICAgICBpcyggJGgsICRob3VyLCAgICAg
InRpbWVsb2NhbCBob3VyIGZvciBAJF8iICk7CiAgICAgICAgICAgICBpcyggJEQsICRtZGF5LCAg
ICAgInRpbWVsb2NhbCBkYXkgZm9yIEAkXyIgKTsKICAgICAgICAgICAgIGlzKCAkTSwgJG1vbiwg
ICAgICAidGltZWxvY2FsIG1vbnRoIGZvciBAJF8iICk7Ci0gICAgICAgICAgICBpcyggJFksICR5
ZWFyLCAgICAgInRpbWVsb2NhbCB5ZWFyIGZvciBAJF8iICk7CisgICAgICAgICAgICBpcyggJFks
ICR5ZWFyIC0gMTkwMCwgICAgICJ0aW1lbG9jYWwgeWVhciBmb3IgQCRfIiApOwogICAgICAgICB9
CiAKICAgICAgICAgIyBUZXN0IHRpbWVnbSgpCiAgICAgICAgIHsKLSAgICAgICAgICAgIG15ICR5
ZWFyX2luID0gJHllYXIgPCA3MCA/ICR5ZWFyICsgMTkwMCA6ICR5ZWFyOwotICAgICAgICAgICAg
bXkgJHRpbWUgPSB0aW1lZ20oICRzZWMsICRtaW4sICRob3VyLCAkbWRheSwgJG1vbiwgJHllYXJf
aW4gKTsKKyAgICAgICAgICAgIG15ICR0aW1lID0gdGltZWdtKCAkc2VjLCAkbWluLCAkaG91ciwg
JG1kYXksICRtb24sICR5ZWFyICk7CiAKICAgICAgICAgICAgIG15ICggJHMsICRtLCAkaCwgJEQs
ICRNLCAkWSApID0gZ210aW1lKCR0aW1lKTsKIApAQCAtMTIxLDE0ICsxMTgsMTMgQEAgU0tJUDog
ewogICAgICAgICAgICAgaXMoICRoLCAkaG91ciwgICAgICJ0aW1lZ20gaG91ciBmb3IgQCRfIiAp
OwogICAgICAgICAgICAgaXMoICRELCAkbWRheSwgICAgICJ0aW1lZ20gZGF5IGZvciBAJF8iICk7
CiAgICAgICAgICAgICBpcyggJE0sICRtb24sICAgICAgInRpbWVnbSBtb250aCBmb3IgQCRfIiAp
OwotICAgICAgICAgICAgaXMoICRZLCAkeWVhciwgICAgICJ0aW1lZ20geWVhciBmb3IgQCRfIiAp
OworICAgICAgICAgICAgaXMoICRZLCAkeWVhciAtIDE5MDAsICAgICAidGltZWdtIHllYXIgZm9y
IEAkXyIgKTsKICAgICAgICAgfQogICAgIH0KIH0KIAogZm9yIChAYmFkX3RpbWUpIHsKICAgICBt
eSAoICR5ZWFyLCAkbW9uLCAkbWRheSwgJGhvdXIsICRtaW4sICRzZWMgKSA9IEAkXzsKLSAgICAk
eWVhciAtPSAxOTAwOwogICAgICRtb24tLTsKIAogICAgIGV2YWwgeyB0aW1lZ20oICRzZWMsICRt
aW4sICRob3VyLCAkbWRheSwgJG1vbiwgJHllYXIgKSB9OwpAQCAtMjI5LDYgKzIyNSwzMCBAQCBT
S0lQOgogICAgICk7CiB9CiAKKyMgMi1kaWdpdCB5ZWFycworeworCW15ICRjdXJyZW50X3llYXIg
PSAoIGxvY2FsdGltZSgpIClbNV07CisJbXkgJHByZV9icmVhayAgICA9ICggJGN1cnJlbnRfeWVh
ciArIDQ5ICkgLSAxMDA7CisJbXkgJGJyZWFrICAgICAgICA9ICggJGN1cnJlbnRfeWVhciArIDUw
ICkgLSAxMDA7CisJbXkgJHBvc3RfYnJlYWsgICA9ICggJGN1cnJlbnRfeWVhciArIDUxICkgLSAx
MDA7CisKKwlpcygKKwkJKCAoIGxvY2FsdGltZSggdGltZWxvY2FsKCAwLCAwLCAwLCAxLCAxLCAk
cHJlX2JyZWFrICkgKSApWzVdICksCisJCSRwcmVfYnJlYWsgKyAxMDAsCisJCSJ5ZWFyICRwcmVf
YnJlYWsgaXMgdHJlYXRlZCBhcyBuZXh0IGNlbnR1cnkiLAorCSk7CisJaXMoCisJCSggKCBsb2Nh
bHRpbWUoIHRpbWVsb2NhbCggMCwgMCwgMCwgMSwgMSwgJGJyZWFrICkgKSApWzVdICksCisJCSRi
cmVhayArIDEwMCwKKwkJInllYXIgJGJyZWFrIGlzIHRyZWF0ZWQgYXMgbmV4dCBjZW50dXJ5IiwK
KwkpOworCWlzKAorCQkoICggbG9jYWx0aW1lKCB0aW1lbG9jYWwoIDAsIDAsIDAsIDEsIDEsICRw
b3N0X2JyZWFrICkgKSApWzVdICksCisJCSRwb3N0X2JyZWFrLAorCQkieWVhciAkcG9zdF9icmVh
ayBpcyB0cmVhdGVkIGFzIGN1cnJlbnQgY2VudHVyeSIsCisJKTsKK30KKwogU0tJUDoKIHsKICAg
ICBza2lwICdUaGVzZSB0ZXN0cyBvbmx5IHJ1biBmb3IgdGhlIHBhY2thZ2UgbWFpbnRhaW5lci4n
LCA4Cg==
TIMELOCALT1
  }
  if ( $num < 5.025004 && $num > 5.013008 ) {
    return _patch_b64(<<'TIMELOCALT2');
LS0tIGNwYW4vVGltZS1Mb2NhbC90L0xvY2FsLnQKKysrIGNwYW4vVGltZS1Mb2NhbC90L0xvY2Fs
LnQKQEAgLTkxLDcgKzkxLDcgQEAgZm9yIChAdGltZSwgQG5lZ190aW1lKSB7CiAKICAgICAgICAg
IyBUZXN0IHRpbWVsb2NhbCgpCiAgICAgICAgIHsKLSAgICAgICAgICAgIG15ICR5ZWFyX2luID0g
JHllYXIgPCA3MCA/ICR5ZWFyICsgMTkwMCA6ICR5ZWFyOworICAgICAgICAgICAgbXkgJHllYXJf
aW4gPSAkeWVhciArIDE5MDA7CiAgICAgICAgICAgICBteSAkdGltZSA9IHRpbWVsb2NhbCgkc2Vj
LCRtaW4sJGhvdXIsJG1kYXksJG1vbiwkeWVhcl9pbik7CiAKICAgICAgICAgICAgIG15KCRzLCRt
LCRoLCRELCRNLCRZKSA9IGxvY2FsdGltZSgkdGltZSk7CkBAIC0xMDcsNyArMTA3LDcgQEAgZm9y
IChAdGltZSwgQG5lZ190aW1lKSB7CiAKICAgICAgICAgIyBUZXN0IHRpbWVnbSgpCiAgICAgICAg
IHsKLSAgICAgICAgICAgIG15ICR5ZWFyX2luID0gJHllYXIgPCA3MCA/ICR5ZWFyICsgMTkwMCA6
ICR5ZWFyOworICAgICAgICAgICAgbXkgJHllYXJfaW4gPSAkeWVhciArIDE5MDA7CiAgICAgICAg
ICAgICBteSAkdGltZSA9IHRpbWVnbSgkc2VjLCRtaW4sJGhvdXIsJG1kYXksJG1vbiwkeWVhcl9p
bik7CiAKICAgICAgICAgICAgIG15KCRzLCRtLCRoLCRELCRNLCRZKSA9IGdtdGltZSgkdGltZSk7
CkBAIC0xMjUsNyArMTI1LDYgQEAgZm9yIChAdGltZSwgQG5lZ190aW1lKSB7CiAKIGZvciAoQGJh
ZF90aW1lKSB7CiAgICAgbXkoJHllYXIsICRtb24sICRtZGF5LCAkaG91ciwgJG1pbiwgJHNlYykg
PSBAJF87Ci0gICAgJHllYXIgLT0gMTkwMDsKICAgICAkbW9uLS07CiAKICAgICBldmFsIHsgdGlt
ZWdtKCRzZWMsJG1pbiwkaG91ciwkbWRheSwkbW9uLCR5ZWFyKSB9OwpAQCAtMTM0LDE0ICsxMzMs
MTQgQEAgZm9yIChAYmFkX3RpbWUpIHsKIH0KIAogewotICAgIGlzKHRpbWVsb2NhbCgwLDAsMSwx
LDAsOTApIC0gdGltZWxvY2FsKDAsMCwwLDEsMCw5MCksIDM2MDAsCisgICAgaXModGltZWxvY2Fs
KDAsMCwxLDEsMCwxOTkwKSAtIHRpbWVsb2NhbCgwLDAsMCwxLDAsMTk5MCksIDM2MDAsCiAgICAg
ICAgJ29uZSBob3VyIGRpZmZlcmVuY2UgYmV0d2VlbiB0d28gY2FsbHMgdG8gdGltZWxvY2FsJyk7
CiAKLSAgICBpcyh0aW1lbG9jYWwoMSwyLDMsMSwwLDEwMCkgLSB0aW1lbG9jYWwoMSwyLDMsMzEs
MTEsOTkpLCAyNCAqIDM2MDAsCisgICAgaXModGltZWxvY2FsKDEsMiwzLDEsMCwyMDAwKSAtIHRp
bWVsb2NhbCgxLDIsMywzMSwxMSwxOTk5KSwgMjQgKiAzNjAwLAogICAgICAgICdvbmUgZGF5IGRp
ZmZlcmVuY2UgYmV0d2VlbiB0d28gY2FsbHMgdG8gdGltZWxvY2FsJyk7CiAKICAgICAjIERpZmYg
YmV3ZWVuIEphbiAxLCAxOTgwIGFuZCBNYXIgMSwgMTk4MCA9ICgzMSArIDI5ID0gNjAgZGF5cykK
LSAgICBpcyh0aW1lZ20oMCwwLDAsIDEsIDIsIDgwKSAtIHRpbWVnbSgwLDAsMCwgMSwgMCwgODAp
LCA2MCAqIDI0ICogMzYwMCwKKyAgICBpcyh0aW1lZ20oMCwwLDAsIDEsIDIsIDE5ODApIC0gdGlt
ZWdtKDAsMCwwLCAxLCAwLCAxOTgwKSwgNjAgKiAyNCAqIDM2MDAsCiAgICAgICAgJzYwIGRheSBk
aWZmZXJlbmNlIGJldHdlZW4gdHdvIGNhbGxzIHRvIHRpbWVnbScpOwogfQogCg==
TIMELOCALT2
  }
  if ( $num < 5.013009 && $num > 5.010001 ) {
    return _patch_b64(<<'TIMELOCALT3');
LS0tIGV4dC9UaW1lLUxvY2FsL3QvTG9jYWwudAorKysgZXh0L1RpbWUtTG9jYWwvdC9Mb2NhbC50
CkBAIC04NCw3ICs4NCw3IEBAIGZvciAoQHRpbWUsIEBuZWdfdGltZSkgewogCiAgICAgIyBUZXN0
IHRpbWVsb2NhbCgpCiAgICAgewotICAgICAgICBteSAkeWVhcl9pbiA9ICR5ZWFyIDwgNzAgPyAk
eWVhciArIDE5MDAgOiAkeWVhcjsKKyAgICAgICAgbXkgJHllYXJfaW4gPSAkeWVhciArIDE5MDA7
CiAgICAgICAgIG15ICR0aW1lID0gdGltZWxvY2FsKCRzZWMsJG1pbiwkaG91ciwkbWRheSwkbW9u
LCR5ZWFyX2luKTsKIAogICAgICAgICBteSgkcywkbSwkaCwkRCwkTSwkWSkgPSBsb2NhbHRpbWUo
JHRpbWUpOwpAQCAtMTAwLDcgKzEwMCw3IEBAIGZvciAoQHRpbWUsIEBuZWdfdGltZSkgewogCiAg
ICAgIyBUZXN0IHRpbWVnbSgpCiAgICAgewotICAgICAgICBteSAkeWVhcl9pbiA9ICR5ZWFyIDwg
NzAgPyAkeWVhciArIDE5MDAgOiAkeWVhcjsKKyAgICAgICAgbXkgJHllYXJfaW4gPSAkeWVhciAr
IDE5MDA7CiAgICAgICAgIG15ICR0aW1lID0gdGltZWdtKCRzZWMsJG1pbiwkaG91ciwkbWRheSwk
bW9uLCR5ZWFyX2luKTsKIAogICAgICAgICBteSgkcywkbSwkaCwkRCwkTSwkWSkgPSBnbXRpbWUo
JHRpbWUpOwpAQCAtMTE3LDcgKzExNyw2IEBAIGZvciAoQHRpbWUsIEBuZWdfdGltZSkgewogCiBm
b3IgKEBiYWRfdGltZSkgewogICAgIG15KCR5ZWFyLCAkbW9uLCAkbWRheSwgJGhvdXIsICRtaW4s
ICRzZWMpID0gQCRfOwotICAgICR5ZWFyIC09IDE5MDA7CiAgICAgJG1vbi0tOwogCiAgICAgZXZh
bCB7IHRpbWVnbSgkc2VjLCRtaW4sJGhvdXIsJG1kYXksJG1vbiwkeWVhcikgfTsKQEAgLTEyNiwx
NCArMTI1LDE0IEBAIGZvciAoQGJhZF90aW1lKSB7CiB9CiAKIHsKLSAgICBpcyh0aW1lbG9jYWwo
MCwwLDEsMSwwLDkwKSAtIHRpbWVsb2NhbCgwLDAsMCwxLDAsOTApLCAzNjAwLAorICAgIGlzKHRp
bWVsb2NhbCgwLDAsMSwxLDAsMTk5MCkgLSB0aW1lbG9jYWwoMCwwLDAsMSwwLDE5OTApLCAzNjAw
LAogICAgICAgICdvbmUgaG91ciBkaWZmZXJlbmNlIGJldHdlZW4gdHdvIGNhbGxzIHRvIHRpbWVs
b2NhbCcpOwogCi0gICAgaXModGltZWxvY2FsKDEsMiwzLDEsMCwxMDApIC0gdGltZWxvY2FsKDEs
MiwzLDMxLDExLDk5KSwgMjQgKiAzNjAwLAorICAgIGlzKHRpbWVsb2NhbCgxLDIsMywxLDAsMjAw
MCkgLSB0aW1lbG9jYWwoMSwyLDMsMzEsMTEsMTk5OSksIDI0ICogMzYwMCwKICAgICAgICAnb25l
IGRheSBkaWZmZXJlbmNlIGJldHdlZW4gdHdvIGNhbGxzIHRvIHRpbWVsb2NhbCcpOwogCiAgICAg
IyBEaWZmIGJld2VlbiBKYW4gMSwgMTk4MCBhbmQgTWFyIDEsIDE5ODAgPSAoMzEgKyAyOSA9IDYw
IGRheXMpCi0gICAgaXModGltZWdtKDAsMCwwLCAxLCAyLCA4MCkgLSB0aW1lZ20oMCwwLDAsIDEs
IDAsIDgwKSwgNjAgKiAyNCAqIDM2MDAsCisgICAgaXModGltZWdtKDAsMCwwLCAxLCAyLCAxOTgw
KSAtIHRpbWVnbSgwLDAsMCwgMSwgMCwgMTk4MCksIDYwICogMjQgKiAzNjAwLAogICAgICAgICc2
MCBkYXkgZGlmZmVyZW5jZSBiZXR3ZWVuIHR3byBjYWxscyB0byB0aW1lZ20nKTsKIH0KIAo=
TIMELOCALT3
  }
  if ( ( $num <= 5.010001 && $num > 5.009003 ) || $num == 5.008009 ) {
    return _patch_b64(<<'TIMELOCALT4');
LS0tIGxpYi9UaW1lL0xvY2FsLnQKKysrIGxpYi9UaW1lL0xvY2FsLnQKQEAgLTk2LDcgKzk2LDcg
QEAgZm9yIChAdGltZSwgQG5lZ190aW1lKSB7CiAgICAgICAgICAgICBpZiAkeWVhciA8IDcwICYm
ICEgJG5lZ19lcG9jaF9vazsKIAogICAgICAgICB7Ci0gICAgICAgICAgICBteSAkeWVhcl9pbiA9
ICR5ZWFyIDwgNzAgPyAkeWVhciArIDE5MDAgOiAkeWVhcjsKKyAgICAgICAgICAgIG15ICR5ZWFy
X2luID0gJHllYXIgKyAxOTAwOwogICAgICAgICAgICAgbXkgJHRpbWUgPSB0aW1lbG9jYWwoJHNl
YywkbWluLCRob3VyLCRtZGF5LCRtb24sJHllYXJfaW4pOwogCiAgICAgICAgICAgICBteSgkcywk
bSwkaCwkRCwkTSwkWSkgPSBsb2NhbHRpbWUoJHRpbWUpOwpAQCAtMTEwLDcgKzExMCw3IEBAIGZv
ciAoQHRpbWUsIEBuZWdfdGltZSkgewogICAgICAgICB9CiAKICAgICAgICAgewotICAgICAgICAg
ICAgbXkgJHllYXJfaW4gPSAkeWVhciA8IDcwID8gJHllYXIgKyAxOTAwIDogJHllYXI7CisgICAg
ICAgICAgICBteSAkeWVhcl9pbiA9ICR5ZWFyICsgMTkwMDsKICAgICAgICAgICAgIG15ICR0aW1l
ID0gdGltZWdtKCRzZWMsJG1pbiwkaG91ciwkbWRheSwkbW9uLCR5ZWFyX2luKTsKIAogICAgICAg
ICAgICAgbXkoJHMsJG0sJGgsJEQsJE0sJFkpID0gZ210aW1lKCR0aW1lKTsKQEAgLTEyNyw3ICsx
MjcsNiBAQCBmb3IgKEB0aW1lLCBAbmVnX3RpbWUpIHsKIAogZm9yIChAYmFkX3RpbWUpIHsKICAg
ICBteSgkeWVhciwgJG1vbiwgJG1kYXksICRob3VyLCAkbWluLCAkc2VjKSA9IEAkXzsKLSAgICAk
eWVhciAtPSAxOTAwOwogICAgICRtb24tLTsKIAogICAgIGV2YWwgeyB0aW1lZ20oJHNlYywkbWlu
LCRob3VyLCRtZGF5LCRtb24sJHllYXIpIH07CkBAIC0xMzYsMTQgKzEzNSwxNCBAQCBmb3IgKEBi
YWRfdGltZSkgewogfQogCiB7Ci0gICAgaXModGltZWxvY2FsKDAsMCwxLDEsMCw5MCkgLSB0aW1l
bG9jYWwoMCwwLDAsMSwwLDkwKSwgMzYwMCwKKyAgICBpcyh0aW1lbG9jYWwoMCwwLDEsMSwwLDE5
OTApIC0gdGltZWxvY2FsKDAsMCwwLDEsMCwxOTkwKSwgMzYwMCwKICAgICAgICAnb25lIGhvdXIg
ZGlmZmVyZW5jZSBiZXR3ZWVuIHR3byBjYWxscyB0byB0aW1lbG9jYWwnKTsKIAotICAgIGlzKHRp
bWVsb2NhbCgxLDIsMywxLDAsMTAwKSAtIHRpbWVsb2NhbCgxLDIsMywzMSwxMSw5OSksIDI0ICog
MzYwMCwKKyAgICBpcyh0aW1lbG9jYWwoMSwyLDMsMSwwLDIwMDApIC0gdGltZWxvY2FsKDEsMiwz
LDMxLDExLDE5OTkpLCAyNCAqIDM2MDAsCiAgICAgICAgJ29uZSBkYXkgZGlmZmVyZW5jZSBiZXR3
ZWVuIHR3byBjYWxscyB0byB0aW1lbG9jYWwnKTsKIAogICAgICMgRGlmZiBiZXdlZW4gSmFuIDEs
IDE5ODAgYW5kIE1hciAxLCAxOTgwID0gKDMxICsgMjkgPSA2MCBkYXlzKQotICAgIGlzKHRpbWVn
bSgwLDAsMCwgMSwgMiwgODApIC0gdGltZWdtKDAsMCwwLCAxLCAwLCA4MCksIDYwICogMjQgKiAz
NjAwLAorICAgIGlzKHRpbWVnbSgwLDAsMCwgMSwgMiwgMTk4MCkgLSB0aW1lZ20oMCwwLDAsIDEs
IDAsIDE5ODApLCA2MCAqIDI0ICogMzYwMCwKICAgICAgICAnNjAgZGF5IGRpZmZlcmVuY2UgYmV0
d2VlbiB0d28gY2FsbHMgdG8gdGltZWdtJyk7CiB9CiAK
TIMELOCALT4
  }
  if ( ( $num == 5.009002 || $num == 5.009003 )
       ||
       ( $num == 5.008008 || $num == 5.008007 ) ) {
    return _patch_b64(<<'TIMELOCALT5');
LS0tIGxpYi9UaW1lL0xvY2FsLnQKKysrIGxpYi9UaW1lL0xvY2FsLnQKQEAgLTgzLDcgKzgzLDcg
QEAgZm9yIChAdGltZSwgQG5lZ190aW1lKSB7CiAgICAgfSBlbHNpZiAoJHllYXIgPCA3MCAmJiAh
ICRuZWdfZXBvY2hfb2spIHsKICAgICAgICAgc2tpcCgxLCAic2tpcHBpbmcgbmVnYXRpdmUgZXBv
Y2guXG4iKSBmb3IgMS4uNjsKICAgICB9IGVsc2UgewotICAgICAgICBteSAkeWVhcl9pbiA9ICR5
ZWFyIDwgNzAgPyAkeWVhciArIDE5MDAgOiAkeWVhcjsKKyAgICAgICAgbXkgJHllYXJfaW4gPSAk
eWVhciArIDE5MDA7CiAgICAgICAgIG15ICR0aW1lID0gdGltZWxvY2FsKCRzZWMsJG1pbiwkaG91
ciwkbWRheSwkbW9uLCR5ZWFyX2luKTsKIAogICAgICAgICBteSgkcywkbSwkaCwkRCwkTSwkWSkg
PSBsb2NhbHRpbWUoJHRpbWUpOwpAQCAtMTAxLDcgKzEwMSw3IEBAIGZvciAoQHRpbWUsIEBuZWdf
dGltZSkgewogICAgIH0gZWxzaWYgKCR5ZWFyIDwgNzAgJiYgISAkbmVnX2Vwb2NoX29rKSB7CiAg
ICAgICAgIHNraXAoMSwgInNraXBwaW5nIG5lZ2F0aXZlIGVwb2NoLlxuIikgZm9yIDEuLjY7CiAg
ICAgfSBlbHNlIHsKLSAgICAgICAgbXkgJHllYXJfaW4gPSAkeWVhciA8IDcwID8gJHllYXIgKyAx
OTAwIDogJHllYXI7CisgICAgICAgIG15ICR5ZWFyX2luID0gJHllYXIgKyAxOTAwOwogICAgICAg
ICBteSAkdGltZSA9IHRpbWVnbSgkc2VjLCRtaW4sJGhvdXIsJG1kYXksJG1vbiwkeWVhcl9pbik7
CiAKICAgICAgICAgbXkoJHMsJG0sJGgsJEQsJE0sJFkpID0gZ210aW1lKCR0aW1lKTsKQEAgLTEx
Nyw3ICsxMTcsNiBAQCBmb3IgKEB0aW1lLCBAbmVnX3RpbWUpIHsKIAogZm9yIChAYmFkX3RpbWUp
IHsKICAgICBteSgkeWVhciwgJG1vbiwgJG1kYXksICRob3VyLCAkbWluLCAkc2VjKSA9IEAkXzsK
LSAgICAkeWVhciAtPSAxOTAwOwogICAgICRtb24tLTsKIAogICAgIGV2YWwgeyB0aW1lZ20oJHNl
YywkbWluLCRob3VyLCRtZGF5LCRtb24sJHllYXIpIH07CkBAIC0xMjUsMTQgKzEyNCwxNCBAQCBm
b3IgKEBiYWRfdGltZSkgewogICAgIG9rKCRALCBxci8uKm91dCBvZiByYW5nZS4qLywgJ2ludmFs
aWQgdGltZSBjYXVzZWQgYW4gZXJyb3InKTsKIH0KIAotb2sodGltZWxvY2FsKDAsMCwxLDEsMCw5
MCkgLSB0aW1lbG9jYWwoMCwwLDAsMSwwLDkwKSwgMzYwMCwKK29rKHRpbWVsb2NhbCgwLDAsMSwx
LDAsMTk5MCkgLSB0aW1lbG9jYWwoMCwwLDAsMSwwLDE5OTApLCAzNjAwLAogICAgJ29uZSBob3Vy
IGRpZmZlcmVuY2UgYmV0d2VlbiB0d28gY2FsbHMgdG8gdGltZWxvY2FsJyk7CiAKLW9rKHRpbWVs
b2NhbCgxLDIsMywxLDAsMTAwKSAtIHRpbWVsb2NhbCgxLDIsMywzMSwxMSw5OSksIDI0ICogMzYw
MCwKK29rKHRpbWVsb2NhbCgxLDIsMywxLDAsMjAwMCkgLSB0aW1lbG9jYWwoMSwyLDMsMzEsMTEs
MTk5OSksIDI0ICogMzYwMCwKICAgICdvbmUgZGF5IGRpZmZlcmVuY2UgYmV0d2VlbiB0d28gY2Fs
bHMgdG8gdGltZWxvY2FsJyk7CiAKICMgRGlmZiBiZXdlZW4gSmFuIDEsIDE5ODAgYW5kIE1hciAx
LCAxOTgwID0gKDMxICsgMjkgPSA2MCBkYXlzKQotb2sodGltZWdtKDAsMCwwLCAxLCAyLCA4MCkg
LSB0aW1lZ20oMCwwLDAsIDEsIDAsIDgwKSwgNjAgKiAyNCAqIDM2MDAsCitvayh0aW1lZ20oMCww
LDAsIDEsIDIsIDE5ODApIC0gdGltZWdtKDAsMCwwLCAxLCAwLCAxOTgwKSwgNjAgKiAyNCAqIDM2
MDAsCiAgICAnNjAgZGF5IGRpZmZlcmVuY2UgYmV0d2VlbiB0d28gY2FsbHMgdG8gdGltZWdtJyk7
CiAKICMgYnVnaWQgIzE5MzkzCg==
TIMELOCALT5
  }
}

sub _patch_pp_c_libc {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num > 5.008000;
  return unless $num < 5.028000;
  _patch_b64(<<'PPCLIBC');
LS0tIHBwLmMKKysrIHBwLmMKQEAgLTM2NTMsOCArMzY1MywxMiBAQCBQUChwcF9jcnlwdCkKICNp
ZiBkZWZpbmVkKF9fR0xJQkNfXykgfHwgZGVmaW5lZChfX0VNWF9fKQogCWlmIChQTF9yZWVudHJh
bnRfYnVmZmVyLT5fY3J5cHRfc3RydWN0X2J1ZmZlcikgewogCSAgICBQTF9yZWVudHJhbnRfYnVm
ZmVyLT5fY3J5cHRfc3RydWN0X2J1ZmZlci0+aW5pdGlhbGl6ZWQgPSAwOwotCSAgICAvKiB3b3Jr
IGFyb3VuZCBnbGliYy0yLjIuNSBidWcgKi8KKyNpZiAoZGVmaW5lZChfX0dMSUJDX18pICYmIF9f
R0xJQkNfXyA9PSAyKSAmJiBcCisgICAgKGRlZmluZWQoX19HTElCQ19NSU5PUl9fKSAmJiBfX0dM
SUJDX01JTk9SX18gPj0gMiAmJiBfX0dMSUJDX01JTk9SX18gPCA0KQorCSAgICAvKiB3b3JrIGFy
b3VuZCBnbGliYy0yLjIuNSBidWcsIGhhcyBiZWVuIGZpeGVkIGF0IHNvbWUKKwkgICAgICogdGlt
ZSBpbiBnbGliYy0yLjMuWCAqLwogCSAgICBQTF9yZWVudHJhbnRfYnVmZmVyLT5fY3J5cHRfc3Ry
dWN0X2J1ZmZlci0+Y3VycmVudF9zYWx0Yml0cyA9IDA7CisjZW5kaWYKIAl9CiAjZW5kaWYKICAg
ICB9Cg==
PPCLIBC
}

sub _patch_conf_gcc10 {
  my $perlver = shift;
  my $num = _norm_ver( $perlver );
  return unless $num < 5.031006;
  return if     $num >= 5.030002;
  if ( $num <= 5.006001 or ( $num >= 5.00700 and $num < 5.00800 ) ) {
    return _patch_b64(<<'CONFGCC10561');
LS0tIENvbmZpZ3VyZQorKysgQ29uZmlndXJlCkBAIC0zMTQ5LDcgKzMxNDksNyBAQCBlbHNlCiBm
aQogJHJtIC1mIGdjY3ZlcnMqCiBjYXNlICIkZ2NjdmVyc2lvbiIgaW4KLTEqKSBjcHA9YC4vbG9j
IGdjYy1jcHAgJGNwcCAkcHRoYCA7OworMS4qKSBjcHA9YC4vbG9jIGdjYy1jcHAgJGNwcCAkcHRo
YCA7OwogZXNhYwogY2FzZSAiJGdjY3ZlcnNpb24iIGluCiAnJykgZ2Njb3NhbmR2ZXJzPScnIDs7
CkBAIC0zOTIzLDEzICszOTIzLDEzIEBAIGRmbHQ9JycKIGNhc2UgIiRoaW50IiBpbgogZGVmYXVs
dHxyZWNvbW1lbmRlZCkKIAljYXNlICIkZ2NjdmVyc2lvbiIgaW4KLQkxKikgZGZsdD0nLWZwY2Mt
c3RydWN0LXJldHVybicgOzsKKwkxLiopIGRmbHQ9Jy1mcGNjLXN0cnVjdC1yZXR1cm4nIDs7CiAJ
ZXNhYwogCWNhc2UgIiRvcHRpbWl6ZSIgaW4KIAkqLWcqKSBkZmx0PSIkZGZsdCAtRERFQlVHR0lO
RyI7OwogCWVzYWMKIAljYXNlICIkZ2NjdmVyc2lvbiIgaW4KLQkyKikgaWYgdGVzdCAtZCAvZXRj
L2NvbmYva2NvbmZpZy5kICYmCisJMi4qKSBpZiB0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQg
JiYKIAkJCSRjb250YWlucyBfUE9TSVhfVkVSU0lPTiAkdXNyaW5jL3N5cy91bmlzdGQuaCA+L2Rl
di9udWxsIDI+JjEKIAkJdGhlbgogCQkJZGZsdD0iJGRmbHQgLXBvc2l4IgpAQCAtMzkzNyw3ICsz
OTM3LDcgQEAgZGVmYXVsdHxyZWNvbW1lbmRlZCkKIAkJOzsKIAllc2FjCiAJY2FzZSAiJGdjY3Zl
cnNpb24iIGluCi0JMSopIDs7CisJMS4qKSA7OwogCTIuWzAtOF0qKSA7OwogCT8qKSAJZWNobyAi
ICIKIAkJZWNobyAiQ2hlY2tpbmcgaWYgeW91ciBjb21waWxlciBhY2NlcHRzIC1mbm8tc3RyaWN0
LWFsaWFzaW5nIiAyPiYxCkBAIC00MDI4LDcgKzQwMjgsNyBAQCBlc2FjCiA6IHRoZSBmb2xsb3dp
bmcgd2VlZHMgb3B0aW9ucyBmcm9tIGNjZmxhZ3MgdGhhdCBhcmUgb2Ygbm8gaW50ZXJlc3QgdG8g
Y3BwCiBjcHBmbGFncz0iJGNjZmxhZ3MiCiBjYXNlICIkZ2NjdmVyc2lvbiIgaW4KLTEqKSBjcHBm
bGFncz0iJGNwcGZsYWdzIC1EX19HTlVDX18iCisxLiopIGNwcGZsYWdzPSIkY3BwZmxhZ3MgLURf
X0dOVUNfXyIKIGVzYWMKIGNhc2UgIiRtaXBzX3R5cGUiIGluCiAnJyk7Owo=
CONFGCC10561
  }
  if ( $num <= 5.008008 or ( $num > 5.008009 and $num < 5.009004 ) ) {
    return _patch_b64(<<'CONFGCC10588');
LS0tIENvbmZpZ3VyZQorKysgQ29uZmlndXJlCkBAIC0zOTA5LDcgKzM5MDksNyBAQCBlbHNlCiBm
aQogJHJtIC1mIHRyeSB0cnkuKgogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwPWAuL2xv
YyBnY2MtY3BwICRjcHAgJHB0aGAgOzsKKzEuKikgY3BwPWAuL2xvYyBnY2MtY3BwICRjcHAgJHB0
aGAgOzsKIGVzYWMKIGNhc2UgIiRnY2N2ZXJzaW9uIiBpbgogJycpIGdjY29zYW5kdmVycz0nJyA7
OwpAQCAtMzk0OSw3ICszOTQ5LDcgQEAgZXNhYwogIyBnY2MgMy4qIGNvbXBsYWluIGFib3V0IGFk
ZGluZyAtSWRpcmVjdG9yaWVzIHRoYXQgdGhleSBhbHJlYWR5IGtub3cgYWJvdXQsCiAjIHNvIHdl
IHdpbGwgdGFrZSB0aG9zZSBvZmYgZnJvbSBsb2NpbmNwdGguCiBjYXNlICIkZ2NjdmVyc2lvbiIg
aW4KLTMqKQorMy4qKQogICAgIGVjaG8gIm1haW4oKXt9Ij50cnkuYwogICAgIGZvciBpbmNkaXIg
aW4gJGxvY2luY3B0aDsgZG8KICAgICAgICB3YXJuPWAkY2MgJGNjZmxhZ3MgLUkkaW5jZGlyIC1j
IHRyeS5jIDI+JjEgfCBcCkBAIC00NzI0LDEzICs0NzI0LDEzIEBAIGRmbHQ9JycKIGNhc2UgIiRo
aW50IiBpbgogZGVmYXVsdHxyZWNvbW1lbmRlZCkKIAljYXNlICIkZ2NjdmVyc2lvbiIgaW4KLQkx
KikgZGZsdD0nLWZwY2Mtc3RydWN0LXJldHVybicgOzsKKwkxLiopIGRmbHQ9Jy1mcGNjLXN0cnVj
dC1yZXR1cm4nIDs7CiAJZXNhYwogCWNhc2UgIiRvcHRpbWl6ZSIgaW4KIAkqLWcqKSBkZmx0PSIk
ZGZsdCAtRERFQlVHR0lORyI7OwogCWVzYWMKIAljYXNlICIkZ2NjdmVyc2lvbiIgaW4KLQkyKikg
aWYgdGVzdCAtZCAvZXRjL2NvbmYva2NvbmZpZy5kICYmCisJMi4qKSBpZiB0ZXN0IC1kIC9ldGMv
Y29uZi9rY29uZmlnLmQgJiYKIAkJCSRjb250YWlucyBfUE9TSVhfVkVSU0lPTiAkdXNyaW5jL3N5
cy91bmlzdGQuaCA+L2Rldi9udWxsIDI+JjEKIAkJdGhlbgogCQkJIyBJbnRlcmFjdGl2ZSBTeXN0
ZW1zIChJU0MpIFBPU0lYIG1vZGUuCkBAIC00NzM5LDcgKzQ3MzksNyBAQCBkZWZhdWx0fHJlY29t
bWVuZGVkKQogCQk7OwogCWVzYWMKIAljYXNlICIkZ2NjdmVyc2lvbiIgaW4KLQkxKikgOzsKKwkx
LiopIDs7CiAJMi5bMC04XSopIDs7CiAJPyopIAllY2hvICIgIgogCQllY2hvICJDaGVja2luZyBp
ZiB5b3VyIGNvbXBpbGVyIGFjY2VwdHMgLWZuby1zdHJpY3QtYWxpYXNpbmciIDI+JjEKQEAgLTQ4
NjcsNyArNDg2Nyw3IEBAIGNhc2UgIiRjcHBmbGFncyIgaW4KICopICBjcHBmbGFncz0iJGNwcGZs
YWdzICRjY2ZsYWdzIiA7OwogZXNhYwogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwZmxh
Z3M9IiRjcHBmbGFncyAtRF9fR05VQ19fIgorMS4qKSBjcHBmbGFncz0iJGNwcGZsYWdzIC1EX19H
TlVDX18iCiBlc2FjCiBjYXNlICIkbWlwc190eXBlIiBpbgogJycpOzsK
CONFGCC10588
  }
  if ( $num <= 5.010000 ) {
    return _patch_b64(<<'CONFGCC10510');
LS0tIENvbmZpZ3VyZQorKysgQ29uZmlndXJlCkBAIC00NDg1LDcgKzQ0ODUsNyBAQCBlbHNlCiBm
aQogJHJtIC1mIHRyeSB0cnkuKgogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwPWAuL2xv
YyBnY2MtY3BwICRjcHAgJHB0aGAgOzsKKzEuKikgY3BwPWAuL2xvYyBnY2MtY3BwICRjcHAgJHB0
aGAgOzsKIGVzYWMKIGNhc2UgIiRnY2N2ZXJzaW9uIiBpbgogJycpIGdjY29zYW5kdmVycz0nJyA7
OwpAQCAtNDUyNSw3ICs0NTI1LDcgQEAgZXNhYwogIyBnY2MgMy4qIGNvbXBsYWluIGFib3V0IGFk
ZGluZyAtSWRpcmVjdG9yaWVzIHRoYXQgdGhleSBhbHJlYWR5IGtub3cgYWJvdXQsCiAjIHNvIHdl
IHdpbGwgdGFrZSB0aG9zZSBvZmYgZnJvbSBsb2NpbmNwdGguCiBjYXNlICIkZ2NjdmVyc2lvbiIg
aW4KLTMqKQorMy4qKQogICAgIGVjaG8gIm1haW4oKXt9Ij50cnkuYwogICAgIGZvciBpbmNkaXIg
aW4gJGxvY2luY3B0aDsgZG8KICAgICAgICB3YXJuPWAkY2MgJGNjZmxhZ3MgLUkkaW5jZGlyIC1j
IHRyeS5jIDI+JjEgfCBcCkBAIC01MDUwLDEzICs1MDUwLDEzIEBAIGVzYWMKIGNhc2UgIiRoaW50
IiBpbgogZGVmYXVsdHxyZWNvbW1lbmRlZCkKIAljYXNlICIkZ2NjdmVyc2lvbiIgaW4KLQkxKikg
ZGZsdD0iJGRmbHQgLWZwY2Mtc3RydWN0LXJldHVybiIgOzsKKwkxLiopIGRmbHQ9IiRkZmx0IC1m
cGNjLXN0cnVjdC1yZXR1cm4iIDs7CiAJZXNhYwogCWNhc2UgIiRvcHRpbWl6ZTokREVCVUdHSU5H
IiBpbgogCSotZyo6b2xkKSBkZmx0PSIkZGZsdCAtRERFQlVHR0lORyI7OwogCWVzYWMKIAljYXNl
ICIkZ2NjdmVyc2lvbiIgaW4KLQkyKikgaWYgdGVzdCAtZCAvZXRjL2NvbmYva2NvbmZpZy5kICYm
CisJMi4qKSBpZiB0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYKIAkJCSRjb250YWlucyBf
UE9TSVhfVkVSU0lPTiAkdXNyaW5jL3N5cy91bmlzdGQuaCA+L2Rldi9udWxsIDI+JjEKIAkJdGhl
bgogCQkJIyBJbnRlcmFjdGl2ZSBTeXN0ZW1zIChJU0MpIFBPU0lYIG1vZGUuCkBAIC01MDY1LDcg
KzUwNjUsNyBAQCBkZWZhdWx0fHJlY29tbWVuZGVkKQogCQk7OwogCWVzYWMKIAljYXNlICIkZ2Nj
dmVyc2lvbiIgaW4KLQkxKikgOzsKKwkxLiopIDs7CiAJMi5bMC04XSopIDs7CiAJPyopIAllY2hv
ICIgIgogCQllY2hvICJDaGVja2luZyBpZiB5b3VyIGNvbXBpbGVyIGFjY2VwdHMgLWZuby1zdHJp
Y3QtYWxpYXNpbmciIDI+JjEKQEAgLTUxNzksNyArNTE3OSw3IEBAIGNhc2UgIiRjcHBmbGFncyIg
aW4KICopICBjcHBmbGFncz0iJGNwcGZsYWdzICRjY2ZsYWdzIiA7OwogZXNhYwogY2FzZSAiJGdj
Y3ZlcnNpb24iIGluCi0xKikgY3BwZmxhZ3M9IiRjcHBmbGFncyAtRF9fR05VQ19fIgorMS4qKSBj
cHBmbGFncz0iJGNwcGZsYWdzIC1EX19HTlVDX18iCiBlc2FjCiBjYXNlICIkbWlwc190eXBlIiBp
bgogJycpOzsK
CONFGCC10510
  }
  if ( $num < 5.021002 ) {
    return _patch_b64(<<'CONFGCC10520');
LS0tIENvbmZpZ3VyZQorKysgQ29uZmlndXJlCkBAIC00NTkxLDcgKzQ1OTEsNyBAQCBlbHNlCiBm
aQogJHJtIC1mIHRyeSB0cnkuKgogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwPWAuL2xv
YyBnY2MtY3BwICRjcHAgJHB0aGAgOzsKKzEuKikgY3BwPWAuL2xvYyBnY2MtY3BwICRjcHAgJHB0
aGAgOzsKIGVzYWMKIGNhc2UgIiRnY2N2ZXJzaW9uIiBpbgogJycpIGdjY29zYW5kdmVycz0nJyA7
OwpAQCAtNDYzMSw3ICs0NjMxLDcgQEAgZXNhYwogIyBnY2MgMy4qIGNvbXBsYWluIGFib3V0IGFk
ZGluZyAtSWRpcmVjdG9yaWVzIHRoYXQgdGhleSBhbHJlYWR5IGtub3cgYWJvdXQsCiAjIHNvIHdl
IHdpbGwgdGFrZSB0aG9zZSBvZmYgZnJvbSBsb2NpbmNwdGguCiBjYXNlICIkZ2NjdmVyc2lvbiIg
aW4KLTMqKQorMy4qKQogICAgIGVjaG8gIm1haW4oKXt9Ij50cnkuYwogICAgIGZvciBpbmNkaXIg
aW4gJGxvY2luY3B0aDsgZG8KICAgICAgICB3YXJuPWAkY2MgJGNjZmxhZ3MgLUkkaW5jZGlyIC1j
IHRyeS5jIDI+JjEgfCBcCkBAIC01MzI4LDEzICs1MzI4LDEzIEBAIGZpCiBjYXNlICIkaGludCIg
aW4KIGRlZmF1bHR8cmVjb21tZW5kZWQpCiAJY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0JMSopIGRm
bHQ9IiRkZmx0IC1mcGNjLXN0cnVjdC1yZXR1cm4iIDs7CisJMS4qKSBkZmx0PSIkZGZsdCAtZnBj
Yy1zdHJ1Y3QtcmV0dXJuIiA7OwogCWVzYWMKIAljYXNlICIkb3B0aW1pemU6JERFQlVHR0lORyIg
aW4KIAkqLWcqOm9sZCkgZGZsdD0iJGRmbHQgLURERUJVR0dJTkciOzsKIAllc2FjCiAJY2FzZSAi
JGdjY3ZlcnNpb24iIGluCi0JMiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYK
KwkyLiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYKIAkJCSRjb250YWlucyBf
UE9TSVhfVkVSU0lPTiAkdXNyaW5jL3N5cy91bmlzdGQuaCA+L2Rldi9udWxsIDI+JjEKIAkJdGhl
bgogCQkJIyBJbnRlcmFjdGl2ZSBTeXN0ZW1zIChJU0MpIFBPU0lYIG1vZGUuCkBAIC01MzQzLDcg
KzUzNDMsNyBAQCBkZWZhdWx0fHJlY29tbWVuZGVkKQogCQk7OwogCWVzYWMKIAljYXNlICIkZ2Nj
dmVyc2lvbiIgaW4KLQkxKikgOzsKKwkxLiopIDs7CiAJMi5bMC04XSopIDs7CiAJPyopCXNldCBz
dHJpY3QtYWxpYXNpbmcgLWZuby1zdHJpY3QtYWxpYXNpbmcKIAkJZXZhbCAkY2hlY2tjY2ZsYWcK
QEAgLTU0NDUsNyArNTQ0NSw3IEBAIGNhc2UgIiRjcHBmbGFncyIgaW4KICopICBjcHBmbGFncz0i
JGNwcGZsYWdzICRjY2ZsYWdzIiA7OwogZXNhYwogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikg
Y3BwZmxhZ3M9IiRjcHBmbGFncyAtRF9fR05VQ19fIgorMS4qKSBjcHBmbGFncz0iJGNwcGZsYWdz
IC1EX19HTlVDX18iCiBlc2FjCiBjYXNlICIkbWlwc190eXBlIiBpbgogJycpOzsK
CONFGCC10520
  }
  if ( $num < 5.023005 and ! ( $num >= 5.022002 and $num < 5.023000 ) ) {
    return _patch_b64(<<'CONFGCC10522');
LS0tIENvbmZpZ3VyZQorKysgQ29uZmlndXJlCkBAIC00NjU3LDcgKzQ2NTcsNyBAQCBlbHNlCiBm
aQogJHJtIC1mIHRyeSB0cnkuKgogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwPWAuL2xv
YyBnY2MtY3BwICRjcHAgJHB0aGAgOzsKKzEuKikgY3BwPWAuL2xvYyBnY2MtY3BwICRjcHAgJHB0
aGAgOzsKIGVzYWMKIGNhc2UgIiRnY2N2ZXJzaW9uIiBpbgogJycpIGdjY29zYW5kdmVycz0nJyA7
OwpAQCAtNDY5Nyw3ICs0Njk3LDcgQEAgZXNhYwogIyBnY2MgMy4qIGNvbXBsYWluIGFib3V0IGFk
ZGluZyAtSWRpcmVjdG9yaWVzIHRoYXQgdGhleSBhbHJlYWR5IGtub3cgYWJvdXQsCiAjIHNvIHdl
IHdpbGwgdGFrZSB0aG9zZSBvZmYgZnJvbSBsb2NpbmNwdGguCiBjYXNlICIkZ2NjdmVyc2lvbiIg
aW4KLTMqKQorMy4qKQogICAgIGVjaG8gIm1haW4oKXt9Ij50cnkuYwogICAgIGZvciBpbmNkaXIg
aW4gJGxvY2luY3B0aDsgZG8KICAgICAgICB3YXJuPWAkY2MgJGNjZmxhZ3MgLUkkaW5jZGlyIC1j
IHRyeS5jIDI+JjEgfCBcCkBAIC01NDA5LDEzICs1NDA5LDEzIEBAIGZpCiBjYXNlICIkaGludCIg
aW4KIGRlZmF1bHR8cmVjb21tZW5kZWQpCiAJY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0JMSopIGRm
bHQ9IiRkZmx0IC1mcGNjLXN0cnVjdC1yZXR1cm4iIDs7CisJMS4qKSBkZmx0PSIkZGZsdCAtZnBj
Yy1zdHJ1Y3QtcmV0dXJuIiA7OwogCWVzYWMKIAljYXNlICIkb3B0aW1pemU6JERFQlVHR0lORyIg
aW4KIAkqLWcqOm9sZCkgZGZsdD0iJGRmbHQgLURERUJVR0dJTkciOzsKIAllc2FjCiAJY2FzZSAi
JGdjY3ZlcnNpb24iIGluCi0JMiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYK
KwkyLiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYKIAkJCSRjb250YWlucyBf
UE9TSVhfVkVSU0lPTiAkdXNyaW5jL3N5cy91bmlzdGQuaCA+L2Rldi9udWxsIDI+JjEKIAkJdGhl
bgogCQkJIyBJbnRlcmFjdGl2ZSBTeXN0ZW1zIChJU0MpIFBPU0lYIG1vZGUuCkBAIC01NDI0LDcg
KzU0MjQsNyBAQCBkZWZhdWx0fHJlY29tbWVuZGVkKQogCQk7OwogCWVzYWMKIAljYXNlICIkZ2Nj
dmVyc2lvbiIgaW4KLQkxKikgOzsKKwkxLiopIDs7CiAJMi5bMC04XSopIDs7CiAJPyopCXNldCBz
dHJpY3QtYWxpYXNpbmcgLWZuby1zdHJpY3QtYWxpYXNpbmcKIAkJZXZhbCAkY2hlY2tjY2ZsYWcK
QEAgLTU1MzMsNyArNTUzMyw3IEBAIGNhc2UgIiRjcHBmbGFncyIgaW4KICopICBjcHBmbGFncz0i
JGNwcGZsYWdzICRjY2ZsYWdzIiA7OwogZXNhYwogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikg
Y3BwZmxhZ3M9IiRjcHBmbGFncyAtRF9fR05VQ19fIgorMS4qKSBjcHBmbGFncz0iJGNwcGZsYWdz
IC1EX19HTlVDX18iCiBlc2FjCiBjYXNlICIkbWlwc190eXBlIiBpbgogJycpOzsKQEAgLTIyOTYx
LDcgKzIyOTYxLDcgQEAgZmkKIAogOiBhZGQgLURfRk9SVElGWV9TT1VSQ0UgaWYgZmVhc2libGUg
YW5kIG5vdCBhbHJlYWR5IHRoZXJlCiBjYXNlICIkZ2NjdmVyc2lvbiIgaW4KLTQuKikJY2FzZSAi
JG9wdGltaXplJGNjZmxhZ3MiIGluCitbNDU2Nzg5XS4qfFsxLTldWzAtOV0qKQljYXNlICIkb3B0
aW1pemUkY2NmbGFncyIgaW4KIAkqLU8qKQljYXNlICIkY2NmbGFncyRjcHBzeW1ib2xzIiBpbgog
CQkqX0ZPUlRJRllfU09VUkNFPSopICMgRG9uJ3QgYWRkIGl0IGFnYWluLgogCQkJZWNobyAiWW91
IHNlZW0gdG8gaGF2ZSAtRF9GT1JUSUZZX1NPVVJDRSBhbHJlYWR5LCBub3QgYWRkaW5nIGl0LiIg
PiY0Cg==
CONFGCC10522
  }
  if ( ( $num <= 5.026000 or ( $num >= 5.027000 and $num < 5.027003 ) )
       and ! ( $num >= 5.024003 and $num < 5.025000  )
  ) {
    return _patch_b64(<<'CONFGCC10526');
LS0tIENvbmZpZ3VyZQorKysgQ29uZmlndXJlCkBAIC00NzAzLDcgKzQ3MDMsNyBAQCBlbHNlCiBm
aQogJHJtIC1mIHRyeSB0cnkuKgogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwPWAuL2xv
YyBnY2MtY3BwICRjcHAgJHB0aGAgOzsKKzEuKikgY3BwPWAuL2xvYyBnY2MtY3BwICRjcHAgJHB0
aGAgOzsKIGVzYWMKIGNhc2UgIiRnY2N2ZXJzaW9uIiBpbgogJycpIGdjY29zYW5kdmVycz0nJyA7
OwpAQCAtNDc0Myw3ICs0NzQzLDcgQEAgZXNhYwogIyBnY2MgMy4qIGNvbXBsYWluIGFib3V0IGFk
ZGluZyAtSWRpcmVjdG9yaWVzIHRoYXQgdGhleSBhbHJlYWR5IGtub3cgYWJvdXQsCiAjIHNvIHdl
IHdpbGwgdGFrZSB0aG9zZSBvZmYgZnJvbSBsb2NpbmNwdGguCiBjYXNlICIkZ2NjdmVyc2lvbiIg
aW4KLTMqKQorMy4qKQogICAgIGVjaG8gIm1haW4oKXt9Ij50cnkuYwogICAgIGZvciBpbmNkaXIg
aW4gJGxvY2luY3B0aDsgZG8KICAgICAgICB3YXJuPWAkY2MgJGNjZmxhZ3MgLUkkaW5jZGlyIC1j
IHRyeS5jIDI+JjEgfCBcCkBAIC01NDY5LDEzICs1NDY5LDEzIEBAIGZpCiBjYXNlICIkaGludCIg
aW4KIGRlZmF1bHR8cmVjb21tZW5kZWQpCiAJY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0JMSopIGRm
bHQ9IiRkZmx0IC1mcGNjLXN0cnVjdC1yZXR1cm4iIDs7CisJMS4qKSBkZmx0PSIkZGZsdCAtZnBj
Yy1zdHJ1Y3QtcmV0dXJuIiA7OwogCWVzYWMKIAljYXNlICIkb3B0aW1pemU6JERFQlVHR0lORyIg
aW4KIAkqLWcqOm9sZCkgZGZsdD0iJGRmbHQgLURERUJVR0dJTkciOzsKIAllc2FjCiAJY2FzZSAi
JGdjY3ZlcnNpb24iIGluCi0JMiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYK
KwkyLiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYKIAkJCSRjb250YWlucyBf
UE9TSVhfVkVSU0lPTiAkdXNyaW5jL3N5cy91bmlzdGQuaCA+L2Rldi9udWxsIDI+JjEKIAkJdGhl
bgogCQkJIyBJbnRlcmFjdGl2ZSBTeXN0ZW1zIChJU0MpIFBPU0lYIG1vZGUuCkBAIC01NDg0LDcg
KzU0ODQsNyBAQCBkZWZhdWx0fHJlY29tbWVuZGVkKQogCQk7OwogCWVzYWMKIAljYXNlICIkZ2Nj
dmVyc2lvbiIgaW4KLQkxKikgOzsKKwkxLiopIDs7CiAJMi5bMC04XSopIDs7CiAJPyopCXNldCBz
dHJpY3QtYWxpYXNpbmcgLWZuby1zdHJpY3QtYWxpYXNpbmcKIAkJZXZhbCAkY2hlY2tjY2ZsYWcK
QEAgLTU2MDIsNyArNTYwMiw3IEBAIGNhc2UgIiRjcHBmbGFncyIgaW4KICAgICA7OwogZXNhYwog
Y2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwZmxhZ3M9IiRjcHBmbGFncyAtRF9fR05VQ19f
IgorMS4qKSBjcHBmbGFncz0iJGNwcGZsYWdzIC1EX19HTlVDX18iCiBlc2FjCiBjYXNlICIkbWlw
c190eXBlIiBpbgogJycpOzsKQEAgLTIzNjEyLDcgKzIzNjEyLDcgQEAgZmkKIAogOiBhZGQgLURf
Rk9SVElGWV9TT1VSQ0UgaWYgZmVhc2libGUgYW5kIG5vdCBhbHJlYWR5IHRoZXJlCiBjYXNlICIk
Z2NjdmVyc2lvbiIgaW4KLVs0NV0uKikJY2FzZSAiJG9wdGltaXplJGNjZmxhZ3MiIGluCitbNDU2
Nzg5XS4qfFsxLTldWzAtOV0qKQljYXNlICIkb3B0aW1pemUkY2NmbGFncyIgaW4KIAkqLU8qKQlj
YXNlICIkY2NmbGFncyRjcHBzeW1ib2xzIiBpbgogCQkqX0ZPUlRJRllfU09VUkNFPSopICMgRG9u
J3QgYWRkIGl0IGFnYWluLgogCQkJZWNobyAiWW91IHNlZW0gdG8gaGF2ZSAtRF9GT1JUSUZZX1NP
VVJDRSBhbHJlYWR5LCBub3QgYWRkaW5nIGl0LiIgPiY0Cg==
CONFGCC10526
  }
  if ( $num < 5.029003 ) {
    return _patch_b64(<<'CONFGCC10528');
LS0tIENvbmZpZ3VyZQorKysgQ29uZmlndXJlCkBAIC00Njg5LDcgKzQ2ODksNyBAQCBlbHNlCiBm
aQogJHJtIC1mIHRyeSB0cnkuKgogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwPWAuL2xv
YyBnY2MtY3BwICRjcHAgJHB0aGAgOzsKKzEuKikgY3BwPWAuL2xvYyBnY2MtY3BwICRjcHAgJHB0
aGAgOzsKIGVzYWMKIGNhc2UgIiRnY2N2ZXJzaW9uIiBpbgogJycpIGdjY29zYW5kdmVycz0nJyA7
OwpAQCAtNDcyOSw3ICs0NzI5LDcgQEAgZXNhYwogIyBnY2MgMy4qIGNvbXBsYWluIGFib3V0IGFk
ZGluZyAtSWRpcmVjdG9yaWVzIHRoYXQgdGhleSBhbHJlYWR5IGtub3cgYWJvdXQsCiAjIHNvIHdl
IHdpbGwgdGFrZSB0aG9zZSBvZmYgZnJvbSBsb2NpbmNwdGguCiBjYXNlICIkZ2NjdmVyc2lvbiIg
aW4KLTMqKQorMy4qKQogICAgIGVjaG8gIm1haW4oKXt9Ij50cnkuYwogICAgIGZvciBpbmNkaXIg
aW4gJGxvY2luY3B0aDsgZG8KICAgICAgICB3YXJuPWAkY2MgJGNjZmxhZ3MgLUkkaW5jZGlyIC1j
IHRyeS5jIDI+JjEgfCBcCkBAIC01NDU1LDEzICs1NDU1LDEzIEBAIGZpCiBjYXNlICIkaGludCIg
aW4KIGRlZmF1bHR8cmVjb21tZW5kZWQpCiAJY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0JMSopIGRm
bHQ9IiRkZmx0IC1mcGNjLXN0cnVjdC1yZXR1cm4iIDs7CisJMS4qKSBkZmx0PSIkZGZsdCAtZnBj
Yy1zdHJ1Y3QtcmV0dXJuIiA7OwogCWVzYWMKIAljYXNlICIkb3B0aW1pemU6JERFQlVHR0lORyIg
aW4KIAkqLWcqOm9sZCkgZGZsdD0iJGRmbHQgLURERUJVR0dJTkciOzsKIAllc2FjCiAJY2FzZSAi
JGdjY3ZlcnNpb24iIGluCi0JMiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYK
KwkyLiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYKIAkJCSRjb250YWlucyBf
UE9TSVhfVkVSU0lPTiAkdXNyaW5jL3N5cy91bmlzdGQuaCA+L2Rldi9udWxsIDI+JjEKIAkJdGhl
bgogCQkJIyBJbnRlcmFjdGl2ZSBTeXN0ZW1zIChJU0MpIFBPU0lYIG1vZGUuCkBAIC01NDcwLDcg
KzU0NzAsNyBAQCBkZWZhdWx0fHJlY29tbWVuZGVkKQogCQk7OwogCWVzYWMKIAljYXNlICIkZ2Nj
dmVyc2lvbiIgaW4KLQkxKikgOzsKKwkxLiopIDs7CiAJMi5bMC04XSopIDs7CiAJPyopCXNldCBz
dHJpY3QtYWxpYXNpbmcgLWZuby1zdHJpY3QtYWxpYXNpbmcKIAkJZXZhbCAkY2hlY2tjY2ZsYWcK
QEAgLTU1ODgsNyArNTU4OCw3IEBAIGNhc2UgIiRjcHBmbGFncyIgaW4KICAgICA7OwogZXNhYwog
Y2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwZmxhZ3M9IiRjcHBmbGFncyAtRF9fR05VQ19f
IgorMS4qKSBjcHBmbGFncz0iJGNwcGZsYWdzIC1EX19HTlVDX18iCiBlc2FjCiBjYXNlICIkbWlw
c190eXBlIiBpbgogJycpOzsKQEAgLTIzMDI2LDcgKzIzMDI2LDcgQEAgZmkKIAogOiBhZGQgLURf
Rk9SVElGWV9TT1VSQ0UgaWYgZmVhc2libGUgYW5kIG5vdCBhbHJlYWR5IHRoZXJlCiBjYXNlICIk
Z2NjdmVyc2lvbiIgaW4KLVs0NTY3XS4qKQljYXNlICIkb3B0aW1pemUkY2NmbGFncyIgaW4KK1s0
NTY3ODldLip8WzEtOV1bMC05XSopCWNhc2UgIiRvcHRpbWl6ZSRjY2ZsYWdzIiBpbgogCSotTyop
CWNhc2UgIiRjY2ZsYWdzJGNwcHN5bWJvbHMiIGluCiAJCSpfRk9SVElGWV9TT1VSQ0U9KikgIyBE
b24ndCBhZGQgaXQgYWdhaW4uCiAJCQllY2hvICJZb3Ugc2VlbSB0byBoYXZlIC1EX0ZPUlRJRllf
U09VUkNFIGFscmVhZHksIG5vdCBhZGRpbmcgaXQuIiA+JjQK
CONFGCC10528
  }
  _patch_b64(<<'CONFGCC10');
LS0tIENvbmZpZ3VyZQorKysgQ29uZmlndXJlCkBAIC00NzAzLDcgKzQ3MDMsNyBAQCBlbHNlCiBm
aQogJHJtIC1mIHRyeSB0cnkuKgogY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwPWAuL2xv
YyBnY2MtY3BwICRjcHAgJHB0aGAgOzsKKzEuKikgY3BwPWAuL2xvYyBnY2MtY3BwICRjcHAgJHB0
aGAgOzsKIGVzYWMKIGNhc2UgIiRnY2N2ZXJzaW9uIiBpbgogJycpIGdjY29zYW5kdmVycz0nJyA7
OwpAQCAtNDc0Myw3ICs0NzQzLDcgQEAgZXNhYwogIyBnY2MgMy4qIGNvbXBsYWluIGFib3V0IGFk
ZGluZyAtSWRpcmVjdG9yaWVzIHRoYXQgdGhleSBhbHJlYWR5IGtub3cgYWJvdXQsCiAjIHNvIHdl
IHdpbGwgdGFrZSB0aG9zZSBvZmYgZnJvbSBsb2NpbmNwdGguCiBjYXNlICIkZ2NjdmVyc2lvbiIg
aW4KLTMqKQorMy4qKQogICAgIGVjaG8gIm1haW4oKXt9Ij50cnkuYwogICAgIGZvciBpbmNkaXIg
aW4gJGxvY2luY3B0aDsgZG8KICAgICAgICB3YXJuPWAkY2MgJGNjZmxhZ3MgLUkkaW5jZGlyIC1j
IHRyeS5jIDI+JjEgfCBcCkBAIC01NDY5LDEzICs1NDY5LDEzIEBAIGZpCiBjYXNlICIkaGludCIg
aW4KIGRlZmF1bHR8cmVjb21tZW5kZWQpCiAJY2FzZSAiJGdjY3ZlcnNpb24iIGluCi0JMSopIGRm
bHQ9IiRkZmx0IC1mcGNjLXN0cnVjdC1yZXR1cm4iIDs7CisJMS4qKSBkZmx0PSIkZGZsdCAtZnBj
Yy1zdHJ1Y3QtcmV0dXJuIiA7OwogCWVzYWMKIAljYXNlICIkb3B0aW1pemU6JERFQlVHR0lORyIg
aW4KIAkqLWcqOm9sZCkgZGZsdD0iJGRmbHQgLURERUJVR0dJTkciOzsKIAllc2FjCiAJY2FzZSAi
JGdjY3ZlcnNpb24iIGluCi0JMiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYK
KwkyLiopIGlmICR0ZXN0IC1kIC9ldGMvY29uZi9rY29uZmlnLmQgJiYKIAkJCSRjb250YWlucyBf
UE9TSVhfVkVSU0lPTiAkdXNyaW5jL3N5cy91bmlzdGQuaCA+L2Rldi9udWxsIDI+JjEKIAkJdGhl
bgogCQkJIyBJbnRlcmFjdGl2ZSBTeXN0ZW1zIChJU0MpIFBPU0lYIG1vZGUuCkBAIC01NDg0LDcg
KzU0ODQsNyBAQCBkZWZhdWx0fHJlY29tbWVuZGVkKQogCQk7OwogCWVzYWMKIAljYXNlICIkZ2Nj
dmVyc2lvbiIgaW4KLQkxKikgOzsKKwkxLiopIDs7CiAJMi5bMC04XSopIDs7CiAJPyopCXNldCBz
dHJpY3QtYWxpYXNpbmcgLWZuby1zdHJpY3QtYWxpYXNpbmcKIAkJZXZhbCAkY2hlY2tjY2ZsYWcK
QEAgLTU2MDIsNyArNTYwMiw3IEBAIGNhc2UgIiRjcHBmbGFncyIgaW4KICAgICA7OwogZXNhYwog
Y2FzZSAiJGdjY3ZlcnNpb24iIGluCi0xKikgY3BwZmxhZ3M9IiRjcHBmbGFncyAtRF9fR05VQ19f
IgorMS4qKSBjcHBmbGFncz0iJGNwcGZsYWdzIC1EX19HTlVDX18iCiBlc2FjCiBjYXNlICIkbWlw
c190eXBlIiBpbgogJycpOzsKQEAgLTIzMjI5LDcgKzIzMjI5LDcgQEAgZmkKIAogOiBhZGQgLURf
Rk9SVElGWV9TT1VSQ0UgaWYgZmVhc2libGUgYW5kIG5vdCBhbHJlYWR5IHRoZXJlCiBjYXNlICIk
Z2NjdmVyc2lvbiIgaW4KLVs0NTY3ODldLiopCWNhc2UgIiRvcHRpbWl6ZSRjY2ZsYWdzIiBpbgor
WzQ1Njc4OV0uKnxbMS05XVswLTldKikJY2FzZSAiJG9wdGltaXplJGNjZmxhZ3MiIGluCiAJKi1P
KikJY2FzZSAiJGNjZmxhZ3MkY3Bwc3ltYm9scyIgaW4KIAkJKl9GT1JUSUZZX1NPVVJDRT0qKSAj
IERvbid0IGFkZCBpdCBhZ2Fpbi4KIAkJCWVjaG8gIllvdSBzZWVtIHRvIGhhdmUgLURfRk9SVElG
WV9TT1VSQ0UgYWxyZWFkeSwgbm90IGFkZGluZyBpdC4iID4mNAo=
CONFGCC10
}

sub _patch_useshrplib {
  # from https://github.com/Perl/perl5/commit/191f8909fa4eca1db16a91ada42dd4a065c04890
  _patch(<<'END');
diff --git a/Makefile.SH b/Makefile.SH
index 6e4d5ee684f..bebe50dc131 100755
--- Makefile.SH
+++ Makefile.SH
@@ -67,8 +67,16 @@ true)
                             -compatibility_version \
 				${api_revision}.${api_version}.${api_subversion} \
 			     -current_version \
-				${revision}.${patchlevel}.${subversion} \
-			     -install_name \$(shrpdir)/\$@"
+				${revision}.${patchlevel}.${subversion}"
+		case "$osvers" in
+	        1[5-9]*|[2-9]*)
+			shrpldflags="$shrpldflags -install_name `pwd`/\$@ -Xlinker -headerpad_max_install_names"
+			exeldflags="-Xlinker -headerpad_max_install_names"
+			;;
+		*)
+			shrpldflags="$shrpldflags -install_name \$(shrpdir)/\$@"
+			;;
+		esac
 		;;
 	cygwin*)
 		shrpldflags="$shrpldflags -Wl,--out-implib=libperl.dll.a -Wl,--image-base,0x52000000"
@@ -339,6 +347,14 @@ MANIFEST_SRT = MANIFEST.srt
 
 !GROK!THIS!
 
+case "$useshrplib$osname" in
+truedarwin)
+	$spitshell >>$Makefile <<!GROK!THIS!
+PERL_EXE_LDFLAGS=$exeldflags
+!GROK!THIS!
+	;;
+esac
+
 case "$usecrosscompile$perl" in
 define?*)
 	$spitshell >>$Makefile <<!GROK!THIS!
@@ -1050,6 +1066,20 @@ $(PERL_EXE): $& $(perlmain_dep) $(LIBPERL) $(static_ext) ext.libs $(PERLEXPORT)
 	$(SHRPENV) $(CC) -o perl $(CLDFLAGS) $(CCDLFLAGS) $(perlmain_objs) $(LLIBPERL) $(static_ext) `cat ext.libs` $(libs)
 !NO!SUBS!
         ;;
+
+	darwin)
+	    case "$useshrplib$osvers" in
+	    true1[5-9]*|true[2-9]*) $spitshell >>$Makefile <<'!NO!SUBS!'
+	$(SHRPENV) $(CC) -o perl $(PERL_EXE_LDFLAGS) $(CLDFLAGS) $(CCDLFLAGS) $(perlmain_objs) $(static_ext) $(LLIBPERL) `cat ext.libs` $(libs)
+!NO!SUBS!
+	       ;;
+	    *) $spitshell >>$Makefile <<'!NO!SUBS!'
+	$(SHRPENV) $(CC) -o perl $(CLDFLAGS) $(CCDLFLAGS) $(perlmain_objs) $(static_ext) $(LLIBPERL) `cat ext.libs` $(libs)
+!NO!SUBS!
+	       ;;
+	    esac
+        ;;
+
         *) $spitshell >>$Makefile <<'!NO!SUBS!'
 	$(SHRPENV) $(CC) -o perl $(CLDFLAGS) $(CCDLFLAGS) $(perlmain_objs) $(static_ext) $(LLIBPERL) `cat ext.libs` $(libs)
 !NO!SUBS!
diff --git a/installperl b/installperl
index 3bf79d2d6fc..6cd65a09238 100755
--- installperl
+++ installperl
@@ -304,6 +304,7 @@ elsif ($^O ne 'dos') {
 	safe_unlink("$installbin/$perl_verbase$ver$exe_ext");
 	copy("perl$exe_ext", "$installbin/$perl_verbase$ver$exe_ext");
 	strip("$installbin/$perl_verbase$ver$exe_ext");
+	fix_dep_names("$installbin/$perl_verbase$ver$exe_ext");
 	chmod(0755, "$installbin/$perl_verbase$ver$exe_ext");
     }
     else {
@@ -388,6 +389,7 @@ foreach my $file (@corefiles) {
     if (copy_if_diff($file,"$installarchlib/CORE/$file")) {
 	if ($file =~ /\.(\Q$so\E|\Q$dlext\E)$/) {
 	    strip("-S", "$installarchlib/CORE/$file") if $^O eq 'darwin';
+	    fix_dep_names("$installarchlib/CORE/$file");
 	    chmod($SO_MODE, "$installarchlib/CORE/$file");
 	} else {
 	    chmod($NON_SO_MODE, "$installarchlib/CORE/$file");
@@ -791,4 +793,27 @@ sub strip
     }
 }
 
+sub fix_dep_names {
+    my $file = shift;
+
+    $^O eq "darwin" && $Config{osvers} =~ /^(1[5-9]|[2-9])/
+      && $Config{useshrplib}
+      or return;
+
+    my @opts;
+    my $so = $Config{so};
+    my $libperl = "$Config{archlibexp}/CORE/libperl.$Config{so}";
+    if ($file =~ /\blibperl.\Q$Config{so}\E$/a) {
+        push @opts, -id => $libperl;
+    }
+    else {
+        push @opts, -change => getcwd . "/libperl.$so", $libperl;
+    }
+    push @opts, $file;
+
+    $opts{verbose} and print "  install_name_tool @opts\n";
+    system "install_name_tool", @opts
+      and die "Cannot update $file dependency paths\n";
+}
+
 # ex: set ts=8 sts=4 sw=4 et:
END
}

qq[patchin'];

=pod

=head1 SYNOPSIS

  use strict;
  use warnings;

  use Devel::PatchPerl;

  Devel::PatchPerl->patch_source( '5.6.1', '/path/to/untarred/perl/source/perl-5.6.1' );

=head1 DESCRIPTION

Devel::PatchPerl is a modularisation of the patching code contained in L<Devel::PPPort>'s
C<buildperl.pl>.

It does not build perls, it merely provides an interface to the source patching
functionality.

=head1 FUNCTION

=over

=item C<patch_source>

Takes two parameters, a C<perl> version and the path to unwrapped perl source for that version.
It dies on any errors.

If you don't supply a C<perl> version, it will attempt to auto-determine the
C<perl> version from the specified path.

If you don't supply the path to unwrapped perl source, it will assume the
current working directory.

=item C<determine_version>

Takes one optional parameter, the path to unwrapped perl source. It returns the perl version
of the source code at the given location. It returns undef on error.

If you don't supply the path to unwrapped perl source, it will assume the
current working directory.

=back

=head1 PLUGIN SYSTEM

See L<Devel::PatchPerl::Plugin> for details of Devel::PatchPerl's plugin system.

=head1 PATCHLEVEL

Devel::PatchPerl will normally update the C<patchlevel.h> file in the perl source tree
to indicate that it has applied local patches. This behaviour is negated if it is
detected that it is operating in a git repository. To override this and update
C<patchlevel.h> when in a Git repository, set the env var C<PERL5_PATCHPERL_PATCHLEVEL>
to a true value.

Alternatively, call C<patchperl> with the C<--patchlevel> option.

=head1 CAVEAT

Devel::PatchPerl is intended only to facilitate the C<building> of perls, not to
facilitate the C<testing> of perls. This means that it will not patch failing tests
in the perl testsuite.

=head1 SEE ALSO

L<Devel::PPPort>

L<Devel::PatchPerl::Plugin>

=cut
