package Devel::PatchPerl;

# ABSTRACT: Patch perl source a la Devel::PPPort's buildperl.pl

use strict;
use warnings;
use File::pushd qw[pushd];
use File::Spec;
use IO::File;
use Devel::PatchPerl::Hints qw[hint_file];
use Module::Pluggable search_path => ['Devel::PatchPerl::Plugin'];
use vars qw[@ISA @EXPORT_OK];

use constant CERTIFIED => 5.029010; # Anything less than this

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
              [ \&_patch_hints ],
              [ \&_patch_patchlevel ],
              [ \&_patch_develpatchperlversion ],
              [ \&_patch_errno_gcc5 ],
              [ \&_patch_conf_fwrapv ],
              [ \&_patch_utils_h2ph ],
              [ \&_patch_lib_h2ph ],
            ],
  },
  {
    perl => [
              qr/^5\.6\.[0-2]$/,
              qr/^5\.7\.[0-3]$/,
              qr/^5\.8\.[0-8]$/,
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
              qr/^5\.24\.[01]$/,
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
  if ( _norm_ver( $vers ) >= CERTIFIED ) {
      warn "Nothing to do '$vers' is fine\n";
      return;
  }
  $source = File::Spec->rel2abs($source);
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

sub _patch
{
  my($patch) = @_;
  my @ro = ();
  for ($patch =~ /^\+{3}\s+(\S+)/gm) {
    print "patching $_\n";
    # some filesystems (e.g., Lustre) will kill this process if there
    # is an attempt to write to a file that is 0444, so make these
    # files 0644 for the duration of the patch
    if (-r $_ and not -w $_) {
      push @ro, $_; # save for chmod back to 0444
      chmod 0644, $_;
    }
  }
  my $diff = 'tmp.diff';
  _write_or_die($diff, $patch);
  die "No patch utility found\n" unless $patch_exe;
  local $ENV{PATCH_GET} = 0; # I can't reproduce this at all, but meh.
  _run_or_die("$patch_exe -f -s -p0 <$diff");
  unlink $diff or die "unlink $diff: $!\n";
  # put back ro to 0444
  chmod 0444, @ro;
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

# adapted from patchlevel.h for use with perls that predate it
sub _patch_patchlevel {
  return if -d '.git';
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
  # If 5.8.[12345678]
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
  return unless $num < 5.021010;
  return if    $num == 5.020003;
  if ( $num < 5.006001 ) {
    return _patch(<<'UH2PH560');
--- utils/h2ph.PL
+++ utils/h2ph.PL
@@ -36,13 +36,21 @@ $Config{startperl}
 
 print OUT <<'!NO!SUBS!';
 
+use strict;
+
 use Config;
 use File::Path qw(mkpath);
 use Getopt::Std;
 
-getopts('Dd:rlhaQ');
+# Make sure read permissions for all are set:
+if (defined umask && (umask() & 0444)) {
+    umask (umask() & ~0444);
+}
+
+getopts('Dd:rlhaQe');
+use vars qw($opt_D $opt_d $opt_r $opt_l $opt_h $opt_a $opt_Q $opt_e);
 die "-r and -a options are mutually exclusive\n" if ($opt_r and $opt_a);
-@inc_dirs = inc_dirs() if $opt_a;
+my @inc_dirs = inc_dirs() if $opt_a;
 
 my $Exit = 0;
 
@@ -50,7 +58,7 @@ my $Dest_dir = $opt_d || $Config{installsitearch};
 die "Destination directory $Dest_dir doesn't exist or isn't a directory\n"
     unless -d $Dest_dir;
 
-@isatype = split(' ',<<END);
+my @isatype = split(' ',<<END);
 	char	uchar	u_char
 	short	ushort	u_short
 	int	uint	u_int
@@ -58,14 +66,26 @@ die "Destination directory $Dest_dir doesn't exist or isn't a directory\n"
 	FILE	key_t	caddr_t
 END
 
+my %isatype;
 @isatype{@isatype} = (1) x @isatype;
-$inif = 0;
+my $inif = 0;
+my %Is_converted;
+my %bad_file = ();
 
 @ARGV = ('-') unless @ARGV;
 
 build_preamble_if_necessary();
 
-while (defined ($file = next_file())) {
+sub reindent($) {
+    my($text) = shift;
+    $text =~ s/\n/\n    /g;
+    $text =~ s/        /\t/g;
+    $text;
+}
+
+my ($t, $tab, %curargs, $new, $eval_index, $dir, $name, $args, $outfile);
+my ($incl, $incl_type, $incl_quote, $next);
+while (defined (my $file = next_file())) {
     if (-l $file and -d $file) {
         link_if_possible($file) if ($opt_l);
         next;
@@ -100,36 +120,23 @@ while (defined ($file = next_file())) {
 	open(OUT,">$Dest_dir/$outfile") || die "Can't create $outfile: $!\n";
     }
 
-    print OUT "require '_h2ph_pre.ph';\n\n";
-    while (<IN>) {
-	chop;
-	while (/\\$/) {
-	    chop;
-	    $_ .= <IN>;
-	    chop;
-	}
-	print OUT "# $_\n" if $opt_D;
-
-	if (s:/\*:\200:g) {
-	    s:\*/:\201:g;
-	    s/\200[^\201]*\201//g;	# delete single line comments
-	    if (s/\200.*//) {		# begin multi-line comment?
-		$_ .= '/*';
-		$_ .= <IN>;
-		redo;
-	    }
-	}
+    print OUT
+        "require '_h2ph_pre.ph';\n\n",
+        "no warnings 'redefine';\n\n";
+
+    while (defined (local $_ = next_line($file))) {
 	if (s/^\s*\#\s*//) {
 	    if (s/^define\s+(\w+)//) {
 		$name = $1;
 		$new = '';
 		s/\s+$//;
+		s/\(\w+\s*\(\*\)\s*\(\w*\)\)\s*(-?\d+)/$1/; # (int (*)(foo_t))0
 		if (s/^\(([\w,\s]*)\)//) {
 		    $args = $1;
     	    	    my $proto = '() ';
 		    if ($args ne '') {
     	    	    	$proto = '';
-			foreach $arg (split(/,\s*/,$args)) {
+			foreach my $arg (split(/,\s*/,$args)) {
 			    $arg =~ s/^\s*([^\s].*[^\s])\s*$/$1/;
 			    $curargs{$arg} = 1;
 			}
@@ -177,22 +184,32 @@ while (defined ($file = next_file())) {
                       print OUT $t,"unless(defined(\&$name)) {\n    sub $name () {\t",$new,";}\n}\n";
 		    }
 		}
-	    } elsif (/^(include|import)\s*[<"](.*)[>"]/) {
-		($incl = $2) =~ s/\.h$/.ph/;
-		print OUT $t,"require '$incl';\n";
-	    } elsif(/^include_next\s*[<"](.*)[>"]/) {
-		($incl = $1) =~ s/\.h$/.ph/;
+	    } elsif (/^(include|import|include_next)\s*([<\"])(.*)[>\"]/) {
+                $incl_type = $1;
+                $incl_quote = $2;
+                $incl = $3;
+                if (($incl_type eq 'include_next') ||
+                    ($opt_e && exists($bad_file{$incl}))) {
+                    $incl =~ s/\.h$/.ph/;
 		print OUT ($t,
 			   "eval {\n");
                 $tab += 4;
                 $t = "\t" x ($tab / 8) . ' ' x ($tab % 8);
+                    print OUT ($t, "my(\@REM);\n");
+                    if ($incl_type eq 'include_next') {
 		print OUT ($t,
 			   "my(\%INCD) = map { \$INC{\$_} => 1 } ",
-			   "(grep { \$_ eq \"$incl\" } keys(\%INC));\n");
+			           "(grep { \$_ eq \"$incl\" } ",
+                                   "keys(\%INC));\n");
 		print OUT ($t,
-			   "my(\@REM) = map { \"\$_/$incl\" } ",
+			           "\@REM = map { \"\$_/$incl\" } ",
 			   "(grep { not exists(\$INCD{\"\$_/$incl\"})",
-			   "and -f \"\$_/$incl\" } \@INC);\n");
+			           " and -f \"\$_/$incl\" } \@INC);\n");
+                    } else {
+                        print OUT ($t,
+                                   "\@REM = map { \"\$_/$incl\" } ",
+                                   "(grep {-r \"\$_/$incl\" } \@INC);\n");
+                    }
 		print OUT ($t,
 			   "require \"\$REM[0]\" if \@REM;\n");
                 $tab -= 4;
@@ -201,6 +218,14 @@ while (defined ($file = next_file())) {
 			   "};\n");
 		print OUT ($t,
 			   "warn(\$\@) if \$\@;\n");
+                } else {
+                    $incl =~ s/\.h$/.ph/;
+                    # copy the prefix in the quote syntax (#include "x.h") case
+                    if ($incl !~ m|/| && $incl_quote eq q{"} && $file =~ m|^(.*)/|) {
+                        $incl = "$1/$incl";
+                    }
+		    print OUT $t,"require '$incl';\n";
+                }
 	    } elsif (/^ifdef\s+(\w+)/) {
 		print OUT $t,"if(defined(&$1)) {\n";
 		$tab += 4;
@@ -248,20 +273,24 @@ while (defined ($file = next_file())) {
 	    } elsif(/^ident\s+(.*)/) {
 		print OUT $t, "# $1\n";
 	    }
- 	} elsif(/^\s*(typedef\s*)?enum\s*(\s+[a-zA-Z_]\w*\s*)?\{/) {
-	    until(/\}.*?;/) {
-		chomp($next = <IN>);
+	} elsif(/^\s*(typedef\s*)?enum\s*(\s+[a-zA-Z_]\w*\s*)?/) {
+	    until(/\{[^}]*\}.*;/ || /;/) {
+		last unless defined ($next = next_line($file));
+		chomp $next;
+		# drop "#define FOO FOO" in enums
+		$next =~ s/^\s*#\s*define\s+(\w+)\s+\1\s*$//;
 		$_ .= $next;
 		print OUT "# $next\n" if $opt_D;
 	    }
+	    s/#\s*if.*?#\s*endif//g; # drop #ifdefs
 	    s@/\*.*?\*/@@g;
 	    s/\s+/ /g;
-	    /^\s?(typedef\s?)?enum\s?([a-zA-Z_]\w*)?\s?\{(.*)\}\s?([a-zA-Z_]\w*)?\s?;/;
-	    ($enum_subs = $3) =~ s/\s//g;
-	    @enum_subs = split(/,/, $enum_subs);
-	    $enum_val = -1;
-	    for $enum (@enum_subs) {
-		($enum_name, $enum_value) = $enum =~ /^([a-zA-Z_]\w*)(=.+)?$/;
+	    next unless /^\s?(typedef\s?)?enum\s?([a-zA-Z_]\w*)?\s?\{(.*)\}\s?([a-zA-Z_]\w*)?\s?;/;
+	    (my $enum_subs = $3) =~ s/\s//g;
+	    my @enum_subs = split(/,/, $enum_subs);
+	    my $enum_val = -1;
+	    foreach my $enum (@enum_subs) {
+		my ($enum_name, $enum_value) = $enum =~ /^([a-zA-Z_]\w*)(=.+)?$/;
 		$enum_value =~ s/^=//;
 		$enum_val = (length($enum_value) ? $enum_value : $enum_val + 1);
 		if ($opt_h) {
@@ -278,31 +307,47 @@ while (defined ($file = next_file())) {
 	    }
 	}
     }
-    print OUT "1;\n";
-
-    $is_converted{$file} = 1;
+    $Is_converted{$file} = 1;
+    if ($opt_e && exists($bad_file{$file})) {
+        unlink($Dest_dir . '/' . $outfile);
+        $next = '';
+    } else {
+        print OUT "1;\n";
     queue_includes_from($file) if ($opt_a);
+    }
 }
 
-exit $Exit;
-
-sub reindent($) {
-    my($text) = shift;
-    $text =~ s/\n/\n    /g;
-    $text =~ s/        /\t/g;
-    $text;
+if ($opt_e && (scalar(keys %bad_file) > 0)) {
+    warn "Was unable to convert the following files:\n";
+    warn "\t" . join("\n\t",sort(keys %bad_file)) . "\n";
 }
 
+exit $Exit;
+
 sub expr {
+    my $joined_args;
     if(keys(%curargs)) {
-	my($joined_args) = join('|', keys(%curargs));
+	$joined_args = join('|', keys(%curargs));
     }
     while ($_ ne '') {
 	s/^\&\&// && do { $new .= " &&"; next;}; # handle && operator
 	s/^\&([\(a-z\)]+)/$1/i;	# hack for things that take the address of
 	s/^(\s+)//		&& do {$new .= ' '; next;};
-	s/^(0X[0-9A-F]+)[UL]*//i	&& do {$new .= lc($1); next;};
-	s/^(-?\d+\.\d+E[-+]\d+)F?//i	&& do {$new .= $1; next;};
+	s/^0X([0-9A-F]+)[UL]*//i 
+	    && do {my $hex = $1;
+		   $hex =~ s/^0+//;
+		   if (length $hex > 8 && !$Config{use64bitint}) {
+		       # Croak if nv_preserves_uv_bits < 64 ?
+		       $new .=         hex(substr($hex, -8)) +
+			       2**32 * hex(substr($hex,  0, -8));
+		       # The above will produce "errorneus" code
+		       # if the hex constant was e.g. inside UINT64_C
+		       # macro, but then again, h2ph is an approximation.
+		   } else {
+		       $new .= lc("0x$hex");
+		   }
+		   next;};
+	s/^(-?\d+\.\d+E[-+]?\d+)[FL]?//i	&& do {$new .= $1; next;};
 	s/^(\d+)\s*[LU]*//i	&& do {$new .= $1; next;};
 	s/^("(\\"|[^"])*")//	&& do {$new .= $1; next;};
 	s/^'((\\"|[^"])*)'//	&& do {
@@ -341,13 +386,13 @@ sub expr {
 	# Eliminate typedefs
 	/\(([\w\s]+)[\*\s]*\)\s*[\w\(]/ && do {
 	    foreach (split /\s+/, $1) {  # Make sure all the words are types,
-		last unless ($isatype{$_} or $_ eq 'struct');
+		last unless ($isatype{$_} or $_ eq 'struct' or $_ eq 'union');
 	    }
 	    s/\([\w\s]+[\*\s]*\)// && next;      # then eliminate them.
 	};
 	# struct/union member, including arrays:
 	s/^([_A-Z]\w*(\[[^\]]+\])?((\.|->)[_A-Z]\w*(\[[^\]]+\])?)+)//i && do {
-	    $id = $1;
+	    my $id = $1;
 	    $id =~ s/(\.|(->))([^\.\-]*)/->\{$3\}/g;
 	    $id =~ s/\b([^\$])($joined_args)/$1\$$2/g if length($joined_args);
 	    while($id =~ /\[\s*([^\$\&\d\]]+)\]/) {
@@ -363,8 +408,8 @@ sub expr {
 	    $new .= " (\$$id)";
 	};
 	s/^([_a-zA-Z]\w*)//	&& do {
-	    $id = $1;
-	    if ($id eq 'struct') {
+	    my $id = $1;
+	    if ($id eq 'struct' || $id eq 'union') {
 		s/^\s+(\w+)//;
 		$id .= ' ' . $1;
 		$isatype{$id} = 1;
@@ -377,8 +422,8 @@ sub expr {
 		$new .= '->' if /^[\[\{]/;
 	    } elsif ($id eq 'defined') {
 		$new .= 'defined';
-	    } elsif (/^\(/) {
-		s/^\((\w),/("$1",/ if $id =~ /^_IO[WR]*$/i;	# cheat
+	    } elsif (/^\s*\(/) {
+		s/^\s*\((\w),/("$1",/ if $id =~ /^_IO[WR]*$/i;	# cheat
 		$new .= " &$id";
 	    } elsif ($isatype{$id}) {
 		if ($new =~ /{\s*$/) {
@@ -391,7 +436,7 @@ sub expr {
 		}
 	    } else {
 		if ($inif && $new !~ /defined\s*\($/) {
-		    $new .= '(defined(&' . $id . ') ? &' . $id . ' : 0)';
+		    $new .= '(defined(&' . $id . ') ? &' . $id . ' : undef)';
 		} elsif (/^\[/) {
 		    $new .= " \$$id";
 		} else {
@@ -405,6 +450,101 @@ sub expr {
 }
 
 
+sub next_line
+{
+    my $file = shift;
+    my ($in, $out);
+    my $pre_sub_tri_graphs = 1;
+
+    READ: while (not eof IN) {
+        $in  .= <IN>;
+        chomp $in;
+        next unless length $in;
+
+        while (length $in) {
+            if ($pre_sub_tri_graphs) {
+                # Preprocess all tri-graphs 
+                # including things stuck in quoted string constants.
+                $in =~ s/\?\?=/#/g;                         # | ??=|  #|
+                $in =~ s/\?\?\!/|/g;                        # | ??!|  ||
+                $in =~ s/\?\?'/^/g;                         # | ??'|  ^|
+                $in =~ s/\?\?\(/[/g;                        # | ??(|  [|
+                $in =~ s/\?\?\)/]/g;                        # | ??)|  ]|
+                $in =~ s/\?\?\-/~/g;                        # | ??-|  ~|
+                $in =~ s/\?\?\//\\/g;                       # | ??/|  \|
+                $in =~ s/\?\?</{/g;                         # | ??<|  {|
+                $in =~ s/\?\?>/}/g;                         # | ??>|  }|
+            }
+	    if ($in =~ /^\#ifdef __LANGUAGE_PASCAL__/) {
+                # Tru64 disassembler.h evilness: mixed C and Pascal.
+		while (<IN>) {
+		    last if /^\#endif/; 
+		}
+		next READ;
+	    }
+	    if ($in =~ /^extern inline / && # Inlined assembler.
+		$^O eq 'linux' && $file =~ m!(?:^|/)asm/[^/]+\.h$!) {
+ 		while (<IN>) {
+		    last if /^}/; 
+		}
+		next READ;
+	    }
+            if ($in =~ s/\\$//) {                           # \-newline
+                $out    .= ' ';
+                next READ;
+            } elsif ($in =~ s/^([^"'\\\/]+)//) {            # Passthrough
+                $out    .= $1;
+            } elsif ($in =~ s/^(\\.)//) {                   # \...
+                $out    .= $1;
+            } elsif ($in =~ /^'/) {                         # '...
+                if ($in =~ s/^('(\\.|[^'\\])*')//) {
+                    $out    .= $1;
+                } else {
+                    next READ;
+                }
+            } elsif ($in =~ /^"/) {                         # "...
+                if ($in =~ s/^("(\\.|[^"\\])*")//) {
+                    $out    .= $1;
+                } else {
+                    next READ;
+                }
+            } elsif ($in =~ s/^\/\/.*//) {                  # //...
+                # fall through
+            } elsif ($in =~ m/^\/\*/) {                     # /*...
+                # C comment removal adapted from perlfaq6:
+                if ($in =~ s/^\/\*[^*]*\*+([^\/*][^*]*\*+)*\///) {
+                    $out    .= ' ';
+                } else {                                    # Incomplete /* */
+                    next READ;
+                }
+            } elsif ($in =~ s/^(\/)//) {                    # /...
+                $out    .= $1;
+            } elsif ($in =~ s/^([^\'\"\\\/]+)//) {
+                $out    .= $1;
+            } elsif ($^O eq 'linux' &&
+                     $file =~ m!(?:^|/)linux/byteorder/pdp_endian\.h$! &&
+                     $in   =~ s!\'T KNOW!!) {
+                $out    =~ s!I DON$!I_DO_NOT_KNOW!;
+            } else {
+                if ($opt_e) {
+                    warn "Cannot parse $file:\n$in\n";
+                    $bad_file{$file} = 1;
+                    $in = '';
+                    $out = undef;
+                    last READ;
+                } else {
+		die "Cannot parse:\n$in\n";
+                }
+            }
+        }
+
+        last READ if $out =~ /\S/;
+    }
+
+    return $out;
+}
+
+
 # Handle recursive subdirectories without getting a grotesquely big stack.
 # Could this be implemented using File::Find?
 sub next_file
@@ -504,8 +644,13 @@ sub queue_includes_from
                 $line .= <HEADER>;
             }
 
-            if ($line =~ /^#\s*include\s+<(.*?)>/) {
-                push(@ARGV, $1) unless $is_converted{$1};
+            if ($line =~ /^#\s*include\s+([<"])(.*?)[>"]/) {
+                my ($delimiter, $new_file) = ($1, $2);
+                # copy the prefix in the quote syntax (#include "x.h") case
+                if ($delimiter eq q{"} && $file =~ m|^(.*)/|) {
+                    $new_file = "$1/$new_file";
+                }
+                push(@ARGV, $new_file) unless $Is_converted{$new_file};
             }
         }
     close HEADER;
@@ -546,25 +691,50 @@ sub build_preamble_if_necessary
     my (%define) = _extract_cc_defines();
 
     open  PREAMBLE, ">$preamble" or die "Cannot open $preamble:  $!";
-        print PREAMBLE "# This file was created by h2ph version $VERSION\n";
-
-        foreach (sort keys %define) {
-            if ($opt_D) {
-                print PREAMBLE "# $_=$define{$_}\n";
-            }
-
-            if ($define{$_} =~ /^\d+$/) {
-                print PREAMBLE
-                    "unless (defined &$_) { sub $_() { $define{$_} } }\n\n";
-            } elsif ($define{$_} =~ /^\w+$/) {
-                print PREAMBLE
-                    "unless (defined &$_) { sub $_() { &$define{$_} } }\n\n";
-            } else {
+	print PREAMBLE "# This file was created by h2ph version $VERSION\n";
+        # Prevent non-portable hex constants from warning.
+        #
+        # We still produce an overflow warning if we can't represent
+        # a hex constant as an integer.
+        print PREAMBLE "no warnings qw(portable);\n";
+
+	foreach (sort keys %define) {
+	    if ($opt_D) {
+		print PREAMBLE "# $_=$define{$_}\n";
+	    }
+	    if ($define{$_} =~ /^\((.*)\)$/) {
+		# parenthesized value:  d=(v)
+		$define{$_} = $1;
+	    }
+	    if ($define{$_} =~ /^([+-]?(\d+)?\.\d+([eE][+-]?\d+)?)[FL]?$/) {
+		# float:
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { $1 } }\n\n";
+	    } elsif ($define{$_} =~ /^([+-]?\d+)U?L{0,2}$/i) {
+		# integer:
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { $1 } }\n\n";
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
                 print PREAMBLE
-                    "unless (defined &$_) { sub $_() { \"",
-                    quotemeta($define{$_}), "\" } }\n\n";
-            }
-        }
+                    "unless (defined &$_) { sub $_() { $code } }\n\n";
+	    } elsif ($define{$_} =~ /^\w+$/) {
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { &$define{$_} } }\n\n";
+	    } else {
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { \"",
+		    quotemeta($define{$_}), "\" } }\n\n";
+	    }
+	}
     close PREAMBLE               or die "Cannot close $preamble:  $!";
 }
 
@@ -575,15 +745,15 @@ sub build_preamble_if_necessary
 sub _extract_cc_defines
 {
     my %define;
-    my $allsymbols = join " ", @Config{ccsymbols, cppsymbols, cppccsymbols};
+    my $allsymbols  = join " ",
+	@Config{'ccsymbols', 'cppsymbols', 'cppccsymbols'};
 
     # Split compiler pre-definitions into `key=value' pairs:
-    foreach (split /\s+/, $allsymbols) {
-        /(.+?)=(.+)/ and $define{$1} = $2;
-
-        if ($opt_D) {
-            print STDERR "$_:  $1 -> $2\n";
-        }
+    while ($allsymbols =~ /([^\s]+)=((\\\s|[^\s])+)/g) {
+	$define{$1} = $2;
+	if ($opt_D) {
+	    print STDERR "$_:  $1 -> $2\n";
+	}
     }
 
     return %define;
@@ -612,6 +782,10 @@ It is most easily run while in /usr/include:
 
 	cd /usr/include; h2ph * sys/*
 
+or
+
+	cd /usr/include; h2ph * sys/* arpa/* netinet/*
+
 or
 
 	cd /usr/include; h2ph -r -l .
@@ -629,7 +803,7 @@ If run with no arguments, filters standard input to standard output.
 =item -d destination_dir
 
 Put the resulting B<.ph> files beneath B<destination_dir>, instead of
-beneath the default Perl library location (C<$Config{'installsitsearch'}>).
+beneath the default Perl library location (C<$Config{'installsitearch'}>).
 
 =item -r
 
@@ -708,18 +882,16 @@ that it can translate.
 It's only intended as a rough tool.
 You may need to dicker with the files produced.
 
-Doesn't run with C<use strict>
-
 You have to run this program by hand; it's not run as part of the Perl
 installation.
 
 Doesn't handle complicated expressions built piecemeal, a la:
 
     enum {
-        FIRST_VALUE,
-        SECOND_VALUE,
+	FIRST_VALUE,
+	SECOND_VALUE,
     #ifdef ABC
-        THIRD_VALUE
+	THIRD_VALUE
     #endif
     };
 
UH2PH560
  }
  if ( $num < 5.008000 ) {
    return _patch(<<'UH2PH562');
--- utils/h2ph.PL
+++ utils/h2ph.PL
@@ -42,8 +42,13 @@ use Config;
 use File::Path qw(mkpath);
 use Getopt::Std;
 
-getopts('Dd:rlhaQ');
-use vars qw($opt_D $opt_d $opt_r $opt_l $opt_h $opt_a $opt_Q);
+# Make sure read permissions for all are set:
+if (defined umask && (umask() & 0444)) {
+    umask (umask() & ~0444);
+}
+
+getopts('Dd:rlhaQe');
+use vars qw($opt_D $opt_d $opt_r $opt_l $opt_h $opt_a $opt_Q $opt_e);
 die "-r and -a options are mutually exclusive\n" if ($opt_r and $opt_a);
 my @inc_dirs = inc_dirs() if $opt_a;
 
@@ -65,13 +70,21 @@ my %isatype;
 @isatype{@isatype} = (1) x @isatype;
 my $inif = 0;
 my %Is_converted;
+my %bad_file = ();
 
 @ARGV = ('-') unless @ARGV;
 
 build_preamble_if_necessary();
 
+sub reindent($) {
+    my($text) = shift;
+    $text =~ s/\n/\n    /g;
+    $text =~ s/        /\t/g;
+    $text;
+}
+
 my ($t, $tab, %curargs, $new, $eval_index, $dir, $name, $args, $outfile);
-my ($incl, $next);
+my ($incl, $incl_type, $incl_quote, $next);
 while (defined (my $file = next_file())) {
     if (-l $file and -d $file) {
         link_if_possible($file) if ($opt_l);
@@ -107,30 +120,17 @@ while (defined (my $file = next_file())) {
 	open(OUT,">$Dest_dir/$outfile") || die "Can't create $outfile: $!\n";
     }
 
-    print OUT "require '_h2ph_pre.ph';\n\n";
-    while (<IN>) {
-	chop;
-	while (/\\$/) {
-	    chop;
-	    $_ .= <IN>;
-	    chop;
-	}
-	print OUT "# $_\n" if $opt_D;
-
-	if (s:/\*:\200:g) {
-	    s:\*/:\201:g;
-	    s/\200[^\201]*\201//g;	# delete single line comments
-	    if (s/\200.*//) {		# begin multi-line comment?
-		$_ .= '/*';
-		$_ .= <IN>;
-		redo;
-	    }
-	}
+    print OUT
+        "require '_h2ph_pre.ph';\n\n",
+        "no warnings 'redefine';\n\n";
+
+    while (defined (local $_ = next_line($file))) {
 	if (s/^\s*\#\s*//) {
 	    if (s/^define\s+(\w+)//) {
 		$name = $1;
 		$new = '';
 		s/\s+$//;
+		s/\(\w+\s*\(\*\)\s*\(\w*\)\)\s*(-?\d+)/$1/; # (int (*)(foo_t))0
 		if (s/^\(([\w,\s]*)\)//) {
 		    $args = $1;
     	    	    my $proto = '() ';
@@ -184,22 +184,32 @@ while (defined (my $file = next_file())) {
                       print OUT $t,"unless(defined(\&$name)) {\n    sub $name () {\t",$new,";}\n}\n";
 		    }
 		}
-	    } elsif (/^(include|import)\s*[<"](.*)[>"]/) {
-		($incl = $2) =~ s/\.h$/.ph/;
-		print OUT $t,"require '$incl';\n";
-	    } elsif(/^include_next\s*[<"](.*)[>"]/) {
-		($incl = $1) =~ s/\.h$/.ph/;
+	    } elsif (/^(include|import|include_next)\s*([<\"])(.*)[>\"]/) {
+                $incl_type = $1;
+                $incl_quote = $2;
+                $incl = $3;
+                if (($incl_type eq 'include_next') ||
+                    ($opt_e && exists($bad_file{$incl}))) {
+                    $incl =~ s/\.h$/.ph/;
 		print OUT ($t,
 			   "eval {\n");
                 $tab += 4;
                 $t = "\t" x ($tab / 8) . ' ' x ($tab % 8);
+                    print OUT ($t, "my(\@REM);\n");
+                    if ($incl_type eq 'include_next') {
 		print OUT ($t,
 			   "my(\%INCD) = map { \$INC{\$_} => 1 } ",
-			   "(grep { \$_ eq \"$incl\" } keys(\%INC));\n");
+			           "(grep { \$_ eq \"$incl\" } ",
+                                   "keys(\%INC));\n");
 		print OUT ($t,
-			   "my(\@REM) = map { \"\$_/$incl\" } ",
+			           "\@REM = map { \"\$_/$incl\" } ",
 			   "(grep { not exists(\$INCD{\"\$_/$incl\"})",
-			   "and -f \"\$_/$incl\" } \@INC);\n");
+			           " and -f \"\$_/$incl\" } \@INC);\n");
+                    } else {
+                        print OUT ($t,
+                                   "\@REM = map { \"\$_/$incl\" } ",
+                                   "(grep {-r \"\$_/$incl\" } \@INC);\n");
+                    }
 		print OUT ($t,
 			   "require \"\$REM[0]\" if \@REM;\n");
                 $tab -= 4;
@@ -208,6 +218,14 @@ while (defined (my $file = next_file())) {
 			   "};\n");
 		print OUT ($t,
 			   "warn(\$\@) if \$\@;\n");
+                } else {
+                    $incl =~ s/\.h$/.ph/;
+                    # copy the prefix in the quote syntax (#include "x.h") case
+                    if ($incl !~ m|/| && $incl_quote eq q{"} && $file =~ m|^(.*)/|) {
+                        $incl = "$1/$incl";
+                    }
+		    print OUT $t,"require '$incl';\n";
+                }
 	    } elsif (/^ifdef\s+(\w+)/) {
 		print OUT $t,"if(defined(&$1)) {\n";
 		$tab += 4;
@@ -255,15 +273,19 @@ while (defined (my $file = next_file())) {
 	    } elsif(/^ident\s+(.*)/) {
 		print OUT $t, "# $1\n";
 	    }
- 	} elsif(/^\s*(typedef\s*)?enum\s*(\s+[a-zA-Z_]\w*\s*)?\{/) {
-	    until(/\}.*?;/) {
-		chomp($next = <IN>);
+	} elsif(/^\s*(typedef\s*)?enum\s*(\s+[a-zA-Z_]\w*\s*)?/) {
+	    until(/\{[^}]*\}.*;/ || /;/) {
+		last unless defined ($next = next_line($file));
+		chomp $next;
+		# drop "#define FOO FOO" in enums
+		$next =~ s/^\s*#\s*define\s+(\w+)\s+\1\s*$//;
 		$_ .= $next;
 		print OUT "# $next\n" if $opt_D;
 	    }
+	    s/#\s*if.*?#\s*endif//g; # drop #ifdefs
 	    s@/\*.*?\*/@@g;
 	    s/\s+/ /g;
-	    /^\s?(typedef\s?)?enum\s?([a-zA-Z_]\w*)?\s?\{(.*)\}\s?([a-zA-Z_]\w*)?\s?;/;
+	    next unless /^\s?(typedef\s?)?enum\s?([a-zA-Z_]\w*)?\s?\{(.*)\}\s?([a-zA-Z_]\w*)?\s?;/;
 	    (my $enum_subs = $3) =~ s/\s//g;
 	    my @enum_subs = split(/,/, $enum_subs);
 	    my $enum_val = -1;
@@ -285,22 +307,22 @@ while (defined (my $file = next_file())) {
 	    }
 	}
     }
-    print OUT "1;\n";
-
     $Is_converted{$file} = 1;
+    if ($opt_e && exists($bad_file{$file})) {
+        unlink($Dest_dir . '/' . $outfile);
+        $next = '';
+    } else {
+        print OUT "1;\n";
     queue_includes_from($file) if ($opt_a);
+    }
 }
 
-exit $Exit;
-
-
-sub reindent($) {
-    my($text) = shift;
-    $text =~ s/\n/\n    /g;
-    $text =~ s/        /\t/g;
-    $text;
+if ($opt_e && (scalar(keys %bad_file) > 0)) {
+    warn "Was unable to convert the following files:\n";
+    warn "\t" . join("\n\t",sort(keys %bad_file)) . "\n";
 }
 
+exit $Exit;
 
 sub expr {
     my $joined_args;
@@ -311,8 +333,21 @@ sub expr {
 	s/^\&\&// && do { $new .= " &&"; next;}; # handle && operator
 	s/^\&([\(a-z\)]+)/$1/i;	# hack for things that take the address of
 	s/^(\s+)//		&& do {$new .= ' '; next;};
-	s/^(0X[0-9A-F]+)[UL]*//i	&& do {$new .= lc($1); next;};
-	s/^(-?\d+\.\d+E[-+]\d+)F?//i	&& do {$new .= $1; next;};
+	s/^0X([0-9A-F]+)[UL]*//i 
+	    && do {my $hex = $1;
+		   $hex =~ s/^0+//;
+		   if (length $hex > 8 && !$Config{use64bitint}) {
+		       # Croak if nv_preserves_uv_bits < 64 ?
+		       $new .=         hex(substr($hex, -8)) +
+			       2**32 * hex(substr($hex,  0, -8));
+		       # The above will produce "errorneus" code
+		       # if the hex constant was e.g. inside UINT64_C
+		       # macro, but then again, h2ph is an approximation.
+		   } else {
+		       $new .= lc("0x$hex");
+		   }
+		   next;};
+	s/^(-?\d+\.\d+E[-+]?\d+)[FL]?//i	&& do {$new .= $1; next;};
 	s/^(\d+)\s*[LU]*//i	&& do {$new .= $1; next;};
 	s/^("(\\"|[^"])*")//	&& do {$new .= $1; next;};
 	s/^'((\\"|[^"])*)'//	&& do {
@@ -351,7 +386,7 @@ sub expr {
 	# Eliminate typedefs
 	/\(([\w\s]+)[\*\s]*\)\s*[\w\(]/ && do {
 	    foreach (split /\s+/, $1) {  # Make sure all the words are types,
-		last unless ($isatype{$_} or $_ eq 'struct');
+		last unless ($isatype{$_} or $_ eq 'struct' or $_ eq 'union');
 	    }
 	    s/\([\w\s]+[\*\s]*\)// && next;      # then eliminate them.
 	};
@@ -374,7 +409,7 @@ sub expr {
 	};
 	s/^([_a-zA-Z]\w*)//	&& do {
 	    my $id = $1;
-	    if ($id eq 'struct') {
+	    if ($id eq 'struct' || $id eq 'union') {
 		s/^\s+(\w+)//;
 		$id .= ' ' . $1;
 		$isatype{$id} = 1;
@@ -387,8 +422,8 @@ sub expr {
 		$new .= '->' if /^[\[\{]/;
 	    } elsif ($id eq 'defined') {
 		$new .= 'defined';
-	    } elsif (/^\(/) {
-		s/^\((\w),/("$1",/ if $id =~ /^_IO[WR]*$/i;	# cheat
+	    } elsif (/^\s*\(/) {
+		s/^\s*\((\w),/("$1",/ if $id =~ /^_IO[WR]*$/i;	# cheat
 		$new .= " &$id";
 	    } elsif ($isatype{$id}) {
 		if ($new =~ /{\s*$/) {
@@ -401,7 +436,7 @@ sub expr {
 		}
 	    } else {
 		if ($inif && $new !~ /defined\s*\($/) {
-		    $new .= '(defined(&' . $id . ') ? &' . $id . ' : 0)';
+		    $new .= '(defined(&' . $id . ') ? &' . $id . ' : undef)';
 		} elsif (/^\[/) {
 		    $new .= " \$$id";
 		} else {
@@ -415,6 +450,101 @@ sub expr {
 }
 
 
+sub next_line
+{
+    my $file = shift;
+    my ($in, $out);
+    my $pre_sub_tri_graphs = 1;
+
+    READ: while (not eof IN) {
+        $in  .= <IN>;
+        chomp $in;
+        next unless length $in;
+
+        while (length $in) {
+            if ($pre_sub_tri_graphs) {
+                # Preprocess all tri-graphs 
+                # including things stuck in quoted string constants.
+                $in =~ s/\?\?=/#/g;                         # | ??=|  #|
+                $in =~ s/\?\?\!/|/g;                        # | ??!|  ||
+                $in =~ s/\?\?'/^/g;                         # | ??'|  ^|
+                $in =~ s/\?\?\(/[/g;                        # | ??(|  [|
+                $in =~ s/\?\?\)/]/g;                        # | ??)|  ]|
+                $in =~ s/\?\?\-/~/g;                        # | ??-|  ~|
+                $in =~ s/\?\?\//\\/g;                       # | ??/|  \|
+                $in =~ s/\?\?</{/g;                         # | ??<|  {|
+                $in =~ s/\?\?>/}/g;                         # | ??>|  }|
+            }
+	    if ($in =~ /^\#ifdef __LANGUAGE_PASCAL__/) {
+                # Tru64 disassembler.h evilness: mixed C and Pascal.
+		while (<IN>) {
+		    last if /^\#endif/; 
+		}
+		next READ;
+	    }
+	    if ($in =~ /^extern inline / && # Inlined assembler.
+		$^O eq 'linux' && $file =~ m!(?:^|/)asm/[^/]+\.h$!) {
+ 		while (<IN>) {
+		    last if /^}/; 
+		}
+		next READ;
+	    }
+            if ($in =~ s/\\$//) {                           # \-newline
+                $out    .= ' ';
+                next READ;
+            } elsif ($in =~ s/^([^"'\\\/]+)//) {            # Passthrough
+                $out    .= $1;
+            } elsif ($in =~ s/^(\\.)//) {                   # \...
+                $out    .= $1;
+            } elsif ($in =~ /^'/) {                         # '...
+                if ($in =~ s/^('(\\.|[^'\\])*')//) {
+                    $out    .= $1;
+                } else {
+                    next READ;
+                }
+            } elsif ($in =~ /^"/) {                         # "...
+                if ($in =~ s/^("(\\.|[^"\\])*")//) {
+                    $out    .= $1;
+                } else {
+                    next READ;
+                }
+            } elsif ($in =~ s/^\/\/.*//) {                  # //...
+                # fall through
+            } elsif ($in =~ m/^\/\*/) {                     # /*...
+                # C comment removal adapted from perlfaq6:
+                if ($in =~ s/^\/\*[^*]*\*+([^\/*][^*]*\*+)*\///) {
+                    $out    .= ' ';
+                } else {                                    # Incomplete /* */
+                    next READ;
+                }
+            } elsif ($in =~ s/^(\/)//) {                    # /...
+                $out    .= $1;
+            } elsif ($in =~ s/^([^\'\"\\\/]+)//) {
+                $out    .= $1;
+            } elsif ($^O eq 'linux' &&
+                     $file =~ m!(?:^|/)linux/byteorder/pdp_endian\.h$! &&
+                     $in   =~ s!\'T KNOW!!) {
+                $out    =~ s!I DON$!I_DO_NOT_KNOW!;
+            } else {
+                if ($opt_e) {
+                    warn "Cannot parse $file:\n$in\n";
+                    $bad_file{$file} = 1;
+                    $in = '';
+                    $out = undef;
+                    last READ;
+                } else {
+		die "Cannot parse:\n$in\n";
+                }
+            }
+        }
+
+        last READ if $out =~ /\S/;
+    }
+
+    return $out;
+}
+
+
 # Handle recursive subdirectories without getting a grotesquely big stack.
 # Could this be implemented using File::Find?
 sub next_file
@@ -514,8 +644,13 @@ sub queue_includes_from
                 $line .= <HEADER>;
             }
 
-            if ($line =~ /^#\s*include\s+<(.*?)>/) {
-                push(@ARGV, $1) unless $Is_converted{$1};
+            if ($line =~ /^#\s*include\s+([<"])(.*?)[>"]/) {
+                my ($delimiter, $new_file) = ($1, $2);
+                # copy the prefix in the quote syntax (#include "x.h") case
+                if ($delimiter eq q{"} && $file =~ m|^(.*)/|) {
+                    $new_file = "$1/$new_file";
+                }
+                push(@ARGV, $new_file) unless $Is_converted{$new_file};
             }
         }
     close HEADER;
@@ -556,25 +691,50 @@ sub build_preamble_if_necessary
     my (%define) = _extract_cc_defines();
 
     open  PREAMBLE, ">$preamble" or die "Cannot open $preamble:  $!";
-        print PREAMBLE "# This file was created by h2ph version $VERSION\n";
-
-        foreach (sort keys %define) {
-            if ($opt_D) {
-                print PREAMBLE "# $_=$define{$_}\n";
-            }
-
-            if ($define{$_} =~ /^\d+$/) {
-                print PREAMBLE
-                    "unless (defined &$_) { sub $_() { $define{$_} } }\n\n";
-            } elsif ($define{$_} =~ /^\w+$/) {
-                print PREAMBLE
-                    "unless (defined &$_) { sub $_() { &$define{$_} } }\n\n";
-            } else {
+	print PREAMBLE "# This file was created by h2ph version $VERSION\n";
+        # Prevent non-portable hex constants from warning.
+        #
+        # We still produce an overflow warning if we can't represent
+        # a hex constant as an integer.
+        print PREAMBLE "no warnings qw(portable);\n";
+
+	foreach (sort keys %define) {
+	    if ($opt_D) {
+		print PREAMBLE "# $_=$define{$_}\n";
+	    }
+	    if ($define{$_} =~ /^\((.*)\)$/) {
+		# parenthesized value:  d=(v)
+		$define{$_} = $1;
+	    }
+	    if ($define{$_} =~ /^([+-]?(\d+)?\.\d+([eE][+-]?\d+)?)[FL]?$/) {
+		# float:
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { $1 } }\n\n";
+	    } elsif ($define{$_} =~ /^([+-]?\d+)U?L{0,2}$/i) {
+		# integer:
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { $1 } }\n\n";
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
                 print PREAMBLE
-                    "unless (defined &$_) { sub $_() { \"",
-                    quotemeta($define{$_}), "\" } }\n\n";
-            }
-        }
+                    "unless (defined &$_) { sub $_() { $code } }\n\n";
+	    } elsif ($define{$_} =~ /^\w+$/) {
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { &$define{$_} } }\n\n";
+	    } else {
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { \"",
+		    quotemeta($define{$_}), "\" } }\n\n";
+	    }
+	}
     close PREAMBLE               or die "Cannot close $preamble:  $!";
 }
 
@@ -586,15 +746,14 @@ sub _extract_cc_defines
 {
     my %define;
     my $allsymbols  = join " ",
-        @Config{'ccsymbols', 'cppsymbols', 'cppccsymbols'};
+	@Config{'ccsymbols', 'cppsymbols', 'cppccsymbols'};
 
     # Split compiler pre-definitions into `key=value' pairs:
-    foreach (split /\s+/, $allsymbols) {
-        /(.+?)=(.+)/ and $define{$1} = $2;
-
-        if ($opt_D) {
-            print STDERR "$_:  $1 -> $2\n";
-        }
+    while ($allsymbols =~ /([^\s]+)=((\\\s|[^\s])+)/g) {
+	$define{$1} = $2;
+	if ($opt_D) {
+	    print STDERR "$_:  $1 -> $2\n";
+	}
     }
 
     return %define;
@@ -623,6 +782,10 @@ It is most easily run while in /usr/include:
 
 	cd /usr/include; h2ph * sys/*
 
+or
+
+	cd /usr/include; h2ph * sys/* arpa/* netinet/*
+
 or
 
 	cd /usr/include; h2ph -r -l .
@@ -640,7 +803,7 @@ If run with no arguments, filters standard input to standard output.
 =item -d destination_dir
 
 Put the resulting B<.ph> files beneath B<destination_dir>, instead of
-beneath the default Perl library location (C<$Config{'installsitsearch'}>).
+beneath the default Perl library location (C<$Config{'installsitearch'}>).
 
 =item -r
 
@@ -725,10 +888,10 @@ installation.
 Doesn't handle complicated expressions built piecemeal, a la:
 
     enum {
-        FIRST_VALUE,
-        SECOND_VALUE,
+	FIRST_VALUE,
+	SECOND_VALUE,
     #ifdef ABC
-        THIRD_VALUE
+	THIRD_VALUE
     #endif
     };
 
UH2PH562
  }
  if ( $num < 5.008009 ) {
    return _patch(<<'UH2PH588');
--- utils/h2ph.PL
+++ utils/h2ph.PL
@@ -84,7 +84,7 @@ sub reindent($) {
 }
 
 my ($t, $tab, %curargs, $new, $eval_index, $dir, $name, $args, $outfile);
-my ($incl, $incl_type, $next);
+my ($incl, $incl_type, $incl_quote, $next);
 while (defined (my $file = next_file())) {
     if (-l $file and -d $file) {
         link_if_possible($file) if ($opt_l);
@@ -184,9 +184,10 @@ while (defined (my $file = next_file())) {
                       print OUT $t,"unless(defined(\&$name)) {\n    sub $name () {\t",$new,";}\n}\n";
 		    }
 		}
-	    } elsif (/^(include|import|include_next)\s*[<\"](.*)[>\"]/) {
+	    } elsif (/^(include|import|include_next)\s*([<\"])(.*)[>\"]/) {
                 $incl_type = $1;
-                $incl = $2;
+                $incl_quote = $2;
+                $incl = $3;
                 if (($incl_type eq 'include_next') ||
                     ($opt_e && exists($bad_file{$incl}))) {
                     $incl =~ s/\.h$/.ph/;
@@ -219,6 +220,10 @@ while (defined (my $file = next_file())) {
 			   "warn(\$\@) if \$\@;\n");
                 } else {
                     $incl =~ s/\.h$/.ph/;
+                    # copy the prefix in the quote syntax (#include "x.h") case
+                    if ($incl !~ m|/| && $incl_quote eq q{"} && $file =~ m|^(.*)/|) {
+                        $incl = "$1/$incl";
+                    }
 		    print OUT $t,"require '$incl';\n";
                 }
 	    } elsif (/^ifdef\s+(\w+)/) {
@@ -431,7 +436,7 @@ sub expr {
 		}
 	    } else {
 		if ($inif && $new !~ /defined\s*\($/) {
-		    $new .= '(defined(&' . $id . ') ? &' . $id . ' : 0)';
+		    $new .= '(defined(&' . $id . ') ? &' . $id . ' : undef)';
 		} elsif (/^\[/) {
 		    $new .= " \$$id";
 		} else {
@@ -639,8 +644,13 @@ sub queue_includes_from
                 $line .= <HEADER>;
             }
 
-            if ($line =~ /^#\s*include\s+<(.*?)>/) {
-                push(@ARGV, $1) unless $Is_converted{$1};
+            if ($line =~ /^#\s*include\s+([<"])(.*?)[>"]/) {
+                my ($delimiter, $new_file) = ($1, $2);
+                # copy the prefix in the quote syntax (#include "x.h") case
+                if ($delimiter eq q{"} && $file =~ m|^(.*)/|) {
+                    $new_file = "$1/$new_file";
+                }
+                push(@ARGV, $new_file) unless $Is_converted{$new_file};
             }
         }
     close HEADER;
@@ -681,25 +691,50 @@ sub build_preamble_if_necessary
     my (%define) = _extract_cc_defines();
 
     open  PREAMBLE, ">$preamble" or die "Cannot open $preamble:  $!";
-        print PREAMBLE "# This file was created by h2ph version $VERSION\n";
-
-        foreach (sort keys %define) {
-            if ($opt_D) {
-                print PREAMBLE "# $_=$define{$_}\n";
-            }
-
-            if ($define{$_} =~ /^(\d+)U?L{0,2}$/i) {
-                print PREAMBLE
-                    "unless (defined &$_) { sub $_() { $1 } }\n\n";
-            } elsif ($define{$_} =~ /^\w+$/) {
-                print PREAMBLE
-                    "unless (defined &$_) { sub $_() { &$define{$_} } }\n\n";
-            } else {
+	print PREAMBLE "# This file was created by h2ph version $VERSION\n";
+        # Prevent non-portable hex constants from warning.
+        #
+        # We still produce an overflow warning if we can't represent
+        # a hex constant as an integer.
+        print PREAMBLE "no warnings qw(portable);\n";
+
+	foreach (sort keys %define) {
+	    if ($opt_D) {
+		print PREAMBLE "# $_=$define{$_}\n";
+	    }
+	    if ($define{$_} =~ /^\((.*)\)$/) {
+		# parenthesized value:  d=(v)
+		$define{$_} = $1;
+	    }
+	    if ($define{$_} =~ /^([+-]?(\d+)?\.\d+([eE][+-]?\d+)?)[FL]?$/) {
+		# float:
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { $1 } }\n\n";
+	    } elsif ($define{$_} =~ /^([+-]?\d+)U?L{0,2}$/i) {
+		# integer:
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { $1 } }\n\n";
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
                 print PREAMBLE
-                    "unless (defined &$_) { sub $_() { \"",
-                    quotemeta($define{$_}), "\" } }\n\n";
-            }
-        }
+                    "unless (defined &$_) { sub $_() { $code } }\n\n";
+	    } elsif ($define{$_} =~ /^\w+$/) {
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { &$define{$_} } }\n\n";
+	    } else {
+		print PREAMBLE
+		    "unless (defined &$_) { sub $_() { \"",
+		    quotemeta($define{$_}), "\" } }\n\n";
+	    }
+	}
     close PREAMBLE               or die "Cannot close $preamble:  $!";
 }
 
@@ -711,15 +746,14 @@ sub _extract_cc_defines
 {
     my %define;
     my $allsymbols  = join " ",
-        @Config{'ccsymbols', 'cppsymbols', 'cppccsymbols'};
+	@Config{'ccsymbols', 'cppsymbols', 'cppccsymbols'};
 
     # Split compiler pre-definitions into `key=value' pairs:
-    foreach (split /\s+/, $allsymbols) {
-        /(.+?)=(.+)/ and $define{$1} = $2;
-
-        if ($opt_D) {
-            print STDERR "$_:  $1 -> $2\n";
-        }
+    while ($allsymbols =~ /([^\s]+)=((\\\s|[^\s])+)/g) {
+	$define{$1} = $2;
+	if ($opt_D) {
+	    print STDERR "$_:  $1 -> $2\n";
+	}
     }
 
     return %define;
@@ -769,7 +803,7 @@ If run with no arguments, filters standard input to standard output.
 =item -d destination_dir
 
 Put the resulting B<.ph> files beneath B<destination_dir>, instead of
-beneath the default Perl library location (C<$Config{'installsitsearch'}>).
+beneath the default Perl library location (C<$Config{'installsitearch'}>).
 
 =item -r
 
@@ -854,10 +888,10 @@ installation.
 Doesn't handle complicated expressions built piecemeal, a la:
 
     enum {
-        FIRST_VALUE,
-        SECOND_VALUE,
+	FIRST_VALUE,
+	SECOND_VALUE,
     #ifdef ABC
-        THIRD_VALUE
+	THIRD_VALUE
     #endif
     };
 
UH2PH588
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

=head1 CAVEAT

Devel::PatchPerl is intended only to facilitate the C<building> of perls, not to
facilitate the C<testing> of perls. This means that it will not patch failing tests
in the perl testsuite.

=head1 SEE ALSO

L<Devel::PPPort>

L<Devel::PatchPerl::Plugin>

=cut
