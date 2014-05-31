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

@ISA       = qw(Exporter);
@EXPORT_OK = qw(patch_source);

my $patch_exe = _can_run('patch') || _can_run('gpatch');

my @patch = (
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
  print "patching $_\n" for $patch =~ /^\+{3}\s+(\S+)/gm;
  my $diff = 'tmp.diff';
  _write_or_die($diff, $patch);
  die "No patch utility found\n" unless $patch_exe;
  local $ENV{PATCH_GET} = 0; # I can't reproduce this at all, but meh.
  _run_or_die("$patch_exe -f -s -p0 <$diff");
  unlink $diff or die "unlink $diff: $!\n";
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

sub _norm_ver {
  my $ver = shift;
  my @v = split(qr/[._]0*/, $ver);
  $v[2] ||= 0;
  return sprintf '%d.%03d%03d', @v;
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
