package Devel::PatchPerl;

# ABSTRACT: Patch perl source a la Devel::PPort's buildperl.pl

use strict;
use warnings;
use File::pushd qw[pushd];
use File::Spec;
use IO::File;
use IPC::Cmd qw[can_run run];
use vars qw[@ISA @EXPORT_OK];

@ISA       = qw(Exporter);
@EXPORT_OK = qw(patch_source);

my $patch_exe = can_run('patch');

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
              [ \&_patch_nbsd_hints ],
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
);

sub patch_source {
  my $vers = shift;
  $vers = shift if eval { $vers->isa(__PACKAGE__) };
  my $source = shift || '.';
  if ( !$vers and $source eq '.' ) {
    $vers = _determine();
    if ( $vers ) {
      warn "Auto-guessed '$vers'\n";
    }
    else {
      die "You didn't provide a perl version and I don't appear to be in a perl source tree\n";
    }
  }
  $source = File::Spec->rel2abs($source);
  warn "No patch utility found\n" unless $patch_exe;
  {
    my $dir = pushd( $source );
    for my $p ( grep { _is( $_->{perl}, $vers ) } @patch ) {
       for my $s (@{$p->{subs}}) {
         my($sub, @args) = @$s;
         push @args, $vers unless scalar @args;
         $sub->(@args);
       }
    }
  }
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
  die unless scalar run( command => [ @_ ], verbose => 1 );
}

sub _determine {
  return unless -e 'patchlevel.h';
  my $version;
  {
    open my $fh, '<', 'patchlevel.h';
    my @vers;
    while (<$fh>) {
      chomp;
      next unless /^#define PERL_[RVS]/;
      push @vers, (split /\s+/)[2];
    }
    $version = join '.', @vers;
  }
  return $version;
}

sub _patch_db
{
  my $ver = shift;
  print "patching ext/DB_File/DB_File.xs\n";
  _run_or_die($^X, '-pi.bak', '-e', "s/<db.h>/<db$ver\\/db.h>/", 'ext/DB_File/DB_File.xs');
  unlink 'ext/DB_File/DB_File.xs.bak' if -e 'ext/DB_File/DB_File.xs.bak';
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

sub _patch_nbsd_hints {
return unless $^O eq 'netbsd';
chmod 0755, 'hints/netbsd.sh' or die "$!\n";
open my $fh, '>', 'hints/netbsd.sh' or die "$\n";
print $fh <<BADGER;
# hints/netbsd.sh
case "\$archname" in
'')
    archname=`uname -m`-\${osname}
    ;;
esac

case "\$osvers" in
0.9|0.8*)
	usedl="\$undef"
	;;
*)
	case `uname -m` in
	pmax)
		# NetBSD 1.3 and 1.3.1 on pmax shipped an `old' ld.so,
		# which will not work.
		case "\$osvers" in
		1.3|1.3.1)
			d_dlopen=\$undef
			;;
		esac
		;;
	esac
	if test -f /usr/libexec/ld.elf_so; then
		# ELF
		d_dlopen=\$define
		d_dlerror=\$define
		cccdlflags="-DPIC -fPIC \$cccdlflags"
		lddlflags="--whole-archive -shared \$lddlflags"
		rpathflag="-Wl,-rpath,"
		case "\$osvers" in
		1.[0-5]*)
			ccdlflags="-Wl,-whole-archive -lgcc \
				-Wl,-no-whole-archive -Wl,-E \$ccdlflags"
			;;
		*)
			ccdlflags="-Wl,-E \$ccdlflags"
			;;
		esac
	elif test -f /usr/libexec/ld.so; then
		# a.out
		d_dlopen=\$define
		d_dlerror=\$define
		cccdlflags="-DPIC -fPIC \$cccdlflags"
		lddlflags="-Bshareable \$lddlflags"
		rpathflag="-R"
	else
		d_dlopen=\$undef
		rpathflag=
	fi
	;;
esac

case "\$osvers" in
0.9*|1.[012]*|1.3|1.3.1)
	d_setregid="\$undef"
	d_setreuid="\$undef"
	;;
esac
case "\$osvers" in
0.9*|1.*|2.*|3.*|4.*|5.*)
	d_getprotoent_r="\$undef"
	d_getprotobyname_r="\$undef"
	d_getprotobynumber_r="\$undef"
	d_setprotoent_r="\$undef"
	d_endprotoent_r="\$undef"
	d_getservent_r="\$undef"
	d_getservbyname_r="\$undef"
	d_getservbyport_r="\$undef"
	d_setservent_r="\$undef"
	d_endservent_r="\$undef"
	d_getprotoent_r_proto="0"
	d_getprotobyname_r_proto="0"
	d_getprotobynumber_r_proto="0"
	d_setprotoent_r_proto="0"
	d_endprotoent_r_proto="0"
	d_getservent_r_proto="0"
	d_getservbyname_r_proto="0"
	d_getservbyport_r_proto="0"
	d_setservent_r_proto="0"
	d_endservent_r_proto="0"
	;;
esac

# These are obsolete in any netbsd.
d_setrgid="\$undef"
d_setruid="\$undef"

# there's no problem with vfork.
usevfork=true

# This is there but in machine/ieeefp_h.
ieeefp_h="define"

# This script UU/usethreads.cbu will get 'called-back' by Configure
# after it has prompted the user for whether to use threads.
cat > UU/usethreads.cbu <<'EOCBU'
case "\$usethreads" in
\$define|true|[yY]*)
	lpthread=
	for xxx in pthread; do
		for yyy in \$loclibpth \$plibpth \$glibpth dummy; do
			zzz=\$yyy/lib\$xxx.a
			if test -f "\$zzz"; then
				lpthread=\$xxx
				break;
			fi
			zzz=\$yyy/lib\$xxx.so
			if test -f "\$zzz"; then
				lpthread=\$xxx
				break;
			fi
			zzz=`ls \$yyy/lib\$xxx.so.* 2>/dev/null`
			if test "X\$zzz" != X; then
				lpthread=\$xxx
				break;
			fi
		done
		if test "X\$lpthread" != X; then
			break;
		fi
	done
	if test "X\$lpthread" != X; then
		# Add -lpthread.
		libswanted="\$libswanted \$lpthread"
		# There is no libc_r as of NetBSD 1.5.2, so no c -> c_r.
		# This will be revisited when NetBSD gains a native pthreads
		# implementation.
	else
		echo "\$0: No POSIX threads library (-lpthread) found.  " \
		     "You may want to install GNU pth.  Aborting." >&4
		exit 1
	fi
	unset lpthread

	# several reentrant functions are embeded in libc, but haven't
	# been added to the header files yet.  Let's hold off on using
	# them until they are a valid part of the API
	case "\$osvers" in
	[012].*|3.[0-1])
		d_getprotobyname_r=\$undef
		d_getprotobynumber_r=\$undef
		d_getprotoent_r=\$undef
		d_getservbyname_r=\$undef
		d_getservbyport_r=\$undef
		d_getservent_r=\$undef
		d_setprotoent_r=\$undef
		d_setservent_r=\$undef
		d_endprotoent_r=\$undef
		d_endservent_r=\$undef ;;
	esac
	;;

esac
EOCBU

# Set sensible defaults for NetBSD: look for local software in
# /usr/pkg (NetBSD Packages Collection) and in /usr/local.
#
loclibpth="/usr/pkg/lib /usr/local/lib"
locincpth="/usr/pkg/include /usr/local/include"
case "\$rpathflag" in
'')
	ldflags=
	;;
*)
	ldflags=
	for yyy in \$loclibpth; do
		ldflags="\$ldflags \$rpathflag\$yyy"
	done
	;;
esac

case `uname -m` in
alpha)
    echo 'int main() {}' > try.c
    gcc=`\${cc:-cc} -v -c try.c 2>&1|grep 'gcc version egcs-2'`
    case "\$gcc" in
    '' | "gcc version egcs-2.95."[3-9]*) ;; # 2.95.3 or better okay
    *)	cat >&4 <<EOF
***
*** Your gcc (\$gcc) is known to be
*** too buggy on netbsd/alpha to compile Perl with optimization.
*** It is suggested you install the lang/gcc package which should
*** have at least gcc 2.95.3 which should work okay: use for example
*** Configure -Dcc=/usr/pkg/gcc-2.95.3/bin/cc.  You could also
*** Configure -Doptimize=-O0 to compile Perl without any optimization
*** but that is not recommended.
***
EOF
	exit 1
	;;
    esac
    rm -f try.*
    ;;
esac

# NetBSD/sparc 1.5.3/1.6.1 dumps core in the semid_ds test of Configure.
case `uname -m` in
sparc) d_semctl_semid_ds=undef ;;
esac
BADGER
close $fh;
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


qq[patchin'];

=pod

=head1 SYNOPSIS

  use strict;
  use warnings;

  use Devel::PatchPerl;

  Devel::PatchPerl->patch_source( '5.6.1', '/path/to/untarred/perl/source/perl-5.6.1' );

=head1 DESCRIPTION

Devel::PatchPerl is a modularisation of the patching code contained in L<Devel::PPort>'s 
C<buildperl.pl>.

It does not build perls, it merely provides an interface to the source patching 
functionality.

=head1 FUNCTION

=over

=item C<patch_source>

Takes two parameters, a C<perl> version and the path to unwrapped perl source for that version.
It dies on any errors.

If you don't supply either a C<perl> version and the path to unwrapped perl source, it will assume
the current working directory and attempt to auto-determine the C<perl> version.

=back

=head1 SEE ALSO

L<Devel::PPPort>

=cut

