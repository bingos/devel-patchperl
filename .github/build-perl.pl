#!/usr/bin/env perl
# Perl builder for GitHub Actions

use utf8;
use warnings;
use strict;
use 5.026000;
use CPAN::Perl::Releases::MetaCPAN;
use Devel::PatchPerl;
use Try::Tiny;
use File::pushd qw/pushd/;
use Carp qw/croak/;
use File::Temp qw/tempdir/;
use File::Spec;

local $| = 1;

sub perl_release {
    my $version = shift;
    my $releases = CPAN::Perl::Releases::MetaCPAN->new->get;
    for my $release (@$releases) {
        if ($release->{name} eq "perl-$version") {
            return $release->{download_url};
        }
    }
    die "not found the tarball for perl-$version\n";
}

sub group {
    my ($name, $sub) = @_;
    try {
        print "::group::$name\n";
        $sub->();
    } catch {
        die $_;
    } finally {
        print "::endgroup::\n";
    };
}

sub execute_or_die {
    my @cmd = @_;
    my $cmd = join ' ', @cmd;
    print "> $cmd\n";
    my $code = system(@cmd);
    if ($code != 0) {
        croak "failed to execute $cmd: exit code $code";
    }
}

sub run {
    my $version = shift;
    my $url = perl_release($version);
    $url =~ m/\/(perl-.*)$/;
    my $filename = $1;
    my $tmpdir = tempdir( CLEANUP => 1 );
    my $perldir = File::Spec->catdir($tmpdir, "perl-$version");

    group "downloading perl $version from $url" => sub {
        my $dir = pushd($tmpdir);
        execute_or_die("curl", "-sSL", "-o", $filename, $url);
        execute_or_die("tar", "xf", $filename);
    };
    group "patchperl" => sub {
        my $dir = pushd($perldir);
        execute_or_die("patchperl");
    };
    group "build" => sub {
        my $dir = pushd($perldir);
        execute_or_die("./Configure", "-des", "-Dusedevel");
        execute_or_die("make", "depend");
        execute_or_die("make");
    };
}

try {
    run($ARGV[0]);
} catch {
    print "::error::$_\n";
    exit 1;
};

1;
