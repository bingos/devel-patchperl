#!/usr/bin/env perl

use warnings;
use strict;

use Text::Wrap qw(wrap fill $columns $huge);
use Getopt::Long;
use POSIX qw(strftime);

my $days_back  = 110365;   # Go back a year by default.
my $send_help  = 0;     # Display help and exit.
my $tag_pattern = "^\\d";

GetOptions(
	"age=s"      => \$days_back,
	"help"       => \$send_help,
	"tags=s"     => \$tag_pattern,
) or $send_help = 1;

# Find the trunk for the current repository if one isn't specified.

die(
	"$0 usage:\n",
	"  [--age DAYS]  limit report to DAYS in the past (default: 365)\n",
	"  [--tags REGEXP]  report on tags matching REGEXP (default: ^v\\d+_)\n",
) if $send_help;

my $earliest_date = strftime(
	"%FT %T +0000", gmtime(time() - $days_back * 86400)
);

$Text::Wrap::huge     = "wrap";
$Text::Wrap::columns  = 74;

chomp(my @tags = `git tag`);

{
	my $i = @tags;
	while ($i--) {
		unless ($tags[$i] =~ /$tag_pattern/o) {
			splice @tags, $i, 1;
			next;
		}

		my $commit = `git show $tags[$i] --pretty='tformat:(((((%ci)))))' | grep '(((((' | head -1`;
		die $commit unless $commit =~ /\(\(\(\(\((.+?)\)\)\)\)\)/;

		$tags[$i] = {
			'time' => $1,
			'tag'  => $tags[$i],
		};
	}
}

push @tags, { 'time' => '9999-99-99 99:99:99 +0000', 'tag' => 'HEAD' };

@tags = sort { $a->{'time'} cmp $b->{'time'} } @tags;

{
	my $i = @tags;
	while ($i--) {
		last if $tags[$i]{time} lt $earliest_date;

		my @commit;

		open my $commit, "-|", "git log $tags[$i-1]{tag}..$tags[$i]{tag} ." or die $!;
		local $/ = "\n\n";
		while (<$commit>) {
			if (/^\S/) {
				s/^/  /mg;
				push @commit, $_;
				next;
			}

			# Trim off identical leading whitespace.
			my ($whitespace) = /^(\s*)/;
			if (length $whitespace) {
				s/^$whitespace//mg;
			}

			# Re-flow the paragraph if it isn't indented from the norm.
			# This should preserve indented quoted text, wiki-style.
			unless (/^\s/) {
				push @commit, fill("    ", "    ", $_), "\n\n";
			}
			else {
				push @commit, $_;
			}
		}

		# Don't display the tag if there's nothing under it.
		next unless @commit;

		my $tag_line = "$tags[$i]{time} $tags[$i]{tag}";
		print(
			("=" x length($tag_line)), "\n",
			$tag_line, "\n",
			("=" x length($tag_line)), "\n",
			"\n",
		);

		print @commit;
	}
}

print(
	"==============\n",
	"End of Excerpt\n",
	"==============\n",
);
