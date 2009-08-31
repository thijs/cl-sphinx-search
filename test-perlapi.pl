#!/bin/env perl

use strict;
use warnings;

use lib ".";

use Data::Dumper;
use Sphinx::Search;


my $sphinx = Sphinx::Search->new();

warn 'sphinx: ' . Dumper $sphinx;

$sphinx->AddQuery("first");

my $results = $sphinx->RunQueries;


warn 'results: ' . Dumper $results;

1;
