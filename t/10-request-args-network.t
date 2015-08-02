use strict;
use warnings;

use Test::More;

BEGIN {
    if ($ENV{NO_NETWORK_TESTING} and not $ENV{RELEASE_TESTING}
        or (not $ENV{AUTHOR_TESTING} and not $ENV{AUTOMATED_TESTING} and not $ENV{EXTENDED_TESTING})
    )
    {
        plan skip_all => 'these tests use the network: unset NO_NETWORK_TESTING and set EXTENDED_TESTING, AUTHOR_TESTING or AUTOMATED_TESTING to run';
    }
}

# if tests are getting to this point and then skip due to not being able to
# reach this site, we know they are not setting NO_NETWORK_TESTING as they should.
use Test::RequiresInternet ( 'cpan.org' => 80 );

use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::Deep;
use Path::Tiny;
use Test::LWP::UserAgent;

# the root problem here was that we were not passing along additional
# arguments to request() to the superclass, e.g. the option to save the
# content to a file.

my $useragent = Test::LWP::UserAgent->new(network_fallback => 1);
my $response = $useragent->get('http://cpan.org/');
my $expected_content = $response->decoded_content;

{
    # network_fallback case

    my $tmpfile = Path::Tiny->tempfile;

    my $response = $useragent->get(
        'http://cpan.org/',
        ':content_file' => $tmpfile->stringify,
    );

    my $contents = $tmpfile->slurp_utf8;
    is($contents, $expected_content, 'response body is saved to file (network responses)');

    is($response->content, '', 'response body is removed');
    cmp_deeply(
        $response,
        methods(
            [ header => 'X-Died' ] => undef,
            [ header => 'Content-Type' ], => re(qr{^text/html}),
            [ header => 'Client-Date' ] => ignore,
        ),
        'response headers look ok',
    );
}

done_testing;
