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
use Test::RequiresInternet ( 'www.iana.org' => 80 );

use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::HTTP::Thin;
use HTTP::Request::Common;
use URI;

# I use POST rather than GET everywhere so as to not process the "302
# Redirect" response - there is no need, and the first response is much
# shorter than the second.
my $redirect_url = 'http://www.iana.org/domains/example/';

# allow HTTP::Thin to carp about unknown constructor arguments
$^W = 1;

local $TODO = "not quite ready yet";

{
    my $useragent = Test::HTTP::Thin->new;
    my $useragent2 = Test::HTTP::Thin->new;

    ok(!Test::HTTP::Thin->network_fallback, 'network_fallback not set globally');
    ok(!$useragent->network_fallback, 'network_fallback not enabled for the instance');
    ok(!$useragent2->network_fallback, 'network_fallback not enabled for the other instance');

    test_send_request('no mappings', $useragent, POST($redirect_url), '404');


    $useragent->network_fallback(1);
    ok($useragent->network_fallback, 'network_fallback enabled for the instance');

    test_send_request('network_fallback on instance', $useragent, POST($redirect_url), '302');
    test_send_request('no network_fallback on other instance', $useragent2, POST($redirect_url), '404');

    $useragent->network_fallback(0);
    ok(!$useragent->network_fallback, 'network_fallback disnabled for the instance');
    test_send_request('no network_fallback on instance', $useragent, POST($redirect_url), '404');
    test_send_request('no network_fallback on other instance', $useragent2, POST($redirect_url), '404');
}

{
    my $useragent = Test::HTTP::Thin->new;
    my $useragent2 = Test::HTTP::Thin->new;

    $useragent->network_fallback(1);
    ok($useragent->network_fallback, 'network_fallback enabled for the instance');

    Test::HTTP::Thin->network_fallback(1);
    ok(Test::HTTP::Thin->network_fallback, 'network_fallback set globally');
    ok($useragent->network_fallback, 'network_fallback enabled for the instance');
    ok($useragent->network_fallback, 'network_fallback enabled for the other instance');

    test_send_request('network_fallback on other instance', $useragent2, POST($redirect_url), '302');
    test_send_request('network_fallback, with redirect', $useragent2, GET($redirect_url), '200');

    Test::HTTP::Thin->network_fallback(0);
    ok($useragent->network_fallback, 'network_fallback still enabled for the instance');
    ok(!$useragent2->network_fallback, 'network_fallback not enabled for the other instance');

    test_send_request('network_fallback instance flag still remains', $useragent, POST($redirect_url), '302');
    test_send_request('global network_fallback clearable', $useragent2, POST($redirect_url), '404');
}

{
    my $useragent = Test::HTTP::Thin->new;
    my $useragent2 = Test::HTTP::Thin->new;

    my $host = URI->new($redirect_url)->host;
    $useragent->map_network_response($host);
    ok(!$useragent->network_fallback, 'network_fallback not enabled for the instance');
    test_send_request('network response mapped on instance', $useragent, POST($redirect_url), '302');
    test_send_request('network response not mapped on other instance', $useragent2, POST($redirect_url), '404');

    Test::HTTP::Thin->map_network_response($host);
    test_send_request('network response mapped globally', $useragent2, POST($redirect_url), '302');
    Test::HTTP::Thin->unmap_all;
}

{
    my $useragent = Test::HTTP::Thin->new(network_fallback => 1);
    my $useragent2 = Test::HTTP::Thin->new;

    ok(!Test::HTTP::Thin->network_fallback, 'network_fallback not set globally');
    ok($useragent->network_fallback, 'network_fallback enabled for the instance');
    ok(!$useragent2->network_fallback, 'network_fallback not enabled for the other instance');

    test_send_request('network_fallback on instance', $useragent, POST($redirect_url), '302');
    test_send_request('network_fallback on other instance', $useragent2, POST($redirect_url), '404');
}

sub test_send_request
{
    my ($name, $useragent, $request, $expected_code) = @_;

    note "\n$name";

    local $Test::Builder::Level = $Test::Builder::Level + 1;
    is($useragent->request($request)->code, $expected_code, $name);
}

done_testing;
