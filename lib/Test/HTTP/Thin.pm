use strict;
use warnings;
package Test::HTTP::Thin;
# ABSTRACT: A HTTP::Thin user agent suitable for simulating and testing network calls

use parent 'HTTP::Thin';

use Scalar::Util qw(blessed reftype);
use Storable 'freeze';
use HTTP::Request;
use HTTP::Response;
use URI;
use HTTP::Date;
use HTTP::Status qw(:constants status_message);
use Try::Tiny;
use Safe::Isa;
use Carp;
use Class::Method::Modifiers;

use namespace::clean 0.19 -also => [qw(__isa_coderef __is_regexp)];

my @response_map;
my $network_fallback;
my $last_useragent;

sub new
{
    my ($class, %options) = @_;

    my $_network_fallback = delete $options{network_fallback};

    my $self = $class->SUPER::new(%options);
    $self->{__last_http_request_sent} = undef;
    $self->{__last_http_response_received} = undef;
    $self->{__response_map} = [];
    $self->{__network_fallback} = $_network_fallback;

    # strips default User-Agent header added by LWP::UserAgent, to make it
    # easier to define literal HTTP::Requests to match against
    $self->agent(undef) if defined $self->agent and $self->agent eq $self->_agent;

    return $self;
}

sub map_response
{
    my ($self, $request_description, $response) = @_;

    if (not defined $response and blessed $self)
    {
        # mask a global domain mapping
        my $matched;
        foreach my $mapping (@{$self->{__response_map}})
        {
            if ($mapping->[0] eq $request_description)
            {
                $matched = 1;
                undef $mapping->[1];
            }
        }

        push @{$self->{__response_map}}, [ $request_description, undef ]
            if not $matched;

        return;
    }

    if (not $response->$_isa('HTTP::Response') and try { $response->can('request') })
    {
        my $oldres = $response;
        $response = sub { $oldres->request($_[0]) };
    }

    carp 'map_response: response is not a coderef or an HTTP::Response, it\'s a ',
            (blessed($response) || 'non-object')
        unless __isa_coderef($response) or $response->$_isa('HTTP::Response');

    if (blessed $self)
    {
        push @{$self->{__response_map}}, [ $request_description, $response ];
    }
    else
    {
        push @response_map, [ $request_description, $response ];
    }
    return $self;
}

sub map_network_response
{
    my ($self, $request_description) = @_;

    if (blessed $self)
    {
        # we cannot call ::request here, or we end up in an infinite loop
        push @{$self->{__response_map}},
            [ $request_description, sub { $self->SUPER::send_request($_[0]) } ];
    }
    else
    {
        push @response_map,
            [ $request_description, sub { LWP::UserAgent->new->send_request($_[0]) } ];
    }
    return $self;
}

sub unmap_all
{
    my ($self, $instance_only) = @_;

    if (blessed $self)
    {
        $self->{__response_map} = [];
        @response_map = () unless $instance_only;
    }
    else
    {
        carp 'instance-only unmap requests make no sense when called globally'
            if $instance_only;
        @response_map = ();
    }
    return $self;
}

sub register_psgi
{
    my ($self, $domain, $app) = @_;

    return $self->map_response($domain, undef) if not defined $app;

    carp 'register_psgi: app is not a coderef, it\'s a ', ref($app)
        unless __isa_coderef($app);

    carp 'register_psgi: did you forget to load HTTP::Message::PSGI?'
        unless HTTP::Request->can('to_psgi') and HTTP::Response->can('from_psgi');

    return $self->map_response(
        $domain,
        sub { HTTP::Response->from_psgi($app->($_[0]->to_psgi)) },
    );
}

sub unregister_psgi
{
    my ($self, $domain, $instance_only) = @_;

    if (blessed $self)
    {
        @{$self->{__response_map}} = grep { $_->[0] ne $domain } @{$self->{__response_map}};

        @response_map = grep { $_->[0] ne $domain } @response_map
            unless $instance_only;
    }
    else
    {
        @response_map = grep { $_->[0] ne $domain } @response_map;
    }
    return $self;
}

sub last_http_request_sent
{
    my $self = shift;
    return blessed($self)
        ? $self->{__last_http_request_sent}
        : $last_useragent
            ? $last_useragent->last_http_request_sent
            : undef;
}

sub last_http_response_received
{
    my $self = shift;
    return blessed($self)
        ? $self->{__last_http_response_received}
        : $last_useragent
            ? $last_useragent->last_http_response_received
            : undef;
}

sub last_useragent
{
    return $last_useragent;
}

sub network_fallback
{
    my ($self, $value) = @_;

    if (@_ == 1)
    {
        return blessed $self
            ? $self->{__network_fallback}
            : $network_fallback;
    }

    return $self->{__network_fallback} = $value if blessed $self;
    $network_fallback = $value;
}

sub request_args_to_request {
    return shift if @_ == 1;

    my( $method, $url, $options ) = @_;

    return HTTP::Request->new(
        $method, $url, HTTP::Headers->new( %{ $options->{headers} || {} } ), $options->{content} || undef 
    );

}

around 'request' => sub {
    my( $original, $self, @args ) = @_;

    my $request = $self->{__last_http_request_sent} = request_args_to_request(@args);

    my $resp = $self->{__last_http_response_received} = $original->($self, @args);

    $resp->request($request);

    return $resp;
};

sub _request {
    my ($self, $method, $uri, $args) = @_;

    my $request = eval {
        request_args_to_request($method,$uri,$args);
    };
    warn $@ if $@;

    my $matched_response;

    foreach my $entry (@{$self->{__response_map}}, @response_map)
    {
        last if $matched_response;
        next if not defined $entry;
        my ($request_desc, $response) = @$entry;

        if ($request_desc->$_isa('HTTP::Request'))
        {
            local $Storable::canonical = 1;
            $matched_response = $response, last
                if freeze($request) eq freeze($request_desc);
        }
        elsif (__is_regexp($request_desc))
        {
            $matched_response = $response, last
                if $uri =~ $request_desc;
        }
        elsif (__isa_coderef($request_desc))
        {
            $matched_response = $response, last
                if $request_desc->($request);
        }
        else
        {
            $uri = URI->new($uri) if not $uri->$_isa('URI');
            $matched_response = $response, last
                if $uri->host eq $request_desc;
        }
    }

    $last_useragent = $self;

    if (not defined $matched_response and
        ($self->{__network_fallback} or $network_fallback))
    {
        return $self->SUPER::_request($method,$uri,$args);
    }

    my $response = defined $matched_response
        ? $matched_response
        : HTTP::Response->new('404');

    if (__isa_coderef($response))
    {
        $response = $response->($request);
    }

    if (not $response->$_isa('HTTP::Response'))
    {
        carp 'response from coderef is not a HTTP::Response, it\'s a ',
            (blessed($response) || 'non-object');
        $response = LWP::UserAgent::_new_response($request, HTTP_INTERNAL_SERVER_ERROR, status_message(HTTP_INTERNAL_SERVER_ERROR));
    }
    else
    {
        $response->request($request);  # record request for reference
        $response->header('Client-Date' => HTTP::Date::time2str(time));
    }

    return http_response_to_hash($response);
}

sub __isa_coderef
{
    ref $_[0] eq 'CODE'
        or (reftype($_[0]) || '') eq 'CODE'
        or overload::Method($_[0], '&{}')
}

sub __is_regexp
{
    re->can('is_regexp') ? re::is_regexp(shift) : ref(shift) eq 'Regexp';
}

sub http_response_to_hash {
    my $resp = shift;

    return $resp unless ref $resp eq 'HTTP::Response';

    return {
        status => $resp->code,
        reason => $resp->message,
        headers => {
            map { $_ => $resp->header($_) } $resp->header_field_names
        },
        content => $resp->content,
    },
}

1;
__END__

=pod

=head1 SYNOPSIS

In your application code:

    use URI;
    use HTTP::Request::Common;
    use HTTP::Tiny;

    my $useragent = HTTPP::Tiny->new;

    my $response = $useragent->get( 'http://foo.com/bar' );

Then, in your tests:

    use Test::HTTP::Thin;
    use Test::More;

    my $useragent = Test::HTTP::Thin->new;
    $useragent->map_response(
        qr{example.com/success}, HTTP::Response->new('200', 'OK', ['Content-Type' => 'text/plain'], ''));
    $useragent->map_response(
        qr{example.com/fail}, HTTP::Response->new('500', 'ERROR', ['Content-Type' => 'text/plain'], ''));

    # now, do something that sends a request, and test how your application
    # responds to that response

=head1 DESCRIPTION

This module is a direct port of the excellent L<Test::LWP::UserAgent>,
but for L<HTTP::Thin>, which is itself a very thin wrapper around
L<HTTP::Tiny>.

Except for differences noted in this document, the API and behavior is
meant to be identical to L<Test::LWP::UserAgent>'s.

=head2 Requests and responses

Thanks to L<HTTP::Thin>, requests can either use the style typical of 
L<HTTP::Tiny>, or can take a L<HTTP::Request>. The requests and responses
are 
stored in C<last_http_request_sent> and C<last_http_response_received>
as L<HTTP::Request> and L<HTTP::Response> objects. The response provided
by C<request()> and the other helper methods is a L<HTTP:Response> object.


=head1 SEE ALSO

=over

=item L<Test::LWP::UserAgent>

=back

=cut
