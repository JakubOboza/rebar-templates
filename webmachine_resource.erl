%%% @author {{author_name}} <{{author_email}}>
%%% @copyright {{copyright_year}} {{author_name}}.
%%% @doc {{description}}

-module({{app_name}}_{{name}}_resource).
-author('{{author_name}} <{{author_email}}>').

% Exports
-export([init/1, resource_exists/2, is_authorized/2, forbidden/2]).
-export([malformed_request/2, allowed_methods/2, content_types_provided/2]).
-export([content_types_accepted/2, charsets_provided/2, encodings_provided/2]).
-export([previously_existed/2, moved_permanently/2, moved_temporarily/2]).
-export([last_modified/2, expires/2, generate_etag/2, finish_request/2]).
-export([service_available/2]).
-export([to_json/2, to_plain_text/2, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(DEFAULT_ENCODINGS, [{"identity", fun (X) -> X end}]).
-define(DEFAULT_CONTENT_TYPES, [
    {"application/json", to_json},
    {"text/plain", to_plain_text},
    {"text/html", to_html}
  ]).

-ifdef(TEST).
-compile(export_all).
-endif.


-spec init(InitArgs) -> {ok, Context} | {error, Reason}
  when
      InitArgs :: list(),
      Context :: any(),
      Reason :: term().
init([]) -> {ok, undefined}.

-spec resource_exists(ReqData, Context) -> 
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: boolean() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
resource_exists(ReqData, Context) ->
  {true, ReqData, Context}.

-spec service_available(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: boolean() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
service_available(ReqData, Context) ->
  {true, ReqData, Context}.

-spec is_authorized(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: boolean() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
is_authorized(ReqData, Context) ->
  {true, ReqData, Context}.

-spec forbidden(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: boolean() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
forbidden(ReqData, Context) ->
  {false, ReqData, Context}.

-spec malformed_request(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: boolean() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
malformed_request(ReqData, Context) ->
  {false, ReqData, Context}.

-spec allowed_methods(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: list(atom()) | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
allowed_methods(ReqData, Context) ->
  Methods = ['GET', 'POST', 'PUT', 'DELETE', 'HEAD', 'OPTIONS'],
  {[], ReqData, Context}.

-spec content_types_provided(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: [{MimeType :: string(), MimeTypeConverter :: function()}] |
        [] | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
content_types_provided(ReqData, Context) ->
  ContentTypes = ?DEFAULT_CONTENT_TYPES,
  {ContentTypes, ReqData, Context}.

-spec content_types_accepted(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: [{MimeType :: string(), MimeTypeConverter :: function()}] |
        [] | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
content_types_accepted(ReqData, Context) ->
  ContentTypes = ?DEFAULT_CONTENT_TYPES,
  {ContentTypes, ReqData, Context}.

-spec charsets_provided(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: [{Charset :: string(), CharsetConverter :: function()}] | 
        no_charset | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
charsets_provided(ReqData, Context) ->
  Charsets = [
  ],
  {Charsets, ReqData, Context}.

-spec encodings_provided(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: [{Charset :: string(), CharsetConverter :: function()}] | 
        no_charset | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
encodings_provided(ReqData, Context) ->
  Encodings = case wrq:method(ReqData) of
    'GET' ->
      [ {"gzip", fun (X) -> zlib:gzip(X) end} | ?DEFAULT_ENCODINGS ];
    _OtherMethod ->
      ?DEFAULT_ENCODINGS
  end,
  {Encodings, ReqData, Context}.

-spec previously_existed(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: boolean() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
previously_existed(ReqData, Context) ->
  {false, ReqData, Context}.

-spec moved_permanently(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: boolean() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
moved_permanently(ReqData, Context) ->
  {false, ReqData, Context}.

-spec moved_temporarily(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: boolean() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
moved_temporarily(ReqData, Context) ->
  {false, ReqData, Context}.

-spec last_modified(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Year :: integer(4),
      Month :: integer(2),
      Date :: integer(2),
      Hour :: integer(2),
      Minute :: integer(2),
      Second :: integer(2),
      Result :: undefined | { {Year, Month, Date}, {Hour, Minute, Second} } | 
        {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
last_modified(ReqData, Context) ->
  {undefined, ReqData, Context}.

-spec expires(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Year :: integer(4),
      Month :: integer(2),
      Date :: integer(2),
      Hour :: integer(2),
      Minute :: integer(2),
      Second :: integer(2),
      Result :: undefined | { {Year, Month, Date}, {Hour, Minute, Second} } | 
        {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
expires(ReqData, Context) ->
  {undefined, ReqData, Context}.

-spec generate_etag(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: undefined | string() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().
generate_etag(ReqData, Context) ->
  {undefined, ReqData, Context}.

-spec finish_request(ReqData, Context) ->
  {Result, NewReqData, NewContext}
  when
      ReqData :: #wm_reqdata{},
      Context :: any(),
      Result :: undefined | string() | {error, term()} | {halt, integer()},
      NewReqData :: #wm_reqdata{},
      NewContext :: any().

finish_request(ReqData, Context) ->
  {true, ReqData, Context}.

% format functions
to_html(ReqData, Context) ->
  {"<html><body>OK</body></html>", ReqData, Context}.

to_plain_text(ReqData, Context) ->
  {"OK", ReqData, Context}.

to_json(ReqData, Context) ->
  {"{'status':'OK'}", ReqData, Context}.

