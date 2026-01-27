-module(sp_error_format).

-export([to_json/2]).

-include_lib("spectra/include/spectra.hrl").
-include_lib("spectra/include/spectra_internal.hrl").

%% @doc Convert a list of spectra errors to a FastAPI-style JSON response body
-spec to_json([#sp_error{}] | {atom(), term()}, module()) -> binary().
to_json(Errors, Module) when is_list(Errors) ->
    DetailList = [format_error(Error, Module) || Error <- Errors],
    iolist_to_binary(json:encode(#{~"detail" => DetailList}));
to_json({missing_header, FieldName}, _Module) ->
    %% Handle missing header error
    FieldNameBin = ensure_binary(FieldName),
    DetailList = [
        #{
            ~"loc" => [~"header", FieldNameBin],
            ~"msg" => ~"Required header is missing",
            ~"type" => ~"missing_data"
        }
    ],
    iolist_to_binary(json:encode(#{~"detail" => DetailList}));
to_json({missing_path_arg, FieldName}, _Module) ->
    %% Handle missing path arg error
    FieldNameBin = ensure_binary(FieldName),
    DetailList = [
        #{
            ~"loc" => [~"path", FieldNameBin],
            ~"msg" => ~"Required path parameter is missing",
            ~"type" => ~"missing_data"
        }
    ],
    iolist_to_binary(json:encode(#{~"detail" => DetailList}));
to_json(Error, _Module) ->
    %% Generic fallback for unknown error formats
    DetailList = [
        #{
            ~"loc" => [~"unknown"],
            ~"msg" => iolist_to_binary(io_lib:format("Error: ~p", [Error])),
            ~"type" => ~"unknown_error"
        }
    ],
    iolist_to_binary(json:encode(#{~"detail" => DetailList})).

%% @doc Format a single error into a map structure
-spec format_error(#sp_error{}, module()) -> map().
format_error(#sp_error{location = Location, type = Type, ctx = Ctx}, Module) ->
    BaseMap = #{
        ~"loc" => format_location(Location),
        ~"msg" => format_message(Type, Ctx),
        ~"type" => atom_to_binary(Type, utf8)
    },

    %% Add expected schema if available
    MapWithSchema =
        case get_expected_schema(Ctx, Module) of
            undefined -> BaseMap;
            Schema -> BaseMap#{~"expected_schema" => Schema}
        end,

    %% Add received value if available and safe to include
    case maps:get(value, Ctx, undefined) of
        undefined -> MapWithSchema;
        Value -> MapWithSchema#{~"received_value" => format_value(Value)}
    end.

%% @doc Convert location list to JSON array with "body" prefix
-spec format_location([string() | atom()]) -> [binary()].
format_location(Location) ->
    [~"body" | [format_location_part(Part) || Part <- Location]].

%% @doc Format a single location part
-spec format_location_part(string() | atom()) -> binary().
format_location_part(Part) when is_atom(Part) ->
    atom_to_binary(Part, utf8);
format_location_part(Part) when is_list(Part) ->
    iolist_to_binary(io_lib:format("~s", [Part])).

%% @doc Generate human-readable error message
-spec format_message(atom(), map()) -> binary().
format_message(type_mismatch, Ctx) ->
    case maps:get(message, Ctx, undefined) of
        undefined ->
            Value = maps:get(value, Ctx, undefined),
            ExpectedType = get_type_description(maps:get(type, Ctx, undefined)),
            ActualType = get_value_type(Value),
            iolist_to_binary(
                io_lib:format(
                    "Expected type ~s, got ~s value ~p",
                    [ExpectedType, ActualType, Value]
                )
            );
        Message when is_list(Message) ->
            list_to_binary(Message);
        Message when is_binary(Message) ->
            Message
    end;
format_message(missing_data, _Ctx) ->
    ~"Required field is missing";
format_message(no_match, Ctx) ->
    case maps:get(errors, Ctx, undefined) of
        undefined ->
            ~"Value did not match any of the expected types";
        Errors when is_list(Errors) ->
            TypeCount = length(Errors),
            iolist_to_binary(
                io_lib:format(
                    "Value did not match any of the ~p expected types",
                    [TypeCount]
                )
            )
    end;
format_message(not_matched_fields, Ctx) ->
    case maps:get(value, Ctx, undefined) of
        Value when is_map(Value) ->
            Fields = maps:keys(Value),
            FieldList = string:join([binary_to_list(F) || F <- Fields, is_binary(F)], ", "),
            iolist_to_binary(io_lib:format("Unexpected fields in object: ~s", [FieldList]));
        _ ->
            ~"Unexpected fields in object"
    end;
format_message(decode_error, Ctx) ->
    case maps:get(message, Ctx, undefined) of
        undefined ->
            case maps:get(err_reason, Ctx, undefined) of
                content_type_mismatch ->
                    Expected = maps:get(expected, Ctx, ~"unknown"),
                    Got = maps:get(got, Ctx, ~"unknown"),
                    iolist_to_binary(
                        io_lib:format(
                            "Content-Type mismatch: expected ~s, got ~s",
                            [Expected, Got]
                        )
                    );
                _ ->
                    ~"Failed to decode value"
            end;
        Message when is_list(Message) ->
            list_to_binary(Message);
        Message when is_binary(Message) ->
            Message
    end.

%% @doc Get type description from spectra type
-spec get_type_description(undefined | term()) -> binary().
get_type_description(undefined) ->
    ~"unknown";
get_type_description(#sp_simple_type{type = Type}) ->
    atom_to_binary(Type, utf8);
get_type_description(#sp_literal{value = Value}) ->
    iolist_to_binary(io_lib:format("literal ~p", [Value]));
get_type_description(#sp_union{types = Types}) ->
    iolist_to_binary(io_lib:format("union of ~p types", [length(Types)]));
get_type_description(_) ->
    ~"complex type".

%% @doc Get type description from a value
-spec get_value_type(term()) -> binary().
get_value_type(V) when is_integer(V) -> ~"integer";
get_value_type(V) when is_float(V) -> ~"float";
get_value_type(V) when is_binary(V) -> ~"binary";
get_value_type(V) when is_list(V) -> ~"list";
get_value_type(V) when is_map(V) -> ~"map";
get_value_type(V) when is_atom(V) -> ~"atom";
get_value_type(V) when is_boolean(V) -> ~"boolean";
get_value_type(_) -> ~"unknown".

%% @doc Safely get JSON schema for expected type
-spec get_expected_schema(map(), module()) -> undefined | term().
get_expected_schema(Ctx, Module) ->
    case maps:get(type, Ctx, undefined) of
        undefined ->
            undefined;
        Type ->
            try
                SchemaJson = spectra:schema(json_schema, Module, Type),
                SchemaBin = iolist_to_binary(SchemaJson),
                Decoded = json:decode(SchemaBin),
                case Decoded of
                    Map when is_map(Map) -> Map;
                    _ -> undefined
                end
            catch
                _:_ -> undefined
            end
    end.

%% @doc Safely format a value for output
-spec format_value(term()) -> term().
format_value(Value) when is_binary(Value) ->
    %% Truncate very long binaries
    case byte_size(Value) > 100 of
        true ->
            Truncated = binary:part(Value, 0, 100),
            <<Truncated/binary, "...">>;
        false ->
            Value
    end;
format_value(Value) when is_list(Value) ->
    %% Truncate very long lists
    case length(Value) > 10 of
        true ->
            Truncated = lists:sublist(Value, 10),
            Truncated ++ ['...'];
        false ->
            Value
    end;
format_value(Value) ->
    Value.

%% @doc Convert term to binary
-spec ensure_binary(term()) -> binary().
ensure_binary(Bin) when is_binary(Bin) ->
    Bin;
ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
ensure_binary(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).
