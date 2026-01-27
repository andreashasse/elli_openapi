.PHONY: all compile format test cover clean doc hank format_verify build-test dialyzer xref type_check check_app_calls hex

all: compile format test cover

compile:
	rebar3 compile

format:
	rebar3 fmt

hank:
	rebar3 hank

format_verify:
	rebar3 fmt --check

test:
	rebar3 eunit
	rebar3 ct

cover:
	rebar3 cover

build-test: compile xref type_check test hank check_app_calls format_verify cover dialyzer

dialyzer:
	@rebar3 dialyzer || \
	(grep -q "elli_tcp.erl" _build/default/28.2.dialyzer_warnings && \
	 ! grep -q "sp_error_format.erl" _build/default/28.2.dialyzer_warnings && \
	 ! grep -q "elli_openapi.erl" _build/default/28.2.dialyzer_warnings)

clean:
	rebar3 clean

xref:
	rebar3 xref

type_check:
	@output=$$(elp eqwalize-all); \
	echo "$$output"; \
	if ! echo "$$output" | grep -q "NO ERRORS"; then \
		exit 1; \
	else \
		exit 0; \
	fi

check_app_calls:
	rebar3 check_app_calls

doc:
	rebar3 ex_doc

hex:
	rebar3 hex build
	rebar3 hex publish
