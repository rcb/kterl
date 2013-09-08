DIALYZER = dialyzer
REBAR = ./rebar

all: app

app:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

tests: eunit

eunit:
	@$(REBAR) eunit skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .kterl_dialyzer.plt -I include \
			--apps erts kernel stdlib sasl inets crypto public_key ssl

# -Wunderspecs -Wrace_conditions
dialyze:
	@$(DIALYZER) --src src --verbose --plt .kterl_dialyzer.plt -I include \
			-Werror_handling -Wunmatched_returns

docs:
	priv/gen_docs.erl edoc
	@$(REBAR) doc

