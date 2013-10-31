REBAR=./rebar

all: compile

clean:
	@$(REBAR) clean
	@rm -rf ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile
