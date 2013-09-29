
install-template:
	mkdir -p ~/.rebar/templates
	cp priv/templates/* ~/.rebar/templates

compile:
	./rebar compile

deps:
	./rebar update-deps

rel: compile
	./rebar generate

clean:
	./rebar clean

test:
	./rebar eunit apps=ulti_game

xref: compile
	./rebar xref apps=ulti_game


