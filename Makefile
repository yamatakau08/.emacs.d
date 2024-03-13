.PHONY: all clean-all clean-elpa clean-straight clean-custom clean-consult

all: ;

clean-custom:
	rm -f custom.el

clean-elpa:
	rm -rf elpa

clean-straight:
	rm -rf straight

clean-consult:
	rm -rf elpa/consult-*
	rm -rf straight/repos/consult
	rm -rf straight/build/consult
	rm -rf straight/links/consult

clean-transient:
	rm -rf transient

clean-quickstart:
	rm -rf package-quickstart.*

clean-all: clean-custom clean-elpa clean-straight clean-transient clean-quickstart;
