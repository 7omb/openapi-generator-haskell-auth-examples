.PHONY: openapi all noauth apikey basic bearer custom-monad apikey-custom-monad clean

# NOTE: change to source of openapi-generator
GENERATOR_SOURCE := /home/tom/git/openapi-generator

GENERATOR_JAR := $(GENERATOR_SOURCE)/modules/openapi-generator-cli/target/openapi-generator-cli.jar

# NOTE: Useful debug flags:
# --global-property debugSupportingFiles=true
# --global-property debugOpenAPI=true
# --global-property debugModels=true
GENERATOR_OPTS := -p disallowAdditionalPropertiesIfNotPresent=false -g haskell --global-property debugSupportingFiles=true

all:
	$(MAKE) openapi
	$(MAKE) noauth
	$(MAKE) apikey
	$(MAKE) basic
	$(MAKE) bearer
	$(MAKE) custom-monad
	$(MAKE) apikey-custom-monad

openapi:
	(cd $(GENERATOR_SOURCE) && ./mvnw package)

noauth:
	java -jar $(GENERATOR_JAR) generate -i noauth.yaml -o noauth $(GENERATOR_OPTS)

apikey:
	java -jar $(GENERATOR_JAR) generate -i apikey.yaml -o apikey $(GENERATOR_OPTS)

basic:
	java -jar $(GENERATOR_JAR) generate -i basic.yaml -o basic $(GENERATOR_OPTS)

bearer:
	java -jar $(GENERATOR_JAR) generate -i bearer.yaml -o bearer $(GENERATOR_OPTS)

custom-monad:
	java -jar $(GENERATOR_JAR) generate -i custom-monad.yaml -o custom-monad  -p useCustomMonad=true $(GENERATOR_OPTS)

apikey-custom-monad:
	java -jar $(GENERATOR_JAR) generate -i apikey-custom-monad.yaml -o apikey-custom-monad  -p useCustomMonad=true $(GENERATOR_OPTS)

clean:
	rm -rf noauth apikey basic bearer custom-monad apikey-custom-monad
