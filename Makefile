MAIN_FILE = src/Main.elm
OUTPUT_FILE = elm.js

.PHONY: production debug deploy

production:
	elm make --optimize $(MAIN_FILE) --output=$(OUTPUT_FILE)

debug:
	elm make --debug $(MAIN_FILE) --output=$(OUTPUT_FILE)

deploy:
	./scripts/deploy.sh