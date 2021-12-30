
.PHONY: test-lts-3 

# Test oldest known working stack LTS
test-lts-3:
	stack-1.9.3 --stack-yaml=stack-lts-3.yaml test --fast \
		--flag hup:EnableWebTests --flag hup:BuildStackBasedTests

README.md:

%.md: %.pmd
	pweave --format=markdown $<

clean:
		-rm -f README.md

