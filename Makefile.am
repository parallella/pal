ACLOCAL_AMFLAGS = -I config/m4
AUTOMAKE_OPTIONS = foreign

if ENABLE_TESTS
    MAYBE_TESTS = tests
endif

if ENABLE_DOC
    MAYBE_DOC = doc
endif

SUBDIRS = src examples include $(MAYBE_TESTS) $(MAYBE_DOC)

EXTRA_DIST = config include bootstrap CHANGELOG.md CODING-STYLE.md \
             CONTRIBUTING.md LICENSE QUESTIONS.md README.md scripts

# Reports
.PHONY: reports report-code-size report-stack-usage
report-code-size:
	@mkdir -p reports
	$(SHELL) $(top_srcdir)/scripts/report-code-size.sh src \
	    > reports/code-size.txt
	@echo Code size report saved to reports/code-size.txt

report-stack-usage:
	@mkdir -p reports
	$(SHELL) $(top_srcdir)/scripts/report-stack-usage.sh src \
	    > reports/stack-usage.txt
	@echo Code size report saved to reports/stack-usage.txt


reports: report-code-size report-stack-usage

all-local: reports

clean-local:
	-rm -rf reports
	-find . -type f -name "*.su" -delete
