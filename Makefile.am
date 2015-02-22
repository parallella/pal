ACLOCAL_AMFLAGS = -I config/m4
AUTOMAKE_OPTIONS = foreign

if ENABLE_TESTS
    MAYBE_TESTS = tests
endif

SUBDIRS = src examples include $(MAYBE_TESTS)

EXTRA_DIST = config include bootstrap CHANGELOG.md CODING-STYLE.md \
             CONTRIBUTING.md LICENSE QUESTIONS.md README.md
