if HAVE_DOXYGEN

# TODO: Add install targets. Generate man pages.

#directory = $(top_srcdir)/docs/man/man3/

#dist_man_MANS = $(directory)/man_page_1.3 $(directory)/man_page_2.3
#$(directory)/man_page_1.3: doxyfile.stamp
#    $(directory)/man_page_2.3: doxyfile.stamp

doxyfile.stamp:
	$(DOXYGEN) Doxyfile
	echo Timestamp > doxyfile.stamp

CLEANFILES = doxyfile.stamp

html-local: doxyfile.stamp
all-local: html-local

install-html-local:
	find html -type d -exec $(mkinstalldirs) $(DESTDIR)$(htmldir)/'{}' ';'
	find html -type f -exec $(INSTALL_DATA) '{}' $(DESTDIR)$(htmldir)/'{}' ';'

uninstall-local:
	rm -rf $(DESTDIR)$(htmldir)/html

install-data-local: install-html-local

clean-local:
	rm -rf html

endif
