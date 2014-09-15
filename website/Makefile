#include ../master.Makefile

#MYSITE=/gnu/mysite/mysite
MYSITE=/opt/mysite/mysite
#MYSITE=/local/mysite/mysite
RSS2CONTENT=/opt/godi/bin/cduce -I /opt/mysite --run rss.cdo --arg

DEST_DIR=$(ROOT)/../website/

XML_FILES=\
	about.xml \
	download.xml \
	latest.xml \
	news.xml \
	sidemenu.xml \
	site.xml \
	sitemap.xml

all: $(XML_FILES)
	$(MKDIR) $(DEST_DIR)
	$(MYSITE) site.xml -lang fr -o $(DEST_DIR)
	$(MYSITE) site.xml -lang en -o $(DEST_DIR)
	$(CP) news.rss $(DEST_DIR)
	cp style.css $(DEST_DIR)
	cp ../Changelog $(DEST_DIR)
	$(CP) $(DEST_DIR)/index.en.html $(DEST_DIR)/index.html
	rpl "&amp;&amp;" "&&" $(DEST_DIR)/*.html

news.xml: news.rss
	echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"yes\"?>" > $@
	$(RSS2CONTENT) $< >> $@

refdoc: dummy
	cd ../src && make doc
	cp -r ../src/ocamldoc/* $(DEST_DIR)/refdoc/

clean:
	$(RM) *~ \#*\#

dummy:
