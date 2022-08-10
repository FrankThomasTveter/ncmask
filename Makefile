#! gmake
#
# NOTE: make sure you are using GNU's "make-3.75.tar.gz" or older...
#
#
# A list of the files or directories that should NOT be 'made'
 ORDER = libsim libsort libpoly libncf libshape libparse
 STATIC = .git ccc  cloud2ascii  clouds  Makefile  MakeInclude  pcloud  ppcloud  vplot LICENSE README.md Makefile~ work old ice
#
include ./MakeInclude
#
