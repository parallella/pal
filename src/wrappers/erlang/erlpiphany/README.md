The erlpiphany application directory contains source, examples and
precompiled binaries that implement a Natively Implemented Library
(NIF) interface to the Epiphany eHAL library. Files and directories
are as follows:

Makefile - Rebuilds the C loadable library and Erlang interface module
makedocs.script - Rebuilds the eDoc documentation

c_src/ - Source code for the NIF library
doc/   - Erlang eDoc documentation for Erlpiphany
ebin/  - Compiled Erlang BEAM files from the src directory
examples/ - Erlang program and script examples using Erlpiphany
include/  - An Erlang include file containing eHAL constants
priv/     - Where the NIF loadable library resides
src/      - Erlang source for the Erlpiphany module interface
test/     - Developer test files

The main documentation can be found on the doc/ directory, either as
PDF files or as eDoc generated HTML. To use Erlpiphany, you'll need
the latest Erlang package installed on your Parallella board. The
standard Ubuntu package can be installed using the command

sudo apt-get install erlang

The documentation provides a tour of the Erlpiphany application and
the supplied examples.