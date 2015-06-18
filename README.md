RLTEST
======

This package implements a large subset of the tcltest::test command interface for use within AOLserver / NaviServer - the results are returned as a string rather than being printed to stdout.

It also provides some test helpers: stubbing of commands and methods, assertions about HTML and JSON documents.

License
-------

This package is licensed under the same terms as the Tcl core.

Assert checks
-------------

The `html_assert` proc supports a very flexible set of tests with 
the `-asserts` option, and a simplified but more limited set with `-checks`

checks format
-------------

The checks option is a list of `xpath expectation` pairs.  For each pair, 
the xpath is run against the document and the text value of every result 
must much match the expectation.  Every xpath must also match
some element to succeed, unless the expectation is a non-match.

The text value of an attribute is that attribute's value; while the
text value of a element is the concatenation of all text nodes underneath
that node. 

The expectation is normally an ordinary string glob (that is, matched with 
`string match`).  For particular use cases, several other syntaxes are
supported:
* `*` - existence
The matched path must exist.  It can be blank.
* `!` - nonexistence
The matched path must **not** exist.  A blank element still exists, and 
so will fail this check.

* `(` _words_ `)` - word check
The _words_ string is broken at whitespace, and each resulting word is 
checked to exist in the matched value as an indivudual word.  This 
is useful for checking for one or more html class values that may be in 
arbitrary order.  All words must exist.
* `!(` _words_ `)` - word negative check.
As word check, but all of the words **must not** exist.  Any one of the words
existing will cause the check to fail.  An element or attribute that does
not exist at all will implicitly match a negative check.
* `+` - nonblank
The matched value must be nonblank.  
* `!+` - blank
The matched value must be blank.  An element/attribute must exist and be
blank for this check to pass.
* `&....` - subst value
The expectation value is expanded with `subst`, with the result checked
for a match as an ordinary glob.  This is useful when you need to pull
in a variable to the check without being forced to list-ify the entire check
list.


Examples
--------

For the example html result

    <div id="d1">
	    <img alt="title" src="/foo.gif">
	    <img alt="title" title="title" src="/foo.gif">
		<span class="xs-3 sm-6">Caption</span>
		<span id="s2"></span>
	</div>

* `//div *` - matches (a div exists)
* `//div/@id *` - matches (a div with an id attribute exists)
* `//div/@class *` - fails (no div with a class attribute exists)
* `//img/@alt title` - matches (every img tag has "title" as its alt attribute)
* `//img/@title title` - matches (every img tag with a title attribute has "title" as that attribute).  Note that this does not verify that every img tag has
a title attribute.
* `//span +` - fails (a blank span is present)
* `//span/@class {(sm-6 xs-3)}` - matches (all of the words are in the given attribute, although they are not in the given order)
* `//span/@class "*s*" - matches, since the class attribute contains an "s"
* `//span/@class {(s)}" - doesn't match, since the "s" in the attribute is not a word by itself.

There is **no** support for matching strings that would otherwise match the 
magic syntax.  If that is needed, you will need to use the more general 
`-asserts` mechanism.

