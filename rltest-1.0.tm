# Test framework in the spirit of tcltest

package require Tcl 8.6
package require parse_args

namespace eval ::rltest {
	namespace export *
	namespace ensemble create -prefixes 0

	proc _run_if_set script { #<<<
		if {$script eq ""} return
		uplevel 2 $script
	}

	#>>>
	proc _match {mode a b} { #<<<
		switch -- $mode {
			exact   { expr {$a eq $b} }
			glob    { string match $a $b }
			regexp  { regexp $a $b }
			default { error "Invalid match type \"$mode\", should be one of exact, glob, regexp" }
		}
	}

	#>>>
	proc _intersect3 {list1 list2} { #<<<
		set firstonly		{}
		set intersection	{}
		set lastonly		{}

		set list1	[lsort -unique $list1]
		set list2	[lsort -unique $list2]

		array set b [join [lmap e $list2 {list $e 1}]]

		foreach e $list1 {
			if {[info exists b($e)]} {
				lappend intersection $e
				unset b($e)
			} else {
				lappend firstonly $e
			}
		}

		list $firstonly $intersection [lsort [array names b]]
	}

	#>>>
	proc test {name desc args} { #<<<
		global _rl_test_config _rl_test_stats errorInfo errorCode

		parse_args::parse_args $args {
			-setup			{-default {}}
			-body			{-default {}}
			-cleanup		{-default {}}
			-match			{-default exact}
			-returnCodes	{ok return}
		} _
		array set opts $_

		set normalized_codes [lmap e $opts(returnCodes) {
			switch -- $e {
				ok       {list 0}
				error    {list 1}
				return   {list 2}
				break    {list 3}
				continue {list 4}
				default  {set e}
			}
		}]

		if {![string match $_rl_test_config(match) $name]} {
			incr _rl_test_stats(skipped)
			return
		}
		incr _rl_test_stats(run)

		_run_if_set $opts(setup)
		if {[info exists errorInfo]} {set errorInfo	""}
		if {[info exists errorCode]} {set errorCode	""}
		set code [catch {uplevel 1 $opts(body)} res]
		if {[info exists errorInfo]} {set errorinfo $errorInfo} else {set errorinfo ""}
		if {[info exists errorCode]} {set errorcode $errorCode} else {set errorcode ""}
		_run_if_set $opts(cleanup)

		set passes		1
		set messages	""

		if {$passes && $code ni $normalized_codes} {
			set passes	0
			switch -- $code {
				0 {append messages "---- Test completed normally"}
				1 {append messages "---- Test generated error"}
				2 {append messages "---- Test generated return exception"}
				3 {append messages "---- Test generated break exception"}
				4 {append messages "---- Test generated continue exception"}
				default {append messages "---- Test generated exception"}
			}
			append messages "; Return code was $code\n"
			append messages "---- Return code should have been one of: $normalized_codes\n"
			if {$code == 1} {
				append messages "---- errorInfo: $errorinfo\n"
				append messages "---- errorCode: $errorcode\n"
			}
		}

		if {
			$passes && $code == 1 &&
			[info exists opts(errorCode)] &&
			![_match $opts(match) $opts(errorCode) $errorcode]
		} {
			set passes	0
			append messages "---- errorCode was:\n$errorcode\n"
			append messages "---- errorCode should have been: ($opts(match) matching):\n$opts(errorCode)\n"
		}

		if {
			$passes && $code == 1 &&
			[info exists opts(errorInfo)] &&
			![_match $opts(match) $opts(errorInfo) $errorinfo]
		} {
			set passes	0
			append messages "---- errorInfo was:\n$errorinfo\n"
			append messages "---- errorInfo should have been: ($opts(match) matching):\n$opts(errorInfo)\n"
		}

		if {$passes && [info exists opts(result)]} {
			if {![_match $opts(match) $opts(result) $res]} {
				set passes 0
				append messages "---- Result was:\n$res\n"
				append messages "---- Result should have been: ($opts(match) matching):\n$opts(result)\n"
			}
		}

		if {$passes} {
			incr _rl_test_stats(passed)
			append _rl_test_stats(messages) "PASSED: $name ($desc)\n"
		} else {
			incr _rl_test_stats(failed)
			lappend _rl_test_stats(failing_tests)	$name
			append _rl_test_stats(results) "\n\n==== $name $desc FAILED\n"
			append _rl_test_stats(results) "==== Contents of test case:\n$opts(body)\n"
			append _rl_test_stats(results) $messages
			append _rl_test_stats(results) "==== $name FAILED\n\n"
		}
	}

	#>>>
	proc rl_runAllTests args { #<<<
		global _rl_test_stats _rl_test_config

		parse_args::parse_args $args {
			-match		{-default *}
			-verbose	{-default 0}
		} _
		array set _rl_test_config $args

		array set _rl_test_stats {
			skipped			0
			run				0
			passed			0
			failed			0
			failing_tests	{}
			results			""
			messages		""
		}

		set output	"Tests began at [clock format [clock seconds]]\n"
		set fresh_stats	[array get _rl_test_stats]

		set filecount	0
		foreach fn [glob -nocomplain tests/*.test] {
			incr filecount
			# tcltest prints out each file it sources, but that is hurting usability in this case
			#append output	[file tail $fn] \n
			try {
				uplevel #0 [list source $fn]
			} on error {errmsg options} {
				append output "Test file \"$fn\" threw error:\n[dict get $options -errorinfo]\n"
			}
		}

		if {$_rl_test_config(verbose)} {
			append output \n $_rl_test_stats(messages) \n
		}

		append output $_rl_test_stats(results)
		append output "Tests ended at [clock format [clock seconds]]\n"
		append output "\tTotal\t$_rl_test_stats(run)"
		append output "\tPassed\t$_rl_test_stats(passed)"
		append output "\tSkipped\t$_rl_test_stats(skipped)"
		append output "\tFailed\t$_rl_test_stats(failed)\n"
		if {$_rl_test_stats(failing_tests) ne ""} {
			append output "Failing tests:\n\t[join $_rl_test_stats(failing_tests) \n\t]\n"
		}
		set output
	}

	#>>>

	variable stubseq	0

	namespace eval stubbed {}

	proc stub_command cmd { #<<<
		set fqcmd	[uplevel 1 [list namespace origin $cmd]]
		set key		[binary encode base64 [encoding convertto utf-8 $fqcmd]]
		rename $fqcmd ::rltest::stubbed::$key
		return ::rltest::stubbed::$key
	}

	#>>>
	proc unstub_command key { #<<<
		set fqcmd	[encoding convertfrom utf-8 [binary decode base64 [namespace tail $key]]]
		catch {rename $fqcmd {}}
		rename $key $fqcmd
	}

	#>>>
	proc stub_commands {commands script} { #<<<
		set stubbed	{}
		try {
			foreach {command arglist body} $commands {
				set fqcommand	[uplevel 1 [list namespace origin $command]]
				lappend stubbed	[uplevel 1 [list rltest stub_command $command]]
				proc $fqcommand $arglist $body
			}
			uplevel 1 $script
		} finally {
			foreach key $stubbed {
				rltest unstub_command $key
			}
		}
	}

	#>>>
	proc stub_methods {class methods script} { #<<<
		variable stubseq

		set fqclass	[uplevel 1 [list namespace origin $class]]
		set mixin	_rl_test_stubber_[incr stubseq]

		set methodmap	{}
		foreach {target arglist body} $methods {
			lassign $target tclass method
			set fqtclass	[uplevel 1 [list namespace origin $tclass]]
			dict set methodmap [list $fqtclass $method] [list $arglist $body]
		}

		oo::class create $mixin [format {
			filter _stubber

			variable _stubber_methods

			constructor args {
				set _stubber_methods	%s
				if {[self next] ne ""} {next {*}$args}
			}

			method _stubber args {
				set ns		[info object namespace [self]]
				set target	[self target]
				if {[dict exists $_stubber_methods $target]} {
					return [apply [list {*}[dict get $_stubber_methods $target] $ns] {*}$args]
				}
				next {*}$args
			}
		} [list $methodmap]]

		set old	[info class mixins $fqclass]
		oo::define $fqclass mixin -append $mixin
		try {
			uplevel 1 $script
		} finally {
			oo::define $fqclass mixin -set {*}$old
		}
	}

	#>>>

	# Compare two JSON values, ignoring non-semantic elements (optional
	# whitespace, object key ordering, etc)
	proc _compare_json {opts j1 j2 {path {}}} { #<<<
		set mismatch {
			msg {
				upvar 1 path path
				if {[llength $path] != 0} {
					append msg ", at path $path"
				}
				throw {RL TEST JSON_MISMATCH} $msg
			}
		}

		try {
			json get $j1 ?type
		} on error errmsg {
			apply $mismatch "Cannot parse left JSON value \"$j1\":\n$errmsg"
		} on ok j1_type {}

		try {
			json get $j2 ?type
		} on error errmsg {
			apply $mismatch "Cannot parse right JSON value \"$j2\":\n$errmsg"
		} on ok j2_type {}

		set j1_val	[json get $j1]
		set j2_val	[json get $j2]

		if {$j2_type eq "string" && [regexp {^\?([A-Z]):(.*)$} $j2_val - cmp_type cmp_args]} {
			# Custom matching <<<
			set escape	0
			switch -- $cmp_type {
				D { # Compare dates <<<
					if {$j1_type ne "string"} {
						apply $mismatch "left value isn't a date: $j1, expecting a string, got $j1_type"
					}

					if {[regexp {^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z?$} $j1_val]} {
						set j1_val	[string map {T " "} $j1_val]
					}
					set now		[clock seconds]
					try {
						set scanned	[clock scan $j1_val]
					} on error {errmsg options} {
						apply $mismatch "Can't interpret left date \"$j1_val\""
					}

					switch -regexp -matchvar m -- $cmp_args {
						today {
							if {[clock format $scanned -format %Y-%m-%d] ne [clock format $now -format %Y-%m-%d]} {
								apply $mismatch "left date \"$j1_val\" is not today"
							}
						}

						{^within ([0-9]+) (second|minute|hour|day|week|month|year)s?$} {
							lassign $m - offset unit
							if {![tcl::mathop::<= [clock add $now -$offset $unit] $scanned [clock add $now $offset $unit]]} {
								apply $mismatch "left date \"$j1_val\" is not within $offset $unit[s? $offset] of the current time"
							}
						}

						default {
							error "Invalid date comparison syntax: \"$cmp_args\""
						}
					}
					#>>>
				}

				G { # Glob match <<<
					if {$j1_type ne "string"} {
						apply $mismatch "left value isn't a string: $j1"
					}
					if {![string match $cmp_args $j1_val]} {
						apply $mismatch "left value doesn't match glob: \"$cmp_args\""
					}
					#>>>
				}

				R { # Regex match <<<
					if {$j1_type ne "string"} {
						apply $mismatch "left value isn't a string: $j1"
					}
					if {![regexp $cmp_args $j1_val]} {
						apply $mismatch "left value doesn't match regex: \"$cmp_args\""
					}
					#>>>
				}

				L { # Literal value <<<
					set j2_val	$cmp_args
					set escape	1
					#>>>
				}

				default {
					error "Invalid custom comparison type \"$cmp_type\""
				}
			}
			if {!$escape} {
				return
			}
			# Custom matching >>>
		}

		if {$j1_type ne $j2_type} {
			apply $mismatch "JSON value types differ: left $j1_type != right $j2_type"
		}

		switch -- $j1_type {
			object {
				# Two JSON objects are considered to match if they have the same
				# keys (regardless of order), and the values stored in those keys
				# match according to this function
				if {[dict get $opts -subset] eq "none" && [json get $j1 ?size] != [json get $j2 ?size]} {
					apply $mismatch "Object keys differ: left ([json get $j1 ?keys]) vs. right ([json get $j2 ?keys])"
				}

				lassign [_intersect3 [json get $j1 ?keys] [json get $j2 ?keys]] \
					j1_only both j2_only

				switch -glob -- [llength $j1_only],[llength $j2_only] {
					0,0 {}
					*,0 {if {[dict get $opts -subset] ni {right intersection}} {apply $mismatch "Left object has extra keys: $j1_only"}}
					0,* {if {[dict get $opts -subset] ni {left intersection}} {apply $mismatch "Right object has extra keys: $j2_only"}}
					*,* {if {[dict get $opts -subset] ne "intersection"} {apply $mismatch "Left object has extra keys: [list $j1_only] and right object has extra keys: [list $j2_only]"}}
				}

				foreach key $both {
					_compare_json $opts [json extract $j1 $key] [json extract $j2 $key] [list {*}$path $key]
				}
			}

			array {
				# Two JSON arrays are considered to match if they have the same
				# number of elements, and each element (in order) is considered to
				# match by this function
				if {[json get $j1 ?length] != [json get $j2 ?length]} {
					apply $mismatch "Arrays are different length: left [json get $j1 ?length] vs. right [json get $j2 ?length]"
				}
				set idx	-1
				json foreach e1 $j1 e2 $j2 {
					incr idx
					_compare_json $opts $e1 $e2 $idx
				}
			}

			string    { if {[json get $j1] ne [json get $j2]} {apply $mismatch "Strings differ: left: \"[json get $j1]\" vs. right: \"[json get $j2]\""} }
			number    { if {[json get $j1] == [json get $j2]} {apply $mismatch "Numbers differ: left: [json extract $j1] vs. right: [json extract $j2]"} }
			boolean   { if {[json get $j1] == [json get $j2]} {apply $mismatch "Booleans differ: left: [json extract $j1] vs. right: [json extract $j2]"} }
			null      { }

			default {
				error "Unsupported JSON type for compare: \"$j1_type\""
			}
		}
	}

	#>>>
	proc compare_json args { #<<<
		package require rl_json

		parse_args::parse_args $args {
			-subset	{-default none -name "-subset"}
			j1		{}
			j2		{}
		} opts

		try {
			_compare_json $opts [dict get $opts j1] [dict get $opts j2]
		} trap {RL TEST JSON_MISMATCH} {errmsg options} {
			return $errmsg
		} on ok {} {
			return match
		}
	}

	#>>>

	# Parse a fragment of HTML and test a list of assertions against it.
	proc html_assert args { #<<<
		package require tdom

		parse_args::parse_args $args {
			-match		{-default exact}
			-matchargs	{-default ""}
			-textmatch	{}
			-html		{-required}
			-asserts	{-default {}}
		}

		if {[llength $asserts] % 4 != 0} {
			error "asserts should be a list of {xpath e f expecting}"
		}

		try {
			#dom parse -keepEmpties -html <html>$html</html> doc
			dom parse -html <html>$html</html> doc
			$doc documentElement root

			if {[info exists textmatch]} {
				lassign $textmatch textmatch_type expecting textmatch_args
				set doctext	[$root asText]
				switch -- $textmatch_type {
					exact  { set matches	[expr {$doctext eq $expecting}] }
					glob   { set matches	[string match {*}$textmatch_args $expecting $doctext] }
					regexp { set matches	[regexp {*}$textmatch_args -- $expecting $doctext] }
					default {
						error "Invalid -textmatch \"$textmatch\", should be a list whose first element is one of \"exact\", \"glob\" or \"regexp\""
					}
				}

				if {!$matches} {
					return "-textmatch failed, expecting: \"$expecting\", got: \"$doctext\""
				}
			}

			foreach {xpath e f expecting} $asserts {
				set got	[lmap e [uplevel 1 [list $root selectNodes $xpath]] $f]
				switch -- $match {
					exact  { set matches	[expr {$got eq $expecting}] }
					glob   { set matches	[string match {*}$matchargs $expecting $got] }
					regexp { set matches	[regexp {*}$matchargs -- $expecting $got] }

					default {
						error "Invalid value for -match: \"$match\", should be one of \"exact\", \"glob\" or \"regexp\""
					}
				}

				if {!$matches} {
					return "$xpath failed, expecting: \"$expecting\", got: \"$got\""
				}
			}

			return "passes"
		} finally {
			if {[info exists doc]} {
				catch {$doc delete}
			}
		}
	}

	#>>>
}

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

