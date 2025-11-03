const VOWELS = {
	'a': "aáâã"
	'e': "eéê"
	'i': "ií"
	'o': "oóôõ"
	'u': "uú"
}

def make_set [xs: list<string>, is_negative: bool] {
	$xs
	| each { |c| $VOWELS | get -i $c | default $c }
	| str join
	| "[" + (if $is_negative { "^" } else { "" }) + $in + "]"
}

def make_lambda_set [acc, pos] {
	if ($pos | length) == 0 {
		$acc
	} else {
		let fst = $pos | get 0
		if $fst == null {
			make_lambda_set $acc ($pos | skip)
		} else if ($fst | str length) == 0 {
			error make { msg: "string cant be empty" }
		} else if ($fst | str substring 0..0) in ["^", "%"] {
			let set = make_set ($fst | split chars | skip) false
			make_lambda_set ({ |str| $str =~ $set and (do $acc $str) }) ($pos | skip)
		}
		make_lambda_set $acc ($pos | skip)
	}
}

export def filter-words [doesnt: string, pos: list<any>] {
	let $dicio = $in
	let l = (make_lambda_set { |str| true } $pos)
	let a = ($pos | each { |it|
		if $it == null {
			"."
		} else if ($it | str length) == 0 {
			error make { msg: "string cant be empty" }
		} else if ($it | str substring 0..0) in ["^", "%"] {
			make_set ($it | split chars | skip) true
		} else {
			make_set ($it | split chars) false
		}
	} | str join)
	$dicio
	| where { |it| $it =~ ((make_set ($doesnt | split chars) true) + "{5}") and $it =~ $a and (do $l $it) }
}
