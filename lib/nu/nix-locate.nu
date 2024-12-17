export def main [pattern: string] {
	^nix-locate --no-group $pattern
	| detect columns --no-headers
	| rename deriv size type path
	| update size { |row|
		$row.size
		| str replace --all ',' ''
		| into filesize
	}
}
