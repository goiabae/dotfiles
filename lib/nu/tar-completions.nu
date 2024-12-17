export extern tar [
	# modes/subcommands
	--catenate, --concatenate (-A)
	--create (-c)
	--diff, --compare (-d)
	--delete
	--append (-r)
	--list (-t)
	--test-label
	--update (-u)
	--extract, --get (-x)

	# flags
	--file (-f): path
	--to-stdout (-O)


	# compression
	--auto-compress (-a)
	--use-compress-program (-I): string
	--bzip2 (-j)
	--xs (-J)
	--lzip
	--lzma
	--lzop
	--no-auto-compress
	--gzip, --gunzip, --ungzip (-z)
	--compress, --uncompress (-Z)
	--zstd

	# help
	--show-defaults
	--help (-?)
	--usage
	--version
]
