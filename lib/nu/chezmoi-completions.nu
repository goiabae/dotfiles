
export extern "chezmoi unmanaged" [
	...path: string
]

export extern "chezmoi add" [
	...target: string
]

export extern "chezmoi status" [
	...target: string
]

export extern "chezmoi diff" [
	--reverse # Reverse the direction of the diff
	...target: string
]
