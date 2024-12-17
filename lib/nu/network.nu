export def interfaces [] {
	ip -json address | from json
}
