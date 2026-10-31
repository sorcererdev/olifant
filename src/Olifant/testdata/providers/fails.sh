# a provider that fails, saying why on stderr
cat >/dev/null
printf 'no grammar\nmore\n' >&2
exit 3
