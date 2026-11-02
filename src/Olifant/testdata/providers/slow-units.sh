#!/bin/sh
# a units provider that takes a second to answer
cat >/dev/null
sleep 1
printf '[{"name":"add","line":3,"end":5}]'
