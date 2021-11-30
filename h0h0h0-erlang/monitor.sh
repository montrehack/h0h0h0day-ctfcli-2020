#!/bin/sh

while
	pidof hohoho >> /dev/null && /bin/sleep 1 || $(bin/hohoho foreground &)
	[ 1 ]
do :; done
