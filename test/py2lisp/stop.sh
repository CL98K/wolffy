#! /bin/sh

ps -ef |grep "lisp.core" |grep -v grep |awk '{print $2}' |xargs kill -9