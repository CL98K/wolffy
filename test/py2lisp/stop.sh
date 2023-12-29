#! /bin/sh

ps -ef |grep "mmap-test.core" |grep -v grep |awk '{print $2}' |xargs kill -9