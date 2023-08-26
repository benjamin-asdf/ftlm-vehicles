#!/bin/sh

cd "$(dirname "$0")" || exit 3

java -jar ./release.jar
