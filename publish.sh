#!/bin/sh

rsync -avz run.sh  linode:/var/www/ftlm-hearts
rsync -avz target linode:/var/www/ftlm-hearts
rsync -avz release.jar linode:/var/www/ftlm-hearts
