#!/bin/sh

rsync -avz run.sh  linode:/var/www/ftlm-vehicles
rsync -avz target linode:/var/www/ftlm-vehicles
rsync -avz release.jar linode:/var/www/ftlm-vehicles
