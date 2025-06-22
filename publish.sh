#!/bin/sh

rsync -avz run.sh  linode:/var/www/ftlm-vehicles
rsync -avz target linode:/var/www/ftlm-vehicles
rsync -avz release.jar linode:/var/www/ftlm-vehicles

# rsync -avz run.sh  ionos-1:/vehicles
# rsync -avz target ionos-1:/vehicles
# rsync -avz release.jar ionos-1:/vehicles
