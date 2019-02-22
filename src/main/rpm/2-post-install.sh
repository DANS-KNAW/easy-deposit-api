#!/usr/bin/env bash
#
# Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#include <service.sh>

NUMBER_OF_INSTALLATIONS=$1
MODULE_NAME=easy-deposit-api
INSTALL_DIR=/opt/dans.knaw.nl/$MODULE_NAME
DEPOSITS_STAGE_UPLOAD=/var/opt/dans.knaw.nl/tmp/easy-deposit-api/stage-upload
DEPOSITS_DRAFTS=/var/opt/dans.knaw.nl/tmp/easy-deposit-api/drafts
DEPOSITS_STAGE=/var/opt/dans.knaw.nl/tmp/easy-deposit-api/stage
DEPOSITS_SUBMIT_TO=/var/opt/dans.knaw.nl/tmp/easy-ingest-flow-inbox
PHASE="POST-INSTALL"

echo "$PHASE: START (Number of current installations: $NUMBER_OF_INSTALLATIONS)"
service_install_initd_service_script "$INSTALL_DIR/install/$MODULE_NAME-initd.sh" $MODULE_NAME
service_install_systemd_unit "$INSTALL_DIR/install/$MODULE_NAME.service" $MODULE_NAME "$INSTALL_DIR/install/override.conf"
service_create_log_directory $MODULE_NAME
echo "$PHASE: DONE"

if [[ ! -d ${DEPOSITS_STAGE_UPLOAD} ]]; then
  echo -n "Creating deposits stage/upload directory at ${DEPOSITS_STAGE_UPLOAD}..."
  mkdir -p ${DEPOSITS_STAGE_UPLOAD}
  chmod 777 ${DEPOSITS_STAGE_UPLOAD}
  echo "OK"
fi

if [[ ! -d ${DEPOSITS_DRAFTS} ]]; then
  echo -n "Creating deposits draft directory at ${DEPOSITS_STAGE}..."
  mkdir -p ${DEPOSITS_DRAFTS}
  chmod 777 ${DEPOSITS_DRAFTS}
  echo "OK"
fi

if [[ ! -d ${DEPOSITS_STAGE} ]]; then
  echo -n "Creating deposits stage directory at ${DEPOSITS_STAGE}..."
  mkdir -p ${DEPOSITS_STAGE}
  chmod 777 ${DEPOSITS_STAGE}
  echo "OK"
fi

if [[ ! -d ${DEPOSITS_SUBMIT_TO} ]]; then
  echo -n "Creating deposits submit directory at ${DEPOSITS_SUBMIT_TO}..."
  mkdir -p ${DEPOSITS_SUBMIT_TO}
  chmod 777 ${DEPOSITS_SUBMIT_TO}
  echo "OK"
fi
