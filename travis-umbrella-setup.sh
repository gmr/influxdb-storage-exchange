#!/bin/bash
BUILD_DIR=${BUILD_DIR:-${HOME}/rabbitmq-public-umbrella}
RMQ_TAG=${RMQ_TAG:-v3_5_1}
echo "Build dir: ${BUILD_DIR}"
echo "Travis build dir: ${TRAVIS_BUILD_DIR}"
if [ ! -d $BUILD_DIR ]; then
    git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git $BUILD_DIR
    cd $BUILD_DIR
    make co
fi
cd $BUILD_DIR
make BRANCH=rabbitmq_${RMQ_TAG} up_c
git clone https://github.com/gmr/gun-wrapper.git
rm -rf ${BUILD_DIR}/influxdb-storage-exchange
cp -r ${TRAVIS_BUILD_DIR} ${BUILD_DIR}/
