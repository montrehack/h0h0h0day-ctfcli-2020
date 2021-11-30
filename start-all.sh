#!/bin/bash

pushd santa-factory/
docker-compose up -d
popd

pushd h0h0h0-erlang/
docker-compose up -d
popd

pushd hohono1
docker-compose up -d
popd

pushd hero-of-dundee
docker-compose up -d
popd

pushd spook-or-doot
docker-compose up -d
popd
