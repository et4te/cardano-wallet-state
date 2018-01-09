#!/bin/bash
stack exec cardano-launcher -- --system-start 0 --topology ./topology.yaml --configuration-file ./configuration.yaml --rebuild-db --keyfile secret.key
