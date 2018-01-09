#!/bin/bash
stack exec cardano-wallet-state -- --system-start 0 --topology ./topology.yaml --configuration-file ./configuration.yaml --rebuild-db --keyfile secret.key
