#!/bin/bash

cur=$(git branch --show-current)
gh workflow run build_accessmod --ref $cur  --field dry_run=true
