#!/bin/bash
for filename in examples/*.pqr; do
    echo "Running $filename"
    qura $filename
done