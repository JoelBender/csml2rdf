#!/bin/bash

for f in *.xml
do
    python ../csml2rdf.py < $f > ${f/.xml/.ttl}
done
