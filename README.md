# csml2rdf

This project contains an application that converts the "classic"
BACnet base types and objects that are provided in CSML to SHACL rules
for validating BACnet content encoded in RDF.

The rules for encoding BACnet content in RDF will be published in
Addendum ct for Standard 135-2024, which is not yet available. They were
developed in the Data Modeling Working Group within ASHRAE SSPC 135,
a.k.a., the BACnet Committee.

## Source Files

The canonical CSML definitions are published by ASHRAE at
<https://data.ashrae.org/bacnet/XD/latest/>. Download
`bacnet-data-types.xml` and `bacnet-object-types.xml` from there into the
project directory; they are not kept in this repository (see `.gitignore`).

`virtual-terminal.xml` is included for backward compatibility. It contains
the `BACnetVTClass` and `BACnetVTSession` definitions, which the Device
object's `vt-classes-supported` and `active-vt-sessions` properties still
reference but which are no longer in `bacnet-data-types.xml`. They were
taken from an earlier combined CSML document (Protocol_Revision 19).

## Usage

Dependencies are managed with [uv](https://docs.astral.sh/uv/):

    uv sync
    uv run python csml2rdf.py bacnet-data-types.xml bacnet-object-types.xml virtual-terminal.xml > bacnet.ttl

The definitions in all of the CSML documents given on the command line are
combined before converting, so references from one document to types defined
in another are resolved. A warning is written to stderr if a definition name
appears more than once. With no files the CSML document is read from stdin.

By default the `BACnet` prefix is removed from type names (for example
`BACnetPropertyIdentifier` becomes `bacnet:PropertyIdentifier`). Use
`--retain` to keep it:

    uv run python csml2rdf.py --retain bacnet-data-types.xml bacnet-object-types.xml virtual-terminal.xml > bacnet.ttl

To regenerate the test outputs (which assume the prefix is removed):

    cd tests && uv run ./run-tests.sh
