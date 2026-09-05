# `invalidSignature` wrongful-rejection extension V1 size plan

The existing two-step chain retains its validator parameters. Step 01 binds
an accepted inclusion or an exact forced-root leaf to the challenged header
and forwards `subject` plus the authenticated witness-set hash. Step 02 opens
field 7 through the shared field-opening door and checks one Ed25519 witness
against the subject transaction ID. Accepted invalid signatures convict;
forced `AddressWitnessSignatureInvalid` rejections convict only when the
exact reason index equals the opened index and that signature verifies, or
the authenticated count proves that the rejected index does not exist.

`Init -> bind source -> open witness / mint proof -> leased removal` remains
the physical transition graph. Both spend states retain cancellation. Recovery
uses the durable subject and witness-set anchor; source, reason and index
cannot be replaced by a replay caller. No universal witness scan is needed:
the rejection asserts invalidity at one authenticated coordinate.

Step 01 imports the shared source substrate and native source verifier.
Step 02 imports the field-opening door, signature primitive, family polarity
rule, and shared finalizer. The largest evidence is a forced membership proof
at binding and a maximum field-7 preimage at finalization; published carriage
must be used when direct evidence exceeds the signed transaction envelope.

Fit evidence must use freshly compiled pinned testnet validators and actual
Lucid transactions, including publication, cancellation, restart, terminal
mint and removal. Every measured transaction must fit 16,384 signed bytes,
16,500,000 memory and 10,000,000,000 CPU; publication scripts must fit 15,872
bytes. The accompanying ledger records actual measurements and margins.

## Measured field maximum

The supported aggregate field cap is 32,768 bytes. Each field-7 witness occupies
103 bytes including its byte-string wrapper, so 318 witnesses (32,757 bytes)
fit with empty other fields and 319 witnesses exceed the field cap itself.
The maximum fixture uses an empty input field; this rejection still asserts
an invalid signature at one specific coordinate, which a valid signature
contradicts independently of other ledger predicates. The final witness is
selected in the maximum test. Three authenticated chunks and a real minted field certificate
carry this preimage. Field-7 fixed-width arithmetic authenticates the count even
for certified carriage. The fixture checks Inline, RawUtxo, and Certified
paths; the ledger records every chunk publication and certificate mint.

The maximum source test uses a 64-branch synthetic membership proof with the
exact forced key/value and count 1. Its reconstructed PHAS root is committed
into the counted root and challenged header. This is combined with the
maximum witness field in the same complete lifecycle; binding remains below
the signed transaction and execution envelopes.

## Consolidated evidence

The integrated testnet blueprint is
`9cd5a8b1f19c2cb5977f69062547e3be24617bf97470a598347d349bd927becc`.
The adjacent ledger uses the shared Van Rossem writer, with named rows and
recomputed byte, memory, CPU, and publication margins. Its test requires the
current blueprint digest and compiler version. The combined minFee and
invalidSignature family regression passed 63 tests on this tree.

Capture a single passing Vitest run of
`invalid-signature-wrongful-rejection-lifecycle.test.ts`,
`submit-init-emulator-invalid-signature-lifecycle.test.ts`, and
`invalid-signature-wrongful-rejection-publication-fit.test.ts` against the final
`MIDGARD_REAL_BLUEPRINT_PATH`. From the repository root run:

```sh
node demo/midgard-fault-proofs/scripts/write-invalid-signature-fit-ledger.mjs CAPTURE_LOG onchain/aiken/plutus.json
```

The generator checks the capture's blueprint marker, all six forced shapes,
the accepted stages, and both publications before writing 68 measured rows.
Then run `invalid-signature-wrongful-rejection-fit-ledger.test.ts`.
