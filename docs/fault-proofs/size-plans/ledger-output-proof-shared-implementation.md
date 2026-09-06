# Shared ledger output proof implementation sketch

The source plans are the ScriptSources output-proof step/finalize and
ResolveInputs membership step/finalize plans. The four dispatchers preserve
all existing auxiliary hashes and semantic successor/rejection predicates.
Step dispatch carries the authenticated input control, the proposed next
control, and a role selector. An empty next-control channel means rejection
on both sides; every advance requires the ordinary carrier successor.
Finalize requires the conjunction of four descriptor facts before either
signer/protected-output authorization branch.

Each physical yield authenticates one dispatcher input, its reference role
NFT and its zero withdrawal. The stage yields read the same 12-item LOP
control but decode only the active sub-control. Initial sub-control encodings
are pinned against the typed encoders. Datum actions split by action family;
additional physical splits are allowed only when measured publication or
aggregate transaction limits require them. The four descriptor yields pin
value and datum leaves independently before composing the output summaries.

First measurement seam: a raw control frame with canonical primitive/list/
Option encoding, without a typed sub-control decoder. The exact LOP control
wire uses definite lists and bytes and indefinite nonempty Option fields.
The Blake sub-control alone uses Plutus Data byte encoding for its active
block and working words; the raw encoder preserves that field-specific form.
The helper is specific to this control grammar and rejects map/foreign
constructor forms. Parity fixtures must cover every stage and optional
sub-control, including the existing long terminal golden. If whole-frame
encoding exceeds reserve, replace unchanged fields by authenticated byte
spans; never remove semantic checks or enlarge limits to fit.

Integration owns six ResolveInputs publication roles plus the new LOP roles.
The existing 29 ScriptSources and all other deployment rosters stay intact.
Verification includes positive and honest-refusal registered-chain journeys,
all physical publications, cancel/fresh recovery, maximum output/descriptor/
datum/reference-script/proof carriage, signed bytes and aggregate execution
reserve. The parent owns maxima for the other four ResolveInputs prunes.
