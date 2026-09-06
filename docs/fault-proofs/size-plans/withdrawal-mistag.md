# Withdrawal mistag completion

The five spending validators establish source/header binding, event/trace
binding, output descriptor and owner/signature authentication, exact value and
payout classification, and permanent proof minting. The installed workflow
reconstructs every prior ledger effect and the selected withdrawal from the
retained current/predecessor envelopes. Its journal contains those envelopes
and the withdrawal index; admission rederives proofs after every restart.

## Evidence and value comparison

Large typed payloads use the shared structured Data reference carrier. The
publication planner preserves Data map/list/byte order, groups oversized
constructor fields, deduplicates identical publications, and records exact
output identities through the authenticated raw-L1 publication prerequisite.
Every consumer authenticates the reconstructed payload against the committed
header or preceding thread state. Publication or reference substitution cannot
replace those checks.

Step03 authenticates the exact ledger descriptor under the output reference
and prior ledger root. Ledger admission has already proved that descriptor's
address, lovelace, canonical asset frontier and Cardano Value size. It binds
these facts, the withdrawal body hash, and the owner/signature result into the
Step04 datum. Full retained output bytes are still admitted and checked during
preparation, but are no longer repeated in the Step03 redeemer: the predicate
uses the authenticated descriptor facts directly.

Step04 validates the withdrawal Value, hashes its positive non-ADA assets,
sorts their hashes by native canonical policy/name order, and compares the
complete frontier and lovelace against the descriptor. Thus mixed-width names
preserve exact value equality across ledger Data and native CBOR map ordering.
Malformed ADA entries, nonpositive quantities, invalid policy/name widths and
quantity substitutions cannot satisfy that equality. The owner/signature,
asset-count bound, exact payout size and minimum lovelace rule all remain
necessary for an actual-valid verdict. The claimed and actual verdicts must
differ before the thread can mint a permanent proof.

The frontier batch builder checks every external leaf and preserves its own
empty-initialized frontier invariant, avoiding repeated full frontier checks
inside the fold. Its result is tested against checked incremental appends.

## Required evidence

The fit ledger records all reference publications, evidence publications and
locally evaluated lifecycle transactions. It is written only after all eight
named cases complete: both mistag directions, maximum payout, 5,000-byte Value
(1,304 assets), the 100-asset boundary, 16,384-byte output, combined 64-branch
source/event/trace/ledger proofs with mixed-width names, and that combined
shape with 100 maximum-length asset names. Both combined shapes also carry a
12,000-byte payout datum. The 64-branch openings are algebraic worst-shape
fixtures, separate from installed retained-history reconstruction cases.

The installed cases use real signed raw-L1 observations, persisted submission
intents, discarded receipts, fresh adapters and journal recovery through
permanent proof minting and queue-suffix removal. Cancellation is exercised at
all five unfinished stages, followed by out-ref-only resume. Honest verdict,
predecessor, coordinate, commitment and reference substitutions must refuse.
All fit assertions retain the actual Van Rossem limits and required reserves;
no evaluator or size exemptions apply.

Final shared-branch ledger, build digest and verification results are recorded
after copying the family commits and rerunning the checks there.
