# Parameter-application audit — issue #609

Companion record for the CRITICAL soundness fix raised out of #605
(classification b1) and authorised by the owner on 2026-08-14: ten fraud-proof
semantic-resolution validators were deployed one parameter short of what they
declare, which under Plutus V3 makes them unconditional always-succeeds
scripts.

This file is the audit deliverable of #609 scope item 3. It is a record, not a
gate: the gates that hold this class shut are
`demo/midgard-fault-proofs/tests/zz605-semantic-resolver-arity.test.ts` and the
arity-checking helpers those tests drive.

Measured against `onchain/aiken/plutus.json` md5
`b20c9a14a8fe445cdddbe5305b3857c1`, 398 validators, aiken `v1.1.23+2a78108`
(fork, md5 `b3acfdf348235798cb6b921d0f87750a`). The blueprint is
**byte-identical before and after #609** — no regeneration was spent, and no
compiled or unapplied identity moved. Everything that moves below is off-chain
applied-hash derivation.

> **Epoch marker (2026-08-16, #606).** That blueprint is superseded. Every
> applied hash printed below is bound to the #609 epoch and is **not** a
> current-tree claim; the audit's finding, rule and antidote are unaffected.
> The current values are tabulated in *Superseded by #606* near the end of this
> file. Nothing above that section has been rewritten or removed.

## The defect, stated once

`applyParamsToScript` applies whatever list it is handed and never consults the
script's own declared arity. Applying too few terms leaves the remaining
`validator main(...)` parameters as lambdas, so the ledger's single Plutus V3
script-context application reduces to a lambda VALUE instead of running the
validator body. Evaluation terminates without error, and the ledger reads "no
error" as SUCCESS. The Aiken guards in such a validator never execute.

## Method

Three independent measurements, none of which can be satisfied by the fix that
made the others pass.

1. **Declared arity** is read from `plutus.json` directly. A validator that
   takes no parameters omits the `parameters` key entirely, so **absent means
   zero declared**, never "unknown, skip the check". Every reader in this fix
   was changed to that reading.
2. **Applied arity** is enforced, not observed: every deployment in
   `demo/midgard-sdk/src/fraud-proof/contracts.ts` and
   `demo/midgard-node/src/services/midgard-contracts.ts` now routes through a
   helper that refuses `declared !== applied` in both directions. A green build
   of the whole contract set is therefore a proof that applied == declared for
   every title it deploys.
3. **Cascade movement** was measured by building the fault-proof contract set
   twice through the same corrected public builder — once against the real
   blueprint, once against a blueprint in which the ten resolvers' declared
   `parameters` are truncated to the two the pre-#609 loop hand-wrote. The
   second run reproduces the shipped (broken) deployment exactly, so every
   identity that differs between the runs is a #609 movement and every identity
   that agrees is proven not to move.

## Inventory — validators declaring a `field_preimage_certificate_*` parameter

The blueprint carries **62 entries** matching that filter, which is **31
distinct validators** each published as a `.spend`/`.mint` entry plus its
`.else` twin (30 spend + 1 mint). "~30 spend validators" in the ticket is this
set.

### Deployed by the SDK fault-proof builder (22)

| Declared | Validator | Verdict |
| --- | --- | --- |
| 3 | `fraud_proofs/validation_trace/input_sets_item_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/signatures_address_item_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/signatures_required_item_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/script_sources_non_output_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 3 | `fraud_proofs/validation_trace/script_sources_stage_seven_observer_semantic_v1` | **WAS BROKEN (2 of 3)** — fixed |
| 4 | `fraud_proofs/validation_trace/canonical_decode_item_observe_v1` | CORRECT (4 of 4) |
| 5 | `fraud_proofs/validation_trace/cek_v1` | CORRECT (5 of 5) |
| 3 | `fraud_proofs/double_spend/step_03` | CORRECT (3 of 3) |
| 4 | `fraud_proofs/double_spend/step_04` | CORRECT (4 of 4) |
| 3 | `fraud_proofs/input_no_idx/step_02` | CORRECT (3 of 3) |
| 4 | `fraud_proofs/input_no_idx/step_04` | CORRECT (4 of 4) |
| 4 | `fraud_proofs/invalid_signature/step_02` | CORRECT (4 of 4) |
| 3 | `fraud_proofs/no_input/step_02` | CORRECT (3 of 3) |
| 3 | `fraud_proofs/no_reference_input/step_02` | CORRECT (3 of 3) |
| 3 | `fraud_proofs/reference_input_no_idx/step_02` | CORRECT (3 of 3) |
| 4 | `fraud_proofs/reference_input_no_idx/step_04` | CORRECT (4 of 4) |
| 4 | `fraud_proofs/zero_input/step_02` | CORRECT (4 of 4) |

### Deployed by the node's real-contract loader (1)

| Declared | Validator | Verdict |
| --- | --- | --- |
| 2 | `user_events/tx_order_v1.mint` | CORRECT (2 of 2) |

### Declared but deployed by nobody (8)

These belong to fraud-proof families the ABI freeze records as unreachable —
no catalogue category registers them and, for most, no off-chain builder
exists. They carry no under-application risk today because no deployment
derivation constructs them. They are listed so that whoever integrates those
families knows the parameter is there; the arity helper will refuse them at the
load site if it is ever missed.

| Declared | Validator |
| --- | --- |
| 4 | `fraud_proofs/canonical_decodability/step_01` |
| 4 | `fraud_proofs/committed_field_shape/step_01` |
| 3 | `fraud_proofs/missing_native_script_tx/step_02` |
| 3 | `fraud_proofs/missing_native_script_tx/step_04` |
| 4 | `fraud_proofs/missing_native_script_tx/step_06` |
| 3 | `fraud_proofs/missing_signature/step_02` |
| 4 | `fraud_proofs/missing_signature/step_04` |
| 3 | `fraud_proofs/withdrawn_reference_input/step_02` |

### Audit outcome

Of the roughly nineteen previously unaudited cert-parameter validators:
**zero additional under-applied validators were found.** Eleven deploy at their
declared arity (ten in the SDK builder plus the node's tx-order mint) and eight
are not deployed at all. The ten proven-broken resolvers and the one
representative proven-correct resolver from #605's bisection are confirmed
unchanged in classification. No validator outside the known ten needed a fix in
this ticket.

The audit is not restricted to cert-parameter validators in practice: the
helper gates **every** `applyParamsToScript` call site in both packages, so the
whole 398-validator surface is now covered by the same rule.

## The structural antidote

| Package | Door | Rule |
| --- | --- | --- |
| `demo/midgard-sdk/src/fraud-proof/contracts.ts` | `applyBlueprintParams` | refuses `declared !== applied`; `getUnappliedScript` refuses deploying bare a title that declares parameters |
| `demo/midgard-node/src/services/midgard-contracts.ts` | `applyBlueprintDeclaredParams` | same rule; `unappliedBlueprintScript` is its bare-deployment twin |

Two escapes were closed along with the point fix:

- The node helper previously **abstained** when the blueprint omitted
  `parameters`. Absent is now read as zero declared.
- The SDK's blueprint parser **dropped** `parameters` at parse time, so the
  declared arity was not even available at the deployment site. It is now
  carried through `FaultProofBlueprintValidator`.

The semantic-resolver loop no longer hand-writes an argument list per index.
It resolves each **declared parameter by name** from a single table, so the
blueprint is the only authority on both count and order, a resolver that grows
a parameter is served automatically, and an unknown parameter name is a loud
refusal rather than a short application. A count-only rule would not have been
enough here: the canonical-decode item resolver declares three parameters that
are a different set entirely, not `award_script_hash` plus one.

### The name-keyed rewrite changes nothing it was not meant to change

The 75 semantic titles declare exactly three distinct parameter shapes, so the
rewrite can be checked exhaustively rather than argued about:

| Declared shape | Count | Indices | What the loop now applies | What the old loop applied |
| --- | --- | --- | --- | --- |
| `award_script_hash, computation_thread_policy_id` | 64 | every index except those below | `[award, computationThread]` | `[award, computationThread]` (the `else` branch) — identical |
| `source_binder_script_hash, computation_thread_policy_id, proof_item_script_hash` | 1 | 1 | `[source, computationThread, proofItem]` | `[source, computationThread, proofItem]` (the `index === 1` branch) — identical |
| `award_script_hash, computation_thread_policy_id, field_preimage_certificate_policy_id` | 10 | 5, 7, 8, 11, 24, 25, 32, 37, 47, 57 | `[award, computationThread, certificate]` | `[award, computationThread]` — **the defect** |

Sixty-five of the seventy-five resolvers are proven byte-for-byte unchanged by
construction, and the two-build cascade measurement below independently
confirms that only the ten moved.

`zz605-semantic-resolver-arity.test.ts` pins **no hash**, on purpose. The gate
it replaces (`validation-resolver-applied-hashes`) pinned the UNDER-APPLIED
hashes and therefore verified the broken deployment against itself. The
replacement asserts instead that (1) every deployed semantic resolver equals
the full application of its own declared parameters and equals **none** of its
under-applied prefixes, (2) no production source outside the helper calls
`applyParamsToScript`, and (3) the builder fails closed — proven by driving the
real public builder with a doctored blueprint one parameter short, one
parameter long, and with a parameter name the builder has no value for.

## Cascade movement table (applied-hash layer only)

Every row measured by the two-build comparison described under Method. 287
deployed identity slots compared; 44 slots moved, which is **19 distinct
validators** (the remaining slots are aliases — `steps`, `resolvers`,
`firstStep` are views onto the same objects). 243 slots are proven unchanged.

Cause for every semantic row: the resolver was deployed with 2 of its 3
declared parameters and is now deployed with 3.

| # | Identity | Old (under-applied) | New (correct) |
| --- | --- | --- | --- |
| semantic 5 | `input_sets_item_semantic_v1` | `581277646da1ce3fd120e051347d365fa2c35156bca7ede9cdc58091` | `eeb668757c5e2ccff37367777ad603ce36f560eece0acf31396d6dbd` |
| semantic 7 | `signatures_address_item_semantic_v1` | `ba54c3145d182704b37ef0fab4afe18fbdebb3e4d4220f35bc9e2074` | `9cc93e727dcf1da92d0d55456a5a4c5b6fe4be209c106ac4340e7d5e` |
| semantic 8 | `signatures_required_item_semantic_v1` | `a862dfdcdbb85b9a75a4bbe65355bc1345e93acd869afcd958b29776` | `84b8412d45b920d38d25b649ffe1e69755b006a62b6ca20d925f8045` |
| semantic 11 | `phase_a_native_scripts_item_semantic_v1` | `989d266a79ec5e20595455d0e255cbabe9affd7221da8b7b3e53862d` | `c61d63060bb613b321ee698de6005beefc5e99eb10df14a09875408f` |
| semantic 24 | `phase_a_script_preconditions_semantic_v1` | `2d304a989cb1d912b280cff2e30d5447232d5f86d17ca4fed5d142fd` | `0e84efc0d0b455635ace2f09ee6c7eb83d32eb20cf9c9d45f0eae8f2` |
| semantic 25 | `phase_a_script_preconditions_item_semantic_v1` | `6b17deb85136bef3bfc21a33d0101b3ea6af8cba19be97ef66bbe14a` | `34237f0e20fb798ca496060be202e9703cb1ca0b2d4bfaea055fbf35` |
| semantic 32 | `script_sources_non_output_semantic_v1` | `d0a3ff332b6479d74d8f3758e092571d244dbd609382456dcaffc1eb` | `f95ce7aa58700c191c12b97e082f70902c3b2138dfb8e94f0ce20799` |
| semantic 37 | `script_sources_stage_zero_begin_semantic_v1` | `7791c91c2ad320ca2cb111a35a7ac98441b2b165cef0e6faa7aeb4aa` | `59443faa6457e4013407ce03bb1f2100c31e5ef990b6f40b1f46c96c` |
| semantic 47 | `script_sources_stage_one_redeemer_semantic_v1` | `9c086225d52c99211ca5be5dce3c0082018877338f7827f05712bf9e` | `06513cb8bbb8577158d5a697cce015778915f576c14d6548203d6c02` |
| semantic 57 | `script_sources_stage_seven_observer_semantic_v1` | `abc6fcb7b5b00adf5b9d125fec483dbcb68a53f558e9fb8378af7ddb` | `afe482c924c7debfc5b208dfe3d55138eb35f2b84ef64784b2ffa273` |

The five PREPARE resolvers embed their group's semantic hash list as a
parameter, so their applied hashes move with the members above. Their own
application logic is unchanged — all twelve were always correctly applied.

| # | Identity | Old | New | Cause |
| --- | --- | --- | --- | --- |
| prepare 3 | `input_sets_v1` | `a2bb7e6fe5d2d2957055c5e3bf597a6e46909dcb67398fcf05c154b5` | `ea33e266580a3ca26a78911ccca9e7c4d6434bc80b255b0f0993c69e` | embeds semantic 5 |
| prepare 4 | `signatures_v1` | `0d94a7fe210a9198f3b85b5111f35acbb63a0cd0a2faddd35f058627` | `7948ae85e0dff1b77f250f61ab2ab948662a8c07097b543b1364b437` | embeds semantics 7, 8 |
| prepare 5 | `phase_a_native_scripts_v1` | `c7188e2e1d37802ca38def6a55ffe705a3da6c539106986160beac76` | `c162fa34025ca8a1d791ae2b0d0cf7321495dda2369e24d44ac511df` | embeds semantic 11 |
| prepare 6 | `phase_a_script_preconditions_v1` | `6f16a045030dc0c67734cb9d2f9ad5caf6f85f39d518ad916cffb007` | `2d57481ff386734a741b5bf9806c88c4a4ca68bdfb12260a1d630983` | embeds semantics 24, 25 |
| prepare 8 | `script_sources_v1` | `a39284143eae8bed47a8c7421a06c5c061f4021e4769744c8afcb71d` | `6903f4e8736a182bd9ec4f7dd9445cc2acea7a5a06e1157b86f7175d` | embeds semantics 32, 37, 47, 57 |

The dispute chain parameterises on the resolver hash list, then on each other,
so the movement propagates once through four more validators.

| Identity | Old | New | Cause |
| --- | --- | --- | --- |
| `boundary` | `3c09d495c95bf03d2cf87c15679567e3220f48c02148a048c9a929c4` | `ff734f28da1b6c97f531112afd9299adbad4e26a6b3803f3c5283717` | embeds the resolver hash list |
| `game` | `f515da95b9ee396be70c15d13a345e00c36c2f20c9b1d5a2d58e0e4f` | `95d4904a1082ee7600e627ca9eacedb449d2be3178de0f5abfaa1f9a` | parameterised on `boundary` |
| `source` | `d34e6d89787b7d33e4da4b1144507c28981d5aa4c05b7faf665a30f9` | `50afe9aacc14c4c266be633a6a2558c56cbdc0d3dd89ccf23991e6de` | parameterised on `game` |
| `dispute` (the family's applied step-01 / catalogue opener) | `156d1b0aeb689314f6fd5efd3587ab45563ebf07d8371035bf5a3857` | `9f3fa57d4989358c6fdecf76c84e6820109c5ce3e3eb0d6455686049` | parameterised on `source` |

### Derived identities that move with them

| Identity | Old | New | Cause |
| --- | --- | --- | --- |
| Q13 fraud-proof catalogue root (`demo/midgard-fault-proofs/tests/inspect-contracts.test.ts`) | `173cabdb279e82cda76d0a7ac4b5a50bfc50406aec97d442790ed2810c2114be` | `61f11db32d208c0f71ffc506e2a2ce1555a72e49f3180e1f92edad3c8a928cdf` | the root folds every category's applied step-01 hash; only the validation-trace category's opener moved |
| Aiken resolver fixture `phase_a_script_precondition_resolvers` (2 entries) | see semantics 24, 25 | see semantics 24, 25 | production-builder fixture identities |
| Aiken resolver fixture `script_source_resolvers` (4 of 29 entries) | see semantics 32, 37, 47, 57 | see semantics 32, 37, 47, 57 | production-builder fixture identities |

`Q13_APPLIED_STEP_HASHES` (the input-no-idx family's four applied step hashes)
was re-measured **unchanged** — that family was never under-applied. It was
checked rather than assumed, because a stale pin hiding behind another failing
pin is how #579 lost one. (**#606, 2026-08-16:** those four *did* move under the
next batch, for a different reason — the certificate policy id is the applied
trailing parameter of every field-opening step. New values in *Superseded by
#606*.)

A repository-wide scan for all nineteen old hashes (excluding `.git`,
`node_modules`, `dist` and log directories) finds them **nowhere except this
report's own old→new movement table above** — documentation of the movement,
not live pins — with **zero occurrences in any test, source, or pin surface**,
so no stale pin survives this movement.

## Superseded by #606 (2026-08-16) — the values moved, the finding did not

This file is the record of a defect and its fix, and that part stands: ten
validators were deployed one parameter short of their declared arity, the
`declared !== applied` rule in both builders now refuses it, and
`zz605-semantic-resolver-arity.test.ts` still pins no hash on purpose. None of
that is touched here. What is epoch-bound is the identity **values**, and #606
moved them. Every row above keeps its text and gains this marker rather than
being rewritten.

#606 — the E2 §8.6 certificate repair (owner ruling 2026-08-16) carrying the
#608 empty-sentinel rider — regenerated the blueprint: `onchain/aiken/plutus.json`
md5 `b20c9a14a8fe445cdddbe5305b3857c1` → `5e38d7c6ccb7987d0aca710307dcaea7`
(SHA-256 `f49cae224f24cfab577f1ed10b5340384b75e541851eb7b77b507a79cb7d5e00`,
still **398 validators / 702 definitions**, same fork `aiken v1.1.23+2a78108`,
same `aiken build --env testnet`). Two consequences reach this file:

1. **The certificate minting policy moved**,
   `c3682abd66d94b999806a624346a517981febcc3a1c94cd73604b178` →
   `f030476f9cddff41d15bdaa7951a9726252c6867901992e2a5f8427e`. That policy id
   is the declared third parameter of *exactly the ten resolvers this audit is
   about*, so **all ten applied hashes move on its value alone** — including
   semantics 5, 7, 8 and 11, whose compiled bodies #606 never touched and which
   no Aiken fixture pins. This is the audit's own thesis holding: applied
   identity is a function of the parameter values, not only of the script body.
2. **The §8.6 door rewrite changed resolver bodies**, so the six fixtured
   resolvers move for both reasons at once.

Derived on the same deployment the tables above use — the production builder
(`buildFaultProofContracts`) over this tree's blueprint with
`hubOraclePolicyId = "bb"×28` and `fraudProofCataloguePolicyId = "cc"×28` —
never read off a diff. Six of the ten are independently gated as well: they are
the six entries #606 re-pinned in
`onchain/aiken/lib/midgard/validation-resolver-v1.test.ak`
(`phase_a_script_precondition_resolvers`, both entries; `script_source_resolvers`,
4 of 29), and the cross-language SDK gate binds that fixture to this same
builder. The other four (5, 7, 8, 11) are fixture-free — `input_sets_resolvers`
carries synthetic prepare/award hashes, not real semantic identities — so they
are derived here and gated only by the arity rule.

### Semantic resolvers — #609's "New (correct)" column is superseded

| # | Identity | #609 (superseded) | #606 (current) |
| --- | --- | --- | --- |
| semantic 5 | `input_sets_item_semantic_v1` | `eeb66875…` | `b5c8539e576f3c2b92af76f13e7d08398c823d14eb8fcbae27d08905` |
| semantic 7 | `signatures_address_item_semantic_v1` | `9cc93e72…` | `cead1152d04e9f8547d136295a1a1d135294af1b07ba959cc871210e` |
| semantic 8 | `signatures_required_item_semantic_v1` | `84b8412d…` | `f9e9d168cf5611aeb7571f357f8edc4716239953b17b973736784a0e` |
| semantic 11 | `phase_a_native_scripts_item_semantic_v1` | `c61d6306…` | `0e542d958d4ad05698045ab4ef3594ac9ccb366df2ccd1918f80b0b8` |
| semantic 24 | `phase_a_script_preconditions_semantic_v1` | `0e84efc0…` | `2abd6e89a6b1a3d03d450ed89337cfd2558d1f583b893e7c57288283` (fixture-gated) |
| semantic 25 | `phase_a_script_preconditions_item_semantic_v1` | `34237f0e…` | `77a54ab5a403ded1a52c70e40782c0a387aafe4f0a351a0622f10b7d` (fixture-gated) |
| semantic 32 | `script_sources_non_output_semantic_v1` | `f95ce7aa…` | `3c2d20d1e0fa53c9243e75f1126b9993544140b66ab65b386d4d0e29` (fixture-gated) |
| semantic 37 | `script_sources_stage_zero_begin_semantic_v1` | `59443faa…` | `d5faf4cd0c81c8285a7d64c6f35f60f29cfe7233184b6016be8291a0` (fixture-gated) |
| semantic 47 | `script_sources_stage_one_redeemer_semantic_v1` | `06513cb8…` | `1907255d1fb58d35027633f3e8d883b57c6f097fe5dd4f1921f57bbe` (fixture-gated) |
| semantic 57 | `script_sources_stage_seven_observer_semantic_v1` | `afe482c9…` | `20362cad6dabb9756ac9ced8ab1caab6d6edcc05b2a00058d912c339` (fixture-gated) |

The remaining 66 slots of the 76-entry resolver set declare **no** certificate
parameter, so cause (1) cannot reach them: measured from this tree's blueprint,
exactly **10 of the 75** `*_semantic_v1` spend entries carry
`field_preimage_certificate_policy_id`, and they are exactly the ten tabulated
above. Twenty-five of the remainder are additionally proven unmoved in the tree
— they are the untouched entries of `script_source_resolvers`, of which #606
re-pinned four and left the other twenty-five byte-identical.

### Prepare resolvers and the validation-trace family chain

Same causal ladder as the tables above: each layer embeds the layer below it.

| Identity | #609 (superseded) | #606 (current) |
| --- | --- | --- |
| prepare 3 `input_sets_v1` | `ea33e266…` | `32613795bfb0c9660661c1e13869487afc76e1eaea10e4a0682cd258` |
| prepare 4 `signatures_v1` | `7948ae85…` | `e54ef83202cc6ecf56d9cf74a28e884da995b46d182e058e0f15ff4e` |
| prepare 5 `phase_a_native_scripts_v1` | `c162fa34…` | `0559066d691af5d244c9cf7cb90a4e3e75dcff0a896456b43bf129d1` |
| prepare 6 `phase_a_script_preconditions_v1` | `2d57481f…` | `a4f2959170850ca17ae5ccbd16c7b07f85f8aa98805fa2257a3c9a39` |
| prepare 8 `script_sources_v1` | `6903f4e8…` | `b54b7a3e672a9f1af1b823b8b5dc2cc2be7718cebdfa6f635846d3ca` |
| `boundary` | `ff734f28…` | `4d60d31fcfa05bbb8bd7f23f97e43a25f859b04c819154209e6e4746` |
| `game` | `95d4904a…` | `306a68cdde4b094b3223127fbbac66a9226319e7828f574281ca6526` |
| `source` | `50afe9aa…` | `2b7d51d069fd03b513ede625856a675e299c127042479b0db6812de4` |
| `dispute` (applied step-01 / catalogue opener) | `9f3fa57d…` | `2248ef11a8ec2e5350c4fb8dad329cffa37ee56847fda2e8811889b4` |

### Derived identities that move with them

Re-pinned in the tree by #606 itself, and quoted here so this file does not
present a second unmarked value for the same identity. Both were produced by
`demo/midgard-fault-proofs/tests/inspect-contracts.test.ts`'s own producer
(`MIDGARD_PRINT_PROOF_FIT=1`, the `q13AppliedIdentities` line) on that suite's
placeholder deployment, which is the deployment the #609 rows for these two
identities were taken on.

| Identity | #609 (superseded) | #606 (current) |
| --- | --- | --- |
| Q13 fraud-proof catalogue root | `61f11db32d208c0f71ffc506e2a2ce1555a72e49f3180e1f92edad3c8a928cdf` | `53f5fc3aab9ddd758ad82a4d27321c2b3c8213312e71bbe5636dfec22de21c52` |
| `Q13_APPLIED_STEP_HASHES` 1 of 4 | `2389abf7…` (unmoved under #609) | `063831d17f9f26542608df346319a333917f14345d2c2df876f593af` |
| `Q13_APPLIED_STEP_HASHES` 2 of 4 | `55c27cdb…` (unmoved under #609) | `5438e14789271ca13e0ec1cebe00b9d7c28e64a9d2e54d6d9f54013f` |
| `Q13_APPLIED_STEP_HASHES` 3 of 4 | `a966c6be…` (unmoved under #609) | `f88c7a7937df342a37eee3132139110d0a7689b037ab99b512937c30` |
| `Q13_APPLIED_STEP_HASHES` 4 of 4 | `b2685117…` (unmoved under #609) | `393aeccc152ae74b067ede8670274e3994bc9577ef0bbd8bdc60f77c` |

The five ABI digests and the deployment-manifest id in the next section are
digests over NAMES and over a synthetic placeholder hash, so they are unmoved by
#606 for the same reason they were unmoved by #609. The nineteen
under-applied hashes this file records remain absent from every pin surface;
#606 changed nothing about that scan.

## Identities that were expected to move and are measured NOT to

Recorded because the ticket brief listed them as expected movements, and the
honest answer is that the applied-hash layer does not reach them.

| Identity | Value | Why it does not move |
| --- | --- | --- |
| ABI-01 deployment contract-name vector (54) | `4ddc94c7e57860f9ef89a73f73822bbb7eed729b4578e5194395af9554df160b` | digest over declared contract NAMES, not applied hashes |
| ABI-02 catalogue category order (11) | `7b60219b9229ed930bf249eba29b55f96e5e97925269dc585d24fee718fc0684` | digest over category names |
| ABI-03 reference-script role→contract map (36) | `06b0383ebecfbe9a4f8ae4dd96dfe4ca51a8ee09d94fa8e2794261617e1688b0` | digest over role names |
| ABI-04 reference-script auth token names (37) | `691f1a606c9c20d82c755ea8e5ea2297e670fc32cecdf1538c8adcdfcfdc2c6e` | digest over token names |
| ABI-07 required transaction-order contracts (9) | `2dc93dc63e0f1fef3bca7050271475c9955f426b19b14216a028567bf0079fae` | digest over contract names |
| Deployment manifest id | `d9c811abd62e8acc619181d1836b749e3fb2e295e0a309f9cf09b03d996813ef` | derived from the manifest SHAPE over a synthetic `CONTRACT_SCRIPT_CBOR = "01"` placeholder hash, not from real applied hashes |
| `onchain/aiken/plutus.json` | `b20c9a14a8fe445cdddbe5305b3857c1`, 398 validators | no product `.ak` source changed; the compiled and unapplied identities are correct and untouched — **#606 (2026-08-16) moved it to md5 `5e38d7c6ccb7987d0aca710307dcaea7`; that is a #606 regeneration, not a #609 movement** |

`docs/exec-plans/evidence/canonical-v1-abi-freeze-v1.json` and
`docs/exec-plans/evidence/canonical-v1-tx-order-receipt-identity-removal-v1.json`
are therefore left byte-unmodified. The latter records
`catalogueRootAfter: 173cabdb…` as a historical measurement of the post-#579
state, which remains a true statement about that state; it has no live
re-derivation gate.

## Commands

Run inside WSL with `MIDGARD_AIKEN_BIN=/home/gumbo/playground/aiken/target/release/aiken`.

```
$MIDGARD_AIKEN_BIN check                       # 2483 checks, 0 errors, 6 pre-existing warnings
$MIDGARD_AIKEN_BIN build --env testnet         # plutus.json byte-identical, 398 validators
$MIDGARD_AIKEN_BIN fmt lib/midgard/validation-resolver-v1.test.ak   # idempotent
node demo/scripts/verify-canonical-v1-abi-freeze.mjs
node demo/scripts/verify-canonical-v1-abi-freeze-self-test.mjs
pnpm --dir demo/midgard-sdk exec vitest run --bail=0
pnpm --dir demo/midgard-fault-proofs exec vitest run --bail=0
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/zz605-semantic-resolver-arity.test.ts --bail=0
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-soundness.test.ts --bail=0
pnpm --dir demo/midgard-node exec vitest run tests/deployment-manifest-v1.test.ts --bail=0
pnpm --dir demo run typecheck
```
