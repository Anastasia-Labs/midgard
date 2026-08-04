# §3.2 necessity artifact — Q13 `input-no-idx` spend-input proof

## Final build and source binding

This artifact binds Q13 to the parent-authoritative Aiken build produced with
released compiler `v1.1.22+39d6b04`, environment `testnet`, and 380
validators. The generated `onchain/aiken/plutus.json` SHA-256 is
`f5ae651e34cf3e1175d928634c002580c4f2af4659a229952007c458945b866b`.
All emulator and signed-transaction evidence below used that exact blueprint.

The remaining deterministic inputs are:

| Input                                           | SHA-256                                                            |
| ----------------------------------------------- | ------------------------------------------------------------------ |
| `onchain/aiken/aiken.toml`                      | `4b7d962c735672eb626f0abb4f31213fafd756a052caa60256cb5af4be380e05` |
| `docs/consensus-profile-v1.md`                  | `c6057096273b23922cf7b45fcd417ccd83f35c531299b555fc577d0f22ae17df` |
| Aiken step-02 ABI source                        | `a3a7269a79e79ac2e29d872a9225642e3fc0d210f0572aff8b15c40659e2a068` |
| Aiken step-02 validator and proof-fit selectors | `216e6f5bb6d2fa97205e3a5d29ee81c6103774d0ea7b2d0306528a96269d7413` |
| SDK Q13 schemas/builders                        | `ed401ced4c605b292a63a4d49aee2a72c73e1e13fa5d1b342c06bd2cb38d6677` |
| SDK ABI tests                                   | `01d46fd81bf15a2793c22ec27a3f719abc3c4fa811979870704e8126554d81a2` |
| Production step-02 submitter                    | `79f31e3e0abb5a0f453e45c0ae48555097848dae278824bdf8444852d026841e` |
| Signed-publication focused test                 | `420e7347e051e5be08919f6ec7f01780c42da9f2b29b4b1d8c4549fa9f396fc0` |
| Four-case lifecycle/fold test                   | `c7994618d1df5323b19d30347efd49e027bdb6a7b2a36643ad6d6a4c6b1e4f08` |
| Applied-hash/catalogue inspection test          | `d694a33384cd4128b5c9b83bc383b0af22c8ad3f6517f4957ba2b364aed3af57` |

The consensus profile supplies a raw transaction execution ceiling of
16,500,000 memory / 10,000,000,000 CPU and the mandatory §3.3 release reserve
ceiling of 13,200,000 memory / 8,000,000,000 CPU. Signed envelope checks use
the Preprod boundary `maxTxSize=16,384`, `maxValSize=5,000`, and
`coinsPerUtxoByte=4,310`.

The lifecycle harness uses two distinct public BIP39 test vectors, derives
fixed Custom-network Base emulator accounts, fixes emulator time to
`1735689600000`, and fixes the genesis account and input ordering. The vectors
are clearly marked test-only and must never fund a real wallet. Ed25519 signing
therefore produces reproducible signed CBOR; host wall-clock timing remains
observational and is not part of the canonical transaction binding.

## Final ABI and applied identities

`SpendRedeemer` remains the fail-closed typed
`ct.StepRedeemer<step_02.Args>`. Its `Continue` payload contains one Args
constructor whose fields are direct, with no intervening record constructor:

| Args constructor    | Tag | Direct field arity |
| ------------------- | --: | -----------------: |
| `Complete`          |   0 |                  4 |
| `CompletePublished` |   1 |                  4 |
| `FoldStart`         |   2 |                  5 |
| `FoldNext`          |   3 |                  4 |

The SDK ABI gate pins the exact raw `Complete` CBOR and rejects an obsolete
nested `CompleteArgs` wrapper, an adjacent-tag payload, short arity, an
out-of-range Args tag, and wrong `Continue` arity. No opaque `Data` redeemer or
decoder fallback is present.

With `network=Preprod`, `hubOraclePolicyId="11" × 28`, and
`fraudProofCataloguePolicyId="22" × 28`, the final applied identities are:

| Contract           | Applied hash / policy ID                                   |
| ------------------ | ---------------------------------------------------------- |
| Q13 step-01        | `5c79063d6b56296f23f7df24380efb980fb43ae1462ee1c01989334f` |
| Q13 step-02        | `a562f6b3f7b1337f0f764aa0d94fc85390dfa74b9c06682e1fcb55e2` |
| Q13 step-03        | `e22e2b38df904c51090c66a7eebb20a78d5b9b60a0c55b833cd80abb` |
| Q13 step-04        | `9984b16ce9b35df88905e1eb732a65febb21a624c56d915a16fcd355` |
| computation-thread | `4526132b94d049f23c67947e4c617c082bdf9361ed7f71c70316a88a` |
| fraud-proof mint   | `15048fdf1279296652f50cac418fb9b82c5e35414024bdc69f208c16` |
| fraud-proof spend  | `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab` |

The Q13 catalogue binding is category ID `00000002`, category script hash
`5c79063d6b56296f23f7df24380efb980fb43ae1462ee1c01989334f`, and catalogue
root `d88f9829ae8856b0fcd1023c0f6377e76319d46d69f0940444a193241bcca394`.
The inspection gate pins the applied step hashes and root, re-derives the root
and membership proof, cross-checks the embedded deployed script bytes, and
requires the category to be ready.

## Complete direct proof: exact 19-versus-296 necessity

The adversarial boundary is the Cardano transaction shape containing 296
script spends, each with a spend redeemer. It is pinned by
`native-tx.max-redeemers.test.ak`; it is not the larger universal field-0
input maximum for inputs without redeemers.

The final guarded Aiken selectors report:

| Direct `Complete` preimage |      Memory |            CPU | Result                                                   |
| -------------------------- | ----------: | -------------: | -------------------------------------------------------- |
| 19 inputs                  |  12,839,676 |  5,674,387,119 | PASS reserve; margins 360,324 memory / 2,325,612,881 CPU |
| 20 inputs                  |  13,366,688 |  5,896,030,186 | FAIL reserve by 166,688 memory; raw L1 still passes      |
| 296 script-spend inputs    | 186,032,040 | 77,853,784,762 | FAIL raw L1 by 169,532,040 memory / 67,853,784,762 CPU   |

Thus the complete spend-input proof fits only 19 inputs under the mandatory
reserve, versus the 296-input Cardano script-spend boundary. At 296 it uses
11.27× the raw memory ceiling and 7.79× the raw CPU ceiling. The 20-input
selector is a successful raw validator evaluation; its failure is the release
proof-fit policy, not a fabricated semantic rejection.

## Ordered §3.2 representations

### 1. Complete item directly in the proof transaction

Implemented as `Complete` and reserve-safe through exactly 19 inputs, as
measured above. The public builder selects the bounded fold above 19 unless an
explicit publication is supplied.

### 2. Complete inline-datum publication and reference consumption

Implemented as typed, prover/thread-bound `PublishedSpendInputsV1` plus
`CompletePublished`. The publication container itself was genuinely built and
signed against the Preprod boundary:

| Inputs | Signed bytes |     Fee | Output min-Ada | Ada margin | Tx-byte margin | Value-byte margin | Inputs / refs / outputs / collateral | Vkeys / redeemers |
| -----: | -----------: | ------: | -------------: | ---------: | -------------: | ----------------: | ------------------------------------ | ----------------- |
|     19 |        1,188 | 207,829 |      4,672,040 |          0 |         15,196 |             4,991 | 1 / 0 / 2 / 0                        | 1 / 0             |
|     20 |        1,227 | 209,545 |      4,840,130 |          0 |         15,157 |             4,991 | 1 / 0 / 2 / 0                        | 1 / 0             |
|    296 |       12,305 | 696,977 |     52,586,310 |          0 |          4,079 |             4,991 | 1 / 0 / 2 / 0                        | 1 / 0             |

All three publication transactions have no script redeemer and therefore use
zero execution reserve. Their maximum output value is 9 bytes.

Publication does not replace the bounded fold as the release fallback because
the consuming validator must still decode, authenticate, and commit the whole
list. Its guarded Aiken evaluations are:

| Published inputs |      Memory |            CPU | Result                                                 |
| ---------------- | ----------: | -------------: | ------------------------------------------------------ |
| 19               |  15,968,649 |  7,088,401,249 | Raw pass, but release-memory failure by 2,768,649      |
| 20               |  16,517,107 |  7,316,522,425 | Raw-memory failure by 17,107                           |
| 296              | 194,507,051 | 80,903,691,577 | Raw failure by 178,007,051 memory / 70,903,691,577 CPU |

A genuine one-item signed `CompletePublished` control consumed the thread
while preserving the publication as one exact reference input:

| Measurement                          |                                                        Exact value |
| ------------------------------------ | -----------------------------------------------------------------: |
| signed bytes / byte margin           |                                                      7,771 / 8,613 |
| signed CBOR SHA-256                  | `8ec9d1d8155b6dad39504584dfcede987848328d01662d342e611a7b7d2f3ff2` |
| fee                                  |                                                            542,885 |
| execution memory / CPU               |                                              521,130 / 209,629,043 |
| reserve margins memory / CPU         |                                         12,678,870 / 7,790,370,957 |
| inputs / refs / outputs / collateral |                                                      2 / 1 / 2 / 1 |
| vkeys / redeemers                    |                                                              1 / 1 |
| output lovelace / required min-Ada   |                                              1,512,810 / 1,499,880 |
| output value bytes / margin          |                                                         73 / 4,927 |
| witnessed proof wall times           |                                               159.751 / 140.329 ms |
| witnessed half-maturity margins      |                               302,399,840.249 / 302,399,859.671 ms |

Two fresh Vitest processes produced that identical canonical tuple and signed
CBOR digest. Their paired witnessed publication wall times were 19.133 ms and
19.636 ms. Wall times differ with host scheduling and are evidence that the
operation completed within the timing envelope, not exact reproducibility
claims. The confirmed publication output remained unspent after consumption.

### 3. Minimum transparent multi-output publication

Structurally N/A for Q13. The complete 296-input typed publication already
fits one signed 16,384-byte transaction at 12,305 bytes with 4,079 bytes of
margin. Splitting it across outputs would not relieve the full-list execution
performed by `CompletePublished`, so it cannot close the actual limiting
constraint and is not added as a second protocol representation.

### 4. Incremental traversal

Implemented as the authenticated `FoldStart` / `FoldNext` traversal. The
caller supplies the complete preimage; the builder derives canonical openings,
and the validator authenticates one item at a time against the unchanged
complete commitment. A true 20-input emulator run confirmed all 19
intermediate datums and the terminal step-03 datum:

| Item | Mode     | Bytes |     Fee |  Memory |         CPU | Tx margin | Reserve memory margin | Reserve CPU margin | Min-Ada margin |
| ---: | -------- | ----: | ------: | ------: | ----------: | --------: | --------------------: | -----------------: | -------------: |
|    0 | start    | 8,029 | 569,136 | 704,217 | 266,707,958 |     8,355 |            12,495,783 |      7,733,292,042 |         12,930 |
|    1 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|    2 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|    3 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|    4 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|    5 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|    6 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|    7 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|    8 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|    9 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|   10 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|   11 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|   12 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|   13 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|   14 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|   15 | next     | 8,028 | 569,488 | 709,395 | 268,055,725 |     8,356 |            12,490,605 |      7,731,944,275 |         12,930 |
|   16 | next     | 7,960 | 565,885 | 701,885 | 265,581,514 |     8,424 |            12,498,115 |      7,734,418,486 |         12,930 |
|   17 | next     | 7,960 | 565,885 | 701,885 | 265,581,514 |     8,424 |            12,498,115 |      7,734,418,486 |         12,930 |
|   18 | next     | 7,960 | 565,885 | 701,885 | 265,581,514 |     8,424 |            12,498,115 |      7,734,418,486 |         12,930 |
|   19 | terminal | 7,955 | 567,813 | 729,467 | 273,306,168 |     8,429 |            12,470,533 |      7,726,693,832 |         34,480 |

Every fold transaction has 2 inputs, 0 reference inputs, 2 outputs, 1
collateral input, 1 vkey witness, 1 redeemer, a 73-byte output value, and a
4,927-byte value margin. The worst envelope remains safe: 8,029 signed bytes,
8,355-byte transaction margin, 729,467 memory, 273,306,168 CPU, 12,470,533
release-memory margin, 7,726,693,832 release-CPU margin, and 12,930 lovelace
above min-Ada. In the final full replay the complete 20-step loop witnessed
4,301.466 ms, leaving 302,395,698.534 ms against half canonical maturity;
these host timings are noncanonical observations.

## Semantic equivalence and lifecycle controls

All three handlers authenticate exactly the same
`bounded_collection_v1.from_items(0, map(inputs, encode_midgard_tx_input))`
commitment and forward exactly the caller-indexed committed input. The Aiken
gate proves direct and published routes reach identical step-03 state; fold
tests prove omission, backward replay, reorder, substitution, wrong proof
metadata, and trailing data are rejected. SDK opening verification mirrors
those controls.

The final real-handler emulator replay passed 4/4 cases using the bound
blueprint:

| Case                                                             | Vitest case time |                             Handler wall evidence | Result |
| ---------------------------------------------------------------- | ---------------: | ------------------------------------------------: | ------ |
| direct lifecycle through permanent proof token and block removal |         2,287 ms |               witnessed direct step-02 153.748 ms | PASS   |
| genuine signed `CompletePublished` publication and consumption   |         1,582 ms | witnessed publication 25.000 ms; proof 195.566 ms | PASS   |
| true 20-input fold through every intermediate root and terminal  |         5,757 ms |                  witnessed fold loop 4,301.466 ms | PASS   |
| valid-block negative                                             |         2,269 ms |                    witnessed fail-closed controls | PASS   |

The direct signed consuming-proof control was 7,768 bytes with 8,616 bytes
of margin, signed-CBOR SHA-256
`48b8409e9d6cd6c7bbfd8ad8384eac15094971197694bfeea24b5e749fbae507`,
fee 535,855, execution 437,378 memory / 179,759,406 CPU, 0 reference inputs,
and otherwise the same 2-input / 2-output / 1-collateral / 1-vkey /
1-redeemer shape. The valid-block case rejects a forged spend-input opening
before build, rejects the true existing-output verdict, rejects a stripped
outputs preimage, and leaves both thread and queued block unchanged.

The final full replay witnessed 13.96 seconds overall (11.90 seconds in the
four cases). These elapsed values are noncanonical. Focused ABI,
signed-publication, inspection, applied-hash, typecheck, lint, and format gates
provide the remaining executable binding.

## Conclusion and C70 invalidation boundary

The exact limiting constraint is direct-proof execution memory: 20 inputs are
already 166,688 units beyond the release reserve, while the required
296-script-spend Cardano shape is far beyond both raw execution limits. A
single-output publication fits the byte envelope even at 296, making tier 3
structurally unnecessary, but its complete consumer does not fit. The ordered,
authenticated fold is therefore necessary and is the implemented release
fallback while preserving the 19-or-fewer complete-item path.

This is local, deterministic Q13 evidence, not a C70 target-network snapshot.
Any change to the bound blueprint, source digests, compiler/environment,
applied deployment tuple, consensus profile, final release capability floor,
or eventual C70 Preprod parameters invalidates the numeric proof-fit binding
and requires replay before CG5. In particular, final C70 evidence must replace
this caveat with the recorded target-network snapshot; this artifact does not
pre-authorize mainnet deployment or overclaim an end-to-end 296-step live run.
