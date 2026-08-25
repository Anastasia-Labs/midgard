/**
 * `native-script-decoding` envelope/frontier measurement suite (offchain
 * design §8.2 item 1; #635).
 *
 * This file runs FIRST in the family's §9 build order because its step-02
 * check is the one place the offchain wave can still escalate (design §2.3):
 * the step-02 redeemer is the family's only step whose worst admissible
 * instance stacks three MPF membership openings, a full block header and a
 * forced leaf into one redeemer. If that worst case cannot fit the 16,384-byte
 * L1 fault-proof envelope, the fix is an on-chain format change on the wave
 * branch — a completeness finding to escalate, never something to absorb
 * offchain. Every other chart here feeds the §5.2 segment planner its byte
 * frontiers.
 *
 * Methodology is inherited from `submit-init-emulator-max-proof-fit.test.ts`:
 * measure a real instance at branch depth 0 and at the grinded adversarial
 * depth, derive the constant marginal cost per further branch level from the
 * difference, and turn the envelope into an exact exhaustion depth. The
 * conclusion for work-bounded axes follows the Q1X-F5 convention — record that
 * a 2^128 adversary can exhaust the envelope, do not pretend safety.
 *
 * Everything here is `Data.to` byte measurement plus blueprint arithmetic; no
 * transaction is evaluated, so this file does not pay the wasm UPLC heap tax
 * its emulator siblings isolate against. The redeemer-to-transaction gap is
 * covered by `STEP_TX_OVERHEAD_ALLOWANCE_BYTES` below and re-measured against
 * complete signed transactions by the §8.2(4–7) emulator journeys.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  deriveMidgardNativeTxProofSourceV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  type MidgardBoundedItemChunkProofV1,
} from "@al-ft/midgard-core";
import {
  AddressData,
  addressDataFromBech32,
  type BoundedItemChunkProofV1,
  type FieldCarriageV1,
  NativeScriptDecodingStep01SpendRedeemer,
  NativeScriptDecodingStep02SpendRedeemer,
  NativeScriptDecodingStep03SpendRedeemer,
  NativeScriptDecodingStep04SpendRedeemer,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1 } from "../src/native-script-decoding/contracts-v1.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  buildTransactionInclusionFixture,
  insertAdversarialMembershipSiblings,
  membershipProofBranchLevelsReachableWithWork,
  outputReferenceCbor,
  PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildNativeScriptDecodingChainV1,
  EMULATOR_PROTOCOL_PARAMETERS,
  h32,
  makeHeader,
  makeNativeTx,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

/** The consensus floor every fault-proof step transaction must fit. */
const L1_ENVELOPE_BYTES = MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;

/**
 * Redeemer-to-transaction gap: everything a step transaction carries besides
 * the measured spend redeemer — tx skeleton, thread input/output with inline
 * datums (the 15-field scan state is ~260 bytes a side), collateral, change,
 * fee, one signature, and the tiny mint/withdraw redeemers. The §8.2(4–7)
 * emulator journeys measure the same steps as complete signed transactions
 * and are the binding check on this allowance; a journey transaction whose
 * non-redeemer bytes exceed this number must fail there, not be absorbed by
 * quietly raising it here.
 */
const STEP_TX_OVERHEAD_ALLOWANCE_BYTES = 2_048;

/** Same reference adversary as the max-proof-fit suite: 2^128 digests. */
const ADVERSARY_LOG2_WORK = 128;

/**
 * Applied compiled sizes pinned from the wave-branch blueprint (patched fork,
 * design §2.3). This is the suite's first pinned datum: a byte drift in any
 * step validator invalidates every frontier this file derives.
 */
const EXPECTED_UNAPPLIED_SIZES_BYTES = {
  step01: 6_783,
  step02: 11_507,
  // 24,862 before the #633 §7.2 BindOutOfDomain closing arm landed on
  // step-03 (2026-08-25); still over the 16,384 inline envelope, so the Q3
  // reference-scripts conclusion is unchanged.
  step03: 25_767,
  step04: 1_673,
} as const;

const dataBytes = (hex: string): number => hex.length / 2;

/** Same opt-in measurement printing convention as `printProofFitV1`. */
const printChartV1 = (headline: string, values: Record<string, number>) => {
  if (process.env["MIDGARD_PRINT_PROOF_FIT"] !== "1") {
    return;
  }
  console.log(`${headline}: ${JSON.stringify(values)}`);
};

const proofFromCbor = (proofCborHex: string) => Data.from(proofCborHex, Proof);

/** Shape of the fixture's `inclusion` payload (typed `unknown` at source). */
type FixtureInclusion = {
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

const chunkProofToData = (
  proof: MidgardBoundedItemChunkProofV1,
): BoundedItemChunkProofV1 => ({
  version: BigInt(proof.version),
  field_index: BigInt(proof.fieldIndex),
  item_index: BigInt(proof.itemIndex),
  total_length: BigInt(proof.totalLength),
  chunk_index: BigInt(proof.chunkIndex),
  chunk: Buffer.from(proof.chunk).toString("hex"),
  frontier: proof.frontier.peaks.map((peak) => ({
    height: BigInt(peak.height),
    hash: Buffer.from(peak.hash).toString("hex"),
  })),
  siblings: proof.siblings.map((sibling) =>
    Buffer.from(sibling).toString("hex"),
  ),
});

describe("native-script-decoding compiled sizes and deployability (Q3)", () => {
  const blueprint = readBlueprint(realBlueprintPath);

  it("pins the four unapplied validator sizes to the design §2.3 numbers", () => {
    for (const [step, title] of Object.entries(
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1,
    )) {
      const validator = blueprint.validators.find(
        (candidate) => candidate.title === title,
      );
      expect(validator, title).toBeDefined();
      expect(
        validator!.compiledCode.length / 2,
        `${title}: unapplied compiled size drifted from the design §2.3 pin — every frontier below is derived against the pinned validators`,
      ).toBe(
        EXPECTED_UNAPPLIED_SIZES_BYTES[
          step as keyof typeof EXPECTED_UNAPPLIED_SIZES_BYTES
        ],
      );
    }
  });

  it("proves Q3 by arithmetic and fits every applied step in the publication host", async () => {
    // Q3 is arithmetic, not preference: step 03 alone exceeds the whole
    // fault-proof envelope, so inline carriage of the validator is impossible
    // and all four steps deploy as reference scripts for uniformity.
    expect(EXPECTED_UNAPPLIED_SIZES_BYTES.step03).toBeGreaterThan(
      L1_ENVELOPE_BYTES,
    );

    // Parameter VALUES do not change applied sizes (all three parameter kinds
    // are fixed-width: 28-byte policies/hashes and a constant-shape address),
    // so a dummy-parameterized chain measures the deployed bytes exactly.
    const fraudProofTokenAddressData = await Effect.runPromise(
      addressDataFromBech32(
        credentialToAddress(network, scriptHashToCredential("22".repeat(28))),
      ).pipe(
        Effect.map((addressData) =>
          Data.from(Data.to(addressData, AddressData)),
        ),
      ),
    );
    const steps = buildNativeScriptDecodingChainV1({
      realBlueprint: blueprint,
      computationThreadPolicyId: "11".repeat(28),
      fraudProofPolicyId: "33".repeat(28),
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId: "44".repeat(28),
      hubOraclePolicyId: "55".repeat(28),
    });

    // Four distinct scripts: equal hashes would mean a parameter list was
    // mis-ordered into another step's (the #609/#610 guards check arity, not
    // order).
    expect(new Set(steps.map((step) => step.spendingScriptHash)).size).toBe(4);

    for (const [index, step] of steps.entries()) {
      const appliedBytes = step.spendingScriptCBOR.length / 2;
      expect(appliedBytes).toBeGreaterThanOrEqual(
        Object.values(EXPECTED_UNAPPLIED_SIZES_BYTES)[index]!,
      );
      // The oversized-publication host (semantic-resolver precedent): each
      // applied step must publish in one transaction under the raised
      // emulator maxTxSize, parked at the unspendable credential; consuming
      // transactions stay inside the 16,384 envelope via readFrom.
      expect(
        appliedBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
        `applied step_0${(index + 1).toString()} no longer fits the oversized reference-script publication host`,
      ).toBeLessThanOrEqual(EMULATOR_PROTOCOL_PARAMETERS.maxTxSize);
    }
  });
});

describe("step-01 redeemer envelope chart (both carriages, Q4)", () => {
  const buildStep01RedeemerBytes = (
    inclusion: FixtureInclusion,
  ): { readonly redeemerCarried: number; readonly publishedChunk: number } => {
    const argsHead = {
      input_index: 0n,
      output_index: 0n,
      hub_ref_input_index: 0n,
      state_queue_node_ref_input_index: 1n,
      native_tx_id: inclusion.nativeTxId,
      native_tx_compact_cbor: inclusion.nativeTxCompactCbor,
      transactions_phas_root: inclusion.transactionsPhasRoot,
    };
    const redeemerCarried: NativeScriptDecodingStep01SpendRedeemer = {
      Continue: [
        {
          BindNormalTransaction: {
            carriage: {
              RedeemerCarriedInclusion: [
                {
                  ...argsHead,
                  tx_membership_proof: proofFromCbor(
                    inclusion.txMembershipProofCbor,
                  ),
                  inclusion_proof_script_withdraw_redeemer_index: 0n,
                },
              ],
            },
          },
        },
      ],
    };
    const chunkCount = Math.max(
      1,
      Math.ceil(
        dataBytes(inclusion.txMembershipProofCbor) /
          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
      ),
    );
    const publishedChunk: NativeScriptDecodingStep01SpendRedeemer = {
      Continue: [
        {
          BindNormalTransaction: {
            carriage: {
              PublishedChunkInclusion: [
                {
                  ...argsHead,
                  ordered_chunk_reference_input_indices: Array.from(
                    { length: chunkCount },
                    (_unused, index) => BigInt(index + 2),
                  ),
                },
              ],
            },
          },
        },
      ],
    };
    return {
      redeemerCarried: dataBytes(
        Data.to(redeemerCarried, NativeScriptDecodingStep01SpendRedeemer),
      ),
      publishedChunk: dataBytes(
        Data.to(publishedChunk, NativeScriptDecodingStep01SpendRedeemer),
      ),
    };
  };

  it("charts both carriages at adversarial membership depth and derives the exhaustion depth", async () => {
    const shallow = await buildTransactionInclusionFixture({});
    const deep = await buildTransactionInclusionFixture({
      adversarialBranchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    const shallowBytes = buildStep01RedeemerBytes(
      shallow.tx1.inclusion as FixtureInclusion,
    );
    const deepBytes = buildStep01RedeemerBytes(
      deep.tx1.inclusion as FixtureInclusion,
    );

    // The compact CBOR is commitment-bounded (§3: body field COMMITMENTS plus
    // witness-set hash and validity code), so the proof source never grows
    // with transaction content — the membership proof is the ONLY axis the
    // adversary moves. Pin that boundedness before using it.
    expect(
      dataBytes((deep.tx1.inclusion as FixtureInclusion).nativeTxCompactCbor),
    ).toBeLessThan(512);

    // The adversarial-depth instance itself must fit, with the overhead
    // allowance, and with real margin left for the derivation to mean much.
    expect(
      deepBytes.redeemerCarried + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);

    // Marginal Plutus-data cost of one further branch level, measured — the
    // MPF proof is a definite list of fixed-shape steps, so this is constant.
    const perLevelBytes =
      (deepBytes.redeemerCarried - shallowBytes.redeemerCarried) /
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS;
    expect(perLevelBytes).toBeGreaterThan(0);

    const exhaustionDepth =
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS +
      Math.floor(
        (L1_ENVELOPE_BYTES -
          STEP_TX_OVERHEAD_ALLOWANCE_BYTES -
          deepBytes.redeemerCarried) /
          perLevelBytes,
      );
    printChartV1("native-script-decoding step-01 chart", {
      shallowRedeemerCarriedBytes: shallowBytes.redeemerCarried,
      deepRedeemerCarriedBytes: deepBytes.redeemerCarried,
      deepPublishedChunkBytes: deepBytes.publishedChunk,
      perLevelBytes,
      exhaustionDepth,
    });
    expect(
      exhaustionDepth,
      "step-01 redeemer-carried exhaustion depth fell below the grinded fixture depth",
    ).toBeGreaterThanOrEqual(ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS);
    // Measured 2026-08-25: per-level 124.4 bytes (a branch step re-encoded as
    // Plutus data measures slightly UNDER the 139-byte raw-CBOR constant — the
    // constr wrapper is cheaper than the CBOR map it replaces), exhaustion
    // depth 111. Bands, not exact pins: the fixture's non-proof bytes may
    // shift a little with compact-encoding changes.
    expect(perLevelBytes).toBeGreaterThanOrEqual(100);
    expect(perLevelBytes).toBeLessThanOrEqual(
      PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
    );
    // The claim this chart CAN make (redeemer bytes only; complete signed
    // transactions are re-measured by the §8.2(4–7) journeys): every branch
    // depth the 2^128 reference adversary can force (level 32) still fits the
    // redeemer-carried carriage with room to spare. If this ever flips, the
    // carried carriage stops covering work-feasible blocks and the
    // published-chunk carriage (Q4) becomes mandatory rather than an
    // optimization — restate the family's finding, do not widen the allowance.
    expect(
      exhaustionDepth,
      "a work-feasible (2^128) membership depth no longer fits the redeemer-carried carriage; the published-chunk carriage is now mandatory for deep blocks — restate the family finding",
    ).toBeGreaterThan(
      membershipProofBranchLevelsReachableWithWork(ADVERSARY_LOG2_WORK),
    );

    // Q4's second carriage is the answer to that exhaustibility: the
    // published-chunk redeemer replaces the proof with reference-input
    // indices, so its size is depth-independent up to the logarithmic index
    // list. It must stay an order of magnitude under the carried carriage at
    // adversarial depth.
    expect(deepBytes.publishedChunk).toBeLessThan(512);
    expect(deepBytes.publishedChunk).toBeLessThan(
      deepBytes.redeemerCarried / 2,
    );
  });
});

describe("step-02 redeemer envelope chart (THE escalation-capable check, §2.3)", () => {
  /**
   * Worst admissible step-02 instance: forced-source thread (both extra
   * openings live), forced leaf carrying a real §3 proof-source triple and the
   * worst `OperatorVerdictV1` arm, and all three MPF proofs at the grinded
   * adversarial branch depth.
   */
  const buildStep02WorstRedeemerBytes = async (
    branchLevels: number,
  ): Promise<number> => {
    const txOrderId = { transactionId: h32("c1"), outputIndex: 0n };
    const forcedKey = outputReferenceCbor({
      transactionId: txOrderId.transactionId,
      outputIndex: txOrderId.outputIndex,
    });
    const eventToStepKey = Buffer.alloc(40, 0xe2);
    const transitionStepKey = Buffer.from("03", "hex");

    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(forcedKey, Buffer.from("f0", "hex"));
    await trie.insert(eventToStepKey, Buffer.from("f1", "hex"));
    await trie.insert(transitionStepKey, Buffer.from("f2", "hex"));
    await insertAdversarialMembershipSiblings({
      trie,
      targets: [
        { key: forcedKey, domain: 0x0b01 },
        { key: eventToStepKey, domain: 0x0b02 },
        { key: transitionStepKey, domain: 0x0b03 },
      ],
      branchLevels,
    });
    const proveData = async (key: Buffer) =>
      proofFromCbor(
        Buffer.from((await trie.prove(key)).toCBOR()).toString("hex"),
      );
    const forcedProof = await proveData(forcedKey);
    const eventToStepProof = await proveData(eventToStepKey);
    const transitionStepProof = await proveData(transitionStepKey);

    // Real commitment-bounded proof source, derived from a real native tx —
    // not synthesized bytes, so a widening of the §3 compact encoding shows up
    // here as a measured regression.
    const forcedNativeTx = makeNativeTx({
      spendInputCbors: [
        { transactionId: h32("c3"), outputIndex: 0n },
        { transactionId: h32("c4"), outputIndex: 1n },
        { transactionId: h32("c5"), outputIndex: 2n },
      ].map(outputReferenceCbor),
      fee: 1_000_000n,
      referenceByte: "c6",
      outputByte: "c7",
      witnessByte: "c8",
    });
    const source = deriveMidgardNativeTxProofSourceV1(forcedNativeTx);

    const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
    const header = makeHeader(
      "77".repeat(28),
      1_700_000_000_000,
      h32("d1"),
      2n,
    );
    const redeemer: NativeScriptDecodingStep02SpendRedeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          header,
          event_to_step_membership: {
            domain: "EventToStepRootDomain",
            root: h32("d2"),
            phas_root: h32("d3"),
            count: 65_535n,
            key: eventKey,
            value: { step_index: 65_535n, phase: "ForcedTransaction" },
            proof: eventToStepProof,
          },
          transition_step_membership: {
            domain: "TransitionTraceRootDomain",
            root: h32("d4"),
            phas_root: h32("d5"),
            count: 65_535n,
            key: 65_535n,
            value: {
              schema_version: 1n,
              step_index: 65_535n,
              event_key: eventKey,
              phase: "ForcedTransaction",
              pre_utxos_root: h32("d6"),
              post_utxos_root: h32("d7"),
            },
            proof: transitionStepProof,
          },
          forced_membership: {
            domain: "ForcedTransactionsV1RootDomain",
            root: h32("d8"),
            phas_root: h32("d9"),
            count: 65_535n,
            key: txOrderId,
            value: {
              tx_id: h32("da"),
              source: {
                compact_cbor: Buffer.from(source.compactCbor).toString("hex"),
                witness_set_compact_cbor: Buffer.from(
                  source.witnessSetCompactCbor,
                ).toString("hex"),
                field_preimage_lengths_cbor: Buffer.from(
                  source.fieldPreimageLengthsCbor,
                ).toString("hex"),
              },
              // Worst verdict arm: two integer payloads (the 47-arm catalogue
              // carries at most two small integers, #633).
              verdict: {
                ForcedTxInvalid: {
                  reason: {
                    OutputAssetAccumulationLimit: {
                      output_index: 65_535n,
                      asset_index: 65_535n,
                    },
                  },
                },
              },
            },
            proof: forcedProof,
          },
          chosen_outpoint_source_kind: 0n,
          chosen_outpoint_cursor: 65_535n,
        },
      ],
    };
    return dataBytes(
      Data.to(redeemer, NativeScriptDecodingStep02SpendRedeemer),
    );
  };

  it("fits the worst forced-leaf instance at adversarial depth — or escalates to the wave branch", async () => {
    const shallowBytes = await buildStep02WorstRedeemerBytes(0);
    const deepBytes = await buildStep02WorstRedeemerBytes(
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    );

    // THE gate. A failure here is a wave-branch on-chain format finding
    // (design §2.3/§9): the step-02 argument layout would need to move
    // openings out of the single redeemer. It must be escalated, not absorbed
    // by shrinking the fixture or the overhead allowance.
    expect(
      deepBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
      "ESCALATE(#635 → wave branch): worst-case step-02 redeemer no longer fits the L1 fault-proof envelope",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);
    // "Fits" alone is not the design's claim — §2.3 expects real margin, so a
    // creeping regression surfaces before it becomes an escalation.
    expect(
      deepBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES + 1_024,
      "worst-case step-02 margin dropped below 1 KiB — investigate before the next widening lands",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);

    // Exhaustion arithmetic over the one adversary-movable axis. Step 02
    // stacks THREE proofs, so its per-level cost is three times step-01's and
    // its exhaustion depth proportionally shallower — still work-bounded, and
    // recorded under the same Q1X-F5 convention.
    const perLevelBytes =
      (deepBytes - shallowBytes) / ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS;
    expect(perLevelBytes).toBeGreaterThan(0);
    const exhaustionDepth =
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS +
      Math.floor(
        (L1_ENVELOPE_BYTES - STEP_TX_OVERHEAD_ALLOWANCE_BYTES - deepBytes) /
          perLevelBytes,
      );
    printChartV1("native-script-decoding step-02 chart", {
      shallowBytes,
      deepBytes,
      perLevelBytes,
      exhaustionDepth,
    });
    expect(exhaustionDepth).toBeGreaterThanOrEqual(
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    );
    // Measured 2026-08-25: per-level 333.6 bytes (three stacked proofs),
    // exhaustion depth 37 against the work-reachable level 32. This is the
    // SHARP number of the whole suite: step 02 has no published-chunk
    // fallback, so if the exhaustion depth ever drops to 32 or below, a
    // work-feasible adversarial block exists whose worst forced-leaf opening
    // cannot reach L1 at all — that is the §2.3 wave-branch escalation, with
    // only ~5 levels of margin today. Watch it, do not absorb it.
    expect(perLevelBytes).toBeGreaterThanOrEqual(250);
    expect(perLevelBytes).toBeLessThanOrEqual(450);
    expect(
      exhaustionDepth,
      "ESCALATE(#635 → wave branch): a work-feasible (2^128) membership depth no longer fits the step-02 redeemer, and step 02 has no alternative carriage",
    ).toBeGreaterThan(
      membershipProofBranchLevelsReachableWithWork(ADVERSARY_LOG2_WORK),
    );
  }, 600_000);
});

describe("step-03 redeemer envelope chart (windows, frames, tier frontier)", () => {
  // Adversarial reference-script item at the §8 aggregate-field ceiling:
  // 32,768 bytes is `maxTransactionAggregateFieldBytes`, the widest a single
  // output field — and so any single item inside it — can commit to. Chunked
  // at the §8.4 stride this is 8 full windows.
  const ADVERSARIAL_ITEM_BYTES = 32_000;
  const item = buildMidgardBoundedItemV1({
    fieldIndex: 4,
    itemIndex: 0,
    bytes: Buffer.alloc(ADVERSARIAL_ITEM_BYTES, 0x82),
  });
  /**
   * The scan control is the canonically-encoded 15-field thread state (the
   * v1/v2 wire vectors pin it at ~120 bytes); 256 bytes is a deliberate
   * over-allowance so this chart cannot be invalidated by a field widening
   * alone. The emulator journeys re-measure it with real control bytes.
   */
  const controlCbor = Buffer.alloc(256, 0x88).toString("hex");
  /**
   * Frames are PLANNER-bounded, not adversary-bounded: the §5.2 planner emits
   * at most 16 nodes per segment (Q-policy default) and a frame tail never
   * exceeds one token's bounded width. 16 frames with 64-byte tails is
   * therefore above the worst plan the planner may legally submit; suites 3–4
   * re-measure the frames axis against real plans.
   */
  const worstFrames = Array.from({ length: 16 }, () => ({
    tail: Buffer.alloc(64, 0x83).toString("hex"),
    kind: 3n,
    child_count: 65_535n,
    remaining: 65_535n,
    valid_count: 65_535n,
    required: 65_535n,
  }));

  it("fits the worst Scan window (two full chunk proofs) and the windowless Verdict", () => {
    const midChunk = chunkProofToData(
      buildMidgardBoundedItemChunkProofV1(item, 3),
    );
    const nextChunk = chunkProofToData(
      buildMidgardBoundedItemChunkProofV1(item, 4),
    );
    expect(dataBytes(midChunk.chunk)).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    expect(dataBytes(nextChunk.chunk)).toBe(
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
    );

    const scanRedeemer: NativeScriptDecodingStep03SpendRedeemer = {
      Continue: [
        {
          Scan: {
            input_index: 0n,
            output_index: 0n,
            control_cbor: controlCbor,
            chunk_proof: midChunk,
            next_chunk_proof: nextChunk,
            frames: worstFrames,
            step_budget: 16n,
          },
        },
      ],
    };
    const scanBytes = dataBytes(
      Data.to(scanRedeemer, NativeScriptDecodingStep03SpendRedeemer),
    );
    expect(
      scanBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
      "worst two-chunk Scan window no longer fits the L1 envelope; the §5.2 planner's window geometry has no smaller legal cut to fall back to",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);

    const verdictRedeemer: NativeScriptDecodingStep03SpendRedeemer = {
      Continue: [
        {
          Verdict: {
            input_index: 0n,
            output_index: 0n,
            control_cbor: controlCbor,
            chunk_proof: midChunk,
            next_chunk_proof: nextChunk,
          },
        },
      ],
    };
    const verdictBytes = dataBytes(
      Data.to(verdictRedeemer, NativeScriptDecodingStep03SpendRedeemer),
    );
    expect(verdictBytes).toBeLessThan(scanBytes);
    expect(verdictBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES).toBeLessThanOrEqual(
      L1_ENVELOPE_BYTES,
    );
  });

  it("charts BindOutpoint at adversarial ledger depth and derives the tier-1 opening frontier", async () => {
    // Real deep ledger-trie proof for the accused outpoint key.
    const accusedKey = outputReferenceCbor({
      transactionId: h32("e1"),
      outputIndex: 0n,
    });
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(accusedKey, Buffer.from("e0", "hex"));
    await insertAdversarialMembershipSiblings({
      trie,
      targets: [{ key: accusedKey, domain: 0x0c01 }],
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    const ledgerProof = proofFromCbor(
      Buffer.from((await trie.prove(accusedKey)).toCBOR()).toString("hex"),
    );
    const firstChunk = chunkProofToData(
      buildMidgardBoundedItemChunkProofV1(item, 0),
    );
    const compactTx = makeNativeTx({
      spendInputCbors: [
        { transactionId: h32("e2"), outputIndex: 0n },
        { transactionId: h32("e3"), outputIndex: 1n },
      ].map(outputReferenceCbor),
      fee: 1_000_000n,
      referenceByte: "e4",
      outputByte: "e5",
      witnessByte: "e6",
    });
    const compactCborHex = Buffer.from(
      deriveMidgardNativeTxProofSourceV1(compactTx).compactCbor,
    ).toString("hex");

    const bindWithCarriage = (carriage: FieldCarriageV1): number => {
      const redeemer: NativeScriptDecodingStep03SpendRedeemer = {
        Continue: [
          {
            BindOutpoint: {
              input_index: 0n,
              output_index: 0n,
              subject_field_opening: {
                BodyFieldOpening: {
                  native_tx_compact_cbor: compactCborHex,
                  carriage,
                },
              },
              // Descriptor frontier: bounded token descriptors, generously
              // over-allowed at 128 bytes.
              descriptor_cbor: Buffer.alloc(128, 0x84).toString("hex"),
              ledger_membership_proof: ledgerProof,
              first_chunk_proof: firstChunk,
            },
          },
        ],
      };
      return dataBytes(
        Data.to(redeemer, NativeScriptDecodingStep03SpendRedeemer),
      );
    };

    // Tier 2 (RawUtxo): the production-default carriage for large subject
    // fields — must fit at adversarial ledger depth with the full chunk-0
    // proof aboard.
    const tier2Bytes = bindWithCarriage({ RawUtxo: { ref_input_index: 3n } });
    expect(
      tier2Bytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
      "tier-2 BindOutpoint at adversarial ledger depth no longer fits the L1 envelope",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);

    // Tier-1 frontier: the largest inline subject-field preimage BindOutpoint
    // can still carry. Derived, recorded, and bounded — the §5.2 planner picks
    // tier 1 only under this number; the spec-side per-field tier-1 cap is
    // 14,336 (§8.1), so the frontier can never exceed it.
    const emptyInlineBytes = bindWithCarriage({ Inline: { preimage: "" } });
    const probeBytes = 4_096;
    const probeInlineBytes = bindWithCarriage({
      Inline: { preimage: Buffer.alloc(probeBytes, 0x85).toString("hex") },
    });
    const perPreimageByte = (probeInlineBytes - emptyInlineBytes) / probeBytes;
    const tier1FrontierBytes = Math.min(
      14_336,
      Math.floor(
        (L1_ENVELOPE_BYTES -
          STEP_TX_OVERHEAD_ALLOWANCE_BYTES -
          emptyInlineBytes) /
          perPreimageByte,
      ),
    );
    expect(
      tier1FrontierBytes,
      "the tier-1 inline-opening frontier fell below two §8.4 chunks; the planner would lose its small-field fast path",
    ).toBeGreaterThanOrEqual(2 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
  }, 600_000);
});

describe("step-04 redeemer pin", () => {
  it("is constant-size and trivially inside the envelope", () => {
    const redeemer: NativeScriptDecodingStep04SpendRedeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 65_535n,
        },
      ],
    };
    const bytes = dataBytes(
      Data.to(redeemer, NativeScriptDecodingStep04SpendRedeemer),
    );
    expect(bytes).toBeLessThan(64);
  });
});
