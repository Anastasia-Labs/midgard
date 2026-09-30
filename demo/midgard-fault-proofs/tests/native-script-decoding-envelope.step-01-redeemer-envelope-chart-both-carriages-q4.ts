import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardBoundedItemChunkProof,
} from "@al-ft/midgard-core";
import {
  type BoundedItemChunkProof,
  buildNativeScriptDecodingFaultProofContracts,
  NativeScriptDecodingStep01SpendRedeemer,
  parseFaultProofBlueprint,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES } from "../src/native-script-decoding/contracts.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  buildTransactionInclusionFixture,
  membershipProofBranchLevelsReachableWithWork,
  PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
} from "./support/submit-init-emulator-fixtures.js";
import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

/** The consensus floor every fault-proof step transaction must fit. */
export const L1_ENVELOPE_BYTES =
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

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
export const STEP_TX_OVERHEAD_ALLOWANCE_BYTES = 2_048;

/** Same reference adversary as the max-proof-fit suite: 2^128 digests. */
export const ADVERSARY_LOG2_WORK = 128;

export const dataBytes = (hex: string): number => hex.length / 2;

/** Same opt-in measurement printing convention as `printProofFitV1`. */
export const printChart = (
  headline: string,
  values: Record<string, number>,
) => {
  if (process.env["MIDGARD_PRINT_PROOF_FIT"] !== "1") {
    return;
  }
  console.log(`${headline}: ${JSON.stringify(values)}`);
};

export const proofFromCbor = (proofCborHex: string) =>
  Data.from(proofCborHex, Proof);

/** Shape of the fixture's `inclusion` payload (typed `unknown` at source). */
type FixtureInclusion = {
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

export const chunkProofToData = (
  proof: MidgardBoundedItemChunkProof,
): BoundedItemChunkProof => ({
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

  it("proves Q3 by arithmetic and fits every applied step in the publication host", async () => {
    // Measure the fully applied production family, including its shared policies.
    const {
      nativeScriptDecoding: { steps },
    } = await Effect.runPromise(
      buildNativeScriptDecodingFaultProofContracts({
        blueprint: parseFaultProofBlueprint(blueprint),
        network,
        hubOraclePolicyId: "55".repeat(28),
        fraudProofCataloguePolicyId: "66".repeat(28),
      }),
    );

    // Six distinct scripts: equal hashes would mean a parameter list was
    // mis-ordered into another step's (the #609/#610 guards check arity, not
    // order).
    expect(new Set(steps.map((step) => step.spendingScriptHash)).size).toBe(6);

    const appliedSizes = Object.fromEntries(
      steps.map((step, index) => [
        Object.keys(NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES)[index]!,
        step.spendingScriptCBOR.length / 2,
      ]),
    );
    printChart("native-script-decoding applied validator sizes", appliedSizes);

    for (const [index, step] of steps.entries()) {
      const appliedBytes = step.spendingScriptCBOR.length / 2;
      expect(
        appliedBytes,
        `applied step_0${(index + 1).toString()} exceeds the L1 envelope before transaction overhead`,
      ).toBeLessThan(L1_ENVELOPE_BYTES);
      // Reference deployment itself also fits the consensus-floor envelope;
      // consumption transactions source the script through readFrom.
      expect(
        appliedBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
        `applied step_0${(index + 1).toString()} no longer fits a reference-script publication transaction`,
      ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);
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
      l2_transaction_source_cbor: inclusion.l2TransactionSourceCbor,
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
          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
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
    printChart("native-script-decoding step-01 chart", {
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
    // list. The load-bearing claim is the ratio to the carried carriage at
    // adversarial depth, asserted immediately below.
    expect(deepBytes.publishedChunk).toBeLessThan(
      deepBytes.redeemerCarried / 2,
    );
  });
});
