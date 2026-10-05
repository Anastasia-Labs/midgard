import {
  encodeMidgardNativeTxCanonical,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";
import {
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  detectDistinctAssetAccumulationCanonicalViolations,
  prepareDistinctAssetAccumulationArtifact,
} from "../src/distinct-asset-accumulation-limit/authenticated-replay.js";
import { prepareDistinctAssetAccumulationEvidence } from "../src/distinct-asset-accumulation-limit/family.js";
import { discoverDistinctAssetRetainedMutationCandidates } from "../src/distinct-asset-accumulation-limit/retained-value-and-mint.js";
import { forcedVerdictForRejection } from "../src/index.js";
import {
  buildForcedCrossingTrace,
  type CrossingScenario,
  type CrossingTrace,
  INPUT_CROSSING,
  inputCrossing,
  LIMIT,
  MINT_CROSSING,
  ORDER_KEY,
  OUTPUT_CROSSING,
  outputCrossing,
  outputReason,
  outputsAndMintCrossing,
} from "./forced-reason-coordinate-distinct-asset.setup.js";
import { commitDistinctAssetBlock } from "./forced-reason-coordinate-distinct-asset.thread.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";

/**
 * A forced AssetAccumulationLimit reason names the step of the validation
 * machine's ValueAndMint walk that first inserts a new unit once the
 * accumulator has seen the bound (16,384 units), and the distinct-asset
 * family's step-01 binds exactly that coordinate. The transactions here
 * really cross: sixteen inputs fold the bound, and the trace each proof opens
 * is the one block production builds. The node's verdict, the machine's
 * trace and the on-chain bind must name the same coordinate: one position
 * early names a unit already seen and convicts, the written coordinate is
 * refused on chain, and a coordinate that is not the committed one is
 * refused at bind.
 */

const crossingCandidate = (retained: readonly [Buffer, Buffer][]) => {
  const candidates = discoverDistinctAssetRetainedMutationCandidates(
    retained.map(([key, value]) => ({ key, value })),
  );
  return candidates[candidates.length - 1]!;
};

/** The node's Phase A and Phase B rejection of `crossing` as a normal tx. */
const nodeNormalRejection = async (crossing: CrossingScenario) => {
  const phaseA = await Effect.runPromise(
    runPhaseAValidation(
      [
        {
          sourceKind: "normal",
          txId: crossing.transactionId,
          txCbor: encodeMidgardNativeTxCanonical(crossing.nativeTx),
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
            [],
          ),
          arrivalSeq: 0n,
          createdAt: new Date(0),
        },
      ],
      {
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      },
    ),
  );
  expect(phaseA.rejected).toHaveLength(0);
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [phaseA.accepted[0]!],
      new Map(
        crossing.ledger.map(([outRef, output]) => [
          outRef.toString("hex"),
          output,
        ]),
      ),
      { nowCardanoSlotNo: 0n, bucketConcurrency: 1, enforceScriptBudget: true },
    ),
  );
  expect(phaseB.rejected).toHaveLength(1);
  return phaseB.rejected[0]!;
};

describe("forced AssetAccumulationLimit coordinate the node writes", () => {
  const crossing = outputCrossing();
  let built: CrossingTrace;
  beforeAll(async () => {
    built = await buildForcedCrossingTrace(crossing, ORDER_KEY);
  }, 600_000);

  it("names the output crossing in the node, the machine and a normal tx alike", async () => {
    const verdict = await nodeForcedVerdict({
      transactionId: crossing.transactionId,
      forcedCanonicalCbor: crossing.forcedCanonicalCbor,
      ledger: crossing.ledger,
    });
    expect(verdict).toStrictEqual({
      ForcedTxInvalid: { reason: outputReason(OUTPUT_CROSSING) },
    });
    // The machine's trace stops at the same step: the last asset step is the
    // crossing, with the bound seen and the unit new.
    expect(built.trace.rejectionCode).toBe(RejectCodes.AssetCount);
    const candidate = crossingCandidate(built.retainedWitnesses);
    expect(candidate.coordinate).toStrictEqual({
      kind: "output",
      ...OUTPUT_CROSSING,
    });
    expect(candidate.control.value_accumulator.seen_asset_count).toBe(
      BigInt(LIMIT),
    );
    expect(candidate.action.evidence.mutation.delta_was_present).toBe(false);
    // A normal transaction of the same shape is rejected with the same
    // reason at the same coordinate.
    const normal = await nodeNormalRejection(crossing);
    expect(normal.code).toBe(RejectCodes.AssetCount);
    expect(normal.consensusPhase).toBe("valueAndMint");
    expect(normal.subject).toStrictEqual({
      arm: "OutputAssetAccumulationLimit",
      index: BigInt(OUTPUT_CROSSING.outputIndex),
      assetIndex: BigInt(OUTPUT_CROSSING.assetIndex),
    });
    expect(forcedVerdictForRejection(normal)).toStrictEqual(verdict);
  }, 600_000);

  it("names an input-fold crossing before value preservation, and the trace builds", async () => {
    const input = inputCrossing();
    expect(
      await nodeForcedVerdict({
        transactionId: input.transactionId,
        forcedCanonicalCbor: input.forcedCanonicalCbor,
        ledger: input.ledger,
      }),
    ).toStrictEqual({
      ForcedTxInvalid: {
        reason: {
          InputAssetAccumulationLimit: {
            input_index: BigInt(INPUT_CROSSING.inputIndex),
            asset_index: BigInt(INPUT_CROSSING.assetIndex),
          },
        },
      },
    });
    // Block production builds this trace; it stops in the input fold, at
    // the schedule position the verdict names.
    const inputTrace = await buildForcedCrossingTrace(input, ORDER_KEY);
    expect(inputTrace.trace.rejectionCode).toBe(RejectCodes.AssetCount);
    expect(
      crossingCandidate(inputTrace.retainedWitnesses).coordinate,
    ).toStrictEqual({ kind: "input", ...INPUT_CROSSING });
  }, 600_000);

  it("names the mint crossing when the outputs and mint alone exceed the bound", async () => {
    // No canonical-decode bound counts these units: the walk alone names
    // where the bound is crossed, for a forced and a normal tx alike.
    const both = outputsAndMintCrossing();
    const verdict = await nodeForcedVerdict({
      transactionId: both.transactionId,
      forcedCanonicalCbor: both.forcedCanonicalCbor,
      ledger: both.ledger,
    });
    expect(verdict).toStrictEqual({
      ForcedTxInvalid: {
        reason: {
          MintAssetAccumulationLimit: {
            mint_index: BigInt(MINT_CROSSING.mintIndex),
          },
        },
      },
    });
    const normal = await nodeNormalRejection(both);
    expect(normal.code).toBe(RejectCodes.AssetCount);
    expect(normal.consensusPhase).toBe("valueAndMint");
    expect(normal.subject).toStrictEqual({
      arm: "MintAssetAccumulationLimit",
      index: BigInt(MINT_CROSSING.mintIndex),
    });
  }, 600_000);

  it("convicts a coordinate one position early, where the unit was already seen", async () => {
    const early = {
      ...OUTPUT_CROSSING,
      assetIndex: OUTPUT_CROSSING.assetIndex - 1,
    };
    const f = await commitDistinctAssetBlock({
      crossing,
      built,
      committed: early,
    });
    // The complete replay finds the contradiction from L1 and DA alone.
    const artifact = await prepareDistinctAssetAccumulationArtifact(
      f.canonicalBlock,
    );
    expect(artifact.finding.coordinate).toStrictEqual({
      kind: "output",
      ...early,
    });
    expect(artifact.evidence.mutationWasPresent).toBe(true);
    let outRef = await f.init();
    outRef = await f.step01(outRef, artifact.finding.coordinate);
    outRef = await f.authenticateAndFold(outRef, artifact.finding.coordinate);
    const proof = await f.finalize(outRef, artifact.evidence);
    expect(proof.fraudProofUnit).toBeTruthy();
    await f.removeBlock();
    expect(
      await f.harness.proverLucid.utxosAtWithUnit(
        f.harness.contracts.stateQueue.spendingScriptAddress,
        f.setup.stateQueueBlockUnit,
      ),
    ).toHaveLength(0);
  }, 1_200_000);

  it("refuses another coordinate at bind and the written coordinate's award on chain", async () => {
    const f = await commitDistinctAssetBlock({
      crossing,
      built,
      committed: OUTPUT_CROSSING,
    });
    // The written coordinate is the crossing: the replay finds nothing.
    expect(
      await detectDistinctAssetAccumulationCanonicalViolations(
        f.canonicalBlock,
      ),
    ).toHaveLength(0);
    const threadOutRef = await f.init();
    // Step-01 binds the coordinate to the committed reason exactly: one
    // asset early is not the reason the leaf commits.
    await expectOnchainRefusal(
      async () =>
        await f.step01(
          threadOutRef,
          {
            kind: "output",
            ...OUTPUT_CROSSING,
            assetIndex: OUTPUT_CROSSING.assetIndex - 1,
          },
          true,
        ),
      {
        refusedBy: "fraud_proofs/distinct_asset_accumulation_limit/step_01",
        // `bind_exact_rejection_reason_v1`, reached from `bind_coordinate_v1`.
        check:
          /^expect cbor\.serialise\(actual\) == cbor\.serialise\(expected\)$/u,
      },
    );
    const written = { kind: "output" as const, ...OUTPUT_CROSSING };
    let outRef = await f.step01(threadOutRef, written);
    outRef = await f.authenticateAndFold(outRef, written);
    // The folds prove the crossing, so the thread's terminal state is not a
    // contradiction. Evidence claiming otherwise only passes the local gate;
    // the validator reads the folded state.
    const subject = SDK.forcedVerdictSubject({
      transactionId: crossing.transactionId.toString("hex"),
      sourceKey: ORDER_KEY,
      rejectionReason: outputReason(OUTPUT_CROSSING),
    });
    const claimed = prepareDistinctAssetAccumulationEvidence({
      finding: { subject, coordinate: written },
      traceStateHashHex: "00".repeat(32),
      workRootHex: "00".repeat(32),
      pre: {
        assetRootHex: "00".repeat(32),
        seenAssetCount: LIMIT - 1,
        nonzeroAssetCount: 0,
        cursor: OUTPUT_CROSSING.assetIndex,
      },
      post: {
        assetRootHex: "00".repeat(32),
        seenAssetCount: LIMIT,
        nonzeroAssetCount: 0,
        cursor: OUTPUT_CROSSING.assetIndex + 1,
      },
      mutationWasPresent: false,
    });
    await expectOnchainRefusal(async () => await f.finalize(outRef, claimed), {
      refusedBy: "fraud_proofs/distinct_asset_accumulation_limit/step_06",
      check: /^Validator returned false$/u,
    });
  }, 1_200_000);
});
