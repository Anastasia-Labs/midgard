import "./typed-reason-retained-classification.typed-reasons-classified-from-committed-retained-da.js";

import { GENESIS_HEADER_HASH } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  admitValidationTraceReplayContext,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildRetainedPlutusIdentityFixture,
  buildRetainedPlutusUnboundVariableFixture,
  captureRetainedPlutusIdentityOrigins,
  classifyRetainedReasonFixture,
} from "./support/retained-reason-classifier.js";
import {
  deploymentFingerprint,
  policy,
  releaseFinalityAuthority,
} from "./typed-reason-retained-classification.reason-case.js";

export const admitRetainedPlutus = async (
  claim: Parameters<typeof buildRetainedPlutusIdentityFixture>[0],
  options: Readonly<{
    sourceKind?: "normal" | "forced";
    program?: "identity" | "unboundVariable";
    omitEvent?: boolean;
    omitOriginAuthority?: boolean;
  }> = {},
) => {
  const fixture =
    options.program === "unboundVariable"
      ? await buildRetainedPlutusUnboundVariableFixture(claim)
      : await buildRetainedPlutusIdentityFixture(claim, options);
  const observation = authenticatedHeaderObservation(fixture.block);
  const daProvenance = {
    trustClass: "public_or_permissionless_da",
    sourceId: "retained-fixture/emulator",
    grade: "security",
  } as const;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor: fixture.block.payloadEnvelopeCbor,
    daProvenance,
    minimumConfirmationDepth: policy.confirmationDepth,
  });
  const predecessor = await admitCompleteCanonicalReplayPredecessor({
    value: {
      observation: authenticatedHeaderObservation(fixture.predecessor),
      payloadEnvelopeCborHex:
        fixture.predecessor.payloadEnvelopeCbor.toString("hex"),
      daProvenance,
    },
    currentEvidence: evidence,
    minimumConfirmationDepth: policy.confirmationDepth,
  });
  const transitionTraceEvents =
    options.sourceKind === "forced" && !options.omitOriginAuthority
      ? await captureRetainedPlutusIdentityOrigins(fixture, options)
      : undefined;
  const validationTraceReplay = await admitValidationTraceReplayContext({
    evidence,
    predecessor,
    transitionTraceEvents,
  });
  return {
    fixture,
    evidence,
    observation,
    context: { predecessor, validationTraceReplay, transitionTraceEvents },
  };
};

describe("ordinary retained Plutus replay admission", () => {
  it("admits an empty first block against the protocol genesis ledger root", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: GENESIS_HEADER_HASH,
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained-fixture/emulator",
        grade: "security",
      },
      minimumConfirmationDepth: policy.confirmationDepth,
    });
    const validationTraceReplay = await admitValidationTraceReplayContext({
      evidence,
    });
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        { validationTraceReplay },
      );
    expect(result.detections).toEqual([]);
  });

  it("reconstructs the existing identity program from retained sidecars and classifies exact agreement", async () => {
    const { fixture, evidence, observation, context } =
      await admitRetainedPlutus({ verdict: "accepted" });
    expect(fixture.replay.trace.verdict).toBe("accepted");
    expect(
      fixture.replay.trace.witnesses.some(
        ({ auxiliary }) => auxiliary?.kind === "cekCoreStep",
      ),
    ).toBe(true);
    expect(
      evidence.reconstruction.payload.block_body.cek_program_material.length,
    ).toBeGreaterThan(0);
    const { decision } = await classifyRetainedReasonFixture({
      observation,
      payloadEnvelopeCbor: fixture.block.payloadEnvelopeCbor,
      deploymentFingerprint,
      releaseFinalityAuthority,
      replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
      replayContext: context,
    });
    expect(decision).toMatchObject({
      decision: "healthy",
      headerHash: fixture.block.headerHash,
    });
    const replay =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    expect(replay.context?.validationTraceReplayDigest).toBe(
      context.validationTraceReplay.replayDigest,
    );
  });

  it("does not turn the shared Plutus/receive-language rejection hash into typed authority", async () => {
    const plutus = await admitRetainedPlutus({
      verdict: "rejected",
      reason: { PlutusExecutionFailed: { execution_index: 0n } },
    });
    const receive = await buildRetainedPlutusIdentityFixture({
      verdict: "rejected",
      reason: { ReceivePurposePlutusV3Forbidden: { execution_index: 0n } },
    });
    expect(receive.block.headerHash).toBe(plutus.fixture.block.headerHash);
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        plutus.evidence,
        plutus.context,
      );
    expect(result.detections).toEqual([]);
  });

  it("refuses missing, copied and cross-header replay authority", async () => {
    const { evidence, context } = await admitRetainedPlutus({
      verdict: "accepted",
    });
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence),
    ).rejects.toThrow(/admitted context/u);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        ...context,
        validationTraceReplay: { ...context.validationTraceReplay },
      }),
    ).rejects.toThrow(/not admitted/u);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        {
          ...evidence,
          headerHash: "e2".repeat(28),
        },
        context,
      ),
    ).rejects.toThrow(/challenged header|not admitted/u);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        validationTraceReplay: context.validationTraceReplay,
      }),
    ).rejects.toThrow(/not admitted/u);
    await expect(
      admitValidationTraceReplayContext({ evidence }),
    ).rejects.toThrow(/admitted predecessor/u);
  });

  it("re-admits the exact retained envelope instead of trusting a supplied reconstruction", async () => {
    const { evidence, context } = await admitRetainedPlutus({
      verdict: "accepted",
    });
    const readmitted = await admitValidationTraceReplayContext({
      evidence: {
        ...evidence,
        transactions: [],
        reconstruction: {
          ...evidence.reconstruction,
          transactions: [],
          sourceEvents: [],
        },
      },
      predecessor: context.predecessor,
    });
    expect(readmitted.replayDigest).toBe(
      context.validationTraceReplay.replayDigest,
    );
  });
});
