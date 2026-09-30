import { describe, expect, it } from "vitest";

import { computeTransitionTraceL1EventEvidenceDigest } from "../src/transition-trace/replay-authority.js";
import {
  admitValidationTraceChallengeFromReplayContext,
  readValidationTraceReplaySelection,
} from "../src/validation-dispute/replay.js";
import {
  requireValidationTraceChallenge,
  W25_CHALLENGE_COORDINATE,
} from "../src/workflow/challenge-authority.js";
import {
  admitValidationTraceReplayContext,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  captureRetainedPlutusIdentityOrigins,
  classifyRetainedReasonFixture,
} from "./support/retained-reason-classifier.js";
import { admitRetainedPlutus } from "./typed-reason-retained-classification.ordinary-retained-plutus-replay-admission.js";
import {
  deploymentFingerprint,
  releaseFinalityAuthority,
} from "./typed-reason-retained-classification.reason-case.js";

describe("forced retained Plutus origin admission", () => {
  it("derives healthy execution from the retained program and admitted original bytes", async () => {
    const { evidence, context } = await admitRetainedPlutus(
      { verdict: "accepted" },
      { sourceKind: "forced" },
    );
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    expect(result.detections).toEqual([]);
    expect(result.context?.validationTraceEventEvidenceDigest).toBe(
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: context.transitionTraceEvents!,
      }),
    );
  });

  it("keeps replay identity stable across fresh captures of the same event evidence", async () => {
    const { fixture, evidence, context } = await admitRetainedPlutus(
      { verdict: "accepted" },
      { sourceKind: "forced" },
    );
    const transitionTraceEvents = await captureRetainedPlutusIdentityOrigins(
      fixture,
      { advanceCapture: true },
    );
    expect(transitionTraceEvents.snapshotDigest).not.toBe(
      context.transitionTraceEvents!.snapshotDigest,
    );
    expect(transitionTraceEvents.evidenceDigest).toBe(
      context.transitionTraceEvents!.evidenceDigest,
    );
    const validationTraceReplay = await admitValidationTraceReplayContext({
      evidence,
      predecessor: context.predecessor,
      transitionTraceEvents,
    });
    expect(validationTraceReplay.eventSnapshotDigest).toBe(
      transitionTraceEvents.snapshotDigest,
    );
    expect(validationTraceReplay.replayDigest).toBe(
      context.validationTraceReplay.replayDigest,
    );
    const previous =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    const refreshed =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        { ...context, transitionTraceEvents, validationTraceReplay },
      );
    expect(refreshed).toEqual(previous);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        ...context,
        transitionTraceEvents,
      }),
    ).rejects.toThrow(/not admitted/u);
  });

  it("selects the exact typed wrongful Plutus rejection and admits its private-material challenge", async () => {
    const { evidence, context } = await admitRetainedPlutus(
      {
        verdict: "rejected",
        reason: { PlutusExecutionFailed: { execution_index: 0n } },
      },
      { sourceKind: "forced" },
    );
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    expect(result.detections).toHaveLength(1);
    const input = {
      evidence,
      context: context.validationTraceReplay,
      predecessor: context.predecessor,
      transitionTraceEvents: context.transitionTraceEvents,
      detectionId: result.detections[0]!.detectionId,
    };
    const selected = readValidationTraceReplaySelection(input);
    expect(selected.coordinate).toEqual({
      domain: "transition_step",
      index: "0",
    });
    expect(Object.keys(selected).sort()).toEqual([
      "coordinate",
      "detectionId",
      "eventKeyCbor",
      "headerHash",
      "payloadEnvelopeSha256",
      "payloadSha256",
    ]);
    // This tests owner admission only. The production adapter independently
    // admits a real transcript before constructing these workflow coordinates.
    const coordinate = {
      schemaVersion: W25_CHALLENGE_COORDINATE,
      deploymentFingerprint,
      stateQueueObservationDigest: "91".repeat(32),
      headerHash: selected.headerHash,
      payloadEnvelopeSha256: selected.payloadEnvelopeSha256,
      payloadSha256: selected.payloadSha256,
      transcriptDigest: "92".repeat(32),
      blockReplayResultDigest: "93".repeat(32),
      coordinate: { ...selected.coordinate },
    };
    await expect(
      admitValidationTraceChallengeFromReplayContext({
        ...input,
        coordinate: {
          ...coordinate,
          coordinate: { ...coordinate.coordinate, index: "1" },
        },
      }),
    ).rejects.toThrow(/changed selected event/u);
    const pending = admitValidationTraceChallengeFromReplayContext({
      ...input,
      coordinate,
    });
    coordinate.coordinate.index = "9";
    coordinate.transcriptDigest = "94".repeat(32);
    const challenge = await pending;
    expect(requireValidationTraceChallenge(challenge)).toBe(challenge);
    expect(challenge.coordinate.coordinate.index).toBe("0");
    expect(challenge.coordinate.transcriptDigest).toBe("92".repeat(32));
    expect(challenge.exactL1ReferenceOutRefs).toHaveLength(2);
    expect(() =>
      readValidationTraceReplaySelection({
        ...input,
        detectionId: "unadmitted",
      }),
    ).toThrow(/not an admitted interactive/u);
  });

  it("refuses the non-Plutus typed reason even when its code hash is shared", async () => {
    const { evidence, context } = await admitRetainedPlutus(
      {
        verdict: "rejected",
        reason: { ReceivePurposePlutusV3Forbidden: { execution_index: 0n } },
      },
      { sourceKind: "forced" },
    );
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    expect(result.detections).toEqual([]);
  });

  it("refuses missing origins and copied origin authority", async () => {
    await expect(
      admitRetainedPlutus(
        { verdict: "accepted" },
        { sourceKind: "forced", omitOriginAuthority: true },
      ),
    ).rejects.toThrow(/origin|events/u);
    await expect(
      admitRetainedPlutus(
        { verdict: "accepted" },
        { sourceKind: "forced", omitEvent: true },
      ),
    ).rejects.toThrow(/origin|event/u);
    const { evidence, context } = await admitRetainedPlutus(
      { verdict: "accepted" },
      { sourceKind: "forced" },
    );
    await expect(
      admitValidationTraceReplayContext({
        evidence,
        predecessor: context.predecessor,
        transitionTraceEvents: { ...context.transitionTraceEvents! },
      }),
    ).rejects.toThrow(/freshly admitted/u);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        ...context,
        transitionTraceEvents: { ...context.transitionTraceEvents! },
      }),
    ).rejects.toThrow(/freshly admitted/u);
  });
});

describe("existing bounded CEK refusal through canonical retained replay", () => {
  it.each(["accepted", "rejected"] as const)(
    "classifies the %s operator claim from the actual canonical CEK boundary",
    async (verdict) => {
      const claim =
        verdict === "accepted"
          ? { verdict }
          : {
              verdict,
              reason: { PlutusExecutionFailed: { execution_index: 0n } },
            };
      const { fixture, evidence, observation, context } =
        await admitRetainedPlutus(claim, { program: "unboundVariable" });
      expect(fixture.replay.trace.verdict).toBe("rejected");
      expect(fixture.replay.trace.rejectionCode).toBe(
        "E_PLUTUS_SCRIPT_INVALID",
      );
      expect(fixture.replay.trace.witnesses.at(-2)?.auxiliary?.kind).toBe(
        "cekCoreStep",
      );
      const result =
        await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        );
      expect(result.detections).toHaveLength(verdict === "accepted" ? 1 : 0);
      const { decision } = await classifyRetainedReasonFixture({
        observation,
        payloadEnvelopeCbor: fixture.block.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
        replayContext: context,
      });
      expect(decision).toMatchObject(
        verdict === "accepted"
          ? { decision: "fault_detected", category: "validationTraceDispute" }
          : { decision: "healthy" },
      );
    },
  );
});
