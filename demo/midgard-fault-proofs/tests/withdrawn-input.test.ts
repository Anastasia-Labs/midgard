import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { encodeData } from "../src/transition-trace/reconstruct.js";
import {
  type CanonicalBlockClassification,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import {
  requireCompleteCanonicalReplayDecision,
  WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY,
  WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  admitWithdrawnInputArtifact,
  prepareWithdrawnInputArtifact,
} from "../src/workflow/withdrawn-input.js";
import {
  admitWithdrawnReferenceInputArtifact,
  prepareWithdrawnReferenceInputArtifact,
} from "../src/workflow/withdrawn-reference-input.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  h28,
  h32,
  outRefCbor,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";

const fault = (
  classification: CanonicalBlockClassification,
): Extract<
  CanonicalBlockClassification,
  { readonly decision: "fault_detected" }
> => {
  if (classification.decision !== "fault_detected") {
    throw new Error("withdrawn-input fixture did not classify a fault");
  }
  return classification;
};

const evidenceWithWithdrawnInput = async (
  field: "spend" | "reference" = "spend",
) => {
  const spent = { tx_id: h32(31), output_index: 3n };
  const transaction = buildFixtureTransaction({
    spendInputs:
      field === "spend" ? [outRefCbor(31, 3n)] : [outRefCbor(30, 0n)],
    referenceInputs: field === "reference" ? [outRefCbor(31, 3n)] : undefined,
    fee: 1n,
  });
  const base = await buildCanonicalBlockFixture({
    transactions: [transaction],
  });
  const withdrawalId: SDK.OutputReference = {
    transactionId: h32(32),
    outputIndex: 0n,
  };
  const withdrawal: SDK.WithdrawalInfo = {
    body: {
      l2_outref: {
        transactionId: spent.tx_id,
        outputIndex: spent.output_index,
      },
      l2_owner: h28(33),
      l2_value: new Map(),
      l1_address: {
        paymentCredential: { PublicKeyCredential: [h28(34)] },
        stakeCredential: null,
      },
      l1_datum: "NoDatum",
    },
    signature: [h32(35), "24".repeat(64)],
    validity: "WithdrawalIsValid",
  };
  const withdrawalEntry: SDK.DaPayloadEntry = [
    SDK.committedWithdrawalKeyBytes(withdrawalId),
    SDK.committedWithdrawalValueBytes(withdrawal),
  ];
  const withdrawalEvent: SDK.DaPayloadEntry = [
    encodeData(
      { WithdrawalEventKey: { withdrawal_id: withdrawalId } },
      SDK.EventKeySchema,
    ).toString("hex"),
    encodeData(
      { step_index: 0n, phase: "Withdrawal" },
      SDK.EventToStepValueSchema,
    ).toString("hex"),
  ];
  const eventToStep = [
    ...base.payload.block_body.event_to_step,
    withdrawalEvent,
  ].sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0));
  const withdrawalsRoot = await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, [
    {
      key: Buffer.from(withdrawalEntry[0], "hex"),
      value: Buffer.from(withdrawalEntry[1], "hex"),
    },
  ]);
  const eventToStepRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.eventToStep,
    eventToStep.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const counts = {
    ...base.payload.block_body.counts,
    withdrawalCount: 1n,
    totalEventCount: 2n,
  };
  const header: SDK.Header = {
    ...base.header,
    withdrawalsRoot: withdrawalsRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    withdrawalCount: 1n,
    totalEventCount: 2n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header_hash: headerHash,
      header,
      withdrawals: [withdrawalEntry],
      event_to_step: eventToStep,
      counts,
    },
  };
  const payloadEnvelopeCbor = await reencodeFixturePayload(payload);
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      ...base,
      payload,
      payloadEnvelopeCbor,
      header,
      headerHash,
    }),
    payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
  });
  const replayer =
    field === "spend"
      ? WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY
      : WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY;
  const replay = await replayer.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision: replay,
  });
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections,
  });
  return Object.freeze({ evidence, replay, classification });
};

describe("production withdrawn-input workflow V1", () => {
  it("detects the accepted spend/withdrawal intersection and re-admits its durable artifact", async () => {
    const fixture = await evidenceWithWithdrawnInput();
    expect(fixture.replay.detections).toHaveLength(1);
    expect(fixture.replay.detections[0]).toMatchObject({
      violationId: SDK.WITHDRAWN_INPUT_VIOLATION_ID,
      position: 0n,
    });
    const artifact = await prepareWithdrawnInputArtifact({
      evidence: fixture.evidence,
      classification: fault(fixture.classification),
    });
    const admitted = await admitWithdrawnInputArtifact(artifact);
    expect(admitted.spendInputs[artifact.badInputIndex]).toEqual({
      tx_id: h32(31),
      output_index: 3n,
    });
    expect(admitted.withdrawalMembership.value.validity).toBe(
      "WithdrawalIsValid",
    );
  });

  it("rejects a substituted membership root and an unsafe detection index", async () => {
    const fixture = await evidenceWithWithdrawnInput();
    const classification = fault(fixture.classification);
    const artifact = await prepareWithdrawnInputArtifact({
      evidence: fixture.evidence,
      classification,
    });
    const membership = Data.from(
      artifact.withdrawalMembershipCbor,
      SDK.WithdrawalSourceMembershipProof,
    );
    await expect(
      admitWithdrawnInputArtifact({
        ...artifact,
        withdrawalMembershipCbor: Data.to(
          { ...membership, root: h32(99) },
          SDK.WithdrawalSourceMembershipProof,
        ),
      }),
    ).rejects.toThrow("count does not open");

    const huge = "9007199254740993";
    await expect(
      prepareWithdrawnInputArtifact({
        evidence: fixture.evidence,
        classification: {
          ...classification,
          selected: {
            ...classification.selected,
            position: BigInt(huge),
            detectionId: classification.selected.detectionId.replace(
              /^withdrawn-input:[^:]+:/u,
              `withdrawn-input:${huge}:`,
            ),
          },
        },
      }),
    ).rejects.toThrow("index is unsafe");
  });
});

describe("production withdrawn-reference-input workflow V1", () => {
  it("detects the accepted reference/withdrawal intersection and re-admits its artifact", async () => {
    const fixture = await evidenceWithWithdrawnInput("reference");
    expect(fixture.replay.detections).toHaveLength(1);
    expect(fixture.replay.detections[0]).toMatchObject({
      violationId: SDK.WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID,
      position: 0n,
    });
    const artifact = await prepareWithdrawnReferenceInputArtifact({
      evidence: fixture.evidence,
      classification: fault(fixture.classification),
    });
    const admitted = await admitWithdrawnReferenceInputArtifact(artifact);
    expect(admitted.referenceInputs[artifact.badReferenceInputIndex]).toEqual({
      tx_id: h32(31),
      output_index: 3n,
    });
    expect(admitted.withdrawalMembership.value.validity).toBe(
      "WithdrawalIsValid",
    );
  });

  it("rejects a substituted reference-input preimage", async () => {
    const fixture = await evidenceWithWithdrawnInput("reference");
    const artifact = await prepareWithdrawnReferenceInputArtifact({
      evidence: fixture.evidence,
      classification: fault(fixture.classification),
    });
    await expect(
      admitWithdrawnReferenceInputArtifact({
        ...artifact,
        referenceInputs: [{ tx_id: h32(98), output_index: "0" }],
      }),
    ).rejects.toThrow();
  });
});
