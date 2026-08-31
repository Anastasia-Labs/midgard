import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { encodeData } from "../src/transition-trace/reconstruct.js";
import {
  type CanonicalBlockClassificationV1,
  classifyCanonicalBlockViolationsV1,
} from "../src/workflow/classification-v1.js";
import {
  requireCompleteCanonicalReplayDecisionV1,
  WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
  WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionWithdrawnInputArtifactV1,
  prepareProductionWithdrawnInputArtifactV1,
} from "../src/workflow/production-withdrawn-input-v1.js";
import {
  admitProductionWithdrawnReferenceInputArtifactV1,
  prepareProductionWithdrawnReferenceInputArtifactV1,
} from "../src/workflow/production-withdrawn-reference-input-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  h28,
  h32,
  outRefCbor,
  reencodeFixturePayloadV1,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const fault = (
  classification: CanonicalBlockClassificationV1,
): Extract<
  CanonicalBlockClassificationV1,
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
  const transaction = buildFixtureTransactionV1({
    spendInputs:
      field === "spend" ? [outRefCbor(31, 3n)] : [outRefCbor(30, 0n)],
    referenceInputs: field === "reference" ? [outRefCbor(31, 3n)] : undefined,
    fee: 1n,
  });
  const base = await buildCanonicalBlockFixtureV1({
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
    SDK.committedWithdrawalKeyBytesV1(withdrawalId),
    SDK.committedWithdrawalValueBytesV1(withdrawal),
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
  const header: SDK.HeaderV1 = {
    ...base.header,
    withdrawalsRoot: withdrawalsRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    withdrawalCount: 1n,
    totalEventCount: 2n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const payload: SDK.DaPayloadV1 = {
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
  const payloadEnvelopeCbor = await reencodeFixturePayloadV1(payload);
  const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1({
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
      ? WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY_V1
      : WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1;
  const replay = await replayer.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecisionV1({
    evidence,
    replayer,
    decision: replay,
  });
  const classification = await classifyCanonicalBlockViolationsV1({
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
      violationId: SDK.WITHDRAWN_INPUT_VIOLATION_ID_V1,
      position: 0n,
    });
    const artifact = await prepareProductionWithdrawnInputArtifactV1({
      evidence: fixture.evidence,
      classification: fault(fixture.classification),
    });
    const admitted = await admitProductionWithdrawnInputArtifactV1(artifact);
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
    const artifact = await prepareProductionWithdrawnInputArtifactV1({
      evidence: fixture.evidence,
      classification,
    });
    const membership = Data.from(
      artifact.withdrawalMembershipCbor,
      SDK.WithdrawalSourceMembershipProof,
    );
    await expect(
      admitProductionWithdrawnInputArtifactV1({
        ...artifact,
        withdrawalMembershipCbor: Data.to(
          { ...membership, root: h32(99) },
          SDK.WithdrawalSourceMembershipProof,
        ),
      }),
    ).rejects.toThrow("count does not open");

    const huge = "9007199254740993";
    await expect(
      prepareProductionWithdrawnInputArtifactV1({
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
      violationId: SDK.WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1,
      position: 0n,
    });
    const artifact = await prepareProductionWithdrawnReferenceInputArtifactV1({
      evidence: fixture.evidence,
      classification: fault(fixture.classification),
    });
    const admitted =
      await admitProductionWithdrawnReferenceInputArtifactV1(artifact);
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
    const artifact = await prepareProductionWithdrawnReferenceInputArtifactV1({
      evidence: fixture.evidence,
      classification: fault(fixture.classification),
    });
    await expect(
      admitProductionWithdrawnReferenceInputArtifactV1({
        ...artifact,
        referenceInputs: [{ tx_id: h32(98), output_index: "0" }],
      }),
    ).rejects.toThrow();
  });
});
