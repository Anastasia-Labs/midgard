import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidenceV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionDoubleWithdrawArtifactV1,
  PRODUCTION_DOUBLE_WITHDRAW_ARTIFACT_V1,
} from "../src/workflow/production-double-withdraw-v1.js";

const FIRST_ID: SDK.OutputReference = {
  transactionId: "8b".repeat(32),
  outputIndex: 2n,
};
const SECOND_ID: SDK.OutputReference = {
  transactionId: "c4".repeat(32),
  outputIndex: 1n,
};
const SHARED_OUTREF: SDK.OutputReference = {
  transactionId: "7e".repeat(32),
  outputIndex: 1n,
};
const PAYABLE_INFO: SDK.WithdrawalInfo = {
  body: {
    l2_outref: SHARED_OUTREF,
    l2_owner: "9c".repeat(28),
    l2_value: new Map(),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["2b".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["ad".repeat(32), "be".repeat(64)],
  validity: "WithdrawalIsValid",
};

const entry = (id: SDK.OutputReference) => ({
  key: Buffer.from(SDK.committedWithdrawalKeyBytesV1(id), "hex"),
  value: Buffer.from(SDK.committedWithdrawalValueBytesV1(PAYABLE_INFO), "hex"),
});

const entries = [entry(FIRST_ID), entry(SECOND_ID)] as const;

describe("production double-withdraw workflow evidence", () => {
  it("re-derives both committed proofs from a strict journal artifact", async () => {
    const counted = await buildCountedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      entries,
    );
    const admitted = await admitProductionDoubleWithdrawArtifactV1({
      schemaVersion: PRODUCTION_DOUBLE_WITHDRAW_ARTIFACT_V1,
      headerHash: "44".repeat(28),
      committedWithdrawalsRoot: counted.root,
      withdrawalCount: 2,
      firstLeafIndex: 0,
      secondLeafIndex: 1,
      entries: entries.map(({ key, value }) => ({
        keyCbor: key.toString("hex"),
        valueCbor: value.toString("hex"),
      })),
    });
    expect(admitted.prepared.firstLeaf.withdrawalId).toEqual(FIRST_ID);
    expect(admitted.prepared.secondLeaf.withdrawalId).toEqual(SECOND_ID);
    expect(admitted.firstInclusion.withdrawalMembershipProof).toBeDefined();
    expect(admitted.secondInclusion.withdrawalMembershipProof).toBeDefined();
  });

  it("rejects a substituted committed withdrawals root", async () => {
    await expect(
      admitProductionDoubleWithdrawArtifactV1({
        schemaVersion: PRODUCTION_DOUBLE_WITHDRAW_ARTIFACT_V1,
        headerHash: "44".repeat(28),
        committedWithdrawalsRoot: "55".repeat(32),
        withdrawalCount: 2,
        firstLeafIndex: 0,
        secondLeafIndex: 1,
        entries: entries.map(({ key, value }) => ({
          keyCbor: key.toString("hex"),
          valueCbor: value.toString("hex"),
        })),
      }),
    ).rejects.toThrow(/withdrawals_root_mismatch/u);
  });

  it("complete-replays every distinct payable same-outref pair", async () => {
    const evidence = {
      headerHash: "44".repeat(28),
      payloadEnvelopeSha256: "66".repeat(32),
      payloadSha256: "77".repeat(32),
      reconstruction: {
        withdrawals: [
          {
            key: FIRST_ID,
            value: PAYABLE_INFO,
            keyBytes: entries[0].key,
            valueBytes: entries[0].value,
          },
          {
            key: SECOND_ID,
            value: PAYABLE_INFO,
            keyBytes: entries[1].key,
            valueBytes: entries[1].value,
          },
        ],
      },
    } as unknown as CanonicalBlockEvidenceV1;
    const decision =
      await DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    expect(decision.launchScope).toEqual(["doubleWithdraw"]);
    expect(decision.detections).toHaveLength(1);
    expect(decision.detections[0]).toMatchObject({
      violationId: SDK.DOUBLE_WITHDRAW_VIOLATION_ID_V1,
      position: 1n,
    });
    expect(decision.detections[0]!.detectionId).toContain(
      SDK.committedWithdrawalKeyBytesV1(FIRST_ID),
    );
  });
});
