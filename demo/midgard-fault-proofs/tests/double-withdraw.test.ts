import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  admitDoubleWithdrawArtifact,
  DOUBLE_WITHDRAW_ARTIFACT,
} from "../src/workflow/double-withdraw.js";

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
  key: Buffer.from(SDK.committedWithdrawalKeyBytes(id), "hex"),
  value: Buffer.from(SDK.committedWithdrawalValueBytes(PAYABLE_INFO), "hex"),
});

const entries = [entry(FIRST_ID), entry(SECOND_ID)] as const;

describe("production double-withdraw workflow evidence", () => {
  it("re-derives both committed proofs from a strict journal artifact", async () => {
    const counted = await buildCountedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      entries,
    );
    const admitted = await admitDoubleWithdrawArtifact({
      schemaVersion: DOUBLE_WITHDRAW_ARTIFACT,
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
      admitDoubleWithdrawArtifact({
        schemaVersion: DOUBLE_WITHDRAW_ARTIFACT,
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
    } as unknown as CanonicalBlockEvidence;
    const decision =
      await DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(decision.launchScope).toEqual(["doubleWithdraw"]);
    expect(decision.detections).toHaveLength(1);
    expect(decision.detections[0]).toMatchObject({
      violationId: SDK.DOUBLE_WITHDRAW_VIOLATION_ID,
      position: 1n,
    });
    expect(decision.detections[0]!.detectionId).toContain(
      SDK.committedWithdrawalKeyBytes(FIRST_ID),
    );
  });
});
