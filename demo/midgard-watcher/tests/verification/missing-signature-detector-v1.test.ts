import { MissingSignatureProvability } from "@al-ft/midgard-fault-proofs";
import {
  type MidgardAddressWitness,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import {
  detectAndJournalMissingSignatureFindings,
  detectMissingSignatureFinding,
  recoverMissingSignatureVkey,
} from "../../src/verification/missing-signature-detector-v1.js";

const VKEY = "11".repeat(32);
const HASH = missingSignatureVkeyHash(VKEY);
const witness: MidgardAddressWitness = {
  verification_key: VKEY,
  signature: "22".repeat(64),
};
const candidate = {
  headerHash: "33".repeat(28),
  eventKey: { L2TransactionEventKey: { tx_id: "44".repeat(32) } } as const,
  fraudulentBlockOutRef: `${"55".repeat(32)}#0`,
  txId: "44".repeat(32),
  nativeTxCompactCbor: "80",
  committedWitnessSetHash: "66".repeat(32),
  committedAccepted: true,
  replayRejectCode: RejectCodes.MissingRequiredWitness,
  requiredSignerHashes: [HASH],
  addrTxWits: [] as readonly MidgardAddressWitness[],
  vkeySources: {
    committedL2Vkeys: [] as readonly string[],
    observedL1Vkeys: [VKEY] as readonly string[],
  },
};

describe("watcher missing-signature detector v1", () => {
  it("recovers vkeys in L2, L1, operator order and emits a provable finding", () => {
    expect(
      recoverMissingSignatureVkey({
        requiredSignerHash: HASH,
        sources: candidate.vkeySources,
      }),
    ).toBe(VKEY);
    expect(detectMissingSignatureFinding({ candidate })?.finding).toMatchObject(
      {
        provability: MissingSignatureProvability.MissingWitness,
        accusedRequiredSignerIndex: 0n,
        accusedRequiredSignerHash: HASH,
        resolvedVkey: VKEY,
        estimatedThreadTxCount: 5,
      },
    );
  });

  it("journals unknown-preimage, present-invalid, and honest classifications", async () => {
    const unknown = {
      ...candidate,
      vkeySources: { committedL2Vkeys: [], observedL1Vkeys: [] },
    };
    const presentInvalid = {
      ...candidate,
      replayRejectCode: RejectCodes.InvalidSignature,
      addrTxWits: [witness],
    };
    const honest = {
      ...candidate,
      replayRejectCode: null,
      addrTxWits: [witness],
    };
    const journaled: string[] = [];
    const detections = await detectAndJournalMissingSignatureFindings({
      candidates: [unknown, presentInvalid, honest],
      journal: ({ finding }) => {
        journaled.push(finding.provability);
      },
    });
    expect(detections.map(({ finding }) => finding.provability)).toEqual([
      MissingSignatureProvability.UnknownVkeyPreimage,
      MissingSignatureProvability.PresentButInvalid,
      MissingSignatureProvability.NotAFault,
    ]);
    expect(journaled).toEqual(
      detections.map(({ finding }) => finding.provability),
    );
  });

  it("honors the isolated default-on kill switch", () => {
    expect(
      detectMissingSignatureFinding({
        candidate,
        config: { enabled: false },
      }),
    ).toBeNull();
  });
});
