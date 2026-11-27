import { createPrivateKey, createPublicKey, sign } from "node:crypto";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  missingSignatureFieldWalkCheckpoint,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk/fraud-proof";
import { beforeAll, describe, expect, it } from "vitest";

import {
  planSignerWitnessScanTurn,
  scanSignerWitnesses,
  SIGNER_WITNESS_SCAN_BATCH,
} from "../src/signer-witness-scan/signer-witness-scan.js";

beforeAll(() => ledgerEd25519SignatureReady());

const transactionId = "5a".repeat(32);

const keyFromSeed = (byte: number) => {
  const privateKey = createPrivateKey({
    key: Buffer.concat([
      Buffer.from("302e020100300506032b657004220420", "hex"),
      Buffer.alloc(32, byte),
    ]),
    format: "der",
    type: "pkcs8",
  });
  const verificationKey = createPublicKey(privateKey)
    .export({ format: "der", type: "spki" })
    .subarray(-32);
  return {
    verificationKey,
    hash: missingSignatureVkeyHash(verificationKey.toString("hex")),
    signature: sign(null, Buffer.from(transactionId, "hex"), privateKey),
  };
};

const signer = keyFromSeed(1);
const other = keyFromSeed(2);
const unsigned = {
  verificationKey: signer.verificationKey,
  signature: Buffer.alloc(64, 0xff),
};
const filler = (count: number) =>
  Array.from({ length: count }, () => ({
    verificationKey: other.verificationKey,
    signature: other.signature,
  }));

describe("scanSignerWitnesses", () => {
  it("records the cumulative signer state at every batch boundary and at the end", () => {
    const witnesses = [
      ...filler(40),
      { verificationKey: signer.verificationKey, signature: signer.signature },
      ...filler(29),
    ];
    const scan = scanSignerWitnesses({
      transactionId,
      witnesses,
      paymentCredentialHex: signer.hash,
    });
    expect(SIGNER_WITNESS_SCAN_BATCH).toBe(32);
    expect(scan.checkpoints).toEqual([
      { cursor: 32, signerPresent: false },
      { cursor: 64, signerPresent: true },
      { cursor: 70, signerPresent: true },
    ]);
    expect(scan.signerPresent).toBe(true);
    expect(Object.isFrozen(scan)).toBe(true);
    expect(Object.isFrozen(scan.checkpoints)).toBe(true);
    expect(Object.isFrozen(scan.checkpoints[0])).toBe(true);
  });

  it("does not count the credential's key under a signature that fails", () => {
    const scan = scanSignerWitnesses({
      transactionId,
      witnesses: [unsigned, ...filler(1)],
      paymentCredentialHex: signer.hash,
    });
    expect(scan.signerPresent).toBe(false);
    expect(scan.checkpoints).toEqual([{ cursor: 2, signerPresent: false }]);
  });

  it("does not count another key's valid signature", () => {
    const scan = scanSignerWitnesses({
      transactionId,
      witnesses: filler(3),
      paymentCredentialHex: signer.hash,
    });
    expect(scan.signerPresent).toBe(false);
  });

  it("checkpoints the initial position of an empty witness field", () => {
    const scan = scanSignerWitnesses({
      transactionId,
      witnesses: [],
      paymentCredentialHex: signer.hash,
    });
    expect(scan).toEqual({
      signerPresent: false,
      checkpoints: [{ cursor: 0, signerPresent: false }],
    });
  });
});

describe("planSignerWitnessScanTurn", () => {
  const itemCount = 70;
  const totalLength = 2 + itemCount * 103;
  const checkpointAt = (nextItemIndex: number) =>
    missingSignatureFieldWalkCheckpoint({
      txId: transactionId,
      itemCount,
      totalLength,
      nextItemIndex,
    });
  const signerAt40 = [
    { cursor: 32, signerPresent: false },
    { cursor: 64, signerPresent: true },
    { cursor: 70, signerPresent: true },
  ];
  const noSigner = [
    { cursor: 32, signerPresent: false },
    { cursor: 64, signerPresent: false },
    { cursor: 70, signerPresent: false },
  ];
  const turnFrom = (
    checkpoints: readonly { cursor: number; signerPresent: boolean }[],
    checkpointHash: string,
  ) =>
    planSignerWitnessScanTurn({
      checkpoints,
      txId: transactionId,
      itemCount,
      totalLength,
      checkpointHash,
    });

  it("opens at cursor 0 from an empty checkpoint and carries no checkpoint bytes", () => {
    expect(turnFrom(signerAt40, "")).toEqual({
      checkpointCbor: null,
      nextCursor: 32,
      signerPresent: false,
      terminal: false,
      nextCheckpointHash: checkpointAt(32).checkpointHash,
    });
  });

  it("ends the scan at the batch where the signer is found", () => {
    expect(turnFrom(signerAt40, checkpointAt(32).checkpointHash)).toEqual({
      checkpointCbor: checkpointAt(32).checkpointCbor,
      nextCursor: 64,
      signerPresent: true,
      terminal: true,
      nextCheckpointHash: undefined,
    });
  });

  it("walks every batch when the signer is absent and clamps the last one to the field", () => {
    expect(turnFrom(noSigner, checkpointAt(32).checkpointHash)).toMatchObject({
      nextCursor: 64,
      terminal: false,
      nextCheckpointHash: checkpointAt(64).checkpointHash,
    });
    expect(turnFrom(noSigner, checkpointAt(64).checkpointHash)).toEqual({
      checkpointCbor: checkpointAt(64).checkpointCbor,
      nextCursor: 70,
      signerPresent: false,
      terminal: true,
      nextCheckpointHash: undefined,
    });
  });

  it("refuses a checkpoint off the batch schedule", () => {
    for (const cursor of [0, 5, 70])
      expect(() =>
        turnFrom(noSigner, checkpointAt(cursor).checkpointHash),
      ).toThrow(/not on the deterministic 32-item schedule/u);
  });

  it("refuses a turn whose end the scan did not checkpoint", () => {
    expect(() =>
      turnFrom(noSigner.slice(0, 1), checkpointAt(32).checkpointHash),
    ).toThrow(/scan has no checkpoint at cursor 64/u);
  });

  it("closes an empty witness field in its opening turn", () => {
    expect(
      planSignerWitnessScanTurn({
        checkpoints: [{ cursor: 0, signerPresent: false }],
        txId: transactionId,
        itemCount: 0,
        totalLength: 1,
        checkpointHash: "",
      }),
    ).toEqual({
      checkpointCbor: null,
      nextCursor: 0,
      signerPresent: false,
      terminal: true,
      nextCheckpointHash: undefined,
    });
  });
});
