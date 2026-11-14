import { createPrivateKey, createPublicKey, sign } from "node:crypto";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  missingSignatureFieldWalkCheckpoint,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk";
import { beforeAll, describe, expect, it } from "vitest";

import {
  planSignerWitnessScanTurn,
  scanSignerWitnesses,
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
      ...filler(5),
      { verificationKey: signer.verificationKey, signature: signer.signature },
      ...filler(4),
    ];
    const scan = scanSignerWitnesses({
      transactionId,
      witnesses,
      paymentCredentialHex: signer.hash,
      batch: 4,
    });
    expect(scan.checkpoints).toEqual([
      { cursor: 4, signerPresent: false },
      { cursor: 8, signerPresent: true },
      { cursor: 10, signerPresent: true },
    ]);
    expect(scan.signerPresent).toBe(true);
    expect(scan.validSignerHashes).toEqual([
      ...Array.from({ length: 5 }, () => other.hash),
      signer.hash,
      ...Array.from({ length: 4 }, () => other.hash),
    ]);
    expect(Object.isFrozen(scan)).toBe(true);
    expect(Object.isFrozen(scan.checkpoints)).toBe(true);
    expect(Object.isFrozen(scan.checkpoints[0])).toBe(true);
    expect(Object.isFrozen(scan.validSignerHashes)).toBe(true);
  });

  it("does not count the credential's key under a signature that fails", () => {
    const scan = scanSignerWitnesses({
      transactionId,
      witnesses: [unsigned, ...filler(1)],
      paymentCredentialHex: signer.hash,
      batch: 16,
    });
    expect(scan.signerPresent).toBe(false);
    expect(scan.validSignerHashes).toEqual([other.hash]);
    expect(scan.checkpoints).toEqual([{ cursor: 2, signerPresent: false }]);
  });

  it("checkpoints the initial position of an empty witness field", () => {
    const scan = scanSignerWitnesses({
      transactionId,
      witnesses: [],
      paymentCredentialHex: signer.hash,
      batch: 32,
    });
    expect(scan).toEqual({
      validSignerHashes: [],
      signerPresent: false,
      checkpoints: [{ cursor: 0, signerPresent: false }],
    });
  });
});

describe("planSignerWitnessScanTurn", () => {
  const itemCount = 10;
  const totalLength = 1 + itemCount * 103;
  const checkpointAt = (nextItemIndex: number) =>
    missingSignatureFieldWalkCheckpoint({
      txId: transactionId,
      itemCount,
      totalLength,
      nextItemIndex,
    });
  const checkpoints = [
    { cursor: 4, signerPresent: false },
    { cursor: 8, signerPresent: true },
    { cursor: 10, signerPresent: true },
  ];
  const turnFrom = (checkpointHash: string) =>
    planSignerWitnessScanTurn({
      checkpoints,
      txId: transactionId,
      itemCount,
      totalLength,
      checkpointHash,
      batch: 4,
    });

  it("resumes at every cursor of the batch schedule", () => {
    expect(turnFrom(checkpointAt(0).checkpointHash)).toEqual({
      checkpoint: checkpointAt(0),
      nextCursor: 4,
      exhausted: false,
      signerPresent: false,
      nextCheckpoint: checkpointAt(4),
    });
    expect(turnFrom(checkpointAt(4).checkpointHash)).toEqual({
      checkpoint: checkpointAt(4),
      nextCursor: 8,
      exhausted: false,
      signerPresent: true,
      nextCheckpoint: checkpointAt(8),
    });
  });

  it("clamps the last batch to the field and carries no further checkpoint", () => {
    expect(turnFrom(checkpointAt(8).checkpointHash)).toEqual({
      checkpoint: checkpointAt(8),
      nextCursor: 10,
      exhausted: true,
      signerPresent: true,
      nextCheckpoint: undefined,
    });
  });

  it("refuses a checkpoint off the batch schedule", () => {
    expect(() => turnFrom(checkpointAt(5).checkpointHash)).toThrow(
      /not on the deterministic 4-item schedule/u,
    );
    expect(() => turnFrom(checkpointAt(10).checkpointHash)).toThrow(
      /not on the deterministic 4-item schedule/u,
    );
  });

  it("refuses a turn whose end the scan did not checkpoint", () => {
    expect(() =>
      planSignerWitnessScanTurn({
        checkpoints: checkpoints.slice(0, 1),
        txId: transactionId,
        itemCount,
        totalLength,
        checkpointHash: checkpointAt(4).checkpointHash,
        batch: 4,
      }),
    ).toThrow(/scan has no checkpoint at cursor 8/u);
  });

  it("closes an empty witness field in its one turn", () => {
    const empty = missingSignatureFieldWalkCheckpoint({
      txId: transactionId,
      itemCount: 0,
      totalLength: 1,
      nextItemIndex: 0,
    });
    expect(
      planSignerWitnessScanTurn({
        checkpoints: [{ cursor: 0, signerPresent: false }],
        txId: transactionId,
        itemCount: 0,
        totalLength: 1,
        checkpointHash: empty.checkpointHash,
        batch: 16,
      }),
    ).toEqual({
      checkpoint: empty,
      nextCursor: 0,
      exhausted: true,
      signerPresent: false,
      nextCheckpoint: undefined,
    });
  });
});
