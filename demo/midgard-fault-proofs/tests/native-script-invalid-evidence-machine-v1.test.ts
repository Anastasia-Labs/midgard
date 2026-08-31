import {
  encodeMidgardAddressWitnessItemV1,
  encodeMidgardNativeScript,
  type MidgardNativeScript,
} from "@al-ft/midgard-core";
import { missingSignatureVkeyHashV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  nativeScriptInvalidPushdownStepV1,
  nativeScriptInvalidSignerScanStateV1,
  nativeScriptInvalidSignerSetV1,
  resolveNativeScriptInvalidPushdownResumeV1,
} from "../src/native-script-invalid/evidence-machine-v1.js";

const witnessItem = (verificationKey: Uint8Array): Buffer =>
  encodeMidgardAddressWitnessItemV1({
    verificationKey,
    signature: Buffer.alloc(64, 0x55),
  });

const sortedWitnessItems = (count: number): readonly Buffer[] =>
  Array.from({ length: count }, (_, index) => {
    const key = Buffer.alloc(32);
    key.writeUInt32BE(index, 28);
    return {
      key,
      hash: Buffer.from(missingSignatureVkeyHashV1(key.toString("hex")), "hex"),
    };
  })
    .sort((left, right) => Buffer.compare(left.hash, right.hash))
    .map(({ key }) => witnessItem(key));

describe("native-script-invalid staged evidence machine", () => {
  it("advances the exact field-7 signer checkpoint and frontier in batches", () => {
    const items = sortedWitnessItems(318);
    const totalLength = 3 + items.length * 103;
    const first = nativeScriptInvalidSignerScanStateV1({
      txId: "11".repeat(32),
      addressWitnessItems: items,
      totalLength,
    });
    expect(first.nextItemIndex).toBe(32);
    expect(first.checkpointBytes).toHaveLength(106);
    expect(first.checkpointHash).toHaveLength(64);
    expect(first.signerCount).toBe(32n);
    expect(first.complete).toBe(false);

    const second = nativeScriptInvalidSignerScanStateV1({
      txId: "11".repeat(32),
      addressWitnessItems: items,
      totalLength,
      committedCheckpointHash: first.checkpointHash,
    });
    expect(second.nextItemIndex).toBe(64);
    expect(second.signerCount).toBe(64n);
    expect(second.checkpointHash).not.toBe(first.checkpointHash);
  });

  it("builds exact membership and ordered nonmembership proof shapes", () => {
    const signerSet = nativeScriptInvalidSignerSetV1(sortedWitnessItems(4));
    const member = signerSet.hashes[1]!;
    expect(signerSet.proofFor(member)).toHaveProperty("SignerMembershipProof");
    expect(signerSet.proofFor(Buffer.alloc(28, 0))).toHaveProperty(
      "SignerBelowFirstProof",
    );
    expect(signerSet.proofFor(Buffer.alloc(28, 0xff))).toHaveProperty(
      "SignerAboveLastProof",
    );
    const empty = nativeScriptInvalidSignerSetV1([]);
    expect(empty.proofFor(Buffer.alloc(28, 0x77))).toEqual({
      EmptySignerSetProof: { peaks: [] },
    });
  });

  it("resumes the 87-byte semantic cursor and reaches the recursive verdict", () => {
    const signerSet = nativeScriptInvalidSignerSetV1([]);
    const script: MidgardNativeScript = {
      type: "all",
      scripts: Array.from({ length: 31 }, (_, index) => ({
        type: "sig" as const,
        keyHash: Buffer.alloc(28, index + 1),
      })),
    };
    const scriptBytes = encodeMidgardNativeScript(script);
    const first = nativeScriptInvalidPushdownStepV1({
      scriptBytes,
      validityIntervalStart: 0n,
      validityIntervalEnd: 100n,
      signerSet,
    });
    expect(first.complete).toBe(false);
    expect(first.nextCursorBytes).toHaveLength(174);
    expect(first.signerHashes.length).toBeGreaterThan(0);

    const second = nativeScriptInvalidPushdownStepV1({
      scriptBytes,
      validityIntervalStart: 0n,
      validityIntervalEnd: 100n,
      signerSet,
      committedCursorHash: first.nextCursorHash,
      cursorBytes: Buffer.from(first.nextCursorBytes, "hex"),
      frames: first.nextFrames,
    });
    expect(second.complete).toBe(true);
    expect(second.satisfied).toBe(false);
    expect(second.nextFrames).toEqual([]);
    const reconstructed = resolveNativeScriptInvalidPushdownResumeV1({
      scriptBytes,
      validityIntervalStart: 0n,
      validityIntervalEnd: 100n,
      signerSet,
      committedCursorHash: first.nextCursorHash,
    });
    expect(reconstructed.cursorBytes.toString("hex")).toBe(
      first.nextCursorBytes,
    );
    expect(reconstructed.frames).toEqual(first.nextFrames);
  });

  it("rejects cursor mutation before producing a resumed action", () => {
    const signerSet = nativeScriptInvalidSignerSetV1([]);
    const scriptBytes = encodeMidgardNativeScript({
      type: "all",
      scripts: Array.from({ length: 31 }, () => ({
        type: "sig" as const,
        keyHash: Buffer.alloc(28, 0x22),
      })),
    });
    const first = nativeScriptInvalidPushdownStepV1({
      scriptBytes,
      validityIntervalStart: 0n,
      validityIntervalEnd: 0n,
      signerSet,
    });
    const mutated = Buffer.from(first.nextCursorBytes, "hex");
    mutated[75] ^= 1;
    expect(() =>
      nativeScriptInvalidPushdownStepV1({
        scriptBytes,
        validityIntervalStart: 0n,
        validityIntervalEnd: 0n,
        signerSet,
        committedCursorHash: first.nextCursorHash,
        cursorBytes: mutated,
        frames: first.nextFrames,
      }),
    ).toThrow(/cursor commitment is invalid/u);
  });
});
