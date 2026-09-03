import {
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeScript,
  type MidgardNativeScript,
} from "@al-ft/midgard-core";
import { missingSignatureVkeyHash } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  assertExecutionNativeScriptInvalidDirectRoute,
  EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT,
  EXECUTION_NATIVE_SCRIPT_INVALID_NODE_BATCH,
  EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_FINALIZE_BATCH,
  EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH,
  EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH,
  executionNativeScriptInvalidPushdownStep,
  executionNativeScriptInvalidSignerScanState,
  executionNativeScriptInvalidSignerSet,
  executionNativeScriptInvalidUsesDirectRoute,
  resolveExecutionNativeScriptInvalidPushdownResume,
} from "../src/execution-native-script-invalid/evidence-machine-v1.js";

const witnessItem = (verificationKey: Uint8Array): Buffer =>
  encodeMidgardAddressWitnessItem({
    verificationKey,
    signature: Buffer.alloc(64, 0x55),
  });

const sortedWitnessItems = (count: number): readonly Buffer[] =>
  Array.from({ length: count }, (_, index) => {
    const key = Buffer.alloc(32);
    key.writeUInt32BE(index, 28);
    return {
      key,
      hash: Buffer.from(missingSignatureVkeyHash(key.toString("hex")), "hex"),
    };
  })
    .sort((left, right) => Buffer.compare(left.hash, right.hash))
    .map(({ key }) => witnessItem(key));

describe("execution-native-script-invalid staged evidence machine", () => {
  it("advances the exact field-7 signer checkpoint and frontier in batches", () => {
    const items = sortedWitnessItems(318);
    const totalLength = 3 + items.length * 103;
    const first = executionNativeScriptInvalidSignerScanState({
      txId: "11".repeat(32),
      addressWitnessItems: items,
      totalLength,
    });
    expect(first.nextItemIndex).toBe(16);
    expect(first.checkpointBytes).toHaveLength(106);
    expect(first.checkpointHash).toHaveLength(64);
    expect(first.signerCount).toBe(16n);
    expect(first.complete).toBe(false);

    const second = executionNativeScriptInvalidSignerScanState({
      txId: "11".repeat(32),
      addressWitnessItems: items,
      totalLength,
      committedCheckpointHash: first.checkpointHash,
    });
    expect(second.nextItemIndex).toBe(32);
    expect(second.signerCount).toBe(32n);
    expect(second.checkpointHash).not.toBe(first.checkpointHash);
  });

  it("builds exact membership and ordered nonmembership proof shapes", () => {
    const signerSet = executionNativeScriptInvalidSignerSet(
      sortedWitnessItems(4),
    );
    const member = signerSet.hashes[1]!;
    expect(signerSet.proofFor(member)).toHaveProperty("SignerMembershipProof");
    expect(signerSet.proofFor(Buffer.alloc(28, 0))).toHaveProperty(
      "SignerBelowFirstProof",
    );
    expect(signerSet.proofFor(Buffer.alloc(28, 0xff))).toHaveProperty(
      "SignerAboveLastProof",
    );
    const empty = executionNativeScriptInvalidSignerSet([]);
    expect(empty.proofFor(Buffer.alloc(28, 0x77))).toEqual({
      EmptySignerSetProof: { peaks: [] },
    });
  });

  it("resumes the 87-byte semantic cursor and reaches the recursive verdict", () => {
    const signerSet = executionNativeScriptInvalidSignerSet([]);
    const script: MidgardNativeScript = {
      type: "all",
      scripts: Array.from({ length: 31 }, (_, index) => ({
        type: "sig" as const,
        keyHash: Buffer.alloc(28, index + 1),
      })),
    };
    const scriptBytes = encodeMidgardNativeScript(script);
    const first = executionNativeScriptInvalidPushdownStep({
      scriptBytes,
      validityIntervalStart: 0n,
      validityIntervalEnd: 100n,
      signerSet,
    });
    expect(first.complete).toBe(false);
    expect(first.nextCursorBytes).toHaveLength(174);
    expect(first.signerHashes.length).toBeGreaterThan(0);

    let terminal = first;
    while (!terminal.complete) {
      terminal = executionNativeScriptInvalidPushdownStep({
        scriptBytes,
        validityIntervalStart: 0n,
        validityIntervalEnd: 100n,
        signerSet,
        committedCursorHash: terminal.nextCursorHash,
        cursorBytes: Buffer.from(terminal.nextCursorBytes, "hex"),
        frames: terminal.nextFrames,
      });
    }
    expect(terminal.satisfied).toBe(false);
    expect(terminal.nextFrames).toEqual([]);
    const reconstructed = resolveExecutionNativeScriptInvalidPushdownResume({
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

  it("pins the direct and independently governed staged frontiers", () => {
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT).toBe(28);
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH).toBe(16);
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH).toBe(16);
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_FINALIZE_BATCH).toBe(16);
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_NODE_BATCH).toBe(16);
    expect(
      executionNativeScriptInvalidUsesDirectRoute({
        signerCount: 28,
        scriptBytes: 1_024,
      }),
    ).toBe(true);
    expect(
      executionNativeScriptInvalidUsesDirectRoute({
        signerCount: 29,
        scriptBytes: 1_024,
      }),
    ).toBe(false);
  });

  it("rejects a forced direct builder above the 28-signer frontier", () => {
    expect(() =>
      assertExecutionNativeScriptInvalidDirectRoute(28),
    ).not.toThrow();
    expect(() => assertExecutionNativeScriptInvalidDirectRoute(29)).toThrow(
      /direct signer limit is 28; use the staged route/u,
    );
  });

  it("rejects cursor mutation before producing a resumed action", () => {
    const signerSet = executionNativeScriptInvalidSignerSet([]);
    const scriptBytes = encodeMidgardNativeScript({
      type: "all",
      scripts: Array.from({ length: 31 }, () => ({
        type: "sig" as const,
        keyHash: Buffer.alloc(28, 0x22),
      })),
    });
    const first = executionNativeScriptInvalidPushdownStep({
      scriptBytes,
      validityIntervalStart: 0n,
      validityIntervalEnd: 0n,
      signerSet,
    });
    const mutated = Buffer.from(first.nextCursorBytes, "hex");
    mutated[75] ^= 1;
    expect(() =>
      executionNativeScriptInvalidPushdownStep({
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

  it("reports a satisfied-script negative instead of an invalidity verdict", () => {
    const transition = executionNativeScriptInvalidPushdownStep({
      scriptBytes: encodeMidgardNativeScript({ type: "all", scripts: [] }),
      validityIntervalStart: 0n,
      validityIntervalEnd: 100n,
      signerSet: executionNativeScriptInvalidSignerSet([]),
    });
    expect(transition.complete).toBe(true);
    expect(transition.satisfied).toBe(true);
  });

  it("bounds the maximum wide and deep evaluator frontiers across batches", () => {
    const signerSet = executionNativeScriptInvalidSignerSet([]);
    const walk = (script: MidgardNativeScript) => {
      const scriptBytes = encodeMidgardNativeScript(script);
      let transition = executionNativeScriptInvalidPushdownStep({
        scriptBytes,
        validityIntervalStart: 0n,
        validityIntervalEnd: 100n,
        signerSet,
      });
      let batches = 1;
      while (!transition.complete) {
        transition = executionNativeScriptInvalidPushdownStep({
          scriptBytes,
          validityIntervalStart: 0n,
          validityIntervalEnd: 100n,
          signerSet,
          committedCursorHash: transition.nextCursorHash,
          cursorBytes: Buffer.from(transition.nextCursorBytes, "hex"),
          frames: transition.nextFrames,
        });
        batches += 1;
      }
      return { transition, batches };
    };
    const wide = walk({
      type: "all",
      scripts: Array.from({ length: 31 }, (_, index) => ({
        type: "sig" as const,
        keyHash: Buffer.alloc(28, index + 1),
      })),
    });
    expect(wide.transition.satisfied).toBe(false);
    expect(wide.batches).toBe(4);

    let deep: MidgardNativeScript = {
      type: "sig",
      keyHash: Buffer.alloc(28, 0x77),
    };
    for (let depth = 0; depth < 15; depth += 1) {
      deep = { type: "all", scripts: [deep] };
    }
    const deepestAdmissible = walk(deep);
    expect(deepestAdmissible.transition.satisfied).toBe(false);
    expect(deepestAdmissible.batches).toBe(2);
    expect(() => walk({ type: "all", scripts: [deep] })).toThrow(
      /depth bound exceeded/u,
    );
  });
});
