import { type NativeScriptPushdownFrame } from "@al-ft/midgard-sdk";

import {
  EXECUTION_NATIVE_SCRIPT_INVALID_NODE_BATCH,
  type ExecutionNativeScriptInvalidSignerSet,
  hash32,
  MAX_NODES,
} from "./evidence-machine.execution-native-script-invalid-signer-set.js";
import {
  complete,
  cursorHash,
  decodeCursor,
  encodeCursor,
  type ExecutionNativeScriptInvalidPushdownStep,
  foldFrame,
  readNode,
} from "./evidence-machine.read-node.js";

export const executionNativeScriptInvalidPushdownStep = ({
  scriptBytes: rawScriptBytes,
  validityIntervalStart,
  validityIntervalEnd,
  signerSet,
  nodeBudget = EXECUTION_NATIVE_SCRIPT_INVALID_NODE_BATCH,
  committedCursorHash,
  cursorBytes,
  frames = [],
}: {
  readonly scriptBytes: Uint8Array;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly signerSet: ExecutionNativeScriptInvalidSignerSet;
  readonly nodeBudget?: number;
  readonly committedCursorHash?: string;
  readonly cursorBytes?: Uint8Array;
  readonly frames?: readonly NativeScriptPushdownFrame[];
}): ExecutionNativeScriptInvalidPushdownStep => {
  if (
    !Number.isSafeInteger(nodeBudget) ||
    nodeBudget <= 0 ||
    nodeBudget > EXECUTION_NATIVE_SCRIPT_INVALID_NODE_BATCH
  ) {
    throw new Error(
      "execution-native-script-invalid: node budget must be 1..16",
    );
  }
  const scriptBytes = Buffer.from(rawScriptBytes);
  let state =
    committedCursorHash === undefined
      ? {
          scriptDigest: hash32(scriptBytes),
          scriptLength: scriptBytes.length,
          offset: 0,
          frames: [],
          nodesVisited: 0,
          pending: 0 as const,
        }
      : decodeCursor({
          bytes:
            cursorBytes ??
            (() => {
              throw new Error(
                "execution-native-script-invalid: resume cursor is missing",
              );
            })(),
          frames,
          scriptBytes,
          committedHash: committedCursorHash,
        });
  const currentCursorBytes =
    committedCursorHash === undefined
      ? undefined
      : encodeCursor(state).toString("hex");
  const queriedSigners: Buffer[] = [];
  for (let index = 0; index < nodeBudget && !complete(state); index += 1) {
    state =
      state.pending === 0
        ? readNode({
            state,
            scriptBytes,
            validityIntervalStart,
            validityIntervalEnd,
            signerIsPresent: (hash) =>
              signerSet.hashes.some((candidate) => candidate.equals(hash)),
            queriedSigners,
          })
        : foldFrame(state);
  }
  const isComplete = complete(state);
  if (isComplete && state.offset !== state.scriptLength) {
    throw new Error(
      "execution-native-script-invalid: native script has trailing bytes",
    );
  }
  return {
    ...(currentCursorBytes === undefined ? {} : { currentCursorBytes }),
    currentFrames: frames,
    nextCursorBytes: encodeCursor(state).toString("hex"),
    nextCursorHash: cursorHash(state).toString("hex"),
    nextFrames: state.frames,
    signerHashes: queriedSigners.map((hash) => hash.toString("hex")),
    complete: isComplete,
    ...(isComplete ? { satisfied: state.pending === 2 } : {}),
  };
};

/**
 * Reconstructs the unique deterministic resume material from a thread-carried
 * cursor hash. This is restart-safe: neither cursor bytes nor frames are
 * trusted journal state, and the walk is bounded by the canonical 32-node
 * native-script maximum.
 */
export const resolveExecutionNativeScriptInvalidPushdownResume = ({
  scriptBytes,
  validityIntervalStart,
  validityIntervalEnd,
  signerSet,
  committedCursorHash,
  nodeBudget = EXECUTION_NATIVE_SCRIPT_INVALID_NODE_BATCH,
}: {
  readonly scriptBytes: Uint8Array;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly signerSet: ExecutionNativeScriptInvalidSignerSet;
  readonly committedCursorHash: string;
  readonly nodeBudget?: number;
}): Readonly<{
  cursorBytes: Buffer;
  frames: readonly NativeScriptPushdownFrame[];
}> => {
  if (!/^[0-9a-f]{64}$/u.test(committedCursorHash)) {
    throw new Error(
      "execution-native-script-invalid: committed resume hash is not 32-byte hex",
    );
  }
  let transition = executionNativeScriptInvalidPushdownStep({
    scriptBytes,
    validityIntervalStart,
    validityIntervalEnd,
    signerSet,
    nodeBudget,
  });
  for (let batches = 0; batches <= MAX_NODES; batches += 1) {
    if (transition.nextCursorHash === committedCursorHash) {
      return Object.freeze({
        cursorBytes: Buffer.from(transition.nextCursorBytes, "hex"),
        frames: Object.freeze([...transition.nextFrames]),
      });
    }
    if (transition.complete) break;
    transition = executionNativeScriptInvalidPushdownStep({
      scriptBytes,
      validityIntervalStart,
      validityIntervalEnd,
      signerSet,
      nodeBudget,
      committedCursorHash: transition.nextCursorHash,
      cursorBytes: Buffer.from(transition.nextCursorBytes, "hex"),
      frames: transition.nextFrames,
    });
  }
  throw new Error(
    "execution-native-script-invalid: committed cursor is unreachable by the deterministic pushdown schedule",
  );
};
