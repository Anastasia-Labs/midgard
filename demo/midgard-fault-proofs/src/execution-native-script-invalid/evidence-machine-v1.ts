import {
  appendMidgardValidationMerkleLeaf,
  buildMidgardValidationMerkleMembershipIndex,
  computeHash32,
  decodeMidgardAddressWitnessItem,
  emptyMidgardValidationMerkleFrontier,
  hashMidgardSignerLeaf,
  type MidgardValidationMerkleFrontier,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "@al-ft/midgard-core";
import {
  type FrontierPeak,
  missingSignatureFieldWalkCheckpoint,
  missingSignatureVkeyHash,
  type NativeScriptPushdownFrame,
  type SignerSetProof,
} from "@al-ft/midgard-sdk";

export const EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT = 28;
export const EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH = 16;
export const EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH = 16;
export const EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_FINALIZE_BATCH = 16;
export const EXECUTION_NATIVE_SCRIPT_INVALID_NODE_BATCH = 16;

export const executionNativeScriptInvalidUsesDirectRoute = ({
  signerCount,
  scriptBytes,
}: {
  readonly signerCount: number;
  readonly scriptBytes: number;
}): boolean =>
  signerCount <= EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT &&
  scriptBytes <= 1_024;

export const assertExecutionNativeScriptInvalidDirectRoute = (
  signerCount: number,
) => {
  if (signerCount > EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT) {
    throw new Error(
      `execution-native-script-invalid: direct signer limit is ${EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT.toString()}; use the staged route`,
    );
  }
};

const hash32 = (bytes: Uint8Array): Buffer => computeHash32(bytes);

const u24 = (value: number, label: string): Buffer => {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xff_ffff) {
    throw new Error(`${label} must fit an unsigned 24-bit word`);
  }
  const result = Buffer.alloc(3);
  result.writeUIntBE(value, 0, 3);
  return result;
};

const exactSignerHashes = (
  addressWitnessItems: readonly Uint8Array[],
): readonly Buffer[] => {
  const hashes: Buffer[] = [];
  for (const item of addressWitnessItems) {
    const witness = decodeMidgardAddressWitnessItem(item);
    const hash = Buffer.from(
      missingSignatureVkeyHash(
        Buffer.from(witness.verificationKey).toString("hex"),
      ),
      "hex",
    );
    const previous = hashes.at(-1);
    if (previous !== undefined && Buffer.compare(previous, hash) > 0) {
      throw new Error(
        "execution-native-script-invalid: address-witness signer hashes are not canonical",
      );
    }
    if (previous === undefined || !previous.equals(hash)) hashes.push(hash);
  }
  return hashes;
};

const frontierWire = (
  frontier: MidgardValidationMerkleFrontier,
): FrontierPeak[] =>
  frontier.peaks.map((peak) => ({
    height: BigInt(peak.height),
    hash: Buffer.from(peak.hash).toString("hex"),
  }));

export type ExecutionNativeScriptInvalidSignerScanState = Readonly<{
  checkpointBytes: string;
  checkpointHash: string;
  previousSignerHash: string;
  signerCount: bigint;
  signerPeaks: readonly FrontierPeak[];
  nextItemIndex: number;
  complete: boolean;
}>;

export const resolveExecutionNativeScriptInvalidSignerCheckpoint = ({
  txId,
  itemCount,
  totalLength,
  committedHash,
}: {
  readonly txId: string;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly committedHash: string;
}) => {
  missingSignatureFieldWalkCheckpoint({
    txId,
    itemCount,
    totalLength,
    nextItemIndex: 0,
  });
  if (committedHash === "") return null;
  if (!/^[0-9a-f]{64}$/u.test(committedHash)) {
    throw new Error(
      "execution-native-script-invalid checkpoint commitment must be 32-byte lowercase hex",
    );
  }
  for (
    let cursor = EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH;
    cursor < itemCount;
    cursor += EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH
  ) {
    const candidate = missingSignatureFieldWalkCheckpoint({
      txId,
      itemCount,
      totalLength,
      nextItemIndex: cursor,
    });
    if (candidate.checkpointHash === committedHash) return candidate;
  }
  throw new Error(
    "execution-native-script-invalid checkpoint commitment is not reachable by the deterministic signer scan schedule",
  );
};

export const executionNativeScriptInvalidSignerScanState = ({
  txId,
  addressWitnessItems,
  totalLength,
  committedCheckpointHash = "",
  batchSize = EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH,
}: {
  readonly txId: string;
  readonly addressWitnessItems: readonly Uint8Array[];
  readonly totalLength: number;
  readonly committedCheckpointHash?: string;
  readonly batchSize?: number;
}): ExecutionNativeScriptInvalidSignerScanState => {
  if (
    !Number.isSafeInteger(batchSize) ||
    batchSize <= 0 ||
    batchSize > EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH
  ) {
    throw new Error(
      "execution-native-script-invalid: signer batch size must be 1..16",
    );
  }
  const current = resolveExecutionNativeScriptInvalidSignerCheckpoint({
    txId,
    itemCount: addressWitnessItems.length,
    totalLength,
    committedHash: committedCheckpointHash,
  });
  const currentIndex = current?.nextItemIndex ?? 0;
  const nextItemIndex = Math.min(
    addressWitnessItems.length,
    currentIndex + batchSize,
  );
  const signerHashes = exactSignerHashes(
    addressWitnessItems.slice(0, nextItemIndex),
  );
  const frontier = signerHashes.reduce(
    (currentFrontier, signerHash) =>
      appendMidgardValidationMerkleLeaf(
        currentFrontier,
        hashMidgardSignerLeaf(signerHash),
      ),
    emptyMidgardValidationMerkleFrontier(),
  );
  const checkpoint = missingSignatureFieldWalkCheckpoint({
    txId,
    itemCount: addressWitnessItems.length,
    totalLength,
    nextItemIndex,
  });
  return {
    checkpointBytes: checkpoint.checkpointCbor,
    checkpointHash: checkpoint.checkpointHash,
    previousSignerHash: signerHashes.at(-1)?.toString("hex") ?? "",
    signerCount: BigInt(signerHashes.length),
    signerPeaks: frontierWire(frontier),
    nextItemIndex,
    complete: nextItemIndex === addressWitnessItems.length,
  };
};

export type ExecutionNativeScriptInvalidSignerSet = Readonly<{
  hashes: readonly Buffer[];
  frontier: MidgardValidationMerkleFrontier;
  proofFor: (signerHash: Uint8Array) => SignerSetProof;
}>;

export const executionNativeScriptInvalidSignerSet = (
  addressWitnessItems: readonly Uint8Array[],
): ExecutionNativeScriptInvalidSignerSet => {
  const hashes = exactSignerHashes(addressWitnessItems);
  const leafHashes = hashes.map(hashMidgardSignerLeaf);
  const membership = buildMidgardValidationMerkleMembershipIndex(leafHashes);
  const peaks = frontierWire(membership.frontier);
  const proofFor = (raw: Uint8Array): SignerSetProof => {
    const signerHash = Buffer.from(raw);
    if (signerHash.length !== 28) {
      throw new Error(
        "execution-native-script-invalid: signer query must be 28 bytes",
      );
    }
    const insertionIndex = hashes.findIndex(
      (candidate) => Buffer.compare(candidate, signerHash) >= 0,
    );
    if (insertionIndex >= 0 && hashes[insertionIndex]!.equals(signerHash)) {
      const exact = membership.membershipAt(insertionIndex);
      return {
        SignerMembershipProof: {
          peaks,
          signer_index: BigInt(insertionIndex),
          siblings: exact.siblings.map((value) =>
            Buffer.from(value).toString("hex"),
          ),
        },
      };
    }
    if (hashes.length === 0) return { EmptySignerSetProof: { peaks } };
    if (insertionIndex === 0) {
      const exact = membership.membershipAt(0);
      return {
        SignerBelowFirstProof: {
          peaks,
          first_signer_hash: hashes[0]!.toString("hex"),
          siblings: exact.siblings.map((value) =>
            Buffer.from(value).toString("hex"),
          ),
        },
      };
    }
    if (insertionIndex === -1) {
      const lastIndex = hashes.length - 1;
      const exact = membership.membershipAt(lastIndex);
      return {
        SignerAboveLastProof: {
          peaks,
          last_signer_hash: hashes[lastIndex]!.toString("hex"),
          siblings: exact.siblings.map((value) =>
            Buffer.from(value).toString("hex"),
          ),
        },
      };
    }
    const lowerIndex = insertionIndex - 1;
    const lower = membership.membershipAt(lowerIndex);
    const upper = membership.membershipAt(insertionIndex);
    return {
      SignerBetweenProof: {
        peaks,
        lower_index: BigInt(lowerIndex),
        lower_signer_hash: hashes[lowerIndex]!.toString("hex"),
        lower_siblings: lower.siblings.map((value) =>
          Buffer.from(value).toString("hex"),
        ),
        upper_signer_hash: hashes[insertionIndex]!.toString("hex"),
        upper_siblings: upper.siblings.map((value) =>
          Buffer.from(value).toString("hex"),
        ),
      },
    };
  };
  return { hashes, frontier: membership.frontier, proofFor };
};

const SCRIPT_CURSOR_DOMAIN = Buffer.from("MidgardNativeScriptWalkV1", "ascii");
const SCRIPT_FRAME_DOMAIN = Buffer.from("MidgardNativeScriptFrameV1", "ascii");
const MAX_NODES = 32;
const MAX_FRAMES = 15;
const UNSATISFIABLE_REQUIRED = MAX_NODES + 1;

type PushdownState = {
  readonly scriptDigest: Buffer;
  readonly scriptLength: number;
  readonly offset: number;
  readonly frames: readonly NativeScriptPushdownFrame[];
  readonly nodesVisited: number;
  readonly pending: 0 | 1 | 2;
};

const encodeFrame = (frame: NativeScriptPushdownFrame): Buffer =>
  Buffer.concat([
    Buffer.from([Number(frame.kind)]),
    u24(Number(frame.remaining), "native script frame remaining"),
    u24(Number(frame.satisfied), "native script frame satisfied"),
    u24(Number(frame.required), "native script frame required"),
  ]);

const chainFrame = (below: Buffer, frame: NativeScriptPushdownFrame): Buffer =>
  hash32(Buffer.concat([SCRIPT_FRAME_DOMAIN, below, encodeFrame(frame)]));

const frameRoots = (
  frames: readonly NativeScriptPushdownFrame[],
): readonly Buffer[] => {
  const roots: Buffer[] = new Array(frames.length);
  let below = hash32(SCRIPT_FRAME_DOMAIN);
  for (let index = frames.length - 1; index >= 0; index -= 1) {
    below = chainFrame(below, frames[index]!);
    roots[index] = below;
  }
  return roots;
};

const encodeCursor = (state: PushdownState): Buffer => {
  const roots = frameRoots(state.frames);
  const stackRoot = roots[0] ?? hash32(SCRIPT_FRAME_DOMAIN);
  const result = Buffer.concat([
    Buffer.from([0x87, 0x58, 0x20]),
    state.scriptDigest,
    Buffer.from([0x58, 0x20]),
    stackRoot,
    Buffer.from([0x43]),
    u24(state.scriptLength, "native script length"),
    Buffer.from([0x43]),
    u24(state.offset, "native script cursor offset"),
    Buffer.from([0x43]),
    u24(state.frames.length, "native script frame depth"),
    Buffer.from([0x43]),
    u24(state.nodesVisited, "native script nodes visited"),
    Buffer.from([0x41, state.pending]),
  ]);
  if (result.length !== 87) {
    throw new Error(
      "execution-native-script-invalid: cursor is not exactly 87 bytes",
    );
  }
  return result;
};

const cursorHash = (state: PushdownState): Buffer =>
  hash32(Buffer.concat([SCRIPT_CURSOR_DOMAIN, encodeCursor(state)]));

const decodeCursor = ({
  bytes,
  frames,
  scriptBytes,
  committedHash,
}: {
  readonly bytes: Uint8Array;
  readonly frames: readonly NativeScriptPushdownFrame[];
  readonly scriptBytes: Uint8Array;
  readonly committedHash: string;
}): PushdownState => {
  const value = Buffer.from(bytes);
  if (value.length !== 87) {
    throw new Error(
      "execution-native-script-invalid: cursor must be exactly 87 bytes",
    );
  }
  const state: PushdownState = {
    scriptDigest: value.subarray(3, 35),
    scriptLength: value.readUIntBE(70, 3),
    offset: value.readUIntBE(74, 3),
    frames,
    nodesVisited: value.readUIntBE(82, 3),
    pending: value[86] as 0 | 1 | 2,
  };
  if (
    !encodeCursor(state).equals(value) ||
    cursorHash(state).toString("hex") !== committedHash ||
    !state.scriptDigest.equals(hash32(scriptBytes)) ||
    state.scriptLength !== scriptBytes.length
  ) {
    throw new Error(
      "execution-native-script-invalid: cursor commitment is invalid",
    );
  }
  return state;
};

const complete = (state: PushdownState): boolean =>
  state.frames.length === 0 && state.pending !== 0;

const readNode = ({
  state,
  scriptBytes,
  validityIntervalStart,
  validityIntervalEnd,
  signerIsPresent,
  queriedSigners,
}: {
  readonly state: PushdownState;
  readonly scriptBytes: Buffer;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly signerIsPresent: (hash: Buffer) => boolean;
  readonly queriedSigners: Buffer[];
}): PushdownState => {
  const outer = readCborArrayHeader(scriptBytes, state.offset, "native script");
  const tag = readCborUnsigned(
    scriptBytes,
    outer.nextOffset,
    "native script tag",
  );
  const kind = Number(tag.value);
  if (kind < 0 || kind > 5 || outer.length !== (kind === 3 ? 3 : 2)) {
    throw new Error(
      "execution-native-script-invalid: malformed native script node",
    );
  }
  const nodesVisited = state.nodesVisited + 1;
  if (nodesVisited > MAX_NODES) {
    throw new Error(
      "execution-native-script-invalid: native script node bound exceeded",
    );
  }
  if (kind === 0) {
    const key = readCborBytes(
      scriptBytes,
      tag.nextOffset,
      "native signer hash",
    );
    if (key.value.length !== 28) {
      throw new Error(
        "execution-native-script-invalid: signer hash is not 28 bytes",
      );
    }
    queriedSigners.push(key.value);
    return {
      ...state,
      offset: key.nextOffset,
      nodesVisited,
      pending: signerIsPresent(key.value) ? 2 : 1,
    };
  }
  if (kind === 4 || kind === 5) {
    const slot = readCborUnsigned(
      scriptBytes,
      tag.nextOffset,
      "native script slot",
    );
    const satisfied =
      kind === 4
        ? validityIntervalStart >= 0n && validityIntervalStart >= slot.value
        : validityIntervalEnd >= 0n && validityIntervalEnd <= slot.value;
    return {
      ...state,
      offset: slot.nextOffset,
      nodesVisited,
      pending: satisfied ? 2 : 1,
    };
  }
  let cursor = tag.nextOffset;
  let required: bigint;
  if (kind === 3) {
    const threshold = readCborUnsigned(
      scriptBytes,
      cursor,
      "native script threshold",
    );
    required = threshold.value;
    cursor = threshold.nextOffset;
  } else {
    required = kind === 2 ? 1n : 0n;
  }
  const children = readCborArrayHeader(
    scriptBytes,
    cursor,
    "native script children",
  );
  if (children.length > MAX_NODES) {
    throw new Error(
      "execution-native-script-invalid: native script child bound exceeded",
    );
  }
  if (kind === 1) required = BigInt(children.length);
  if (required > BigInt(MAX_NODES)) required = BigInt(UNSATISFIABLE_REQUIRED);
  if (children.length === 0) {
    return {
      ...state,
      offset: children.nextOffset,
      nodesVisited,
      pending: 0n >= required ? 2 : 1,
    };
  }
  if (state.frames.length >= MAX_FRAMES) {
    throw new Error(
      "execution-native-script-invalid: native script depth bound exceeded",
    );
  }
  const frame: NativeScriptPushdownFrame = {
    kind: BigInt(kind),
    remaining: BigInt(children.length),
    satisfied: 0n,
    required,
  };
  return {
    ...state,
    offset: children.nextOffset,
    frames: [frame, ...state.frames],
    nodesVisited,
  };
};

const foldFrame = (state: PushdownState): PushdownState => {
  const [frame, ...rest] = state.frames;
  if (frame === undefined) {
    throw new Error(
      "execution-native-script-invalid: no frame for pending verdict",
    );
  }
  const satisfied = Number(frame.satisfied) + (state.pending === 2 ? 1 : 0);
  const remaining = Number(frame.remaining) - 1;
  if (remaining === 0) {
    return {
      ...state,
      frames: rest,
      pending: BigInt(satisfied) >= frame.required ? 2 : 1,
    };
  }
  return {
    ...state,
    frames: [
      {
        ...frame,
        remaining: BigInt(remaining),
        satisfied: BigInt(satisfied),
      },
      ...rest,
    ],
    pending: 0,
  };
};

export type ExecutionNativeScriptInvalidPushdownStep = Readonly<{
  currentCursorBytes?: string;
  currentFrames: readonly NativeScriptPushdownFrame[];
  nextCursorBytes: string;
  nextCursorHash: string;
  nextFrames: readonly NativeScriptPushdownFrame[];
  signerHashes: readonly string[];
  complete: boolean;
  satisfied?: boolean;
}>;

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
