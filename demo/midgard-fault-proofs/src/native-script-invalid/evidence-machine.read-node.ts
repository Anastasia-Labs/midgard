import {
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "@al-ft/midgard-core";
import { type NativeScriptPushdownFrame } from "@al-ft/midgard-sdk";

import {
  chainFrame,
  hash32,
  MAX_FRAMES,
  MAX_NODES,
  type PushdownState,
  SCRIPT_CURSOR_DOMAIN,
  SCRIPT_FRAME_DOMAIN,
  u24,
  UNSATISFIABLE_REQUIRED,
} from "./evidence-machine.native-script-invalid-signer-set.js";

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

export const encodeCursor = (state: PushdownState): Buffer => {
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
    throw new Error("native-script-invalid: cursor is not exactly 87 bytes");
  }
  return result;
};

export const cursorHash = (state: PushdownState): Buffer =>
  hash32(Buffer.concat([SCRIPT_CURSOR_DOMAIN, encodeCursor(state)]));

export const decodeCursor = ({
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
    throw new Error("native-script-invalid: cursor must be exactly 87 bytes");
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
    throw new Error("native-script-invalid: cursor commitment is invalid");
  }
  return state;
};

export const complete = (state: PushdownState): boolean =>
  state.frames.length === 0 && state.pending !== 0;

export const readNode = ({
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
    throw new Error("native-script-invalid: malformed native script node");
  }
  const nodesVisited = state.nodesVisited + 1;
  if (nodesVisited > MAX_NODES) {
    throw new Error("native-script-invalid: native script node bound exceeded");
  }
  if (kind === 0) {
    const key = readCborBytes(
      scriptBytes,
      tag.nextOffset,
      "native signer hash",
    );
    if (key.value.length !== 28) {
      throw new Error("native-script-invalid: signer hash is not 28 bytes");
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
      "native-script-invalid: native script child bound exceeded",
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
      "native-script-invalid: native script depth bound exceeded",
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

export const foldFrame = (state: PushdownState): PushdownState => {
  const [frame, ...rest] = state.frames;
  if (frame === undefined) {
    throw new Error("native-script-invalid: no frame for pending verdict");
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

export type NativeScriptInvalidPushdownStep = Readonly<{
  currentCursorBytes?: string;
  currentFrames: readonly NativeScriptPushdownFrame[];
  nextCursorBytes: string;
  nextCursorHash: string;
  nextFrames: readonly NativeScriptPushdownFrame[];
  signerHashes: readonly string[];
  complete: boolean;
  satisfied?: boolean;
}>;
