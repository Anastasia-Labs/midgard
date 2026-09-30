import { blake2b } from "@noble/hashes/blake2.js";

import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
} from "./bounded-item.js";
import {
  readCborArrayHeader,
  readCborBytesHeader,
  readCborUnsigned,
} from "./codec/cbor.js";
import {
  initialMidgardNativeScriptStructureControl,
  type MidgardNativeScriptStructureControl,
  MidgardNativeScriptStructureResultKinds,
} from "./native-script-scan.js";

// TS twin of `onchain/aiken/lib/midgard/fraud-proofs/native-script-decoding/
// engine.ak`: the machine bind over the versioned script wrapper, the
// machine-control commitment, and the budgeted fold's stop/refusal semantics,
// so the offchain planner predicts exactly where the on-chain fold lands for
// a given (control, window, frames, budget). Everything semantic delegates to
// the frozen scan twin in `native-script-scan.ts`.

export const MidgardNativeScriptDecodingDirections = Object.freeze({
  WrongfulAcceptance: 0,
  WrongfulRejection: 1,
} as const);

export type MidgardNativeScriptDecodingDirection =
  (typeof MidgardNativeScriptDecodingDirections)[keyof typeof MidgardNativeScriptDecodingDirections];

export const MidgardNativeScriptDecodingSourceKinds = Object.freeze({
  Normal: 0,
  Forced: 1,
} as const);

export type MidgardNativeScriptDecodingSourceKind =
  (typeof MidgardNativeScriptDecodingSourceKinds)[keyof typeof MidgardNativeScriptDecodingSourceKinds];

export const MidgardNativeScriptDecodingOutpointSources = Object.freeze({
  Spend: 0,
  Reference: 1,
} as const);

export type MidgardNativeScriptDecodingOutpointSource =
  (typeof MidgardNativeScriptDecodingOutpointSources)[keyof typeof MidgardNativeScriptDecodingOutpointSources];

export const MidgardNativeScriptDecodingRefusalClasses = Object.freeze({
  Malformed: 0,
  NodeLimit: 1,
  DepthLimit: 2,
} as const);

export type MidgardNativeScriptDecodingRefusalClass =
  (typeof MidgardNativeScriptDecodingRefusalClasses)[keyof typeof MidgardNativeScriptDecodingRefusalClasses];

export const MIDGARD_NATIVE_SCRIPT_DECODING_CLASS_PENDING = -1 as const;

export const MIDGARD_NATIVE_SCRIPT_DECODING_LANGUAGE_UNBOUND = -2 as const;

/// `engine.ak`'s conservative safe-read margin: the widest canonical token
/// (`82 00 581c` + 28 key bytes = 32 bytes) plus one spare byte.
export const MIDGARD_NATIVE_SCRIPT_DECODING_MAX_TOKEN_BYTE_WIDTH = 33 as const;

export const MIDGARD_NATIVE_SCRIPT_DECODING_CONTROL_DOMAIN =
  "midgard/fraud-proofs/native-script-decoding/control-v1" as const;

const CONTROL_DOMAIN_BYTES = Buffer.from(
  MIDGARD_NATIVE_SCRIPT_DECODING_CONTROL_DOMAIN,
  "ascii",
);

export type MidgardVersionedScriptHeader = {
  readonly languageTag: number;
  readonly payloadOffset: number;
  readonly payloadLength: number;
};

/// Twin of `native_script_scan_v1.versioned_script_header_v1`: parses the
/// versioned wrapper heads (definite array(2), canonical-minimal uint
/// language in {0, 3, 128}, definite bytes header whose payload ends exactly
/// at `itemLength`) from the first authenticated chunk. The codec readers
/// enforce the same canonical-minimal head rules as the Aiken
/// `canonical_head` and reject indefinite/reserved additional-info values.
export const parseMidgardVersionedScriptHeader = (
  firstChunk: Uint8Array,
  itemLength: number,
): MidgardVersionedScriptHeader | null => {
  try {
    const outer = readCborArrayHeader(firstChunk, 0, "versioned_script");
    if (outer.length !== 2) {
      return null;
    }
    const language = readCborUnsigned(
      firstChunk,
      outer.nextOffset,
      "versioned_script.language",
    );
    const payload = readCborBytesHeader(
      firstChunk,
      language.nextOffset,
      "versioned_script.payload",
    );
    if (
      (language.value !== 0n &&
        language.value !== 3n &&
        language.value !== 128n) ||
      payload.nextOffset + payload.length !== itemLength
    ) {
      return null;
    }
    return {
      languageTag: Number(language.value),
      payloadOffset: payload.nextOffset,
      payloadLength: payload.length,
    };
  } catch {
    return null;
  }
};

export const MidgardNativeScriptDecodingBindKinds = Object.freeze({
  Malformed: "malformed",
  NonNative: "nonNative",
  Bound: "bound",
} as const);

export type MidgardNativeScriptDecodingBindResult =
  | {
      readonly kind: typeof MidgardNativeScriptDecodingBindKinds.Malformed;
    }
  | {
      readonly kind: typeof MidgardNativeScriptDecodingBindKinds.NonNative;
      readonly languageTag: number;
    }
  | {
      readonly kind: typeof MidgardNativeScriptDecodingBindKinds.Bound;
      readonly control: MidgardNativeScriptStructureControl;
    };

/// Twin of `engine.bind_machine_v1`: undecodable wrapper or empty tag-0
/// payload is Malformed, a non-tag-0 language is NonNative, and a non-empty
/// tag-0 payload binds the initial scan control over the payload region.
export const bindMidgardNativeScriptDecodingMachine = ({
  firstChunk,
  totalLength,
}: {
  readonly firstChunk: Uint8Array;
  readonly totalLength: number;
}): MidgardNativeScriptDecodingBindResult => {
  const header = parseMidgardVersionedScriptHeader(firstChunk, totalLength);
  if (header === null) {
    return { kind: MidgardNativeScriptDecodingBindKinds.Malformed };
  }
  if (header.languageTag !== 0) {
    return {
      kind: MidgardNativeScriptDecodingBindKinds.NonNative,
      languageTag: header.languageTag,
    };
  }
  if (header.payloadLength === 0) {
    return { kind: MidgardNativeScriptDecodingBindKinds.Malformed };
  }
  return {
    kind: MidgardNativeScriptDecodingBindKinds.Bound,
    control: initialMidgardNativeScriptStructureControl({
      startOffset: header.payloadOffset,
      totalLength: header.payloadLength,
    }),
  };
};

/// Twin of `engine.hash_machine_control_v1`: blake2b-256 over the domain
/// separator and the control's canonical CBOR.
export const hashMidgardNativeScriptDecodingControl = (
  controlCbor: Uint8Array,
): Buffer =>
  Buffer.from(
    blake2b(Buffer.concat([CONTROL_DOMAIN_BYTES, Buffer.from(controlCbor)]), {
      dkLen: 32,
    }),
  );

/// Twin of `engine.ScanWindowV1`: `bytes[0]` sits at absolute item offset
/// `startOffset`.
export type MidgardNativeScriptDecodingScanWindow = {
  readonly bytes: Uint8Array;
  readonly startOffset: number;
};

/// The window `engine.authenticated_scan_window_v1` yields for a cursor: the
/// chunk holding the cursor plus — mandatorily, whenever the item has one —
/// the adjacent following chunk.
export const midgardNativeScriptDecodingScanWindowForCursor = ({
  itemBytes,
  cursor,
}: {
  readonly itemBytes: Uint8Array;
  readonly cursor: number;
}): MidgardNativeScriptDecodingScanWindow => {
  if (
    !Number.isSafeInteger(cursor) ||
    cursor < 0 ||
    cursor >= itemBytes.length
  ) {
    throw new Error("V1 decoding scan cursor is outside the item");
  }
  const chunkIndex = Math.floor(cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
  const chunkCount = midgardBoundedItemChunkCount(itemBytes.length);
  const startOffset = chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
  const endChunkIndex =
    chunkIndex + 1 < chunkCount ? chunkIndex + 2 : chunkIndex + 1;
  return {
    bytes: itemBytes.subarray(
      startOffset,
      Math.min(
        endChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        itemBytes.length,
      ),
    ),
    startOffset,
  };
};

/// Twin of `engine.safe_token_read`: a token step may only run when the
/// window provably contains every byte the widest token could need.
export const midgardNativeScriptDecodingSafeTokenRead = ({
  control,
  windowStart,
  windowEnd,
}: {
  readonly control: MidgardNativeScriptStructureControl;
  readonly windowStart: number;
  readonly windowEnd: number;
}): boolean =>
  control.cursor >= windowStart &&
  control.cursor < windowEnd &&
  (windowEnd >= control.endOffset ||
    windowEnd - control.cursor >=
      MIDGARD_NATIVE_SCRIPT_DECODING_MAX_TOKEN_BYTE_WIDTH);

export const MidgardNativeScriptDecodingScanOutcomeKinds = Object.freeze({
  Advanced: "advanced",
  Refused: "refused",
} as const);

export type MidgardNativeScriptDecodingScanOutcome =
  | {
      readonly kind: typeof MidgardNativeScriptDecodingScanOutcomeKinds.Advanced;
      readonly control: MidgardNativeScriptStructureControl;
      readonly framesConsumed: number;
    }
  | {
      readonly kind: typeof MidgardNativeScriptDecodingScanOutcomeKinds.Refused;
      readonly refusalClass: MidgardNativeScriptDecodingRefusalClass;
      readonly framesConsumed: number;
    };

export const refusalClassOfResultKind = (
  kind:
    | typeof MidgardNativeScriptStructureResultKinds.Invalid
    | typeof MidgardNativeScriptStructureResultKinds.NodeLimit
    | typeof MidgardNativeScriptStructureResultKinds.DepthLimit,
): MidgardNativeScriptDecodingRefusalClass =>
  kind === MidgardNativeScriptStructureResultKinds.Invalid
    ? MidgardNativeScriptDecodingRefusalClasses.Malformed
    : kind === MidgardNativeScriptStructureResultKinds.NodeLimit
      ? MidgardNativeScriptDecodingRefusalClasses.NodeLimit
      : MidgardNativeScriptDecodingRefusalClasses.DepthLimit;
