import { blake2b } from "@noble/hashes/blake2.js";

import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  midgardBoundedItemChunkCountV1,
} from "./bounded-item-v1.js";
import {
  readCborArrayHeader,
  readCborBytesHeader,
  readCborUnsigned,
} from "./codec/cbor.js";
import {
  advanceMidgardNativeScriptStructureFrameV1,
  advanceMidgardNativeScriptStructureTokenV1,
  finalizeMidgardNativeScriptStructureV1,
  initialMidgardNativeScriptStructureControlV1,
  isExactMidgardNativeScriptStructureTerminalV1,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1,
  MidgardNativeScriptKindsV1,
  type MidgardNativeScriptScanFrameV1,
  type MidgardNativeScriptStructureControlV1,
  MidgardNativeScriptStructureResultKindsV1,
  MidgardNativeScriptStructureStagesV1,
  readMidgardNativeScriptStructureTokenV1,
} from "./native-script-scan-v1.js";

// TS twin of `onchain/aiken/lib/midgard/fraud-proofs/native-script-decoding/
// engine.ak`: the machine bind over the versioned script wrapper, the
// machine-control commitment, and the budgeted fold's stop/refusal semantics,
// so the offchain planner predicts exactly where the on-chain fold lands for
// a given (control, window, frames, budget). Everything semantic delegates to
// the frozen scan twin in `native-script-scan-v1.ts`.

export const MidgardNativeScriptDecodingDirectionsV1 = Object.freeze({
  WrongfulAcceptance: 0,
  WrongfulRejection: 1,
} as const);

export type MidgardNativeScriptDecodingDirectionV1 =
  (typeof MidgardNativeScriptDecodingDirectionsV1)[keyof typeof MidgardNativeScriptDecodingDirectionsV1];

export const MidgardNativeScriptDecodingSourceKindsV1 = Object.freeze({
  Normal: 0,
  Forced: 1,
} as const);

export type MidgardNativeScriptDecodingSourceKindV1 =
  (typeof MidgardNativeScriptDecodingSourceKindsV1)[keyof typeof MidgardNativeScriptDecodingSourceKindsV1];

export const MidgardNativeScriptDecodingOutpointSourcesV1 = Object.freeze({
  Spend: 0,
  Reference: 1,
} as const);

export type MidgardNativeScriptDecodingOutpointSourceV1 =
  (typeof MidgardNativeScriptDecodingOutpointSourcesV1)[keyof typeof MidgardNativeScriptDecodingOutpointSourcesV1];

export const MidgardNativeScriptDecodingRefusalClassesV1 = Object.freeze({
  Malformed: 0,
  NodeLimit: 1,
  DepthLimit: 2,
} as const);

export type MidgardNativeScriptDecodingRefusalClassV1 =
  (typeof MidgardNativeScriptDecodingRefusalClassesV1)[keyof typeof MidgardNativeScriptDecodingRefusalClassesV1];

export const MIDGARD_NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1 = -1 as const;
export const MIDGARD_NATIVE_SCRIPT_DECODING_LANGUAGE_UNBOUND_V1 = -2 as const;

/// `engine.ak`'s conservative safe-read margin: the widest canonical token
/// (`82 00 581c` + 28 key bytes = 32 bytes) plus one spare byte.
export const MIDGARD_NATIVE_SCRIPT_DECODING_MAX_TOKEN_BYTE_WIDTH_V1 =
  33 as const;

export const MIDGARD_NATIVE_SCRIPT_DECODING_CONTROL_DOMAIN_V1 =
  "midgard/fraud-proofs/native-script-decoding/control-v1" as const;

const CONTROL_DOMAIN_BYTES_V1 = Buffer.from(
  MIDGARD_NATIVE_SCRIPT_DECODING_CONTROL_DOMAIN_V1,
  "ascii",
);

export type MidgardVersionedScriptHeaderV1 = {
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
export const parseMidgardVersionedScriptHeaderV1 = (
  firstChunk: Uint8Array,
  itemLength: number,
): MidgardVersionedScriptHeaderV1 | null => {
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

export const MidgardNativeScriptDecodingBindKindsV1 = Object.freeze({
  Malformed: "malformed",
  NonNative: "nonNative",
  Bound: "bound",
} as const);

export type MidgardNativeScriptDecodingBindResultV1 =
  | {
      readonly kind: typeof MidgardNativeScriptDecodingBindKindsV1.Malformed;
    }
  | {
      readonly kind: typeof MidgardNativeScriptDecodingBindKindsV1.NonNative;
      readonly languageTag: number;
    }
  | {
      readonly kind: typeof MidgardNativeScriptDecodingBindKindsV1.Bound;
      readonly control: MidgardNativeScriptStructureControlV1;
    };

/// Twin of `engine.bind_machine_v1`: undecodable wrapper or empty tag-0
/// payload is Malformed, a non-tag-0 language is NonNative, and a non-empty
/// tag-0 payload binds the initial scan control over the payload region.
export const bindMidgardNativeScriptDecodingMachineV1 = ({
  firstChunk,
  totalLength,
}: {
  readonly firstChunk: Uint8Array;
  readonly totalLength: number;
}): MidgardNativeScriptDecodingBindResultV1 => {
  const header = parseMidgardVersionedScriptHeaderV1(firstChunk, totalLength);
  if (header === null) {
    return { kind: MidgardNativeScriptDecodingBindKindsV1.Malformed };
  }
  if (header.languageTag !== 0) {
    return {
      kind: MidgardNativeScriptDecodingBindKindsV1.NonNative,
      languageTag: header.languageTag,
    };
  }
  if (header.payloadLength === 0) {
    return { kind: MidgardNativeScriptDecodingBindKindsV1.Malformed };
  }
  return {
    kind: MidgardNativeScriptDecodingBindKindsV1.Bound,
    control: initialMidgardNativeScriptStructureControlV1({
      startOffset: header.payloadOffset,
      totalLength: header.payloadLength,
    }),
  };
};

/// Twin of `engine.hash_machine_control_v1`: blake2b-256 over the domain
/// separator and the control's canonical CBOR.
export const hashMidgardNativeScriptDecodingControlV1 = (
  controlCbor: Uint8Array,
): Buffer =>
  Buffer.from(
    blake2b(
      Buffer.concat([CONTROL_DOMAIN_BYTES_V1, Buffer.from(controlCbor)]),
      {
        dkLen: 32,
      },
    ),
  );

/// Twin of `engine.ScanWindowV1`: `bytes[0]` sits at absolute item offset
/// `startOffset`.
export type MidgardNativeScriptDecodingScanWindowV1 = {
  readonly bytes: Uint8Array;
  readonly startOffset: number;
};

/// The window `engine.authenticated_scan_window_v1` yields for a cursor: the
/// chunk holding the cursor plus — mandatorily, whenever the item has one —
/// the adjacent following chunk.
export const midgardNativeScriptDecodingScanWindowForCursorV1 = ({
  itemBytes,
  cursor,
}: {
  readonly itemBytes: Uint8Array;
  readonly cursor: number;
}): MidgardNativeScriptDecodingScanWindowV1 => {
  if (
    !Number.isSafeInteger(cursor) ||
    cursor < 0 ||
    cursor >= itemBytes.length
  ) {
    throw new Error("V1 decoding scan cursor is outside the item");
  }
  const chunkIndex = Math.floor(cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
  const chunkCount = midgardBoundedItemChunkCountV1(itemBytes.length);
  const startOffset = chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
  const endChunkIndex =
    chunkIndex + 1 < chunkCount ? chunkIndex + 2 : chunkIndex + 1;
  return {
    bytes: itemBytes.subarray(
      startOffset,
      Math.min(
        endChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
        itemBytes.length,
      ),
    ),
    startOffset,
  };
};

/// Twin of `engine.safe_token_read`: a token step may only run when the
/// window provably contains every byte the widest token could need.
export const midgardNativeScriptDecodingSafeTokenReadV1 = ({
  control,
  windowStart,
  windowEnd,
}: {
  readonly control: MidgardNativeScriptStructureControlV1;
  readonly windowStart: number;
  readonly windowEnd: number;
}): boolean =>
  control.cursor >= windowStart &&
  control.cursor < windowEnd &&
  (windowEnd >= control.endOffset ||
    windowEnd - control.cursor >=
      MIDGARD_NATIVE_SCRIPT_DECODING_MAX_TOKEN_BYTE_WIDTH_V1);

export const MidgardNativeScriptDecodingScanOutcomeKindsV1 = Object.freeze({
  Advanced: "advanced",
  Refused: "refused",
} as const);

export type MidgardNativeScriptDecodingScanOutcomeV1 =
  | {
      readonly kind: typeof MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced;
      readonly control: MidgardNativeScriptStructureControlV1;
      readonly framesConsumed: number;
    }
  | {
      readonly kind: typeof MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused;
      readonly refusalClass: MidgardNativeScriptDecodingRefusalClassV1;
      readonly framesConsumed: number;
    };

const refusalClassOfResultKind = (
  kind:
    | typeof MidgardNativeScriptStructureResultKindsV1.Invalid
    | typeof MidgardNativeScriptStructureResultKindsV1.NodeLimit
    | typeof MidgardNativeScriptStructureResultKindsV1.DepthLimit,
): MidgardNativeScriptDecodingRefusalClassV1 =>
  kind === MidgardNativeScriptStructureResultKindsV1.Invalid
    ? MidgardNativeScriptDecodingRefusalClassesV1.Malformed
    : kind === MidgardNativeScriptStructureResultKindsV1.NodeLimit
      ? MidgardNativeScriptDecodingRefusalClassesV1.NodeLimit
      : MidgardNativeScriptDecodingRefusalClassesV1.DepthLimit;

/// Twin of `engine.budgeted_scan_v1`: up to `maxSteps` primitive steps,
/// stopping (never refusing) at the terminal, on budget exhaustion, when the
/// frame witnesses run out, when there is no window on a token stage, or
/// when the safe-read margin blocks a token read. A frame witness that does
/// not hash-chain to the control's stack root throws — the on-chain fold
/// aborts the transaction there (witness error, never a verdict), so the
/// twin must never present it as an outcome.
export const budgetedMidgardNativeScriptDecodingScanV1 = ({
  control,
  window,
  frames,
  maxSteps,
}: {
  readonly control: MidgardNativeScriptStructureControlV1;
  readonly window: MidgardNativeScriptDecodingScanWindowV1 | null;
  readonly frames: readonly MidgardNativeScriptScanFrameV1[];
  readonly maxSteps: number;
}): MidgardNativeScriptDecodingScanOutcomeV1 => {
  let current = control;
  let frameIndex = 0;
  let remainingSteps = maxSteps;
  for (;;) {
    if (
      remainingSteps <= 0 ||
      current.stage === MidgardNativeScriptStructureStagesV1.Terminal
    ) {
      return {
        kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced,
        control: current,
        framesConsumed: frameIndex,
      };
    }
    let result;
    if (current.stage === MidgardNativeScriptStructureStagesV1.Token) {
      if (window === null) {
        return {
          kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced,
          control: current,
          framesConsumed: frameIndex,
        };
      }
      const windowEnd = window.startOffset + window.bytes.length;
      if (
        !midgardNativeScriptDecodingSafeTokenReadV1({
          control: current,
          windowStart: window.startOffset,
          windowEnd,
        })
      ) {
        return {
          kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced,
          control: current,
          framesConsumed: frameIndex,
        };
      }
      result = advanceMidgardNativeScriptStructureTokenV1({
        control: current,
        window: window.bytes,
        windowOffset: current.cursor - window.startOffset,
      });
      if (result === null) {
        throw new Error("V1 decoding scan token step rejected its control");
      }
    } else if (current.stage === MidgardNativeScriptStructureStagesV1.Frame) {
      if (frameIndex >= frames.length) {
        return {
          kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced,
          control: current,
          framesConsumed: frameIndex,
        };
      }
      result = advanceMidgardNativeScriptStructureFrameV1({
        control: current,
        frame: frames[frameIndex],
      });
      if (result === null) {
        throw new Error(
          "V1 decoding scan frame witness does not hash-chain to the stack root",
        );
      }
      frameIndex += 1;
    } else {
      result = finalizeMidgardNativeScriptStructureV1(current);
      if (result === null) {
        throw new Error("V1 decoding scan finalize rejected its control");
      }
    }
    if (result.kind !== MidgardNativeScriptStructureResultKindsV1.Advanced) {
      return {
        kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused,
        refusalClass: refusalClassOfResultKind(result.kind),
        framesConsumed: frameIndex,
      };
    }
    current = result.control;
    remainingSteps -= 1;
  }
};

export type MidgardNativeScriptDecodingTraceStepV1 = {
  readonly control: MidgardNativeScriptStructureControlV1;
  readonly next: MidgardNativeScriptStructureControlV1;
  /// The frame witness this step consumed (frame-stage steps only).
  readonly frame: MidgardNativeScriptScanFrameV1 | null;
};

export const MidgardNativeScriptDecodingTraceOutcomeKindsV1 = Object.freeze({
  Terminal: "terminal",
  Refused: "refused",
} as const);

export type MidgardNativeScriptDecodingTraceOutcomeV1 =
  | {
      readonly kind: typeof MidgardNativeScriptDecodingTraceOutcomeKindsV1.Terminal;
      readonly control: MidgardNativeScriptStructureControlV1;
    }
  | {
      readonly kind: typeof MidgardNativeScriptDecodingTraceOutcomeKindsV1.Refused;
      readonly refusalClass: MidgardNativeScriptDecodingRefusalClassV1;
      /// The control the refusing primitive step consumes: the last carried
      /// control of the fold, where the single-step Verdict fold exhibits
      /// the refusal.
      readonly control: MidgardNativeScriptStructureControlV1;
    };

export type MidgardNativeScriptDecodingTraceV1 = {
  readonly bind: MidgardNativeScriptDecodingBindResultV1;
  /// The advanced primitive steps of the payload scan, in fold order; empty
  /// unless `bind.kind` is `"bound"`.
  readonly steps: readonly MidgardNativeScriptDecodingTraceStepV1[];
  /// `null` unless `bind.kind` is `"bound"`.
  readonly outcome: MidgardNativeScriptDecodingTraceOutcomeV1 | null;
};

const isContainerPush = ({
  before,
  after,
}: {
  readonly before: MidgardNativeScriptStructureControlV1;
  readonly after: MidgardNativeScriptStructureControlV1;
}): boolean => after.stackDepth === before.stackDepth + 1;

/// The refusal-capturing whole-item trace: binds the machine over the item
/// bytes and folds the payload scan to its end, capturing a refusal as an
/// outcome instead of throwing (unlike the frozen
/// `buildMidgardNativeScriptStructureTraceV1`, which only accepts canonical
/// scripts). Direction A plans stop one step before `outcome.control`;
/// direction B plans fold through to the exact terminal.
export const buildMidgardNativeScriptDecodingTraceV1 = (
  itemBytes: Uint8Array,
): MidgardNativeScriptDecodingTraceV1 => {
  const bytes = Buffer.from(itemBytes);
  const bind = bindMidgardNativeScriptDecodingMachineV1({
    firstChunk: bytes,
    totalLength: bytes.length,
  });
  if (bind.kind !== MidgardNativeScriptDecodingBindKindsV1.Bound) {
    return { bind, steps: [], outcome: null };
  }
  let control = bind.control;
  const frameStack: MidgardNativeScriptScanFrameV1[] = [];
  const steps: MidgardNativeScriptDecodingTraceStepV1[] = [];
  const maximumSteps = MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1 * 3 + 1;
  while (control.stage !== MidgardNativeScriptStructureStagesV1.Terminal) {
    if (steps.length >= maximumSteps) {
      throw new Error("V1 decoding trace exceeded the frozen machine's bounds");
    }
    let result;
    let frame: MidgardNativeScriptScanFrameV1 | null = null;
    if (control.stage === MidgardNativeScriptStructureStagesV1.Token) {
      result = advanceMidgardNativeScriptStructureTokenV1({
        control,
        window: bytes,
        windowOffset: control.cursor,
      });
      if (result === null) {
        throw new Error("V1 decoding trace token step rejected its control");
      }
      if (
        result.kind === MidgardNativeScriptStructureResultKindsV1.Advanced &&
        isContainerPush({ before: control, after: result.control })
      ) {
        const token = readMidgardNativeScriptStructureTokenV1({
          control,
          window: bytes,
          windowOffset: control.cursor,
        });
        frameStack.push({
          tail: control.stackRoot,
          kind: token.kind as MidgardNativeScriptScanFrameV1["kind"],
          childCount: token.childCount,
          remaining: token.childCount,
          validCount: 0,
          required:
            token.kind === MidgardNativeScriptKindsV1.AtLeast
              ? token.required
              : 0n,
        });
      }
    } else if (control.stage === MidgardNativeScriptStructureStagesV1.Frame) {
      const top = frameStack.at(-1);
      if (top === undefined) {
        throw new Error("V1 decoding trace lost its stack frame");
      }
      frame = top;
      result = advanceMidgardNativeScriptStructureFrameV1({ control, frame });
      if (result === null) {
        throw new Error(
          "V1 decoding trace frame does not hash-chain to the stack root",
        );
      }
      if (frame.remaining === 1) {
        frameStack.pop();
      } else {
        frameStack[frameStack.length - 1] = {
          ...frame,
          remaining: frame.remaining - 1,
        };
      }
    } else {
      result = finalizeMidgardNativeScriptStructureV1(control);
      if (result === null) {
        throw new Error("V1 decoding trace finalize rejected its control");
      }
    }
    if (result.kind !== MidgardNativeScriptStructureResultKindsV1.Advanced) {
      return {
        bind,
        steps,
        outcome: {
          kind: MidgardNativeScriptDecodingTraceOutcomeKindsV1.Refused,
          refusalClass: refusalClassOfResultKind(result.kind),
          control,
        },
      };
    }
    steps.push({ control, next: result.control, frame });
    control = result.control;
  }
  if (
    frameStack.length !== 0 ||
    !isExactMidgardNativeScriptStructureTerminalV1(control)
  ) {
    throw new Error("V1 decoding trace did not terminate exactly");
  }
  return {
    bind,
    steps,
    outcome: {
      kind: MidgardNativeScriptDecodingTraceOutcomeKindsV1.Terminal,
      control,
    },
  };
};
