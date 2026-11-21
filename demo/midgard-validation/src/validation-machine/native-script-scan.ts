/**
 * The bounded native-script scan, in both of its modes: witness mode, which
 * phase A runs over the transaction's script witnesses, and execution mode,
 * which the NativeScripts phase runs over each native execution's source.
 */
import {
  buildMidgardBoundedItemChunkProof,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBlake2b,
  type MidgardBoundedItem,
  midgardBoundedItemChunkCount,
} from "@al-ft/midgard-core";
import { type MidgardRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";

import {
  encodeValidationControlList,
  encodeValidationFrontierPeaks,
} from "./control-encoding.js";
import {
  hashValidationMachineNativeScriptFrame,
  MAX_NATIVE_SCRIPT_SCAN_DEPTH,
  MAX_NATIVE_SCRIPT_SCAN_NODES,
  readValidationMachineNativeScriptPayload,
  readValidationMachineNativeScriptTokenHead,
  type ValidationMachineNativeScriptFrame,
  type ValidationMachineNativeScriptToken,
  type ValidationMachineNativeScriptTokenHead,
} from "./native-script-frame.js";
import {
  preparedSignerProof,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import type { PhaseANativeScriptsScanControl } from "./trace-builder-types.js";
import { type PhaseOutcome, phaseStops, type TraceWalk } from "./trace-walk.js";
import {
  type ValidationMachineSignerSetProof,
  type ValidationMachineWorkWitness,
} from "./types.js";

/** What a scan reads of the prepared transaction. */
export type NativeScanPrepared = Pick<
  PreparedValidationTransaction,
  | "proofSource"
  | "contextCbor"
  | "resolutionScheduleHash"
  | "signerFrontier"
  | "canonicalSignerHashes"
  | "signerLeafHashes"
  | "compactProofTransaction"
>;

/** The NativeScripts scan witness at `control`; a late execution scan
 * passes its NativeScripts continuation. */
export const phaseANativeScriptsScanWitnessCbor = (
  {
    proofSource,
    contextCbor,
    resolutionScheduleHash,
    signerFrontier,
  }: Pick<
    NativeScanPrepared,
    "proofSource" | "contextCbor" | "resolutionScheduleHash" | "signerFrontier"
  >,
  control: PhaseANativeScriptsScanControl,
  continuationCbor: Buffer = Buffer.alloc(0),
): Buffer =>
  encodeValidationControlList([
    proofSource.compactCbor,
    proofSource.witnessSetCompactCbor,
    proofSource.fieldPreimageLengthsCbor,
    contextCbor,
    // A late execution scan binds its NativeScripts continuation here.
    continuationCbor.length === 0
      ? resolutionScheduleHash
      : Buffer.from(midgardBlake2b(continuationCbor, { dkLen: 32 })),
    BigInt(control.stage),
    BigInt(control.scriptCount),
    BigInt(control.scriptSeen),
    BigInt(control.containsNonNativeScript),
    BigInt(control.itemLength),
    control.itemCommitment,
    BigInt(control.cursor),
    control.stackRoot,
    BigInt(control.stackDepth),
    BigInt(control.nodeCount),
    BigInt(control.result),
    BigInt(signerFrontier.count),
    [encodeValidationFrontierPeaks(signerFrontier), continuationCbor],
    control.purposeKind,
    control.purposeIndex,
  ]);

/** A scan's stopping arm, without its mode's prefix. */
type NativeScanArm =
  | "NativeScriptMalformed"
  | "NativeScriptNodeLimit"
  | "NativeScriptDepthLimit"
  | "NativeScriptFalse";

/**
 * Scans `item` from `start`, a stage-1 control at its payload. A scan given
 * a NativeScripts continuation runs in execution mode and names its stops by
 * the control's purpose key; otherwise it names them by `scriptSeen`, as
 * `native_script_malformed_reason` does on chain. It returns the stop, or no
 * stop and the control at the end of the item.
 */
export const scanNativeScriptItem = (
  prepared: NativeScanPrepared,
  push: TraceWalk["push"],
  item: MidgardBoundedItem,
  start: PhaseANativeScriptsScanControl,
  continuationCbor: Buffer = Buffer.alloc(0),
): {
  readonly stop: PhaseOutcome | null;
  readonly control: PhaseANativeScriptsScanControl;
} => {
  const execution = continuationCbor.length > 0;
  const { validityIntervalStart, validityIntervalEnd } =
    prepared.compactProofTransaction.transactionBody;
  const chunkCount = midgardBoundedItemChunkCount(item.bytes.length);
  const frames: ValidationMachineNativeScriptFrame[] = [];
  let control = start;
  const stop = (arm: NativeScanArm, payloadFailure?: string) => ({
    control,
    stop: phaseStops(
      (execution
        ? {
            [`Execution${arm}`]: {
              purpose_kind: control.purposeKind,
              purpose_index: control.purposeIndex,
            },
          }
        : {
            [`Witness${arm}`]: { script_index: BigInt(control.scriptSeen) },
          }) as MidgardRejectionReasonV1,
      payloadFailure !== undefined && !execution
        ? `bounded native-script payload failed at stage=${control.stage},cursor=${control.cursor},bytes=${item.bytes.subarray(control.cursor).toString("hex")}: ${payloadFailure}`
        : `bounded ${execution ? "execution " : ""}native-script scan stopped at stage=${control.stage},cursor=${control.cursor}`,
    ),
  });
  const pushScan = (
    auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
  ): void =>
    push(
      "phaseANativeScripts",
      phaseANativeScriptsScanWitnessCbor(prepared, control, continuationCbor),
      auxiliary,
    );
  const pushToken = (signerProof: ValidationMachineSignerSetProof): void => {
    // An exhausted cursor authenticates an empty parser window on chain, in
    // either mode (`phase_a_native_chunk_window`). Carry the real last
    // chunk; there is no chunk after it.
    const chunkIndex =
      control.cursor === item.bytes.length
        ? chunkCount - 1
        : Math.floor(control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    pushScan({
      kind: "nativeScriptToken",
      chunkProof: buildMidgardBoundedItemChunkProof(item, chunkIndex),
      nextChunkProof:
        chunkIndex + 1 < chunkCount
          ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
          : null,
      signerProof,
    });
  };

  for (;;) {
    if (control.stage === 1) {
      let head: ValidationMachineNativeScriptTokenHead | null = null;
      try {
        head = readValidationMachineNativeScriptTokenHead(
          item.bytes,
          control.cursor,
        );
      } catch {
        // The authenticated token witness still proves the exact malformed
        // bytes to the one-step resolver.
      }
      pushToken({ kind: "none" });
      if (head === null) return stop("NativeScriptMalformed");
      const nodeCount = control.nodeCount + 1;
      if (nodeCount > MAX_NATIVE_SCRIPT_SCAN_NODES) {
        return stop("NativeScriptNodeLimit");
      }
      control = {
        ...control,
        stage: (head.kind + 3) as 3 | 4 | 5 | 6 | 7 | 8,
        cursor: head.payloadOffset,
        nodeCount,
      };
      continue;
    }

    if (control.stage >= 3) {
      const kind = (control.stage - 3) as 0 | 1 | 2 | 3 | 4 | 5;
      let token: ValidationMachineNativeScriptToken | null = null;
      let payloadFailure = "none";
      try {
        token = readValidationMachineNativeScriptPayload(
          item.bytes,
          control.cursor,
          kind,
        );
      } catch (cause) {
        payloadFailure = String(cause);
        // The authenticated payload witness proves the exact malformed
        // bytes to the selected one-step resolver.
      }
      const signerProof: ValidationMachineSignerSetProof =
        token?.kind === 0
          ? preparedSignerProof(prepared, token.keyHash)
          : { kind: "none" };
      pushToken(signerProof);
      if (token === null) return stop("NativeScriptMalformed", payloadFailure);

      if (token.kind >= 1 && token.kind <= 3 && token.childCount > 0) {
        const stackDepth = control.stackDepth + 1;
        if (stackDepth > MAX_NATIVE_SCRIPT_SCAN_DEPTH) {
          return stop("NativeScriptDepthLimit");
        }
        const frame: ValidationMachineNativeScriptFrame = {
          tail: control.stackRoot,
          kind: token.kind as 1 | 2 | 3,
          childCount: token.childCount,
          remaining: token.childCount,
          validCount: 0,
          required: token.required,
        };
        frames.push(frame);
        control = {
          ...control,
          stage: 1,
          cursor: token.nextOffset,
          stackRoot: hashValidationMachineNativeScriptFrame(frame),
          stackDepth,
        };
        continue;
      }

      const valid =
        token.kind === 0
          ? signerProof.kind === "membership"
          : token.kind === 4
            ? validityIntervalStart >= 0n && validityIntervalStart >= token.slot
            : token.kind === 5
              ? validityIntervalEnd >= 0n && validityIntervalEnd <= token.slot
              : token.kind === 1 || (token.kind === 3 && token.required === 0n);
      control = {
        ...control,
        stage: 2,
        cursor: token.nextOffset,
        result: valid ? 1 : 0,
      };
      continue;
    }

    const frame = frames[frames.length - 1];
    if (frame !== undefined) {
      pushScan({ kind: "nativeScriptFrame", frame });
      const validCount = frame.validCount + (control.result === 1 ? 1 : 0);
      if (frame.remaining === 1) {
        frames.pop();
        const valid =
          frame.kind === 1
            ? validCount === frame.childCount
            : frame.kind === 2
              ? validCount > 0
              : BigInt(validCount) >= frame.required;
        control = {
          ...control,
          stackRoot: frame.tail,
          stackDepth: control.stackDepth - 1,
          result: valid ? 1 : 0,
        };
      } else {
        const nextFrame: ValidationMachineNativeScriptFrame = {
          ...frame,
          remaining: frame.remaining - 1,
          validCount,
        };
        frames[frames.length - 1] = nextFrame;
        control = {
          ...control,
          stage: 1,
          stackRoot: hashValidationMachineNativeScriptFrame(nextFrame),
          result: -1,
        };
      }
      continue;
    }

    pushScan();
    if (control.cursor !== control.itemLength) {
      return stop("NativeScriptMalformed");
    }
    if (control.result === 0) return stop("NativeScriptFalse");
    return { stop: null, control };
  }
};
