/**
 * The NativeScripts producer: each script execution's descriptor, and an
 * execution-mode scan of each native one. Its last control is the handoff
 * the later phases continue from.
 */
import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardValidationMerkleMembership,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  decodeMidgardSpendInputItem,
  decodeSingleCbor,
} from "@al-ft/midgard-core/codec";
import { Effect } from "effect";

import { MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX } from "./canonical-field-item.js";
import {
  readValidationMachineVersionedScriptHeader,
  type ValidationMachineVersionedScriptHeader,
} from "./native-script-frame.js";
import {
  type NativeScanPrepared,
  scanNativeScriptItem,
} from "./native-script-scan.js";
import { type PreparedValidationTransaction } from "./prepared-validation-transaction.js";
import type {
  ScriptSourceProofEntry,
  ValidationTraceWalkState,
} from "./trace-builder-types.js";
import {
  phaseContinues,
  type PhaseOutcome,
  type TraceWalk,
} from "./trace-walk.js";

/** `source` as the bounded item of its origin field, checked against the
 * descriptor facts it was authenticated with. */
export const boundedItemForScriptSource = (source: ScriptSourceProofEntry) => {
  // The two origin kinds carry two different keys, and each has exactly one
  // decoder. An inline key is a bare canonical CBOR index; a reference key
  // *is* the ledger out-ref, i.e. §5.3's fixed-index 38-byte item, whose
  // `19 0000` head a minimal-CBOR reader rejects — so it goes through the
  // §5.3 twin, never `decodeSingleCbor`. See `docs/spec/midgard-tx.md` §5.3.
  const itemIndexValue =
    source.originKind === "inline"
      ? decodeSingleCbor(source.sourceKey)
      : decodeMidgardSpendInputItem(source.sourceKey).outputIndex;
  const itemIndex =
    typeof itemIndexValue === "number"
      ? itemIndexValue
      : typeof itemIndexValue === "bigint" &&
          itemIndexValue <= BigInt(Number.MAX_SAFE_INTEGER)
        ? Number(itemIndexValue)
        : -1;
  if (!Number.isSafeInteger(itemIndex) || itemIndex < 0) {
    throw new Error("V1 script source has a noncanonical item index");
  }
  const item = buildMidgardBoundedItem({
    fieldIndex:
      source.originKind === "inline" ? MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX : 2,
    itemIndex,
    bytes: source.authenticatedVersionedItemBytes,
  });
  if (
    item.bytes.length !== source.scriptTotalLength ||
    !item.commitment.equals(source.scriptItemCommitment)
  ) {
    throw new Error(
      "V1 script source bytes disagree with authenticated descriptor facts",
    );
  }
  return item;
};

type ExecutionFrontiers = Pick<
  ValidationTraceWalkState,
  "scriptExecutionEntries" | "scriptPurposeEntries" | "scriptSourceEntries"
>;

/** Execution `executionIndex` with its purpose, its source and its own
 * memberships, as the NativeScripts and CEK phases both carry it. */
export const executionDescriptor = (
  state: ExecutionFrontiers,
  executionIndex: number,
) => {
  const siblings = (
    entries: readonly { readonly leaf: Buffer }[],
    index: number,
  ) =>
    buildMidgardValidationMerkleMembership(
      entries.map((entry) => entry.leaf),
      index,
    ).siblings;
  const { purpose, source, sourceIndex, languageTag, redeemerLeaf } =
    state.scriptExecutionEntries[executionIndex]!;
  return {
    executionIndex,
    languageTag,
    purpose: {
      purposeKind: purpose.purposeKind,
      purposeIndex: purpose.purposeIndex,
      scriptHash: purpose.scriptHash,
      subject: purpose.subject,
      siblings: siblings(state.scriptPurposeEntries, executionIndex),
    },
    source: {
      sourceIndex,
      originKind: source.originKind,
      sourceKey: source.sourceKey,
      scriptTotalLength: source.scriptTotalLength,
      scriptItemCommitment: source.scriptItemCommitment,
      siblings: siblings(state.scriptSourceEntries, sourceIndex),
    },
    redeemerLeaf,
    executionSiblings: siblings(state.scriptExecutionEntries, executionIndex),
  };
};

type NativeScriptsState = ExecutionFrontiers &
  Pick<
    ValidationTraceWalkState,
    | "authenticatedNativeScriptsWitnessCbor"
    | "authenticatedNativeScriptsBaseFields"
  >;

/** The NativeScripts control once `executionCursor` executions are
 * scanned. */
const nativeControlCbor = (
  {
    resolutionScheduleHash,
  }: Pick<NativeScanPrepared, "resolutionScheduleHash">,
  {
    authenticatedNativeScriptsBaseFields: baseFields,
    scriptExecutionEntries,
  }: NativeScriptsState,
  executionCursor: number,
): Buffer => {
  if (baseFields === null) {
    throw new Error(
      "V1 did not authenticate the NativeScripts handoff witness",
    );
  }
  // Each non-native execution sets its language's bit: V3 1, V4 2.
  const languageBitmap = scriptExecutionEntries
    .slice(0, executionCursor)
    .reduce(
      (bitmap, { languageTag }) =>
        bitmap | (languageTag === 0 ? 0 : languageTag === 3 ? 1 : 2),
      0,
    );
  return encodeCbor([
    ...baseFields,
    BigInt(executionCursor),
    BigInt(languageBitmap),
    resolutionScheduleHash,
  ]);
};

/** The NativeScripts handoff: the control after the last execution. */
export const nativeScriptsHandoff = (
  prepared: Pick<NativeScanPrepared, "resolutionScheduleHash">,
  state: NativeScriptsState,
): Buffer =>
  nativeControlCbor(prepared, state, state.scriptExecutionEntries.length);

export const produceNativeScripts = (
  prepared: NativeScanPrepared &
    Pick<PreparedValidationTransaction, "emptyValidationFrontier">,
  state: NativeScriptsState,
  { push }: Pick<TraceWalk, "push">,
): Effect.Effect<PhaseOutcome, Error> =>
  Effect.gen(function* () {
    if (
      state.authenticatedNativeScriptsWitnessCbor === null ||
      state.authenticatedNativeScriptsBaseFields === null
    ) {
      return yield* Effect.fail(
        new Error("V1 did not authenticate the NativeScripts handoff witness"),
      );
    }
    for (const [
      executionIndex,
      execution,
    ] of state.scriptExecutionEntries.entries()) {
      const { purpose, source, languageTag } = execution;
      const item = boundedItemForScriptSource(source);
      const continuationCbor = nativeControlCbor(
        prepared,
        state,
        executionIndex,
      );
      push("nativeScripts", continuationCbor, {
        kind: "nativeExecutionDescriptor",
        ...executionDescriptor(state, executionIndex),
        firstChunkProof:
          languageTag === 0 ? buildMidgardBoundedItemChunkProof(item, 0) : null,
        signerFrontier:
          languageTag === 0
            ? prepared.signerFrontier
            : prepared.emptyValidationFrontier,
      });
      if (languageTag !== 0) continue;
      if (source.script.language !== "NativeCardano") {
        return yield* Effect.fail(
          new Error(
            "V1 native execution language disagrees with its script source",
          ),
        );
      }
      let header: ValidationMachineVersionedScriptHeader;
      try {
        header = readValidationMachineVersionedScriptHeader(item.bytes);
      } catch {
        return yield* Effect.fail(
          new Error(
            "authenticated native script has an invalid versioned-script header",
          ),
        );
      }
      if (header.languageTag !== 0) {
        return yield* Effect.fail(
          new Error(
            "authenticated native script descriptor has a non-native header",
          ),
        );
      }
      const scan = scanNativeScriptItem(
        prepared,
        push,
        item,
        {
          stage: 1,
          scriptCount: 1,
          scriptSeen: 0,
          containsNonNativeScript: 0,
          itemLength: item.bytes.length,
          itemCommitment: item.commitment,
          cursor: header.payloadOffset,
          stackRoot: Buffer.alloc(0),
          stackDepth: 0,
          nodeCount: 0,
          result: -1,
          purposeKind: BigInt(purpose.purposeKind),
          purposeIndex: purpose.purposeIndex,
        },
        continuationCbor,
      );
      if (scan.stop !== null) return scan.stop;
    }
    push("nativeScripts", nativeScriptsHandoff(prepared, state));
    return phaseContinues;
  });
