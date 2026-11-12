import {
  buildMidgardNativeScriptDecodingTrace,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingDirections,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
} from "@al-ft/midgard-core";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type EventLedgerRefusalReason,
  EventLedgerReplayRefusal,
  reconstructionEventLedgerWalk,
} from "../event-ledger.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { retainedOutputReferenceScript } from "../evidence/retained-ledger-output.js";
import { type CheckedLedger } from "../ledger-at.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import {
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import {
  collectReplayFindings,
  type ReplayPrerequisite,
  replayPrerequisiteFailure,
} from "../workflow/replay-prerequisite.js";
import { buildNativeScriptDecodingScanPlan } from "./scan-plan.js";

function failure(message: string): never {
  throw new Error(`nativeScriptDecoding: ${message}`);
}

/** What a refused walk means here: a prerequisite at the event of the step
 * the refusal names, or a fatal corruption of the committed trace. */
const REFUSAL_OUTCOME: Readonly<
  Record<
    EventLedgerRefusalReason,
    Readonly<{ prerequisite: ReplayPrerequisite } | { fatal: string }>
  >
> = {
  withdrawal_input_absent: { prerequisite: "present_spend_input" },
  transaction_input_absent: { prerequisite: "present_spend_input" },
  transaction_output_already_present: {
    prerequisite: "prior_transition_effect",
  },
  post_root_changed: { prerequisite: "prior_transition_effect" },
  phase_order_changed: { prerequisite: "prior_transition_effect" },
  transition_link_changed: { fatal: "prior transition mismatch" },
  source_event_changed: { fatal: "prior event mismatch" },
  step_out_of_range: { fatal: "prior transition mismatch" },
  selected_pre_root_changed: { fatal: "event prior ledger changed" },
};

/**
 * The ledger before step `stepIndex`, from the block's one shared event
 * ledger walk. A step the walk cannot apply is a prerequisite at the event
 * of the step it names: an absent input wants that event's spend-input
 * proof, and any other unopenable effect its transition proof.
 */
export const nativeScriptDecodingPriorLedger = async (
  current: TransitionTraceReconstruction,
  stepIndex: bigint,
  predecessor?: CheckedLedger,
) => {
  if (
    predecessor !== undefined &&
    predecessor.utxosRoot !== current.header.prevUtxosRoot
  )
    failure("unrelated predecessor");
  if (
    predecessor === undefined &&
    current.header.prevUtxosRoot !== SDK.EMPTY_MERKLE_TREE_ROOT
  )
    failure("authenticated predecessor ledger unavailable");
  const target = current.traceByStepIndex.get(stepIndex)?.value;
  if (target === undefined) failure("prior transition mismatch");
  let ledger: CheckedLedger;
  try {
    ledger = await reconstructionEventLedgerWalk(current, predecessor).before(
      target.event_key,
      stepIndex,
    );
  } catch (cause) {
    if (!(cause instanceof EventLedgerReplayRefusal)) throw cause;
    const outcome = REFUSAL_OUTCOME[cause.reason];
    if ("fatal" in outcome) failure(outcome.fatal);
    // Every step a prerequisite names opened when the walk applied it.
    throw replayPrerequisiteFailure(
      current.headerHash,
      current.traceByStepIndex.get(cause.step)!.value.event_key,
      outcome.prerequisite,
    );
  }
  return {
    root: ledger.trie,
    outputs: {
      get: (key: string) => {
        const value = ledger.outputs.get(key);
        return value === undefined
          ? undefined
          : { key: Buffer.from(key, "hex"), value };
      },
      has: (key: string) => ledger.outputs.has(key),
    },
  };
};

export type NativeScriptDecodingCoordinate = Readonly<{
  sourceKind: 0 | 1;
  sourceIndex: number;
  outpointSourceKind: string;
  outpointCursor: string;
}>;
export const nativeScriptDecodingDetectionId = (
  c: NativeScriptDecodingCoordinate,
) =>
  `native-script-decoding:${c.sourceKind}:${c.sourceIndex}:${c.outpointSourceKind}:${c.outpointCursor}`;

export const prepareNativeScriptDecodingReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  current: TransitionTraceReconstruction;
  predecessor?: CheckedLedger;
  coordinate: NativeScriptDecodingCoordinate;
}) => {
  const forced =
    coordinate.sourceKind === 1
      ? current.forcedTransactions[coordinate.sourceIndex]
      : undefined;
  const normal =
    coordinate.sourceKind === 0
      ? current.transactions[coordinate.sourceIndex]
      : undefined;
  if (forced === undefined && normal === undefined) failure("source absent");
  const txId = forced?.value.tx_id ?? normal!.txId;
  const eventKey: SDK.EventKey =
    forced === undefined
      ? { L2TransactionEventKey: { tx_id: txId } }
      : { ForcedTransactionEventKey: { tx_order_id: forced.key } };
  const mapping = current.eventToStepByFingerprint.get(
    eventKeyFingerprint(eventKey),
  )?.value;
  if (mapping === undefined) failure("event mapping absent");
  const transition = current.traceByStepIndex.get(mapping.step_index)?.value;
  if (
    transition === undefined ||
    transition.schema_version !== 1n ||
    transition.step_index !== mapping.step_index ||
    eventKeyFingerprint(transition.event_key) !==
      eventKeyFingerprint(eventKey) ||
    transition.phase !==
      (forced === undefined ? "L2Transaction" : "ForcedTransaction")
  )
    failure("event transition mismatch");
  const direction =
    forced !== undefined && forced.value.verdict !== "ForcedTxValid" ? 1n : 0n;
  const kind = BigInt(coordinate.outpointSourceKind),
    cursor = BigInt(coordinate.outpointCursor);
  let scanReasonClass: bigint | null = null;
  if (direction === 1n) {
    const verdict = forced!.value.verdict;
    if (verdict === "ForcedTxValid") failure("forced verdict changed");
    const reason = verdict.ForcedTxInvalid.reason;
    const names = [
      "ResolvedReferenceScriptMalformed",
      "ResolvedReferenceScriptNodeLimit",
      "ResolvedReferenceScriptDepthLimit",
    ] as const;
    const index = names.findIndex(
      (name) => typeof reason === "object" && name in reason,
    );
    if (index < 0) failure("wrong forced reason");
    const selected = (
      reason as unknown as Record<
        string,
        { source_kind: bigint; input_index: bigint }
      >
    )[names[index]!]!;
    if (selected.source_kind !== kind || selected.input_index !== cursor)
      failure("forced reason coordinate changed");
    scanReasonClass = BigInt(index);
  }
  const raw = forced?.fullTransactionCbor ?? normal!.fullTransactionCbor;
  const material = (
    forced === undefined
      ? deriveMidgardNativeTxFaultEvidenceMaterial
      : deriveMidgardForcedTxFaultEvidenceMaterial
  )(raw);
  const items =
    kind === 0n || kind === 1n
      ? decodeMidgardFieldPreimage(material.fieldPreimages[Number(kind)]!)
      : [];
  const subjectFieldInputs = items.map((bytes) => {
    const item = decodeMidgardSpendInputItem(bytes);
    return {
      tx_id: Buffer.from(item.txId).toString("hex"),
      output_index: BigInt(item.outputIndex),
    };
  });
  const selected =
    cursor >= 0n && cursor < BigInt(items.length)
      ? items[Number(cursor)]
      : undefined;
  if (selected === undefined) {
    if (direction === 0n) return null;
    return {
      coordinate,
      direction,
      scanReasonClass,
      current,
      eventKey,
      txId,
      material,
      subjectFieldInputs,
      descriptorCbor: null,
      referenceScriptItemBytes: null,
      plan: null,
      ledgerTrie: null,
    };
  }
  const ledger = await nativeScriptDecodingPriorLedger(
    current,
    mapping.step_index,
    predecessor,
  );
  const output = ledger.outputs.get(selected.toString("hex"));
  if (output === undefined) return null;
  const descriptorCbor = ledger.root.entries.find((entry) =>
    entry.key.equals(selected),
  )!.value;
  const descriptor = decodeMidgardLedgerOutputCommitment(descriptorCbor);
  const referenceScriptItemBytes = retainedOutputReferenceScript(output.value);
  let plan: ReturnType<typeof buildNativeScriptDecodingScanPlan> | null = null;
  if (descriptor.referenceScriptLanguage !== 0) {
    if (direction === 0n) return null;
  } else {
    if (referenceScriptItemBytes === null)
      failure("native descriptor has no retained script bytes");
    const trace = buildMidgardNativeScriptDecodingTrace(
      referenceScriptItemBytes,
    );
    if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.NonNative)
      return null;
    const malformed =
      trace.bind.kind === MidgardNativeScriptDecodingBindKinds.Malformed ||
      trace.outcome?.kind ===
        MidgardNativeScriptDecodingTraceOutcomeKinds.Refused;
    if ((direction === 0n) !== malformed) return null;
    plan = buildNativeScriptDecodingScanPlan({
      itemBytes: referenceScriptItemBytes,
      direction:
        direction === 0n
          ? MidgardNativeScriptDecodingDirections.WrongfulAcceptance
          : MidgardNativeScriptDecodingDirections.WrongfulRejection,
    });
  }
  const ledgerTrie = {
    rootHex: ledger.root.root,
    prove: async (key: Buffer) =>
      Buffer.from(
        Data.to(
          await keyValuePhasProof(ledger.root, key, descriptorCbor),
          SDK.Proof,
        ),
        "hex",
      ),
  };
  return {
    coordinate,
    direction,
    scanReasonClass,
    current,
    eventKey,
    txId,
    material,
    subjectFieldInputs,
    descriptorCbor,
    referenceScriptItemBytes,
    plan,
    ledgerTrie,
  };
};

export const detectNativeScriptDecodingReplay = async ({
  block,
  predecessor,
}: {
  block: CanonicalBlockEvidence;
  predecessor?: CheckedLedger;
}) => {
  const candidates: NativeScriptDecodingCoordinate[] = [];
  for (const sourceKind of [0, 1] as const) {
    const sources =
      sourceKind === 0
        ? block.reconstruction.transactions
        : block.reconstruction.forcedTransactions;
    for (const [sourceIndex, source] of sources.entries()) {
      const forced =
        sourceKind === 1
          ? block.reconstruction.forcedTransactions[sourceIndex]
          : undefined;
      if (forced !== undefined && forced.value.verdict !== "ForcedTxValid") {
        const reason = forced.value.verdict.ForcedTxInvalid.reason;
        for (const name of [
          "ResolvedReferenceScriptMalformed",
          "ResolvedReferenceScriptNodeLimit",
          "ResolvedReferenceScriptDepthLimit",
        ] as const) {
          if (typeof reason === "object" && name in reason) {
            const pair = (
              reason as unknown as Record<
                string,
                { source_kind: bigint; input_index: bigint }
              >
            )[name]!;
            candidates.push({
              sourceKind,
              sourceIndex,
              outpointSourceKind: pair.source_kind.toString(),
              outpointCursor: pair.input_index.toString(),
            });
          }
        }
      } else {
        const tx = (
          sourceKind === 1
            ? decodeMidgardForcedTxFullFromCanonicalCbor
            : decodeMidgardNativeTxFullFromCanonicalCbor
        )(source.fullTransactionCbor);
        for (const kind of [0, 1] as const)
          for (const cursor of decodeMidgardFieldPreimage(
            kind === 0
              ? tx.body.spendInputsPreimageCbor
              : tx.body.referenceInputsPreimageCbor,
          ).keys())
            candidates.push({
              sourceKind,
              sourceIndex,
              outpointSourceKind: String(kind),
              outpointCursor: String(cursor),
            });
      }
    }
  }
  return collectReplayFindings(candidates, async (coordinate) => {
    const prepared = await prepareNativeScriptDecodingReplay({
      current: block.reconstruction,
      predecessor,
      coordinate,
    });
    return prepared === null
      ? null
      : {
          ...eventSubject(prepared.eventKey),
          detectionId: nativeScriptDecodingDetectionId(coordinate),
          headerHash: block.headerHash,
          violationId: SDK.NATIVE_SCRIPT_DECODING_VIOLATION_ID,
          position: BigInt(coordinate.sourceIndex),
          prepared,
        };
  });
};
