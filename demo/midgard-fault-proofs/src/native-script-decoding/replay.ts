import {
  adjudicateMidgardNativeTxFullValidity,
  buildMidgardNativeScriptDecodingTrace,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingDirections,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { createCanonicalMidgardLedgerDescriptorResolver } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  retainedLedgerDescriptorCandidates,
  retainedOutputReferenceScript,
  retainedUndecodableOutputDescriptor,
} from "../evidence/retained-ledger-output.js";
import {
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";
import {
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";
import {
  collectReplayFindings,
  replayPrerequisiteFailure,
} from "../workflow/replay-prerequisite.js";
import { buildNativeScriptDecodingScanPlan } from "./scan-plan.js";

function failure(message: string): never {
  throw new Error(`nativeScriptDecoding: ${message}`);
}

/** Raw outputs are only preimages: their derived descriptor root is checked at every event. */
export const nativeScriptDecodingPriorLedger = async (
  current: TransitionTraceReconstruction,
  stepIndex: bigint,
  predecessor?: TransitionTraceReconstruction,
) => {
  if (
    predecessor !== undefined &&
    (predecessor.headerHash !== current.header.prevHeaderHash ||
      predecessor.header.utxosRoot !== current.header.prevUtxosRoot)
  )
    failure("unrelated predecessor");
  const outputs = new Map(
    (predecessor?.utxos ?? []).map(({ key, value }) => [
      key.toString("hex"),
      { key, value },
    ]),
  );
  const candidates = retainedLedgerDescriptorCandidates([
    ...(predecessor?.payload.block_body.validation_trace_witnesses ?? []),
    ...current.payload.block_body.validation_trace_witnesses,
  ]);
  const resolveDescriptor = createCanonicalMidgardLedgerDescriptorResolver();
  const descriptorFor = (key: Buffer, value: Buffer) => {
    try {
      return resolveDescriptor({
        outRef: key,
        outputCbor: value,
      });
    } catch {
      return retainedUndecodableOutputDescriptor({
        key,
        output: value,
        candidates,
      });
    }
  };
  const rootOf = async () =>
    await keyValuePhasRootWithCount(
      [...outputs.values()].map(({ key, value }) => ({
        key,
        value: descriptorFor(key, value),
      })),
    );
  let root = await rootOf();
  if (root.root !== current.header.prevUtxosRoot)
    failure("authenticated predecessor ledger unavailable");
  for (let index = 0n; index < stepIndex; index++) {
    const step = current.traceByStepIndex.get(index)?.value;
    if (
      step === undefined ||
      step.step_index !== index ||
      step.schema_version !== 1n ||
      step.pre_utxos_root !== root.root
    )
      failure("prior transition mismatch");
    const event = current.sourceEventsByFingerprint.get(
      eventKeyFingerprint(step.event_key),
    );
    if (event === undefined || event.phase !== step.phase)
      failure("prior event mismatch");
    if (event.phase === "Withdrawal") {
      if (event.entry.value.validity === "WithdrawalIsValid") {
        const outref = event.entry.value.body.l2_outref;
        outputs.delete(
          encodeMidgardSpendInputItem({
            txId: Buffer.from(outref.transactionId, "hex"),
            outputIndex: Number(outref.outputIndex),
          }).toString("hex"),
        );
      }
    } else if (
      event.phase === "ForcedTransaction" ||
      event.phase === "L2Transaction"
    ) {
      const accepted =
        event.phase === "L2Transaction"
          ? event.entry.validity === "TxIsValid"
          : event.entry.value.verdict === "ForcedTxValid";
      if (accepted) {
        const tx = decodeMidgardNativeTxFullFromCanonicalCbor(
          event.entry.fullTransactionCbor,
        );
        for (const key of decodeMidgardFieldPreimage(
          tx.body.spendInputsPreimageCbor,
        ))
          outputs.delete(key.toString("hex"));
        const txId =
          event.phase === "L2Transaction"
            ? event.entry.txId
            : event.entry.value.tx_id;
        for (const [outputIndex, value] of decodeMidgardFieldPreimage(
          tx.body.outputsPreimageCbor,
        ).entries()) {
          const key = encodeMidgardSpendInputItem({
            txId: Buffer.from(txId, "hex"),
            outputIndex,
          });
          outputs.set(key.toString("hex"), { key, value });
        }
      }
    } else
      failure("deposit precedes a transaction outside canonical phase order");
    root = await rootOf();
    if (root.root !== step.post_utxos_root)
      throw replayPrerequisiteFailure(
        current.headerHash,
        step.event_key,
        "prior_transition_effect",
      );
  }
  return { root, outputs };
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
  predecessor?: TransitionTraceReconstruction;
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
  const committed = adjudicateMidgardNativeTxFullValidity(
    decodeMidgardNativeTxFullFromCanonicalCbor(raw),
    direction === 1n ? "TxIsInvalid" : (normal?.validity ?? "TxIsValid"),
  );
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    encodeMidgardNativeTxCanonical(committed),
  );
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
  if (ledger.root.root !== transition.pre_utxos_root)
    failure("event prior ledger changed");
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
  predecessor?: CanonicalBlockEvidence;
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
        const tx = decodeMidgardNativeTxFullFromCanonicalCbor(
          source.fullTransactionCbor,
        );
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
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareNativeScriptDecodingReplay({
        current: block.reconstruction,
        predecessor: predecessor?.reconstruction,
        coordinate,
      });
      return prepared === null
        ? null
        : {
            detectionId: nativeScriptDecodingDetectionId(coordinate),
            headerHash: block.headerHash,
            violationId: SDK.NATIVE_SCRIPT_DECODING_VIOLATION_ID,
            position: BigInt(coordinate.sourceIndex),
            prepared,
          };
    }),
  );
};
