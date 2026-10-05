import { createHash, type Hash } from "node:crypto";

import {
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";

import { Columns as TxColumns } from "../../database/utils/tx.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../../mpf/transition-cbor.js";
import type { CommitDaFrameMeasurement } from "../utils/commit-block-planner.js";
import type { DaPayloadBlockContent } from "./submission.assert-pre-submit-da-payload-size.js";

type Aggregate = SDK.DaPayloadEntrySizeAggregate;
const empty = (): Aggregate => ({ entryCount: 0, encodedTupleBytes: 0 });
const add = (
  aggregate: Aggregate,
  entries: readonly SDK.DaPayloadEntry[],
): Aggregate => ({
  entryCount: aggregate.entryCount + entries.length,
  encodedTupleBytes: entries.reduce(
    (size, entry) => size + SDK.daPayloadEntryEncodedSize(entry),
    aggregate.encodedTupleBytes,
  ),
});
const pair = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];
const bindBytes = (digest: Hash, bytes: Uint8Array): void => {
  const length = Buffer.alloc(8);
  length.writeBigUInt64BE(BigInt(bytes.length));
  digest.update(length).update(bytes);
};
const bindEntries = (
  digest: Hash,
  entries: readonly SDK.DaPayloadEntry[],
): void => {
  bindBytes(digest, Buffer.from(entries.length.toString()));
  for (const [key, value] of entries) {
    bindBytes(digest, Buffer.from(key, "hex"));
    bindBytes(digest, Buffer.from(value, "hex"));
  }
};

/** Each deposit is encoded at five native integer widths once. Its four
 * changes are scheduled into prefix buckets, so no prefix rescans deposits. */
export const depositTraceAggregatesByPrefix = (
  content: Pick<
    DaPayloadBlockContent,
    "transitionTraceMembers" | "eventToStepMembers"
  >,
  ordinaryCount: number,
  mandatoryBeforeCount: number,
): readonly {
  readonly transition: Aggregate;
  readonly eventToStep: Aggregate;
}[] => {
  const transitionChanges = new Float64Array(ordinaryCount + 1);
  const eventChanges = new Float64Array(ordinaryCount + 1);
  let transition = empty();
  let eventToStep = empty();
  const starts = [0, 24, 256, 65536, 4294967296];
  const deposits = content.transitionTraceMembers.filter(
    (member) => member.value.phase === "Deposit",
  );
  for (const [index, member] of deposits.entries()) {
    const event =
      content.eventToStepMembers[mandatoryBeforeCount + ordinaryCount + index];
    if (
      event === undefined ||
      event.value.phase !== "Deposit" ||
      member.stepIndex !== BigInt(mandatoryBeforeCount + ordinaryCount + index)
    ) {
      throw new Error("DA prefix deposit trace order is inconsistent");
    }
    const shiftedSizes = starts.map((stepIndex) => {
      const step = BigInt(stepIndex);
      return {
        transition: SDK.daPayloadEntryEncodedSize(
          pair(
            encodeTransitionIntegerCbor(step),
            encodeTransitionStepCbor({ ...member.value, step_index: step }),
          ),
        ),
        eventToStep: SDK.daPayloadEntryEncodedSize(
          pair(
            event.keyCbor,
            encodeEventToStepValueCbor({ ...event.value, step_index: step }),
          ),
        ),
      };
    });
    const initialStep = mandatoryBeforeCount + index;
    let width = 0;
    while (width + 1 < starts.length && starts[width + 1]! <= initialStep)
      width += 1;
    transition = {
      entryCount: transition.entryCount + 1,
      encodedTupleBytes:
        transition.encodedTupleBytes + shiftedSizes[width]!.transition,
    };
    eventToStep = {
      entryCount: eventToStep.entryCount + 1,
      encodedTupleBytes:
        eventToStep.encodedTupleBytes + shiftedSizes[width]!.eventToStep,
    };
    for (let next = width + 1; next < starts.length; next += 1) {
      const prefix = starts[next]! - initialStep;
      if (prefix > ordinaryCount) break;
      transitionChanges[prefix] +=
        shiftedSizes[next]!.transition - shiftedSizes[next - 1]!.transition;
      eventChanges[prefix] +=
        shiftedSizes[next]!.eventToStep - shiftedSizes[next - 1]!.eventToStep;
    }
    const full =
      shiftedSizes[
        starts.filter((start) => start <= initialStep + ordinaryCount).length -
          1
      ]!;
    if (
      full.transition !==
        SDK.daPayloadEntryEncodedSize(pair(member.keyCbor, member.valueCbor)) ||
      full.eventToStep !==
        SDK.daPayloadEntryEncodedSize(pair(event.keyCbor, event.valueCbor))
    ) {
      throw new Error(
        "DA prefix deposit trace sizing does not match retained bytes",
      );
    }
  }
  return Array.from({ length: ordinaryCount + 1 }, (_, prefix) => {
    transition = {
      ...transition,
      encodedTupleBytes:
        transition.encodedTupleBytes + transitionChanges[prefix]!,
    };
    eventToStep = {
      ...eventToStep,
      encodedTupleBytes: eventToStep.encodedTupleBytes + eventChanges[prefix]!,
    };
    return { transition, eventToStep };
  });
};

/** Exact sizes for the complete accepted prefix family in one material pass.
 * Prefix sizes differ only in enumerated lists and actual count integers;
 * unknown final header fields contribute a common additive overhead. */
export const measureDaPayloadPrefixes = ({
  payload,
  content,
  ordinarySidecars,
  forcedSidecars,
  identityContext,
  rejectedTxIds,
}: {
  readonly payload: SDK.DaPayload;
  readonly content: DaPayloadBlockContent;
  readonly ordinarySidecars: readonly Buffer[];
  readonly forcedSidecars: readonly Buffer[];
  readonly identityContext: Buffer;
  readonly rejectedTxIds: readonly Buffer[];
}): CommitDaFrameMeasurement => {
  const n = content.processedMempoolTxs.length;
  const ledger = content.utxoPayloadAggregatesByPrefix;
  if (
    ledger === undefined ||
    ledger.length !== n + 1 ||
    ordinarySidecars.length !== n
  )
    throw new Error(
      "Complete DA prefix ledger/material accounting is unavailable",
    );
  const body = payload.block_body;
  const w = body.withdrawals.length;
  const f = body.forced_transactions.length;
  const d = body.deposits.length;
  const before = w + f;
  if (
    content.transitionTraceMembers.length !== before + n + d ||
    content.eventToStepMembers.length !== before + n + d ||
    content.validationTraceMembers.length !== f + n
  )
    throw new Error("DA prefix trace counts are inconsistent");
  const digest = createHash("sha256");
  bindBytes(digest, identityContext);
  for (const field of [
    "withdrawals",
    "forced_transactions",
    "forced_transaction_preimages",
    "deposits",
  ] as const)
    bindEntries(digest, body[field]);
  const aggregates: Record<SDK.DaPayloadEntryField, Aggregate> = {
    utxos: ledger[0]!,
    withdrawals: add(empty(), body.withdrawals),
    forced_transactions: add(empty(), body.forced_transactions),
    forced_transaction_preimages: add(
      empty(),
      body.forced_transaction_preimages,
    ),
    deposits: add(empty(), body.deposits),
    transactions: empty(),
    transaction_preimages: empty(),
    cek_program_material: empty(),
    transition_trace: add(empty(), body.transition_trace.slice(0, before)),
    event_to_step: add(empty(), body.event_to_step.slice(0, before)),
    validation_traces: add(empty(), body.validation_traces.slice(0, f)),
    validation_trace_witnesses: add(
      empty(),
      content.validationTraceMembers
        .slice(0, f)
        .flatMap((member) => member.witnesses),
    ),
  };
  bindEntries(digest, body.transition_trace.slice(0, before));
  bindEntries(digest, body.event_to_step.slice(0, before));
  for (const member of content.validationTraceMembers.slice(0, f)) {
    bindEntries(digest, [
      pair(member.keyCbor, member.valueCbor),
      ...member.witnesses,
    ]);
  }
  const materialByRoot = new Map<string, Buffer>();
  const allMaterial: SDK.DaPayloadEntry[] = [];
  const addMaterial = (sidecar: Buffer): void => {
    bindBytes(digest, sidecar);
    for (const entry of decodeMidgardCekProgramMaterialSidecar(sidecar)) {
      const root = entry.root.toString("hex");
      const encoded = encodeMidgardCekProgramMaterialEntry(entry);
      const previous = materialByRoot.get(root);
      if (previous !== undefined) {
        if (!previous.equals(encoded))
          throw new Error("DA prefix CEK material root collision");
        continue;
      }
      materialByRoot.set(root, encoded);
      const tuple = pair(
        entry.root,
        encodeMidgardCekProgramMaterialDaValue(entry),
      );
      allMaterial.push(tuple);
      aggregates.cek_program_material = add(aggregates.cek_program_material, [
        tuple,
      ]);
    }
  };
  for (const sidecar of forcedSidecars) addMaterial(sidecar);
  const deposit = depositTraceAggregatesByPrefix(content, n, before);
  const prefixes: { innerBytesUpperBound: number; materialDigest: string }[] =
    [];
  const acceptedTxIds: Buffer[] = [];
  for (let prefix = 0; prefix <= n; prefix += 1) {
    if (prefix > 0) {
      const index = prefix - 1;
      const txId = content.processedMempoolTxs[index]![TxColumns.TX_ID];
      const transition = content.transitionTraceMembers[before + index]!;
      const validation = content.validationTraceMembers[f + index]!;
      if (
        transition.value.phase !== "L2Transaction" ||
        !("L2TransactionEventKey" in transition.value.event_key) ||
        transition.value.event_key.L2TransactionEventKey.tx_id !==
          txId.toString("hex") ||
        !("L2TransactionEventKey" in validation.eventKey) ||
        validation.eventKey.L2TransactionEventKey.tx_id !== txId.toString("hex")
      )
        throw new Error("DA prefix accepted transaction order is inconsistent");
      acceptedTxIds.push(Buffer.from(txId));
      for (const field of ["transactions", "transaction_preimages"] as const) {
        const entry = body[field][index]!;
        aggregates[field] = add(aggregates[field], [entry]);
        bindEntries(digest, [entry]);
      }
      for (const field of ["transition_trace", "event_to_step"] as const) {
        const entry = body[field][before + index]!;
        aggregates[field] = add(aggregates[field], [entry]);
        bindEntries(digest, [entry]);
      }
      const descriptor = pair(validation.keyCbor, validation.valueCbor);
      aggregates.validation_traces = add(aggregates.validation_traces, [
        descriptor,
      ]);
      aggregates.validation_trace_witnesses = add(
        aggregates.validation_trace_witnesses,
        validation.witnesses,
      );
      bindEntries(digest, [descriptor, ...validation.witnesses]);
      addMaterial(ordinarySidecars[index]!);
    }
    const counts = {
      withdrawalCount: BigInt(w),
      forcedTransactionCount: BigInt(f),
      l2TransactionCount: BigInt(prefix),
      depositCount: BigInt(d),
      totalEventCount: BigInt(before + prefix + d),
      transitionStepCount: BigInt(before + prefix + d),
      validationTraceCount: BigInt(f + prefix),
    };
    const snapshot = {
      ...aggregates,
      utxos: ledger[prefix]!,
      transition_trace: {
        entryCount: aggregates.transition_trace.entryCount + d,
        encodedTupleBytes:
          aggregates.transition_trace.encodedTupleBytes +
          deposit[prefix]!.transition.encodedTupleBytes,
      },
      event_to_step: {
        entryCount: aggregates.event_to_step.entryCount + d,
        encodedTupleBytes:
          aggregates.event_to_step.encodedTupleBytes +
          deposit[prefix]!.eventToStep.encodedTupleBytes,
      },
    };
    const sizedPayload = {
      ...payload,
      block_body: { ...body, header: { ...body.header, ...counts }, counts },
    };
    const innerBytesUpperBound = SDK.daPayloadEncodedSizeFromEntryAggregates(
      sizedPayload,
      snapshot,
    );
    const identity = digest.copy();
    bindBytes(
      identity,
      Buffer.from(
        JSON.stringify([
          prefix,
          ledger[prefix]!.entryCount,
          ledger[prefix]!.encodedTupleBytes,
        ]),
      ),
    );
    prefixes.push({
      innerBytesUpperBound,
      materialDigest: identity.digest("hex"),
    });
    if (
      prefix === n &&
      innerBytesUpperBound !==
        SDK.daPayloadEncodedSizeFromUtxoAggregate(
          {
            ...sizedPayload,
            block_body: {
              ...sizedPayload.block_body,
              cek_program_material: allMaterial,
            },
          },
          ledger[prefix]!,
        )
    )
      throw new Error(
        "Complete DA prefix accounting differs from canonical full block sizing",
      );
  }
  return {
    innerBytesUpperBound: prefixes[n]!.innerBytesUpperBound,
    acceptedTxCount: n,
    acceptedTxIds,
    rejectedTxIds,
    prefixes,
    hasMandatoryWork: before + d > 0,
  };
};
