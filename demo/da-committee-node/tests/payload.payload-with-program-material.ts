import {
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialDaValue,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import { makePayloadFixture } from "./helpers.js";

const sortedEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const rewriteL2EventTxId = (
  event: SDK.EventKey,
  txIds: ReadonlyMap<string, string>,
): SDK.EventKey => {
  if (!("L2TransactionEventKey" in event)) return event;
  const current = event.L2TransactionEventKey.tx_id;
  return {
    L2TransactionEventKey: {
      tx_id: txIds.get(current) ?? current,
    },
  };
};

export const payloadWithProgramMaterial = async (
  envelopes: readonly MidgardCekProgramEnvelope[],
  material: readonly MidgardCekProgramMaterialEntry[],
): Promise<SDK.DaPayload> => {
  const fixture = await makePayloadFixture();
  const scriptWitnesses = encodeMidgardVersionedScriptListPreimage(
    envelopes.map((envelope) => ({
      language: "MidgardV1" as const,
      scriptBytes: encodeMidgardCekProgramEnvelope(envelope),
    })),
  );
  const rewrittenTransactions = fixture.payload.block_body.transaction_preimages
    .map(([oldTxId, txCborHex]) => {
      const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.from(txCborHex, "hex"),
      );
      const tx = materializeMidgardNativeTxFromCanonical({
        ...decoded,
        witnessSet: {
          ...decoded.witnessSet,
          scriptTxWitsPreimageCbor: scriptWitnesses,
        },
      });
      const txId = computeMidgardNativeTxId(tx).toString("hex");
      const source = deriveMidgardNativeTxProofSource(tx);
      const committedSource: SDK.L2TransactionSource = {
        tx_id: txId,
        source: {
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        },
      };
      return {
        oldTxId,
        txId,
        txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
        committedSource,
      };
    })
    .sort((left, right) => left.txId.localeCompare(right.txId));
  const rewrittenTxIds = new Map(
    rewrittenTransactions.map(({ oldTxId, txId }) => [oldTxId, txId]),
  );
  const rewriteEventCbor = (eventCbor: string): string =>
    LucidData.to(
      rewriteL2EventTxId(
        LucidData.from(eventCbor, SDK.EventKeySchema as never) as SDK.EventKey,
        rewrittenTxIds,
      ) as never,
      SDK.EventKeySchema as never,
    );
  const transitionTrace = fixture.payload.block_body.transition_trace.map(
    ([key, value]) => {
      const step = LucidData.from(
        value,
        SDK.TransitionStepSchema as never,
      ) as SDK.TransitionStep;
      return [
        key,
        LucidData.to(
          {
            ...step,
            event_key: rewriteL2EventTxId(step.event_key, rewrittenTxIds),
          } satisfies SDK.TransitionStep as never,
          SDK.TransitionStepSchema as never,
        ),
      ] satisfies SDK.DaPayloadEntry;
    },
  );
  return {
    ...fixture.payload,
    block_body: {
      ...fixture.payload.block_body,
      transactions: rewrittenTransactions.map(({ txId, committedSource }) => [
        txId,
        LucidData.to(
          committedSource as never,
          SDK.L2TransactionSourceSchema as never,
        ),
      ]),
      transaction_preimages: rewrittenTransactions.map(({ txId, txCbor }) => [
        txId,
        txCbor,
      ]),
      cek_program_material: sortedEntries(
        material.map(
          (entry) =>
            [
              Buffer.from(entry.root).toString("hex"),
              encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
            ] satisfies SDK.DaPayloadEntry,
        ),
      ),
      transition_trace: transitionTrace,
      event_to_step: sortedEntries(
        fixture.payload.block_body.event_to_step.map(([key, value]) => [
          rewriteEventCbor(key),
          value,
        ]),
      ),
      validation_traces: sortedEntries(
        fixture.payload.block_body.validation_traces.map(([key, value]) => [
          rewriteEventCbor(key),
          value,
        ]),
      ),
    },
  };
};
