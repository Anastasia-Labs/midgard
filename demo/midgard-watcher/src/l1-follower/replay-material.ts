import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import {
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { reconstructMidgardTransaction } from "@al-ft/midgard-core/consensus-validation";
import {
  aikenSerialisedPlutusDataCbor,
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  attributeWithdrawalClassificationFault,
  type BlockReplayMaterial,
  forcedVerdictForRejection,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Data, type Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type {
  WatcherUserEvent,
  WatcherUserEventKind,
} from "../verification/user-event.js";
import { watcherOriginalDepositAssets } from "../verification/user-event.js";

const canonicalField = (cbor: string, path: readonly number[]) =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(cbor, path),
  );
const keyOf = (id: string) => {
  const ref = Data.from(id, SDK.OutputReference);
  return encodeMidgardSpendInputItem({
    txId: Buffer.from(ref.transactionId, "hex"),
    outputIndex: Number(ref.outputIndex),
  });
};

/** L1 projection supplies original bytes; the shared replay supplies verdicts. */
export const watcherReplayMaterial = (
  input: Readonly<{
    network: Network;
    header: SDK.Header;
    blockTimeMs: number;
    event(kind: WatcherUserEventKind, id: string): Promise<WatcherUserEvent>;
  }>,
): BlockReplayMaterial => {
  const event = async (kind: WatcherUserEventKind, id: string) => {
    const read = await input.event(kind, id);
    const due = BigInt(read.inclusionTime);
    if (due <= input.header.startTime || due > input.header.endTime)
      throw new Error(`${kind} ${id} is outside the block's coverage`);
    return read;
  };
  return {
    blockTimeMs: input.blockTimeMs,
    replayUserEvent: ({ step, source, ledger }) =>
      Effect.gen(function* () {
        if (step.phase === "Deposit") {
          const read = yield* Effect.tryPromise(() =>
            event("deposit", source[0]),
          );
          const infoCbor = canonicalField(read.eventCborHex, [1]);
          if (infoCbor !== source[1])
            throw new Error("deposit differs from its authenticated L1 event");
          const info = Data.from(infoCbor, SDK.DepositInfo);
          const effect = deriveCanonicalOriginalDepositTransitionEffect({
            configuredNetwork: input.network,
            eventId: Data.from(source[0], SDK.OutputReference),
            l2NetworkId: info.l2_network_id,
            l2Address: info.l2_address,
            l2DatumCbor:
              info.l2_datum === null
                ? null
                : Buffer.from(plutusConstrFieldCbor(infoCbor, [2, 0]), "hex"),
            originalAssets: watcherOriginalDepositAssets(read),
          });
          const operation = effect.operations[0];
          if (operation?.type !== "insert")
            throw new Error("deposit did not derive a canonical insert");
          return [
            {
              key: keyOf(source[0]).toString("hex"),
              output: Buffer.from(operation.outputCbor),
            },
          ];
        }
        const read = yield* Effect.tryPromise(() =>
          event("withdrawal", source[0]),
        );
        const infoCbor = canonicalField(read.eventCborHex, [1]);
        const { body } = Data.from(infoCbor, SDK.WithdrawalInfo);
        const outRef = keyOf(Data.to(body.l2_outref, SDK.OutputReference));
        const classified = {
          l2Owner: body.l2_owner,
          l2ValueCbor: plutusConstrFieldCbor(infoCbor, [0, 2]),
          eventInfoCbor: infoCbor,
          ledgerOutRef: outRef,
          ledgerOutput: ledger.get(outRef.toString("hex")) ?? null,
        };
        // A runtime fault of the classifier is this watcher's own: the
        // replay is retried, and no claim is made on it.
        const classification = yield* attributeWithdrawalClassificationFault(
          classified,
          SDK.classifyWithdrawalFromLedger(classified),
        );
        if (classification.settlementEventInfo.toString("hex") !== source[1])
          throw new Error(
            "withdrawal verdict differs from its canonical replay",
          );
        return classification.shouldDeleteLedgerUtxo
          ? [{ key: outRef.toString("hex"), output: null }]
          : [];
      }),
    verifyForcedSource: ({ source, canonicalTransactionCbor, rejection }) =>
      Effect.gen(function* () {
        const read = yield* Effect.tryPromise(() =>
          event("forced_order", source[0]),
        );
        const order = Data.from(read.eventCborHex, SDK.TxOrderEvent).tx;
        const evidence = deriveMidgardForcedTxFaultEvidenceMaterial(
          canonicalTransactionCbor,
        );
        const reconstructed = reconstructMidgardTransaction({
          sourceKind: "forced",
          transactionId: Buffer.from(order.tx_id, "hex"),
          transactionCommitment: Buffer.from(
            order.transaction_commitment,
            "hex",
          ),
          source: {
            compactCbor: Buffer.from(
              order.submitted_source.compact_cbor,
              "hex",
            ),
            witnessSetCompactCbor: Buffer.from(
              order.submitted_source.witness_set_compact_cbor,
              "hex",
            ),
            fieldPreimageLengthsCbor: Buffer.from(
              order.submitted_source.field_preimage_lengths_cbor,
              "hex",
            ),
          },
          fieldPreimages: evidence.fieldPreimages,
        });
        if (!reconstructed.equals(canonicalTransactionCbor))
          throw new Error(
            "forced transaction differs from its admitted commitments",
          );
        const native = decodeMidgardForcedTxFullFromCanonicalCbor(
          canonicalTransactionCbor,
        );
        const submitted = deriveMidgardForcedTxProofSource(native);
        const value: SDK.ForcedInclusionTxV1 = {
          tx_id: computeMidgardNativeTxId(native.compact).toString("hex"),
          submitted_source: {
            compact_cbor: submitted.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              submitted.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              submitted.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict:
            rejection === undefined
              ? "ForcedTxValid"
              : forcedVerdictForRejection(rejection),
        };
        if (
          aikenSerialisedPlutusDataCbor(
            Data.to(value, SDK.ForcedInclusionTxV1),
          ) !== source[1]
        )
          throw new Error("forced verdict differs from its canonical replay");
      }),
  };
};
