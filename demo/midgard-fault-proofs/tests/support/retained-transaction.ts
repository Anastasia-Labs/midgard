import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardSpendInputItem,
  MIDGARD_CONSENSUS_PROFILE,
  submittedForcedTransactionFromNative,
} from "@al-ft/midgard-core";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { replayValidationMachineEvent } from "@al-ft/midgard-validation/tests/validation-machine-event-replay.trace";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildRetainedValidationBlockFixture,
  placeholderValidationTraceEntries,
} from "./retained-reason-classifier.js";

/**
 * Commit an operator's transaction verdict against caller-supplied ledger
 * state. The real validation pipeline replays the event, so callers can read
 * its honest verdict (including its refusal when the operator's accepted
 * verdict is deliberately wrong) next to the committed placeholder descriptor.
 */
export const retainedTransactionFixture = async (input: {
  canonicalTransactionCbor: Buffer;
  predecessor: { header: SDK.Header; headerHash: string };
  ledgerEntries: readonly { outRef: Buffer; output: Buffer }[];
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
  minFeeA?: bigint;
  minFeeB?: bigint;
  source?: {
    kind: "forced";
    orderKey: SDK.OutputReference;
    verdict: SDK.OperatorVerdict;
  };
}) => {
  const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
    input.canonicalTransactionCbor,
  );
  const txId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const eventKey: SDK.EventKey =
    input.source === undefined
      ? { L2TransactionEventKey: { tx_id: txId } }
      : { ForcedTransactionEventKey: { tx_order_id: input.source.orderKey } };
  const verdict = input.source?.verdict ?? "ForcedTxValid";
  const claim =
    verdict === "ForcedTxValid"
      ? { verdict: "accepted" as const }
      : {
          verdict: "rejected" as const,
          reason: verdict.ForcedTxInvalid.reason,
        };
  const canonicalTransactionCbor =
    input.source === undefined
      ? input.canonicalTransactionCbor
      : encodeMidgardForcedTxCanonical(
          submittedForcedTransactionFromNative(nativeTx),
        );
  const prior = input.predecessor.header;
  const replay = await Effect.runPromise(
    replayValidationMachineEvent({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
      canonicalTransactionCbor,
      ...(input.source === undefined
        ? { sourceKind: "normal" as const }
        : {
            sourceKind: "forced" as const,
          }),
      ledgerWitnessEntries: input.ledgerEntries,
      priorUtxosRoot: prior.utxosRoot,
      blockTimeMs: Number(input.endTime),
      expectedNetworkId: prior.expectedNetworkId,
      minFeeA: input.minFeeA ?? prior.minFeeA,
      minFeeB: input.minFeeB ?? prior.minFeeB,
      blockSlot: input.blockSlot,
    }),
  );
  const postLedger = new Map(
    input.ledgerEntries.map(({ outRef, output }) => [
      outRef.toString("hex"),
      output,
    ]),
  );
  if (claim.verdict === "accepted") {
    // An operator accepting an invalid rule still claims the transaction's
    // mechanical ledger effect. Using the honest rejection's empty patch here
    // would introduce an unrelated transition fault. Keep the honest validation
    // trace above unchanged; it is independent of this malicious commitment.
    const spent = decodeMidgardFieldPreimage(
      nativeTx.body.spendInputsPreimageCbor,
    ).map((key) => key.toString("hex"));
    if (
      new Set(spent).size === spent.length &&
      spent.every((key) => postLedger.has(key))
    ) {
      for (const outRef of spent) postLedger.delete(outRef);
      for (const [outputIndex, output] of decodeMidgardFieldPreimage(
        nativeTx.body.outputsPreimageCbor,
      ).entries()) {
        const outRef = encodeMidgardSpendInputItem({
          txId: Buffer.from(txId, "hex"),
          outputIndex,
        });
        postLedger.set(outRef.toString("hex"), output);
      }
    }
  }
  const block = await buildRetainedValidationBlockFixture({
    subject:
      input.source === undefined
        ? { kind: "normal", nativeTx }
        : { ...input.source, nativeTx },
    priorLedgerRoot: prior.utxosRoot,
    prevHeaderHash: input.predecessor.headerHash,
    blockStartTimeMs: Number(prior.endTime),
    blockEndTimeMs: Number(input.endTime),
    blockSlot: input.blockSlot,
    operatorVkey: input.operatorVkey,
    minFeeA: input.minFeeA ?? prior.minFeeA,
    minFeeB: input.minFeeB ?? prior.minFeeB,
    ...placeholderValidationTraceEntries({
      eventKey,
      canonicalTransactionCbor,
      sourceKind: input.source === undefined ? "normal" : "forced",
      claim,
    }),
    postLedgerEntries: [...postLedger].map(([outRef, output]) => ({
      outRef: Buffer.from(outRef, "hex"),
      output,
    })),
  });
  const envelope = await unwrapDaPayload(block.payloadEnvelopeCbor, {
    maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
  });
  return {
    ...block,
    payload: SDK.decodeDaPayload(Buffer.from(envelope.innerBytes)),
    replay,
    postLedgerEntries: [...postLedger].map(([outRef, output]) => ({
      outRef: Buffer.from(outRef, "hex"),
      output: Buffer.from(output),
    })),
  };
};
