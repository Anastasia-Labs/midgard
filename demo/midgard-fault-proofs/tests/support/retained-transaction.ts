import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardForcedTxCanonical,
  encodeMidgardSpendInputItem,
  MIDGARD_CONSENSUS_PROFILE,
  submittedForcedTransactionFromNative,
} from "@al-ft/midgard-core";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { replayValidationMachineEvent } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildRetainedValidationBlockFixture,
  retainValidationTrace,
} from "./retained-reason-classifier.js";

/**
 * Retain an operator's transaction verdict against caller-supplied ledger state.
 * The real validation machine supplies the witnesses, including its actual
 * refusal when the operator's accepted verdict is deliberately fraudulent.
 */
export const retainedTransactionFixture = async (input: {
  canonicalTransactionCbor: Buffer;
  predecessor: { header: SDK.Header; headerHash: string };
  ledgerEntries: readonly { outRef: Buffer; output: Buffer }[];
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
  programMaterial?: Parameters<
    typeof encodeMidgardCekProgramMaterialSidecar
  >[0];
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
  const prior = input.predecessor.header;
  const replay = await Effect.runPromise(
    replayValidationMachineEvent({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
      canonicalTransactionCbor:
        input.source === undefined
          ? input.canonicalTransactionCbor
          : encodeMidgardForcedTxCanonical(
              submittedForcedTransactionFromNative(nativeTx),
            ),
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
        input.programMaterial ?? [],
      ),
      ...(input.source === undefined
        ? { sourceKind: "normal" as const }
        : {
            sourceKind: "forced" as const,
          }),
      ledgerWitnessEntries: input.ledgerEntries,
      priorUtxosRoot: prior.utxosRoot,
      blockEndTimeMs: Number(input.endTime),
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
    ...retainValidationTrace({
      trace: replay.trace,
      eventKey,
      claim,
    }),
    programMaterialEntries: (input.programMaterial ?? []).map((entry) => [
      Buffer.from(entry.root).toString("hex"),
      encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
    ]),
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
  };
};
