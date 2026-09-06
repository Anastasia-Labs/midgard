import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Data, type Network, type UTxO } from "@lucid-evolution/lucid";

import {
  detectTransitionTraceFaults,
  type TransitionTraceDetectionEvidence,
} from "./detect.js";
import { createTransitionTraceLedgerReplay } from "./ledger-replay.js";
import {
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import { buildRetainedValidationClaimWitness } from "./witnesses.js";

/** Exact L1 event preimages; the installed caller must admit their raw snapshot
 * before invoking this pure replay builder. This type grants no replay authority. */
export type TransitionTraceDepositPreimage = Readonly<{
  event: UTxO;
  eventAssetName: string;
  eventRefInputIndex: bigint;
}>;

/** Derives witnesses from the predecessor descriptor trie and retained source
 * bytes. Stops at the first false transition: later operator pre-roots no longer
 * describe the locally replayed ledger. No caller-supplied verdict or MPF proof
 * participates in this derivation. */
export const deriveTransitionTraceReplayEvidence = async ({
  current,
  predecessor,
  deposits,
  network,
  depositPolicyId,
}: {
  current: TransitionTraceReconstruction;
  predecessor?: TransitionTraceReconstruction;
  deposits: readonly TransitionTraceDepositPreimage[];
  network: Network;
  depositPolicyId: string;
}): Promise<TransitionTraceDetectionEvidence> => {
  if (
    predecessor !== undefined &&
    (predecessor.headerHash !== current.header.prevHeaderHash ||
      predecessor.header.utxosRoot !== current.header.prevUtxosRoot)
  )
    throw new Error(
      "Transition replay predecessor differs from authenticated header",
    );
  const ledger = await createTransitionTraceLedgerReplay({
    entries: predecessor?.rootData.utxos.entries ?? [],
    expectedRoot: current.header.prevUtxosRoot,
  });
  const l2: NonNullable<
    TransitionTraceDetectionEvidence["l2TransactionTransitions"]
  >[number][] = [];
  const withdrawal: NonNullable<
    TransitionTraceDetectionEvidence["withdrawalTransitions"]
  >[number][] = [];
  const deposit: NonNullable<
    TransitionTraceDetectionEvidence["depositTransitions"]
  >[number][] = [];
  const claims: NonNullable<
    TransitionTraceDetectionEvidence["acceptedTransactionTransitionMismatches"]
  >[number][] = [];
  const evidence = {
    l2TransactionTransitions: l2,
    withdrawalTransitions: withdrawal,
    depositTransitions: deposit,
    acceptedTransactionTransitionMismatches: claims,
  };
  // Structural faults already have complete authenticated evidence in the block.
  if ((await detectTransitionTraceFaults(current)).some((d) => d.buildable))
    return evidence;
  for (let index = 0n; index < current.counts.transitionStepCount; index++) {
    const step = current.traceByStepIndex.get(index)?.value;
    if (step === undefined || step.pre_utxos_root !== ledger.root())
      throw new Error(
        "Transition replay cannot open the selected operator pre-root",
      );
    const source = current.sourceEventsByFingerprint.get(
      eventKeyFingerprint(step.event_key),
    );
    if (source === undefined)
      throw new Error("Transition replay source is absent");
    if (
      source.phase === "L2Transaction" ||
      source.phase === "ForcedTransaction"
    ) {
      const retained = await buildRetainedValidationClaimWitness({
        reconstruction: current,
        eventKey: step.event_key,
      });
      if (retained.claim.descriptor_membership.value.verdict === "Accepted")
        claims.push({
          claim: retained.claim,
          terminalAcceptanceWitnessCbor: retained.terminalWorkWitnessCbor,
        });
      const valid =
        source.phase === "L2Transaction"
          ? source.entry.validity === "TxIsValid"
          : source.entry.value.verdict === "ForcedTxValid";
      if (valid) {
        const full = decodeMidgardNativeTxFullFromCanonicalCbor(
          source.entry.fullTransactionCbor,
        );
        const spentUtxos: SDK.LedgerDeleteWitness[] = [];
        const producedUtxos: SDK.LedgerInsertWitness[] = [];
        for (const key of decodeMidgardFieldPreimage(
          full.body.spendInputsPreimageCbor,
        ))
          spentUtxos.push(await ledger.delete(key));
        const txId =
          source.phase === "L2Transaction"
            ? source.entry.txId
            : source.entry.value.tx_id;
        for (const [outputIndex, output] of decodeMidgardFieldPreimage(
          full.body.outputsPreimageCbor,
        ).entries())
          producedUtxos.push(
            await ledger.insert(
              encodeMidgardSpendInputItem({
                txId: Buffer.from(txId, "hex"),
                outputIndex,
              }),
              output,
            ),
          );
        if (source.phase === "L2Transaction")
          l2.push({ stepIndex: index, spentUtxos, producedUtxos });
      }
    } else if (source.phase === "Withdrawal") {
      if (source.entry.value.validity === "WithdrawalIsValid") {
        const id = source.entry.value.body.l2_outref;
        withdrawal.push({
          stepIndex: index,
          spentUtxo: await ledger.delete(
            encodeMidgardSpendInputItem({
              txId: Buffer.from(id.transactionId, "hex"),
              outputIndex: Number(id.outputIndex),
            }),
          ),
        });
      }
    } else {
      const selected = deposits.filter(({ event }) => {
        if (event.datum === undefined || event.datum === null) return false;
        const decoded = Data.from(event.datum, SDK.DepositDatum).event;
        return (
          Data.to(decoded.id, SDK.OutputReference) ===
          Data.to(source.entry.key, SDK.OutputReference)
        );
      });
      if (selected.length !== 1)
        throw new Error(
          "Transition replay needs one authenticated deposit event preimage",
        );
      const { event, eventAssetName, eventRefInputIndex } = selected[0]!;
      const decoded = Data.from(event.datum!, SDK.DepositDatum).event;
      if (
        Data.to(decoded.info, SDK.DepositInfo) !==
          source.entry.valueBytes.toString("hex") ||
        event.assets[depositPolicyId + eventAssetName] !== 1n
      )
        throw new Error(
          "Transition deposit preimage differs from authenticated source",
        );
      const info = source.entry.value;
      const effect = deriveCanonicalDepositTransitionEffect({
        configuredNetwork: network,
        eventId: source.entry.key,
        l2NetworkId: info.l2_network_id,
        l2Address: info.l2_address,
        l2DatumCbor:
          info.l2_datum === null
            ? null
            : Buffer.from(Data.to(info.l2_datum), "hex"),
        l1Assets: event.assets,
        depositPolicyId,
        depositAssetNameHex: eventAssetName,
      });
      const op = effect.operations[0];
      if (op?.type !== "insert" || effect.operations.length !== 1)
        throw new Error("Transition deposit projection is not one insert");
      deposit.push({
        stepIndex: index,
        eventRefInputIndex,
        eventAssetName,
        projectedUtxo: await ledger.insert(op.outRefCbor, op.outputCbor),
      });
    }
    if (ledger.root() !== step.post_utxos_root) {
      if (
        !(await detectTransitionTraceFaults(current, evidence)).some(
          (d) => d.buildable,
        )
      )
        throw new Error(
          "Transition root mismatch requires a different authenticated validation dispute",
        );
      return evidence;
    }
  }
  return evidence;
};
