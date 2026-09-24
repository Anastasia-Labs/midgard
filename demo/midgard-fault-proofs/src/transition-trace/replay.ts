import {
  decodeMidgardFieldPreimage,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Data, type Network } from "@lucid-evolution/lucid";

import { classifyCommittedFieldShapeFields } from "../committed-field-shape/prepare-committed-field-shape.js";
import { transactionHasNonCanonicalMintItem } from "../mint-item-non-canonical/replay.js";
import { replayPrerequisiteFailure } from "../workflow/replay-prerequisite.js";
import {
  detectTransitionTraceFaults,
  type TransitionTraceDetectionEvidence,
} from "./detect.js";
import {
  reopenTransitionDeposit,
  type TransitionDepositOpening,
} from "./history-opening.js";
import { createTransitionTraceLedgerReplay } from "./ledger-replay.js";
import {
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import { assertRetainedReplayTerminal } from "./replay-terminal.js";
import { buildRetainedValidationClaimWitness } from "./witnesses.js";

/** Exact L1 event preimages; the installed caller must admit their raw snapshot
 * before invoking this pure replay builder. This type grants no replay authority. */
export type TransitionTraceDepositPreimage = Readonly<{
  history: TransitionDepositOpening;
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
    // Preserve a buildable finding from the already replayed prefix before a
    // later event can leave the semantic proof's domain.
    if (
      (await detectTransitionTraceFaults(current, evidence)).some(
        (d) => d.buildable,
      )
    )
      return evidence;
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
      source.phase === "L2Transaction" &&
      source.entry.validity !== "TxIsValid"
    )
      throw replayPrerequisiteFailure(
        current.headerHash,
        step.event_key,
        "representable_validity_flag",
      );
    if (
      source.phase === "L2Transaction" ||
      source.phase === "ForcedTransaction"
    ) {
      if (
        source.phase === "L2Transaction" &&
        (classifyCommittedFieldShapeFields(
          decodeMidgardNativeTxFullFromCanonicalCbor(
            source.entry.fullTransactionCbor,
          ),
        ).some(({ evidence: field }) => field.isViolation) ||
          transactionHasNonCanonicalMintItem(source.entry.fullTransactionCbor))
      )
        throw replayPrerequisiteFailure(
          current.headerHash,
          step.event_key,
          "representable_field_shape",
        );
      const retained = await buildRetainedValidationClaimWitness({
        reconstruction: current,
        eventKey: step.event_key,
      });
      assertRetainedReplayTerminal(retained);
      const acceptedDescriptor =
        retained.claim.descriptor_membership.value.verdict === "Accepted";
      const acceptedTerminal =
        retained.claim.terminal_state.verdict === "Accepted";
      if (acceptedDescriptor && acceptedTerminal)
        claims.push({
          claim: retained.claim,
          terminalAcceptanceWitnessCbor: retained.terminalWorkWitnessCbor,
        });
      const valid =
        source.phase === "L2Transaction"
          ? source.entry.validity === "TxIsValid"
          : source.entry.value.verdict === "ForcedTxValid";
      if (valid) {
        const full =
          source.phase === "ForcedTransaction"
            ? decodeMidgardForcedTxFullFromCanonicalCbor(
                source.entry.fullTransactionCbor,
              )
            : decodeMidgardNativeTxFullFromCanonicalCbor(
                source.entry.fullTransactionCbor,
              );
        const spentUtxos: SDK.LedgerDeleteWitness[] = [];
        const producedUtxos: SDK.LedgerInsertWitness[] = [];
        for (const key of decodeMidgardFieldPreimage(
          full.body.spendInputsPreimageCbor,
        )) {
          if (!ledger.has(key))
            throw replayPrerequisiteFailure(
              current.headerHash,
              step.event_key,
              "present_spend_input",
            );
          spentUtxos.push(await ledger.delete(key));
        }
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
      if (acceptedDescriptor && !acceptedTerminal) {
        if (retained.claim.terminal_state.verdict !== "Rejected")
          throw new Error(
            "Transition replay retained endpoint is not terminal",
          );
        if (
          (await detectTransitionTraceFaults(current, evidence)).some(
            (d) => d.buildable,
          )
        )
          return evidence;
        throw replayPrerequisiteFailure(
          current.headerHash,
          step.event_key,
          "accepted_terminal",
        );
      }
    } else if (source.phase === "Withdrawal") {
      if (source.entry.value.validity === "WithdrawalIsValid") {
        const id = source.entry.value.body.l2_outref;
        const key = encodeMidgardSpendInputItem({
          txId: Buffer.from(id.transactionId, "hex"),
          outputIndex: Number(id.outputIndex),
        });
        // A payable claim on an output the replayed ledger no longer holds has
        // no delete witness; a direct withdrawal finding must cover the event.
        if (!ledger.has(key))
          throw replayPrerequisiteFailure(
            current.headerHash,
            step.event_key,
            "present_spend_input",
          );
        withdrawal.push({
          stepIndex: index,
          spentUtxo: await ledger.delete(key),
        });
      }
    } else {
      const selected = deposits.filter(({ history }) => {
        const commitment = Data.from(
          history.commitmentCbor,
          SDK.EventHistoryCommitment,
        );
        return (
          Data.to(commitment.event_id, SDK.OutputReference) ===
          Data.to(source.entry.key, SDK.OutputReference)
        );
      });
      if (selected.length > 1)
        throw new Error(
          "Transition replay captured ambiguous deposit event preimages",
        );
      // Decision 0007: a committed deposit with no authentic L1 origin is the
      // `fabricatedDeposit` fraud, not a replay abort. Only that family's
      // finding at this leaf discharges the prerequisite. L1 history Orders,
      // authenticated gaps and previously captured commitments decide authority;
      // a consumed pointer alone is never evidence of a fabricated event.
      if (selected.length === 0)
        throw replayPrerequisiteFailure(
          current.headerHash,
          step.event_key,
          "present_source_origin",
        );
      const { history } = selected[0]!;
      let opened: ReturnType<typeof reopenTransitionDeposit>;
      try {
        opened = reopenTransitionDeposit(history, depositPolicyId, {
          ...source.entry,
          valueCbor: source.entry.valueBytes.toString("hex"),
        });
      } catch {
        throw replayPrerequisiteFailure(
          current.headerHash,
          step.event_key,
          "matching_source_origin",
        );
      }
      const originalAssets = Object.fromEntries(
        [...opened.opening.original_assets].flatMap(([policy, names]) =>
          [...names].map(([name, quantity]) => [
            policy === "" ? "lovelace" : policy + name,
            quantity,
          ]),
        ),
      );
      const info = source.entry.value;
      const effect = deriveCanonicalOriginalDepositTransitionEffect({
        configuredNetwork: network,
        eventId: source.entry.key,
        l2NetworkId: info.l2_network_id,
        l2Address: info.l2_address,
        l2DatumCbor:
          info.l2_datum === null
            ? null
            : Buffer.from(
                plutusConstrFieldCbor(opened.infoCbor, [2, 0]),
                "hex",
              ),
        originalAssets,
      });
      const op = effect.operations[0];
      if (op?.type !== "insert" || effect.operations.length !== 1)
        throw new Error("Transition deposit projection is not one insert");
      // A deposit that re-creates a held output repeats an earlier source
      // event; the finding naming that repeat owns its ledger effect.
      if (ledger.has(op.outRefCbor))
        throw replayPrerequisiteFailure(
          current.headerHash,
          step.event_key,
          "prior_transition_effect",
        );
      deposit.push({
        stepIndex: index,
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
