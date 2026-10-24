import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";

import {
  authenticatedForcedTransactionSource,
  authenticatedL2TransactionSource,
  canonicalNativeOutput,
  nativePreimageItems,
  requireCanonicalFieldCommitment,
} from "./detect.authenticated-l2-transaction-source.js";
import {
  detection,
  type TransitionTraceDetection,
} from "./detect.detect-count-faults.js";
import {
  canonicalNativeSpendInput,
  exactHexBytes,
  mpfProofFromWitness,
  normalizedMpfRoot,
  requireDeletionKeepsTwoChildren,
  verifyProofRoot,
} from "./detect.mpf-proof-from-witness.js";
import { transitionTraceError } from "./errors.js";
import {
  type TransitionL2ReplayMaterial,
  transitionReplayedTransaction,
} from "./proof-material.js";
import { type TransitionTraceReconstruction } from "./reconstruct.js";
import {
  requireTraceEntry,
  sourceEventOrThrow,
} from "./witnesses.build-source-membership-proof.js";
import {
  buildL2TransactionTransitionWitness,
  buildValidForcedTransactionTransitionWitness,
  type L2TransactionTransitionEvidence,
} from "./witnesses.js";

/**
 * Replay an L2 or accepted forced transaction's ledger mutation from its
 * authenticated field preimages (read from the reconstruction, not carried by
 * the witness) and the challenger's delete and insert witnesses. Returns the
 * replayed root and the replay material the on-chain field walks and output
 * steps consume.
 */
const replayL2TransactionTransition = ({
  preRoot,
  transactionBody,
  txId,
  spendInputsPreimage,
  outputsPreimage,
  spentUtxos,
  producedUtxos,
}: {
  readonly preRoot: string;
  readonly transactionBody: {
    readonly spendInputsHash: Buffer;
    readonly outputsHash: Buffer;
  };
  readonly txId: Buffer;
  readonly spendInputsPreimage: string;
  readonly outputsPreimage: string;
  readonly spentUtxos: readonly SDK.LedgerDeleteWitness[];
  readonly producedUtxos: readonly SDK.LedgerInsertWitness[];
}): {
  readonly root: string;
  readonly l2Replay: TransitionL2ReplayMaterial;
} => {
  const spendInputs = nativePreimageItems(
    spendInputsPreimage,
    "L2 transaction spend-input preimage",
  ).map((bytes, index) =>
    canonicalNativeSpendInput(
      bytes,
      `L2 transaction spend-input preimage[${index.toString()}]`,
    ),
  );
  const outputs = nativePreimageItems(
    outputsPreimage,
    "L2 transaction output preimage",
  ).map((bytes, index) =>
    canonicalNativeOutput(
      bytes,
      `L2 transaction output preimage[${index.toString()}]`,
    ),
  );
  requireCanonicalFieldCommitment({
    items: spendInputs,
    expectedCommitment: transactionBody.spendInputsHash,
    label: "L2 transaction spend-input preimage",
  });
  requireCanonicalFieldCommitment({
    items: outputs,
    expectedCommitment: transactionBody.outputsHash,
    label: "L2 transaction output preimage",
  });
  if (spentUtxos.length !== spendInputs.length) {
    throw transitionTraceError(
      "missingWitnessData",
      "L2 transaction delete witnesses must match the authenticated input count exactly.",
    );
  }
  if (producedUtxos.length !== outputs.length) {
    throw transitionTraceError(
      "missingWitnessData",
      "L2 transaction insert witnesses must match the authenticated output count exactly.",
    );
  }

  let root = preRoot;
  for (const [index, item] of spentUtxos.entries()) {
    const label = `L2 transaction delete witness ${index.toString()}`;
    const key = exactHexBytes(item.key, `${label}.key`);
    const value = exactHexBytes(item.value, `${label}.value`);
    if (!key.equals(spendInputs[index]!)) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label} is not ordered and bound to its authenticated spend input.`,
      );
    }
    const deleteProof = mpfProofFromWitness({
      key,
      value,
      proof: item.delete_proof,
      label: `${label}.delete_proof`,
    });
    requireDeletionKeepsTwoChildren({
      proof: item.delete_proof,
      opening: item.opening,
      label: `${label}.delete_proof`,
    });
    verifyProofRoot({
      proof: deleteProof,
      includingItem: true,
      expectedRoot: root,
      label: `${label}.delete_proof`,
    });
    try {
      root = normalizedMpfRoot(
        deleteProof.verify(false),
        `${label}.delete_proof`,
      );
    } catch (cause) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label}.delete_proof cannot derive the post-delete root.`,
        cause,
      );
    }
  }

  for (const [index, item] of producedUtxos.entries()) {
    const label = `L2 transaction insert witness ${index.toString()}`;
    if (index > 65_535) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label} exceeds the canonical V1 output-index domain.`,
      );
    }
    const key = exactHexBytes(item.key, `${label}.key`);
    const value = exactHexBytes(item.value, `${label}.value`);
    // The ledger trie key is the §5.3 field-0/1 item form
    // (`82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, 38 bytes), matching on-chain
    // `ledger_outref_key` / `encode_midgard_tx_input` — not CML's or
    // `encodeCbor`'s minimal-index shape.
    const expectedKey = encodeMidgardSpendInputItem({
      txId,
      outputIndex: index,
    });
    // The ledger trie *value* is the output's LedgerOutputCommitmentV1
    // descriptor, not the full output bytes (spec §5.3: "recomputed with that
    // descriptor as the MPT value, not with the full output bytes"). Every
    // other producer of `utxos_root` — the node, the validation machine, the
    // watcher's block replay and DA reconstruction — commits the descriptor,
    // and on-chain `apply_l2_outputs` derives the same descriptor from the
    // authenticated output. A challenger that submitted full output bytes here
    // would replay a root an honest block can never equal.
    let expectedValue: Buffer;
    try {
      expectedValue = Buffer.from(
        buildCanonicalMidgardLedgerOutputMaterial({
          outputIndex: index,
          outputCbor: outputs[index]!,
        }).descriptorCbor,
      );
    } catch (cause) {
      // An output the canonical ledger-output decoder refuses has no §5.3
      // descriptor, so it has no `utxos_root` value at all and this arm cannot
      // replay the insert. Fail closed with the challenger's own error rather
      // than letting a codec error escape: on-chain `apply_l2_outputs` binds
      // the same derivation with `expect`, so the arm aborts on these inputs
      // too and no witness built here could have minted.
      throw transitionTraceError(
        "missingWitnessData",
        `${label} names a transaction output with no canonical ledger value.`,
        cause,
      );
    }
    if (!key.equals(expectedKey) || !value.equals(expectedValue)) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label} is not ordered and bound to its authenticated transaction output.`,
      );
    }
    const insertProof = mpfProofFromWitness({
      key,
      value,
      proof: item.insert_proof,
      label: `${label}.insert_proof`,
    });
    verifyProofRoot({
      proof: insertProof,
      includingItem: false,
      expectedRoot: root,
      label: `${label}.insert_proof`,
    });
    try {
      root = normalizedMpfRoot(
        insertProof.verify(true),
        `${label}.insert_proof`,
      );
    } catch (cause) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label}.insert_proof cannot derive the post-insert root.`,
        cause,
      );
    }
  }
  return {
    root,
    l2Replay: {
      spendInputCbors: spendInputs.map((item) => item.toString("hex")),
      outputCbors: outputs.map((item) => item.toString("hex")),
      spentUtxos,
      producedUtxos,
    },
  };
};

export const detectL2TransactionTransitions = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly (L2TransactionTransitionEvidence & {
    readonly stepIndex: bigint;
  })[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const trace = requireTraceEntry(reconstruction, item.stepIndex);
    const source = sourceEventOrThrow(reconstruction, trace.value.event_key);
    if (
      source.phase !== "L2Transaction" &&
      source.phase !== "ForcedTransaction"
    ) {
      throw transitionTraceError(
        "missingWitnessData",
        `Trace step ${item.stepIndex.toString()} is not a transaction step.`,
      );
    }
    const forced = source.phase === "ForcedTransaction";
    const witness = forced
      ? await buildValidForcedTransactionTransitionWitness({
          reconstruction,
          stepIndex: item.stepIndex,
        })
      : await buildL2TransactionTransitionWitness({
          reconstruction,
          stepIndex: item.stepIndex,
        });
    const replayed = transitionReplayedTransaction(witness);
    if (replayed === null) {
      throw transitionTraceError(
        "missingWitnessData",
        "Transaction evidence did not build a transaction-transition witness.",
      );
    }
    const { transactionBody, txId } =
      "ValidForcedTransactionTransition" in witness
        ? authenticatedForcedTransactionSource(
            witness.ValidForcedTransactionTransition.source_membership,
          )
        : "L2TransactionTransition" in witness
          ? (() => {
              const l2 = authenticatedL2TransactionSource(
                witness.L2TransactionTransition.source_membership,
              );
              return {
                transactionBody: l2.compact.transactionBody,
                txId: l2.txId,
              };
            })()
          : (() => {
              throw transitionTraceError(
                "missingWitnessData",
                "Transaction evidence did not build a transaction-transition witness.",
              );
            })();
    const preimages =
      source.phase === "ForcedTransaction"
        ? decodeMidgardForcedTxFullFromCanonicalCbor(
            source.entry.fullTransactionCbor,
          ).body
        : {
            spendInputsPreimageCbor: source.entry.spendInputsPreimage,
            outputsPreimageCbor: source.entry.outputsPreimage,
          };
    const { root: replayedPostRoot, l2Replay } = replayL2TransactionTransition({
      preRoot: replayed.traceProof.value.pre_utxos_root,
      transactionBody,
      txId,
      spendInputsPreimage: Buffer.from(
        preimages.spendInputsPreimageCbor,
      ).toString("hex"),
      outputsPreimage: Buffer.from(preimages.outputsPreimageCbor).toString(
        "hex",
      ),
      spentUtxos: item.spentUtxos,
      producedUtxos: item.producedUtxos,
    });
    if (replayedPostRoot === replayed.traceProof.value.post_utxos_root) {
      continue;
    }
    detections.push(
      detection({
        reconstruction,
        kind: "invalidOneStepTransition",
        invariant: forced
          ? "forced_transaction_transition_matches_authenticated_replay"
          : "l2_transaction_transition_matches_authenticated_replay",
        diagnostic: `${forced ? "Accepted forced" : "L2"} transaction trace step ${item.stepIndex.toString()} has authenticated ledger mutation evidence whose replay disagrees with the committed post-state root.`,
        fault: SDK.invalidOneStepTransitionFault(witness),
        l2Replay,
      }),
    );
  }
  return detections;
};
