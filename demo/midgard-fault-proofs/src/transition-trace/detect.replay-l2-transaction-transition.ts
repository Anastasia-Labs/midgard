import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";

import {
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
  verifyProofRoot,
} from "./detect.mpf-proof-from-witness.js";
import { transitionTraceError } from "./errors.js";
import { type TransitionTraceReconstruction } from "./reconstruct.js";
import {
  buildL2TransactionTransitionWitness,
  type L2TransactionTransitionEvidence,
} from "./witnesses.js";

const replayL2TransactionTransition = (
  witness: Extract<
    SDK.InvalidOneStepTransitionWitness,
    { readonly L2TransactionTransition: unknown }
  >["L2TransactionTransition"],
): string => {
  const spendInputs = nativePreimageItems(
    witness.spend_inputs_preimage,
    "L2 transaction spend-input preimage",
  ).map((bytes, index) =>
    canonicalNativeSpendInput(
      bytes,
      `L2 transaction spend-input preimage[${index.toString()}]`,
    ),
  );
  const outputs = nativePreimageItems(
    witness.outputs_preimage,
    "L2 transaction output preimage",
  ).map((bytes, index) =>
    canonicalNativeOutput(
      bytes,
      `L2 transaction output preimage[${index.toString()}]`,
    ),
  );
  const { compact, txId } = authenticatedL2TransactionSource(
    witness.source_membership,
  );
  requireCanonicalFieldCommitment({
    items: spendInputs,
    expectedCommitment: compact.transactionBody.spendInputsHash,
    label: "L2 transaction spend-input preimage",
  });
  requireCanonicalFieldCommitment({
    items: outputs,
    expectedCommitment: compact.transactionBody.outputsHash,
    label: "L2 transaction output preimage",
  });
  if (witness.spent_utxos.length !== spendInputs.length) {
    throw transitionTraceError(
      "missingWitnessData",
      "L2 transaction delete witnesses must match the authenticated input count exactly.",
    );
  }
  if (witness.produced_utxos.length !== outputs.length) {
    throw transitionTraceError(
      "missingWitnessData",
      "L2 transaction insert witnesses must match the authenticated output count exactly.",
    );
  }

  let root = witness.trace_proof.value.pre_utxos_root;
  for (const [index, item] of witness.spent_utxos.entries()) {
    const label = `L2 transaction delete witness ${index.toString()}`;
    const key = exactHexBytes(item.key, `${label}.key`);
    const value = exactHexBytes(item.value, `${label}.value`);
    if (!key.equals(spendInputs[index]!)) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label} is not ordered and bound to its authenticated spend input.`,
      );
    }
    const membershipProof = mpfProofFromWitness({
      key,
      value,
      proof: item.membership_proof,
      label: `${label}.membership_proof`,
    });
    const deleteProof = mpfProofFromWitness({
      key,
      value,
      proof: item.delete_proof,
      label: `${label}.delete_proof`,
    });
    verifyProofRoot({
      proof: membershipProof,
      includingItem: true,
      expectedRoot: root,
      label: `${label}.membership_proof`,
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

  for (const [index, item] of witness.produced_utxos.entries()) {
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
    const nonMembershipProof = mpfProofFromWitness({
      key,
      value: undefined,
      proof: item.non_membership_proof,
      label: `${label}.non_membership_proof`,
    });
    const insertProof = mpfProofFromWitness({
      key,
      value,
      proof: item.insert_proof,
      label: `${label}.insert_proof`,
    });
    verifyProofRoot({
      proof: nonMembershipProof,
      includingItem: false,
      expectedRoot: root,
      label: `${label}.non_membership_proof`,
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
  return root;
};

export const detectL2TransactionTransitions = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly (L2TransactionTransitionEvidence & {
    readonly stepIndex: bigint;
  })[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const witness = await buildL2TransactionTransitionWitness({
      reconstruction,
      stepIndex: item.stepIndex,
      evidence: item,
    });
    if (!("L2TransactionTransition" in witness)) {
      throw transitionTraceError(
        "missingWitnessData",
        "L2 transaction evidence did not build an L2 transaction witness.",
      );
    }
    const replayedPostRoot = replayL2TransactionTransition(
      witness.L2TransactionTransition,
    );
    if (
      replayedPostRoot ===
      witness.L2TransactionTransition.trace_proof.value.post_utxos_root
    ) {
      continue;
    }
    detections.push(
      detection({
        reconstruction,
        kind: "invalidOneStepTransition",
        invariant: "l2_transaction_transition_matches_authenticated_replay",
        diagnostic: `L2 transaction trace step ${item.stepIndex.toString()} has authenticated ledger mutation evidence whose replay disagrees with the committed post-state root.`,
        fault: SDK.invalidOneStepTransitionFault(witness),
      }),
    );
  }
  return detections;
};
