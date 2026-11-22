/**
 * `missing-signature` step-02 submitter (offchain plan §4.2, §5 frontier 2).
 *
 * Opens body field 4 (`required_signers`) through the §8.8 door against the
 * thread-anchored `verified_tx_id` — the **first offchain consumer of field
 * 4** — and selects the accused signer hash by its fixed-28-byte-stride
 * ordinal. Body field, so the opening needs no witness-set pairing;
 * `field_item_at` aborts on an out-of-domain ordinal on-chain, so the same
 * bound is refused here first.
 *
 * A realistic field 4 is tiny (a handful of 28-byte hashes) and rides tier
 * 1, but the tier is chosen by the door's planner from the actual bytes,
 * never assumed: when the plan demands carriage, this submitter publishes
 * the chunks first and reads them back, identically to the decoding
 * family's step-03.
 */
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  MissingSignatureStep02Datum,
  MissingSignatureStep02SpendRedeemer,
  type MissingSignatureStep02State,
  MissingSignatureStep03Datum,
  type MissingSignatureStep03State,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type FaultProofTurn,
  isPrerequisiteTurn,
} from "../fault-proof-turn.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { completeStep, finishStep, fundStep } from "../step-completion.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import {
  type LocallyEvaluatedTransaction,
  type OutsideWorkflowSubmission,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import type { MissingSignatureContracts } from "./contracts.js";
import { planMissingSignatureRequiredSignersOpening } from "./evidence.js";
import {
  missingSignatureStepLabel,
  missingSignatureSubmitError,
  requireMissingSignatureReferenceScript,
  requireMissingSignatureStepState,
  requireMissingSignatureThreadUtxo,
} from "./submit-common.js";

const STEP_LABEL = missingSignatureStepLabel(1);

export type MissingSignatureStep02Build = {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  /** The accused required signer's hash, now pinned in thread state. */
  readonly missingRequiredSignerHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
};

export type SubmitMissingSignatureStep02Result =
  OutsideWorkflowSubmission<MissingSignatureStep02Build>;

export const buildMissingSignatureStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  requiredSignerHashes,
  nativeTxCompactCbor,
  badRequiredSignerHashIndex,
  publishCarriage = false,
  certificateUtxo,
  referenceScriptUtxo,
  materials,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingSignatureContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Complete positional required-signer list — field 4's §5.1 preimage. */
  readonly requiredSignerHashes: readonly string[];
  /** The accused transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly badRequiredSignerHashIndex: bigint;
  /** Force §8 tier-2 carriage publication (testing knob). */
  readonly publishCarriage?: boolean;
  /**
   * Pre-minted §8.6 certificate when the planner selects tier 3; otherwise
   * the one found on chain for this opening.
   */
  readonly certificateUtxo?: UTxO;
  /** §2.3: the published step-02 reference script (required; never inline). */
  readonly referenceScriptUtxo: UTxO;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}): Promise<FaultProofTurn<MissingSignatureStep02Build>> => {
  const { threadUtxo, threadToken } = await requireMissingSignatureThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 1,
    threadOutRef,
  });
  const state: MissingSignatureStep02State = requireMissingSignatureStepState({
    threadUtxo,
    signer,
    schema: MissingSignatureStep02Datum,
    stepIndex: 1,
  });

  // The validator's `field_item_at` aborts outside the domain; refuse the
  // same bound before paying for the attempt.
  const accusedHash =
    requiredSignerHashes[Number(badRequiredSignerHashIndex)]?.toLowerCase();
  if (badRequiredSignerHashIndex < 0n || accusedHash === undefined) {
    throw missingSignatureSubmitError(
      `accused ordinal ${badRequiredSignerHashIndex.toString()} is outside the ${requiredSignerHashes.length.toString()}-item required-signer list.`,
    );
  }

  // The §8.8 door: plan, publish whatever the tier demands, open.
  const planned = planMissingSignatureRequiredSignersOpening({
    anchorSourceKind: 0n,
    anchorTxId: state.verified_tx_id,
    nativeTxCompactCbor,
    requiredSignerHashes,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
  });
  const carriageUtxos = await publishFaultProofFieldCarriage({
    materials,
    lucid,
    signer,
    planned,
    publisherAddress: signer.proofMaterialsAddress,
    label: `${STEP_LABEL} required-signers`,
  });
  if (isPrerequisiteTurn(carriageUtxos)) return carriageUtxos;
  // A configured certificate, or the one found on chain for this opening.
  const certificate =
    certificateUtxo ??
    (await resolveFaultProofFieldPreimageCertificate({
      lucid,
      network: lucid.config().network!,
      planned,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    }));
  const fieldReferenceInputs = [
    ...(certificate === undefined ? [] : [certificate]),
    ...carriageUtxos,
  ];
  const referenceInputs = [
    ...fieldReferenceInputs,
    requireMissingSignatureReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[1].spendingScriptHash,
      stepIndex: 1,
    }),
  ];

  const funding = await fundStep(lucid, {
    signer,
    excluding: carriageUtxos,
  });
  const step03State: MissingSignatureStep03State = {
    missing_required_signer_hash: accusedHash,
    verified_tx_id: state.verified_tx_id,
    verified_witness_set_hash: state.verified_witness_set_hash,
  };
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step03State },
    MissingSignatureStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    const requiredSignersOpening = faultProofFieldOpening({
      planned,
      // §8.7 indices are into the complete reference-input set, including the
      // step's own reference script; resolving against carriage alone is only
      // accidentally correct for some out-ref orderings.
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${STEP_LABEL} required-signers`,
    });
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            required_signers_opening: requiredSignersOpening,
            bad_required_signer_hash_index: badRequiredSignerHashIndex,
          },
        ],
      },
      MissingSignatureStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const withInputs = funding.tx.collectFrom([threadUtxo], redeemer);
  const withReferences =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom(referenceInputs);
  const tx = withReferences.pay.ToContract(
    contracts.steps[2].spendingScriptAddress,
    { kind: "inline", value: step03Datum },
    threadAssets,
  );
  const signed = await completeStep(tx, funding);
  if (resolvedLayout === undefined) {
    throw missingSignatureSubmitError(
      "BuildTxWithRedeemer did not resolve the step-02 layout.",
    );
  }
  const transaction = finishStep(signed, {
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof missing-signature step-02",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[1].spendingScript,
      }),
    ],
  });

  return {
    transaction,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${transaction.txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    missingRequiredSignerHash: accusedHash,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
  };
};
