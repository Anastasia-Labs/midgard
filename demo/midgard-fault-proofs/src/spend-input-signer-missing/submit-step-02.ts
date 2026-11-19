import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import {
  type FieldOpening,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
  reuseOrCertifyFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import {
  SignerWitnessScanDatumSchema,
  SignerWitnessTerminalDatumSchema,
} from "../signer-witness-scan/schemas.js";
import {
  SIGNER_WITNESS_SCAN_STEP_INDEX,
  SIGNER_WITNESS_TERMINAL_STEP_INDEX,
} from "../signer-witness-scan/submit-scan.js";
import { completeStep, fundStep } from "../step-completion.js";
import { finishStep } from "../step-completion.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessWithdrawalValidatorCarriage } from "../witness-reference-scripts.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import { workflowReferenceScript } from "../workflow/transaction-boundary.js";
import type { SpendInputSignerMissingContracts } from "./contracts.js";
import { planSpendInputSignerInputOpening } from "./field-plans.js";
import {
  SpendInputSignerStep02DatumSchema,
  SpendInputSignerStep02RedeemerSchema,
} from "./schemas.js";
import {
  requireSpendInputSignerResolvedOutput,
  requireSpendInputSignerScanEvidence,
  type SpendInputSignerMissingEvidence,
} from "./spend-input-signer-missing.js";

/**
 * Step 02 resolves the bound spend input and classifies its credential the
 * way the validator does: a pub-key credential is handed to the shared
 * witness scan; a script credential, or a negative coordinate or one past the
 * field-0 count, goes straight to the shared terminal with the direct
 * verdict. The out-of-range
 * arm reads no prior-ledger membership, so it carries none.
 */
export const buildSpendInputSignerMissingStep02 = async ({
  lucid,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  referenceScriptUtxo,
  certificateReferenceScriptUtxo,
  membershipReferenceScriptUtxo,
  publishCarriage = false,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: SpendInputSignerMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly membershipReferenceScriptUtxo: UTxO;
  readonly publishCarriage?: boolean;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "spend-input-signer-missing",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    input_index: bigint;
    prior_root: string;
    witness_set_hash: string;
  }>({
    threadUtxo,
    signer,
    schema: SpendInputSignerStep02DatumSchema as never,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  if (
    state.input_index !== evidence.inputIndex ||
    state.prior_root !== evidence.priorRoot ||
    state.witness_set_hash !== evidence.witnessSetHashHex
  )
    throw new Error(
      "spend-input-signer-missing: authenticated bind state changed",
    );
  const outOfRange = evidence.route === "coordinate_out_of_range";
  const resolved = outOfRange
    ? undefined
    : requireSpendInputSignerResolvedOutput(evidence);
  if (resolved !== undefined && resolved.membershipProof === undefined)
    throw new Error(
      "spend-input-signer-missing: production predecessor membership object is absent",
    );
  const planned = planSpendInputSignerInputOpening({
    evidence,
    nativeTxCompactCbor,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
  });
  const carriageUtxos = await publishFaultProofFieldCarriage({
    ownOutputs,
    lucid,
    signer,
    planned,
    publisherAddress: signer.proofMaterialsAddress,
    label: "spend-input-signer-missing spend inputs",
  });
  if (isPrerequisiteTurn(carriageUtxos)) return carriageUtxos;
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await reuseOrCertifyFaultProofFieldCarriage({
          ownOutputs,
          lucid,
          network,
          signer,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          certificateMintingScript:
            contracts.fieldPreimageCertificateMintingScript,
          certificateReferenceScriptUtxo,
          missingReferenceScript:
            "spend-input-signer-missing: certified input opening requires certificate reference script",
          chunkUtxos: carriageUtxos,
          compactCbor: nativeTxCompactCbor,
          witnessSetCompactCbor,
        })
      : undefined;
  if (isPrerequisiteTurn(certificateUtxo)) return certificateUtxo;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  if (membershipReferenceScriptUtxo.scriptRef == null)
    throw new Error(
      "spend-input-signer-missing: predecessor membership reference script is absent",
    );
  const membershipScript = membershipReferenceScriptUtxo.scriptRef;
  const membershipAddress = phasMembershipRewardAddress(
    network,
    membershipScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriage({
    script: membershipScript,
    referenceUtxo: membershipReferenceScriptUtxo,
    label: "spend-input-signer-missing predecessor membership",
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    stepReference,
    ...(resolved === undefined ? [] : membershipCarriage.referenceInputs),
  ];
  // The witness-scan route writes the unopened scan state (`start`); every
  // direct route writes the verdict step 02 derives on chain
  // (`direct_verdict_v1`) straight to the terminal.
  const scan = evidence.route === "witness_scan";
  const nextStepIndex = scan
    ? SIGNER_WITNESS_SCAN_STEP_INDEX
    : SIGNER_WITNESS_TERMINAL_STEP_INDEX;
  const nextDatum = scan
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: evidence.subject,
            witness_set_hash: evidence.witnessSetHashHex,
            payment_credential:
              requireSpendInputSignerScanEvidence(evidence)
                .paymentCredentialHex,
            checkpoint_hash: "",
          },
        } as never,
        SignerWitnessScanDatumSchema as never,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: evidence.subject,
            signer_required: false,
            signer_present: false,
          },
        } as never,
        SignerWitnessTerminalDatumSchema as never,
      );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const opening: FieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: "spend-input-signer-missing spend inputs",
    });
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "spend-input-signer-missing step-02",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "spend-input-signer-missing step-02",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "spend-input-signer-missing step-02 output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            spend_inputs_opening: opening,
            // Past the field's count no membership is read; the carriage
            // slot is filled with an empty proof the validator never opens.
            descriptor_cbor: resolved?.descriptorCborHex ?? "",
            membership: {
              RedeemerCarriedMembership: {
                membership_proof: resolved?.membershipProof ?? [],
                membership_proof_script_redeemer_index:
                  resolved === undefined
                    ? 0n
                    : requireWithdrawalRedeemerIndex(
                        ctx,
                        membershipAddress,
                        "spend-input-signer-missing membership",
                      ),
              },
            },
          },
        ],
      } as never,
      SpendInputSignerStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const funding = await fundStep(lucid, {
    signer,
    excluding: carriageUtxos,
  });
  const withMembership = (tx: TxBuilder): TxBuilder =>
    resolved === undefined
      ? tx
      : membershipCarriage.attach(
          tx.withdraw(
            membershipAddress,
            0n,
            encodeRawPhasMembershipProofRedeemer({
              root: evidence.priorRoot,
              keyBytes: encodeMidgardSpendInputItem({
                txId: Buffer.from(resolved.transactionId, "hex"),
                outputIndex: resolved.outputIndex,
              }).toString("hex"),
              valueBytes: resolved.descriptorCborHex,
              membershipProofCbor: resolved.membershipProofCborHex,
            }),
          ),
        );
  const signed = await completeStep(
    withMembership(
      funding.tx
        .collectFrom([threadUtxo], redeemer)
        .readFrom(referenceInputs)
        .pay.ToContract(
          contracts.steps[nextStepIndex].spendingScriptAddress,
          { kind: "inline", value: nextDatum },
          {
            lovelace: threadUtxo.assets.lovelace ?? 0n,
            [threadToken.unit]: 1n,
          },
        ),
    ),
    funding,
  );
  if (outputIndex === undefined)
    throw new Error("spend-input-signer-missing: step-02 layout unresolved");
  const transaction = finishStep(signed, {
    referenceScripts: [
      workflowReferenceScript({
        role: "spend-input-signer-missing-step-02",
        utxo: stepReference,
        expectedScript: contracts.steps[1].spendingScript,
      }),
      ...(resolved === undefined
        ? []
        : [
            workflowReferenceScript({
              role: "spend-input-signer-missing-membership",
              utxo: membershipReferenceScriptUtxo,
              expectedScript: membershipScript,
            }),
          ]),
    ],
  });
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
    carriageTier: planned.plan.tier,
    route: evidence.route,
    stage: scan ? ("scanning" as const) : ("terminal" as const),
  };
};
