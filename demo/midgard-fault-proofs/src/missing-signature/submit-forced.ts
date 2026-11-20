/** Restartable forced direction: each action reads its current on-chain datum. */
import {
  forcedLeafMembershipV1,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  MissingSignatureForcedSignerDatum,
  MissingSignatureForcedSignerSpendRedeemer,
  MissingSignatureForcedStepDatum,
  MissingSignatureForcedStepSpendRedeemer,
  MissingSignatureForcedWitnessDatum,
  MissingSignatureForcedWitnessSpendRedeemer,
  MissingSignatureStep01SpendRedeemer,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { buildLinearFaultCancel } from "../linear-fault-cancel.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import {
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
} from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { MissingSignatureContracts } from "./contracts.js";
import {
  planMissingSignatureAddressWitnessesOpening,
  planMissingSignatureRequiredSignersOpening,
} from "./evidence.js";
import {
  missingSignatureWrongfulRejectionWitnessIndex,
  type PreparedMissingSignatureWrongfulRejection,
} from "./wrongful-rejection.js";

export type SubmitMissingSignatureForcedParams = Readonly<{
  lucid: LucidEvolution;
  contracts: MissingSignatureContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  prepared: PreparedMissingSignatureWrongfulRejection;
  referenceScriptUtxo: UTxO;
  certificateUtxo?: UTxO;
  publishCarriage?: boolean;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  /** Our carriage claims; a fresh in-memory journal by default. */
  ownOutputs?: OwnOutputJournal;
}>;

export const buildMissingSignatureForcedAction = async (
  params: SubmitMissingSignatureForcedParams,
) => {
  const {
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    prepared,
    referenceScriptUtxo,
  } = params;
  const ownOutputs = params.ownOutputs ?? createOwnOutputJournal();
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "thread"),
    label: "missing-signature forced thread",
  });
  const positions = [
    ["step01", contracts.steps[0]],
    ["forcedStep", contracts.forcedStep],
    ["forcedSigner", contracts.forcedSigner],
    ["forcedWitness", contracts.forcedWitness],
  ] as const;
  const position = positions.find(
    ([, step]) => step.spendingScriptAddress === threadUtxo.address,
  );
  if (position === undefined)
    throw new Error("missingSignature: unknown forced thread position");
  const [stage, step] = position;
  if (
    referenceScriptUtxo.scriptRef == null ||
    validatorToScriptHash(referenceScriptUtxo.scriptRef) !==
      step.spendingScriptHash
  )
    throw new Error("missingSignature: wrong step reference script");
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: "missing-signature",
  });
  if (threadToken.fraudulentHeaderHash !== prepared.headerHash)
    throw new Error("missingSignature: evidence targets another header");
  if (threadUtxo.datum == null)
    throw new Error("missingSignature: missing thread datum");
  const witnessIndex = missingSignatureWrongfulRejectionWitnessIndex(
    prepared.evidence,
  );
  if (witnessIndex === null)
    throw new Error("missingSignature: honest rejection");
  signer.selectWallet(lucid);
  const bound = {
    verified_tx_id: prepared.transactionId,
    verified_witness_set_hash: prepared.verifiedWitnessSetHash,
    forced_source_key: prepared.evidence.subject.source_key,
    signer_index: prepared.evidence.signerIndex,
  };
  const witnessState = {
    ...bound,
    required_signer_hash:
      witnessIndex === -1n
        ? null
        : prepared.evidence.requiredSignerHashes[Number(bound.signer_index)]!,
  };
  if (stage === "step01") requireInitialStepDatum({ threadUtxo, signer });
  else {
    const schema =
      stage === "forcedStep"
        ? MissingSignatureForcedStepDatum
        : stage === "forcedSigner"
          ? MissingSignatureForcedSignerDatum
          : MissingSignatureForcedWitnessDatum;
    const expected = Data.to(
      {
        fraud_prover: signer.paymentKeyHash,
        data:
          stage === "forcedStep"
            ? 1n
            : stage === "forcedSigner"
              ? bound
              : witnessState,
      } as never,
      schema as never,
    );
    if (
      Data.to(Data.from(threadUtxo.datum, schema as never), schema as never) !==
      expected
    )
      throw new Error(
        "missingSignature: thread state differs from authenticated evidence",
      );
  }
  const planned =
    stage === "forcedSigner"
      ? planMissingSignatureRequiredSignersOpening({
          anchorSourceKind: 1n,
          anchorTxId: bound.verified_tx_id,
          nativeTxCompactCbor: prepared.nativeTxCompactCbor,
          requiredSignerHashes: prepared.evidence.requiredSignerHashes,
          owner: signer.paymentKeyHash,
          publish: params.publishCarriage,
        })
      : stage === "forcedWitness" && witnessState.required_signer_hash !== null
        ? planMissingSignatureAddressWitnessesOpening({
            anchorSourceKind: 1n,
            anchorTxId: bound.verified_tx_id,
            nativeTxCompactCbor: prepared.nativeTxCompactCbor,
            addrTxWits: prepared.evidence.addrTxWits,
            witnessSet: prepared.witnessSetCompact,
            anchorWitnessSetHash: bound.verified_witness_set_hash,
            owner: signer.paymentKeyHash,
            publish: params.publishCarriage,
          })
        : undefined;
  const carriageUtxos =
    planned === undefined
      ? []
      : await publishFaultProofFieldCarriage({
          ownOutputs,
          lucid,
          signer,
          planned,
          publisherAddress: signer.proofMaterialsAddress,
          label: "missing-signature forced field",
        });
  if (isPrerequisiteTurn(carriageUtxos)) return carriageUtxos;
  const certificateUtxo =
    params.certificateUtxo ??
    (planned === undefined
      ? undefined
      : await resolveFaultProofFieldPreimageCertificate({
          ownOutputs,
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        }));
  const extraReferenceInputs =
    certificateUtxo === undefined ? [] : [certificateUtxo];
  const fieldOpening = (referenceInputs: readonly UTxO[]) =>
    planned === undefined
      ? null
      : faultProofFieldOpening({
          planned,
          referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: "missing-signature forced field",
        });
  if (stage === "forcedWitness") {
    const result = await buildLinearFaultFinalize({
      lucid,
      family: "missing-signature forced",
      stepIndex: 3,
      step,
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: MissingSignatureForcedWitnessSpendRedeemer,
      buildFamilyArgs: (
        { inputIndex, outputIndex, fraudProofMintRedeemerIndex },
        ctx,
      ) => ({
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
        addr_tx_wits_opening: fieldOpening(ctx.referenceInputs),
        witness_index: witnessIndex === -1n ? 0n : witnessIndex,
      }),
      referenceScriptUtxo,
      carriageUtxos,
      extraReferenceInputs,
      witnessReferenceScripts: params.witnessReferenceScripts,
    });
    return { kind: "proven" as const, ...result };
  }
  const next =
    stage === "step01"
      ? contracts.forcedStep
      : stage === "forcedStep"
        ? contracts.forcedSigner
        : contracts.forcedWitness;
  const nextSchema =
    stage === "step01"
      ? MissingSignatureForcedStepDatum
      : stage === "forcedStep"
        ? MissingSignatureForcedSignerDatum
        : MissingSignatureForcedWitnessDatum;
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data:
        stage === "step01" ? 1n : stage === "forcedStep" ? bound : witnessState,
    } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: next.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "missing-signature forced");
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "missing-signature forced output",
    );
    const layout = {
      input_index: requireInputIndex(
        ctx,
        threadUtxo,
        "missing-signature forced",
      ),
      output_index: outputIndex,
    };
    if (stage === "step01")
      return Data.to(
        { ForcedDispatch: layout },
        MissingSignatureStep01SpendRedeemer,
      );
    if (stage === "forcedStep")
      return Data.to(
        {
          Continue: [
            {
              ...layout,
              header: prepared.forcedSource.header,
              membership: forcedLeafMembershipV1(
                prepared.forcedSource.membership,
              ),
            },
          ],
        },
        MissingSignatureForcedStepSpendRedeemer,
      );
    return Data.to(
      {
        Continue: [
          {
            ...layout,
            required_signers_opening: fieldOpening(ctx.referenceInputs)!,
          },
        ],
      },
      MissingSignatureForcedSignerSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: referenceScriptUtxo,
    stepScript: step.spendingScript,
    stepRole: `missing-signature ${stage}`,
    nextAddress: next.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs,
  });
  if (outputIndex === undefined)
    throw new Error("missingSignature: unresolved output layout");
  return {
    kind: "advanced" as const,
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex}`,
  };
};

export const buildMissingSignatureForcedCancel = (
  params: Omit<
    Parameters<typeof buildLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: MissingSignatureContracts },
) =>
  buildLinearFaultCancel({
    ...params,
    family: "missing-signature",
    steps: [
      ...params.contracts.steps,
      params.contracts.forcedStep,
      params.contracts.forcedSigner,
      params.contracts.forcedWitness,
    ],
    computationThread: params.contracts.computationThread,
  });
