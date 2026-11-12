import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  type FieldOpening,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type FaultProofFieldGrammarDeployment,
  type FaultProofFieldGrammarRoute,
  routeFaultProofFieldGrammar,
  WALKED_FIELD_GRAMMAR_ROUTE,
} from "../field-grammar-certificate.js";
import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
  reuseOrCertifyFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContracts,
} from "./contracts.js";
import {
  outputReferenceScriptControlData,
  type OutputReferenceScriptDecodingEvidence,
} from "./output-reference-script-decoding.js";
import {
  OutputReferenceStep02DatumSchema,
  OutputReferenceStep02RedeemerSchema,
  OutputReferenceStep03DatumSchema,
} from "./schemas.js";

/**
 * Step 02 over an already planned, published and (for tier 3) certified
 * field-2 carriage. The caller chooses the plan's anchor, so a lifecycle
 * suite can name another transaction's genuine certificate and observe the
 * opening door refuse the anchor on chain. Production callers use the
 * classified entry point below, which plans the opening for the bound
 * subject itself.
 */
export const submitOutputReferenceScriptDecodingStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  planned,
  carriageUtxos,
  certificateUtxo,
  grammar = WALKED_FIELD_GRAMMAR_ROUTE,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: OutputReferenceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly planned: ReturnType<typeof planFaultProofFieldOpening>;
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  /** How step 02 reads the field; walked by default. */
  readonly grammar?: FaultProofFieldGrammarRoute;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepState<{
    subject: typeof evidence.subject;
    output_index: bigint;
    accused_class: bigint;
  }>({
    threadUtxo,
    signer,
    schema: OutputReferenceStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    bound.subject.transaction_id !== evidence.subject.transaction_id ||
    bound.output_index !== BigInt(evidence.outputIndex)
  )
    throw new Error(`${FAMILY}: retained output differs from bound subject`);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bound,
        item_length: BigInt(evidence.outputLength),
        item_hash: evidence.outputHashHex,
        chunk_hashes: evidence.outputChunkHashes,
        control: outputReferenceScriptControlData(
          evidence.outputScanControls[0]!,
        ),
        outcome: 0n,
      },
    } as never,
    OutputReferenceStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const opening: FieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${FAMILY} field 2`,
    });
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step02`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step02`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step02 output`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            opening,
            grammar_ref_input_index: grammar.grammarRefInputIndex(
              ctx.referenceInputs,
            ),
          },
        ],
      } as never,
      OutputReferenceStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: `${FAMILY} step02`,
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs: [
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
      ...grammar.referenceInputs,
    ],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: step02 layout unresolved`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    carriageTier: planned.plan.tier,
    carriageUtxos,
    certificateUtxo,
  };
};

export const submitOutputReferenceScriptDecodingStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo: suppliedCertificate,
  certificateReferenceScriptUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  certificatePreSubmitBoundary,
  fieldGrammar,
  preSubmitBoundary,
  awaitConfirmation = true,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: OutputReferenceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundary;
  /** The field grammar certificate, for a field past the walked limit. */
  readonly fieldGrammar?: FaultProofFieldGrammarDeployment;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.outputFieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: 2,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${FAMILY} field 2`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${FAMILY} field 2`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  // A certificate already on chain (the workflow's certify action ran) is
  // reused; only an absent one is minted here.
  const certificateUtxo =
    suppliedCertificate ??
    (planned.plan.tier === "Certified"
      ? await reuseOrCertifyFaultProofFieldCarriage({
          ownOutputs,
          lucid,
          network: lucid.config().network!,
          signer,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          certificateMintingScript:
            contracts.fieldPreimageCertificateMintingScript,
          certificateReferenceScriptUtxo,
          missingReferenceScript: `${FAMILY}: certified opening requires certificate reference`,
          chunkUtxos: carriageUtxos,
          compactCbor: nativeTxCompactCbor,
          witnessSetCompactCbor,
          preSubmitBoundary: certificatePreSubmitBoundary,
        })
      : undefined);
  const grammar = await routeFaultProofFieldGrammar({
    lucid,
    signer,
    plan: planned.plan,
    read: "item",
    itemCount: items.length,
    source: {
      sourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
      compactCbor: nativeTxCompactCbor,
      witnessSetCompactCbor,
    },
    carriageUtxos: [
      ...carriageUtxos,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    fieldPreimageCertificatePolicyId:
      contracts.fieldPreimageCertificatePolicyId,
    fieldGrammar,
    label: `${FAMILY} field 2`,
  });
  return await submitOutputReferenceScriptDecodingStep02Raw({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    evidence,
    planned,
    carriageUtxos,
    certificateUtxo,
    grammar,
    referenceScriptUtxo,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
