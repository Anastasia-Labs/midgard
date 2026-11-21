import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import { type FieldOpening } from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import {
  type FaultProofFieldGrammarDeployment,
  routeFaultProofFieldGrammar,
} from "../field-grammar-certificate.route-fault-proof-field-grammar.js";
import { reuseOrCertifyFaultProofFieldCarriage } from "../field-opening.certify-fault-proof-field-carriage.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { RedeemerCanonicityContracts } from "./contracts.js";
import { redeemerDataScanState } from "./data-plan.js";
import {
  REDEEMER_CANONICITY_FIELD_INDEX,
  type RedeemerCanonicityEvidence,
} from "./family.js";
import {
  RedeemerCanonicityStep02DataDatumSchema,
  RedeemerCanonicityStep02DatumSchema,
  RedeemerCanonicityStep02RedeemerSchema,
  RedeemerCanonicityStep03DatumSchema,
} from "./schemas.js";

export const buildRedeemerCanonicityStep02 = async ({
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
  certificateUtxo: suppliedCertificateUtxo,
  certificateReferenceScriptUtxo,
  referenceScriptUtxo,
  fieldGrammar,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: RedeemerCanonicityEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  /** The field grammar certificate, for a field past the walked limit. */
  readonly fieldGrammar?: FaultProofFieldGrammarDeployment;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "redeemer-canonicity",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    witness_set_hash: string;
    redeemer_index: bigint;
  }>({
    threadUtxo,
    signer,
    schema: RedeemerCanonicityStep02DatumSchema as never,
    family: "redeemer-canonicity",
    stepIndex,
  });
  if (state.redeemer_index !== evidence.redeemerIndex)
    throw new Error(
      "redeemer-canonicity: opening coordinate differs from thread",
    );
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(witnessSetCompactCbor, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: REDEEMER_CANONICITY_FIELD_INDEX,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet: {
      addr_tx_wits_hash: Buffer.from(witnessSet.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(witnessSet.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(
        witnessSet.redeemerTxWitsHash,
      ).toString("hex"),
    },
    anchorWitnessSetHash: state.witness_set_hash,
    label: "redeemer-canonicity field opening",
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid,
      signer,
      planned,
      publisherAddress: signer.proofMaterialsAddress,
      label: "redeemer-canonicity field opening",
    }));
  if (isPrerequisiteTurn(carriageUtxos)) return carriageUtxos;
  const certificateUtxo =
    suppliedCertificateUtxo ??
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
          missingReferenceScript:
            "redeemer-canonicity: certified opening requires certificate reference script",
          chunkUtxos: carriageUtxos,
          compactCbor: nativeTxCompactCbor,
          witnessSetCompactCbor,
        })
      : undefined);
  if (isPrerequisiteTurn(certificateUtxo)) return certificateUtxo;
  const grammar = await routeFaultProofFieldGrammar({
    lucid,
    signer,
    plan: planned.plan,
    // An absent coordinate is closed by the field's count, not an item.
    read: evidence.absent ? "count" : "item",
    itemCount: planned.itemCount,
    source: {
      sourceKind: planned.sourceKind,
      compactCbor: nativeTxCompactCbor,
      witnessSetCompactCbor: witnessSetCompactCbor,
    },
    carriageUtxos: [
      ...carriageUtxos,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    fieldPreimageCertificatePolicyId:
      contracts.fieldPreimageCertificatePolicyId,
    fieldGrammar,
    label: "redeemer-canonicity field opening",
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "redeemer-canonicity",
    stepIndex,
  });
  const bound = {
    subject: evidence.subject,
    witness_set_hash: state.witness_set_hash,
    redeemer_index: evidence.redeemerIndex,
  };
  const plan = evidence.dataPlan;
  // A readable envelope opens the data step; any other item is decided here.
  // An absent coordinate reads no item: its canonical verdict goes straight
  // to the terminal.
  const nextStepIndex = plan === null ? 3 : 2;
  const nextDatum =
    plan === null
      ? Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: { bound, canonical: evidence.absent },
          } as never,
          RedeemerCanonicityStep03DatumSchema as never,
        )
      : Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: redeemerDataScanState(bound, plan),
          } as never,
          RedeemerCanonicityStep02DataDatumSchema as never,
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
      label: "redeemer-canonicity field opening",
    });
    requireOwnSpendPurpose(ctx, threadUtxo, "redeemer-canonicity step-02");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "redeemer-canonicity",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "redeemer-canonicity step-02 output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            opening,
            coordinate_absent: evidence.absent,
            grammar_ref_input_index: grammar.grammarRefInputIndex(
              ctx.referenceInputs,
            ),
          },
        ],
      } as never,
      RedeemerCanonicityStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: "redeemer-canonicity step-02",
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs: [
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
      ...grammar.referenceInputs,
    ],
  });
  if (outputIndex === undefined)
    throw new Error("redeemer-canonicity: step-02 layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
    carriageTier: planned.plan.tier,
    nextStage: plan === null ? ("step03" as const) : ("step02Data" as const),
  };
};
