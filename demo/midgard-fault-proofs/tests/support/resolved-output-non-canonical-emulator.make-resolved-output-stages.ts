import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  type FieldOpening,
  type ForcedInclusionTxV1,
  forcedVerdictSubject,
  type Header,
  OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { submitCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import {
  type ResolvedOutputCoordinate,
  type ResolvedOutputEvidence,
  type ResolvedOutputForcedSource,
  ResolvedOutputStep01RedeemerSchema,
  ResolvedOutputStep02DatumSchema,
  ResolvedOutputStep02RedeemerSchema,
  ResolvedOutputStep03DatumSchema,
  ResolvedOutputStep04DatumSchema,
  ResolvedOutputStep04RedeemerSchema,
  ResolvedOutputStep05DatumSchema,
  ResolvedOutputStep05RedeemerSchema,
  submitResolvedOutputNonCanonicalCancel,
  submitResolvedOutputNonCanonicalStep01Accepted,
  submitResolvedOutputNonCanonicalStep01Forced,
  submitResolvedOutputNonCanonicalStep02,
  submitResolvedOutputNonCanonicalStep03,
  submitResolvedOutputNonCanonicalStep04,
  submitResolvedOutputNonCanonicalStep05,
} from "../../src/resolved-output-non-canonical/index.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import {
  FAMILY,
  network,
  type ResolvedOutputContext,
} from "./resolved-output-non-canonical-emulator.build-prior-ledger.js";
import { type CommittedBlock } from "./resolved-output-non-canonical-emulator.commit-block.js";
import {
  type Captured,
  forcedSourceOf,
} from "./resolved-output-non-canonical-emulator.resolved-output-evidence.js";
import { publishRemovalReferenceScripts } from "./submit-init-emulator-shared.js";

export const makeResolvedOutputStages = async (
  context: ResolvedOutputContext,
  block: CommittedBlock,
  onPublication?: (
    stepIndex: number,
    measurement: CompleteSignedTransactionMeasurement,
  ) => void,
) => {
  const { harness, contracts, steps, catalogue, category } = context;
  const references: UTxO[] = [];
  for (const [index, step] of steps.entries()) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: step.spendingScript,
      label: `${FAMILY}-${index.toString()}`,
    });
    references.push(published.utxo);
    onPublication?.(index, published.publicationMeasurement);
  }
  const certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: `${FAMILY}-certificate`,
    })
  ).utxo;
  const common = (threadOutRef: string, stepIndex: number) =>
    ({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      referenceScriptUtxo: references[stepIndex]!,
    }) as const;
  const capture = <T>(operation: () => Promise<T>) =>
    captureEmulatorSubmission(harness.emulator, operation);

  const init = async () =>
    await capture(() =>
      submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: block.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const threadOf = (
    initialized: Captured<Awaited<ReturnType<typeof init>>["result"]>,
  ) =>
    `${initialized.result.txHash}#${initialized.result.firstStepOutputIndex.toString()}`;

  const step01Accepted = async (
    initialized: Captured<Awaited<ReturnType<typeof init>>["result"]>,
    evidence: ResolvedOutputEvidence,
    txInclusion = block.accepted?.txInclusion,
  ) => {
    if (txInclusion === undefined) throw new Error("block is not accepted");
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: initialized.result.txHash,
        outputIndex: initialized.result.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    return await capture(() =>
      submitResolvedOutputNonCanonicalStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo,
        threadToken: {
          unit: initialized.result.computationThreadUnit,
          fraudulentHeaderHash: initialized.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: block.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  };

  const step01Forced = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
    forcedSource: ResolvedOutputForcedSource = forcedSourceOf(block),
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep01Forced({
        ...common(threadOutRef, 0),
        finding: evidence,
        forcedSource,
      }),
    );

  /**
   * Step 01 over the forced leaf with no off-chain classification: the
   * subject is whatever the leaf and `direction` say, and the claimed
   * coordinate and prior root are handed to the validator verbatim.
   */
  const step01ForcedRaw = async ({
    threadOutRef,
    coordinate,
    priorRoot,
    header = block.header,
    membership = forcedSourceOf(block).membership,
    direction = 1n,
  }: {
    readonly threadOutRef: string;
    readonly coordinate: ResolvedOutputCoordinate;
    readonly priorRoot: string;
    readonly header?: Header;
    readonly membership?: RootMembershipProof<
      OutputReference,
      ForcedInclusionTxV1
    >;
    readonly direction?: bigint;
  }) => {
    const verdict = membership.value.verdict;
    const subject = {
      ...forcedVerdictSubject({
        transactionId: membership.value.tx_id,
        sourceKey: membership.key,
        rejectionReason:
          verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason,
      }),
      direction,
    };
    return await continueRaw({
      threadOutRef,
      stepIndex: 0,
      nextStepIndex: 1,
      nextData: {
        subject,
        source_kind: BigInt(coordinate.sourceKind),
        input_index: BigInt(coordinate.inputIndex),
        prior_root: priorRoot,
      },
      nextDatumSchema: ResolvedOutputStep02DatumSchema,
      redeemerSchema: ResolvedOutputStep01RedeemerSchema,
      args: (input_index, output_index) => ({
        source: {
          ForcedSource: {
            input_index,
            output_index,
            header,
            membership,
            direction,
          },
        },
        source_kind: BigInt(coordinate.sourceKind),
        input_index: BigInt(coordinate.inputIndex),
      }),
    });
  };

  const step02 = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
    options: {
      readonly certificateUtxo?: UTxO;
      readonly publishedCarriageUtxos?: readonly UTxO[];
      readonly compactCborHex?: string;
    } = {},
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep02({
        ...common(threadOutRef, 1),
        evidence,
        nativeTxCompactCbor: options.compactCborHex ?? block.compactCborHex,
        witnessSetCompactCbor: block.witnessSetCompactCborHex,
        certificateReferenceScriptUtxo: certificateReference,
        ...(options.certificateUtxo === undefined
          ? {}
          : { certificateUtxo: options.certificateUtxo }),
        ...(options.publishedCarriageUtxos === undefined
          ? {}
          : { publishedCarriageUtxos: options.publishedCarriageUtxos }),
      }),
    );

  /**
   * Step 02 with the honest Certified opening of the evidence's field, except
   * that the redeemer's certificate index names `otherCertificate` (a genuine
   * certificate for another field of the same transaction), which is also a
   * reference input. The builder's own certificate lookup is bypassed so the
   * field-opening door refuses the substitution itself. Returns the honest
   * carriage so the honest step 02 can reuse it.
   */
  const step02Raw = async ({
    threadOutRef,
    evidence,
    otherCertificate,
  }: {
    readonly threadOutRef: string;
    readonly evidence: ResolvedOutputEvidence;
    readonly otherCertificate: UTxO;
  }) => {
    const fieldIndex = evidence.coordinate.sourceKind;
    const material = (
      block.forced === undefined
        ? deriveMidgardNativeTxFaultEvidenceMaterial
        : deriveMidgardForcedTxFaultEvidenceMaterial
    )(block.canonicalCbor);
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: block.forced === undefined ? 0n : 1n,
      fieldIndex,
      anchorTxId: block.nativeTxId,
      nativeTxCompactCbor: block.compactCborHex,
      itemCbors: decodeMidgardFieldPreimage(
        material.fieldPreimages[fieldIndex]!,
      ),
      owner: harness.proverSigner.paymentKeyHash,
      publish: false,
      label: `${FAMILY} raw field opening`,
    });
    expect(planned.plan.tier).toBe("Certified");
    harness.proverSigner.selectWallet(harness.proverLucid);
    const chunkUtxos = await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: `${FAMILY} raw field opening`,
    });
    const { certificateUtxo } = await certifyFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      network,
      signer: harness.proverSigner,
      planned,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      certificateMintingScript: contracts.fieldPreimageCertificateMintingScript,
      certificateReferenceScriptUtxo: certificateReference,
      chunkUtxos,
      compactCbor: block.compactCborHex,
      witnessSetCompactCbor: block.witnessSetCompactCborHex,
    });
    const stepReference = references[1]!;
    const referenceInputs = [
      ...chunkUtxos,
      stepReference,
      certificateUtxo,
      otherCertificate,
    ];
    const honest = faultProofFieldOpening({
      planned,
      referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${FAMILY} raw field opening`,
    });
    // The ledger orders reference inputs by `(txHash, outputIndex)`.
    const otherIndex = [...referenceInputs]
      .sort((left, right) =>
        left.txHash < right.txHash
          ? -1
          : left.txHash > right.txHash
            ? 1
            : left.outputIndex - right.outputIndex,
      )
      .findIndex(
        (utxo) =>
          utxo.txHash === otherCertificate.txHash &&
          utxo.outputIndex === otherCertificate.outputIndex,
      );
    expect(otherIndex).toBeGreaterThanOrEqual(0);
    if (!("BodyFieldOpening" in honest))
      throw new Error("input fields open through BodyFieldOpening");
    const carriage = honest.BodyFieldOpening.carriage;
    if (!("Certified" in carriage))
      throw new Error("raw field opening expected Certified carriage");
    const opening: FieldOpening = {
      BodyFieldOpening: {
        ...honest.BodyFieldOpening,
        carriage: {
          Certified: {
            ...carriage.Certified,
            cert_ref_input_index: BigInt(otherIndex),
          },
        },
      },
    };
    const submitted = await continueRaw({
      threadOutRef,
      stepIndex: 1,
      nextStepIndex: 2,
      nextData: {
        subject: evidence.subject,
        prior_root: evidence.resolved.priorRoot,
        out_ref: {
          transactionId: evidence.resolved.transactionId,
          outputIndex: BigInt(evidence.resolved.outputIndex),
        },
      },
      nextDatumSchema: ResolvedOutputStep03DatumSchema,
      redeemerSchema: ResolvedOutputStep02RedeemerSchema,
      args: (input_index, output_index) => ({
        input_index,
        output_index,
        opening,
      }),
      carriageUtxos: chunkUtxos,
      extraReferenceInputs: [certificateUtxo, otherCertificate],
    });
    return { ...submitted, chunkUtxos, certificateUtxo };
  };

  const step03 = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep03({
        ...common(threadOutRef, 2),
        network,
        evidence,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );

  const step04 = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep04({
        ...common(threadOutRef, 3),
        evidence,
      }),
    );

  /** Drives the self-loop from `threadOutRef` until the thread leaves step 04. */
  const reconstruct = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
    onTransition?: (
      captured: Captured<Awaited<ReturnType<typeof step04>>["result"]>,
      index: number,
    ) => void,
  ) => {
    let current = threadOutRef;
    let transitions = 0;
    for (;;) {
      const captured = await step04(current, evidence);
      onTransition?.(captured, transitions);
      transitions += 1;
      current = captured.result.nextThreadOutRef;
      if (captured.result.terminal) {
        return { threadOutRef: current, transitions, final: captured.result };
      }
    }
  };

  /** Step 04 with the action and successor state handed to the validator verbatim. */
  const step04Raw = async ({
    threadOutRef,
    evidence,
    action,
    nextData,
    nextStepIndex,
  }: {
    readonly threadOutRef: string;
    readonly evidence: ResolvedOutputEvidence;
    readonly action: unknown;
    /** Successor `ReconstructionV1` or `CanonicalVerdictV1` data. */
    readonly nextData: Record<string, unknown>;
    readonly nextStepIndex: 3 | 4;
  }) =>
    await continueRaw({
      threadOutRef,
      stepIndex: 3,
      nextStepIndex,
      nextData: { subject: evidence.subject, ...nextData },
      nextDatumSchema:
        nextStepIndex === 4
          ? ResolvedOutputStep05DatumSchema
          : ResolvedOutputStep04DatumSchema,
      redeemerSchema: ResolvedOutputStep04RedeemerSchema,
      args: (input_index, output_index) => ({
        input_index,
        output_index,
        action,
      }),
    });

  const step05 = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep05({
        ...common(threadOutRef, 4),
        evidence,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );

  /** Step 05 without the builder's off-chain contradiction check. */
  const step05Raw = async (threadOutRef: string) => {
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      family: FAMILY,
      stepIndex: 4,
      threadOutRef,
    });
    return await submitLinearFaultFinalize({
      lucid: harness.proverLucid,
      family: FAMILY,
      stepIndex: 4,
      step: contracts.steps[4],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer: harness.proverSigner,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: ResolvedOutputStep05RedeemerSchema,
      buildFamilyArgs: (layout) => ({
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: references[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };

  const continueRaw = async ({
    threadOutRef,
    stepIndex,
    nextStepIndex,
    nextData,
    nextDatumSchema,
    redeemerSchema,
    args,
    carriageUtxos = [],
    extraReferenceInputs = [],
  }: {
    readonly threadOutRef: string;
    readonly stepIndex: number;
    readonly nextStepIndex: number;
    readonly nextData: unknown;
    readonly nextDatumSchema: unknown;
    readonly redeemerSchema: unknown;
    readonly args: (
      inputIndex: bigint,
      outputIndex: bigint,
    ) => Record<string, unknown>;
    readonly carriageUtxos?: readonly UTxO[];
    readonly extraReferenceInputs?: readonly UTxO[];
  }) => {
    const lucid = harness.proverLucid;
    const signer = harness.proverSigner;
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid,
      contracts,
      categoryId: category.categoryId,
      family: FAMILY,
      stepIndex,
      threadOutRef,
    });
    const role = `${FAMILY} raw step-0${(stepIndex + 1).toString()}`;
    const nextDatum = Data.to(
      { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
      nextDatumSchema as never,
    );
    const nextAddress = contracts.steps[nextStepIndex]!.spendingScriptAddress;
    const outputMatches = computationThreadOutputPredicate({
      address: nextAddress,
      datum: nextDatum,
      unit: threadToken.unit,
    });
    let outputIndex: bigint | undefined;
    const redeemer = ((ctx) => {
      requireOwnSpendPurpose(ctx, threadUtxo, role);
      const inputIndex = requireInputIndex(ctx, threadUtxo, role);
      outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, role);
      return Data.to(
        { Continue: [args(inputIndex, outputIndex)] } as never,
        redeemerSchema as never,
      );
    }) satisfies BuildTxWithRedeemer;
    signer.selectWallet(lucid);
    const txHash = await submitLinearFaultContinue({
      lucid,
      signerPaymentKeyHash: signer.paymentKeyHash,
      threadUtxo,
      threadUnit: threadToken.unit,
      stepReference: requireLinearFaultReferenceScript({
        utxo: references[stepIndex]!,
        expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
        family: FAMILY,
        stepIndex,
      }),
      stepScript: contracts.steps[stepIndex]!.spendingScript,
      stepRole: role,
      nextAddress,
      nextDatum,
      redeemer,
      carriageUtxos,
      extraReferenceInputs,
      awaitConfirmation: true,
    });
    if (outputIndex === undefined)
      throw new Error(`${role}: layout unresolved`);
    return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
  };

  const cancel = async (threadOutRef: string, stepIndex: number) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalCancel({
        ...common(threadOutRef, stepIndex),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );

  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    const removal = await capture(() =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "resolvedOutputNonCanonical",
        fraudulentHeaderHash: block.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.fraudCategoryId).toBe("00000026");
    return removal;
  };

  /**
   * A field-preimage certificate for another field of the same transaction,
   * minted the way step 02 mints its own, so a certificate seam substitution
   * reaches the validator with a genuine certificate for the wrong field.
   */
  const certifyField = async (fieldIndex: 0 | 1) => {
    const material = (
      block.forced === undefined
        ? deriveMidgardNativeTxFaultEvidenceMaterial
        : deriveMidgardForcedTxFaultEvidenceMaterial
    )(block.canonicalCbor);
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: block.forced === undefined ? 0n : 1n,
      fieldIndex,
      anchorTxId: block.nativeTxId,
      nativeTxCompactCbor: block.compactCborHex,
      itemCbors: decodeMidgardFieldPreimage(
        material.fieldPreimages[fieldIndex]!,
      ),
      owner: harness.proverSigner.paymentKeyHash,
      publish: false,
      label: `${FAMILY} other-field opening`,
    });
    expect(planned.plan.tier).toBe("Certified");
    harness.proverSigner.selectWallet(harness.proverLucid);
    const chunkUtxos = await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: `${FAMILY} other-field opening`,
    });
    return (
      await certifyFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        certificateMintingScript:
          contracts.fieldPreimageCertificateMintingScript,
        certificateReferenceScriptUtxo: certificateReference,
        chunkUtxos,
        compactCbor: block.compactCborHex,
        witnessSetCompactCbor: block.witnessSetCompactCborHex,
      })
    ).certificateUtxo;
  };

  /** The live step-04 checkpoint a fresh process would resume from. */
  const readReconstruction = async (threadOutRef: string) => {
    const [txHash, index] = threadOutRef.split("#");
    const [utxo] = await harness.proverLucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(index) },
    ]);
    if (utxo?.datum == null) throw new Error("step-04 checkpoint absent");
    return (
      Data.from(utxo.datum, ResolvedOutputStep04DatumSchema as never) as {
        data: Data.Static<typeof ResolvedOutputStep04DatumSchema>["data"];
      }
    ).data!;
  };

  return {
    references,
    certificateReference,
    init,
    threadOf,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    step03,
    step04,
    step04Raw,
    reconstruct,
    step05,
    step05Raw,
    cancel,
    remove,
    certifyField,
    readReconstruction,
  };
};
