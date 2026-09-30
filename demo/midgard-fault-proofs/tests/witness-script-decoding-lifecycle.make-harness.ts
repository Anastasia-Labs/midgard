import {
  encodeMidgardForcedTxCompact as forcedCompact,
  materializeMidgardForcedTxFromCanonical as forcedView,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import type { SubmitStep01TxInclusion } from "../src/step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  applyWitnessScriptDecodingScripts,
  planWitnessScriptDecodingStep03Transition,
  submitWitnessScriptDecodingCancel,
  submitWitnessScriptDecodingInit,
  submitWitnessScriptDecodingStep01Accepted,
  submitWitnessScriptDecodingStep01Forced,
  submitWitnessScriptDecodingStep02,
  submitWitnessScriptDecodingStep03,
  submitWitnessScriptDecodingStep04,
  WITNESS_SCRIPT_DECODING_BLUEPRINT_TITLES,
  witnessScriptDecodingCheckpoint,
  type WitnessScriptDecodingContracts,
  type WitnessScriptDecodingEvidence,
} from "../src/witness-script-decoding/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  ADJACENT_FIELD_BYTES,
  overBoundFieldCarriagePlan,
  scriptWitnessField,
  submitWitnessScriptDecodingStep01ForcedRaw,
  submitWitnessScriptDecodingStep02Raw,
  submitWitnessScriptDecodingStep03Raw,
  submitWitnessScriptDecodingStep04Raw,
} from "./support/witness-script-decoding-raw.js";
import {
  CANCELLABLE_STEPS,
  CATEGORY_ID,
  coverage,
  record,
  type Shape,
} from "./witness-script-decoding-lifecycle.authentication-seams.js";

export let publicationsRecorded = false;

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

export const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realWitnessScriptDecoding: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const addressData = await Effect.runPromise(
    SDK.addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(
      Effect.map((address) => Data.from(Data.to(address, SDK.AddressData))),
    ),
  );
  const registered =
    harness.contracts.fraudProofContracts.witnessScriptDecoding;
  const category = harness.catalogue.categories.witnessScriptDecoding;
  if (category === undefined) throw new Error("witness category absent");
  expect(category.categoryId).toBe(CATEGORY_ID);
  expectRegisteredChainParity({
    registered,
    applied: applyWitnessScriptDecodingScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    Object.values(WITNESS_SCRIPT_DECODING_BLUEPRINT_TITLES),
  );
  const contracts: WitnessScriptDecodingContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const categoryId = category.categoryId;
  const references: UTxO[] = [];
  let certificateReference: UTxO | undefined;
  let removalReferenceScripts:
    | Awaited<ReturnType<typeof publishRemovalReferenceScripts>>
    | undefined;
  const publishReferences = async () => {
    if (references.length > 0) return;
    for (const [index, step] of steps.entries()) {
      const published = await captureEmulatorSubmission(harness.emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `witness-script-decoding step ${(index + 1).toString()}`,
        }),
      );
      if (!publicationsRecorded) {
        record(
          `publish-step0${(index + 1).toString()}`,
          "fully applied testnet validator",
          published.measurement,
          "publication",
        );
      }
      references.push(published.result.utxo);
    }
    publicationsRecorded = true;
    certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "witness-script-decoding field certificate",
      })
    ).utxo;
    // Deploy removal references before the journey: the maximum depth scan
    // advances emulator time beyond Lucid's default publication expiry.
    removalReferenceScripts = await publishRemovalReferenceScripts({
      lucid,
      contracts: harness.contracts,
    });
  };
  const ref = (index: 0 | 1 | 2 | 3): UTxO => {
    const utxo = references[index];
    if (utxo === undefined) throw new Error("references not published");
    return utxo;
  };
  const requireCertificateReference = (): UTxO => {
    if (certificateReference === undefined)
      throw new Error("certificate reference not published");
    return certificateReference;
  };

  const measured = async <T>(
    name: string | null,
    shape: string,
    operation: () => Promise<T>,
  ): Promise<{
    result: T;
    measurement: CompleteSignedTransactionMeasurement;
  }> => {
    const captured = await captureEmulatorSubmission(
      harness.emulator,
      operation,
    );
    if (name !== null) record(name, shape, captured.measurement);
    return { result: captured.result, measurement: captured.measurement };
  };

  /** One committed block per scenario: the subject plus decoy transactions. */
  const acceptedBlock = async (
    subject: Shape,
    additional: readonly Shape[] = [],
  ) => {
    const startTime = BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    );
    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime,
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: { kind: "normal", nativeTx: subject.nativeTx },
      additionalTransactions: additional.map((shape) => shape.nativeTx),
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header: block.header,
    });
    await publishReferences();
    const inclusionOf = (shape: Shape): SubmitStep01TxInclusion => {
      const inclusion = block.txInclusions.get(shape.txId);
      if (inclusion === undefined) throw new Error("inclusion absent");
      return inclusion;
    };
    return { block, setup, inclusionOf };
  };

  const forcedBlock = async (
    shape: Shape,
    reason: SDK.RejectionReason,
    orderKey: SDK.OutputReference = {
      transactionId: "cd".repeat(32),
      outputIndex: 0n,
    },
  ) => {
    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      ),
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: {
        kind: "forced",
        nativeTx: shape.nativeTx,
        orderKey,
        verdict: { ForcedTxInvalid: { reason } },
      },
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header: block.header,
    });
    await publishReferences();
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey: { ForcedTransactionEventKey: { tx_order_id: orderKey } },
    });
    return { block, setup, orderKey, membership };
  };

  const init = async (
    fraudulentBlockOutRef: string,
    name: string | null,
    shape: string,
  ) =>
    (
      await measured(name, shape, () =>
        submitWitnessScriptDecodingInit({
          lucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: harness.catalogue.root,
          },
          signer,
          fraudulentBlockOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      )
    ).result.nextThreadOutRef;

  const cancel = async (
    threadOutRef: string,
    stepIndex: 0 | 1 | 2 | 3,
    name: string | null = null,
    shape = "cancel",
  ) => {
    await measured(name, shape, () =>
      submitWitnessScriptDecodingCancel({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        referenceScriptUtxo: ref(stepIndex),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    coverage.cancelled(CANCELLABLE_STEPS[stepIndex]);
  };

  const step01Accepted = async (
    threadOutRef: string,
    inclusion: SubmitStep01TxInclusion,
    stateQueueBlockOutRef: string,
    scriptIndex: bigint,
    name: string | null = null,
    shape = "",
  ) =>
    (
      await measured(name, shape, () =>
        submitWitnessScriptDecodingStep01Accepted({
          lucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          categoryId,
          signer,
          threadOutRef,
          stateQueueBlockOutRef,
          txInclusion: inclusion,
          scriptIndex,
          referenceScriptUtxo: ref(0),
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      )
    ).result.nextThreadOutRef;

  const step01Forced = async (
    threadOutRef: string,
    forced: Awaited<ReturnType<typeof forcedBlock>>,
    witnessSetHash: string,
    scriptIndex: bigint,
    name: string | null = null,
    shape = "",
  ) =>
    (
      await measured(name, shape, () =>
        submitWitnessScriptDecodingStep01Forced({
          lucid,
          contracts,
          categoryId,
          signer,
          threadOutRef,
          header: forced.block.header,
          membership: forced.membership,
          direction: 1n,
          witnessSetHash,
          scriptIndex,
          referenceScriptUtxo: ref(0),
        }),
      )
    ).result.nextThreadOutRef;

  const step01ForcedRaw = (
    threadOutRef: string,
    forced: Awaited<ReturnType<typeof forcedBlock>>,
    evidence: WitnessScriptDecodingEvidence,
    patch: Partial<
      Parameters<typeof submitWitnessScriptDecodingStep01ForcedRaw>[0]
    > = {},
  ) =>
    submitWitnessScriptDecodingStep01ForcedRaw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      header: forced.block.header,
      membership: forced.membership,
      direction: 1n,
      subject: evidence.finding.subject,
      witnessSetHash: evidence.finding.witnessSetHash,
      scriptIndex: BigInt(evidence.finding.scriptIndex),
      accusedClass: BigInt(evidence.finding.accusedClass),
      referenceScriptUtxo: ref(0),
      ...patch,
    });

  const step02 = async (
    threadOutRef: string,
    shape: Shape,
    evidence: WitnessScriptDecodingEvidence,
    name: string | null = null,
    publishedCarriageUtxos?: readonly UTxO[],
    certificateUtxo?: UTxO,
  ) =>
    (
      await measured(name, shape.label, () =>
        submitWitnessScriptDecodingStep02({
          lucid,
          network,
          contracts,
          categoryId,
          signer,
          threadOutRef,
          evidence,
          nativeTxCompactCbor:
            evidence.finding.subject.source_kind === 1n
              ? forcedCompact(forcedView(shape.nativeTx).compact).toString(
                  "hex",
                )
              : shape.carriage.compactCbor,
          witnessSet: shape.carriage.witnessSet,
          witnessSetCompactCbor: shape.carriage.witnessSetCompactCbor,
          scriptWitnessItems: shape.items,
          publishCarriage:
            shape.certified || publishedCarriageUtxos !== undefined,
          publishedCarriageUtxos,
          certificateUtxo,
          certificateReferenceScriptUtxo: shape.certified
            ? requireCertificateReference()
            : undefined,
          referenceScriptUtxo: ref(1),
        }),
      )
    ).result;

  /**
   * Publish (and certify when the tier requires it) a shape's field-6
   * carriage once, so raw step-02 submissions and the production builder
   * resolve the same content-addressed UTxOs.
   */
  const publishField = async (shape: Shape, prefix: string | null) => {
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: SDK.MIDGARD_FIELD_INDEX.scriptWitnesses,
      anchorTxId: shape.txId,
      nativeTxCompactCbor: shape.carriage.compactCbor,
      itemCbors: shape.items,
      owner: signer.paymentKeyHash,
      publish: true,
      witnessSet: shape.carriage.witnessSet,
      anchorWitnessSetHash: shape.carriage.witnessSetHash,
      label: `witness field ${shape.label}`,
    });
    const carriage = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriage({
        lucid,
        signer,
        planned,
        publisherAddress: signer.address,
        label: `witness field ${shape.label}`,
      }),
    );
    if (prefix !== null) {
      carriage.measurements.forEach((measurement, index) =>
        record(
          `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
          shape.label,
          measurement,
        ),
      );
    }
    if (planned.plan.tier !== "Certified") {
      return { carriageUtxos: carriage.result, certificateUtxo: undefined };
    }
    const certificate = await captureEmulatorSubmission(harness.emulator, () =>
      certifyFaultProofFieldCarriage({
        lucid,
        network,
        signer,
        planned,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        certificateMintingScript:
          contracts.fieldPreimageCertificateMintingScript,
        certificateReferenceScriptUtxo: requireCertificateReference(),
        chunkUtxos: carriage.result,
        compactCbor: shape.carriage.compactCbor,
        witnessSetCompactCbor: shape.carriage.witnessSetCompactCbor,
      }),
    );
    if (prefix !== null)
      record(
        `${prefix}-carriage-certificate`,
        shape.label,
        certificate.measurement,
      );
    return {
      carriageUtxos: carriage.result,
      certificateUtxo: certificate.result.certificateUtxo,
    };
  };

  /**
   * The adjacent-over-bound refusal. The three chunks of a field one byte
   * past the aggregate bound are real publications; the certificate mint's
   * `verify_field_preimage_certificate_v1` refuses `total_length` on chain,
   * so no door can ever open the field. The core planner refuses the same
   * preimage before any transaction is built, so the plan is derived by the
   * raw support exactly as an adversarial publisher would derive it.
   */
  const certifyOverBoundField = async (shape: Shape) => {
    const preimage = scriptWitnessField(shape.items);
    expect(preimage).toHaveLength(ADJACENT_FIELD_BYTES);
    const plan = overBoundFieldCarriagePlan({
      owner: signer.paymentKeyHash,
      txId: shape.txId,
      preimage,
    });
    const planned: FaultProofFieldOpeningPlan = {
      sourceKind: 0n,
      fieldIndex: SDK.MIDGARD_FIELD_INDEX.scriptWitnesses,
      nativeTxId: shape.txId,
      nativeTxCompactCbor: shape.carriage.compactCbor,
      preimage,
      itemCount: 1,
      commitment: plan.commitment.toString("hex"),
      plan,
      witnessSet: shape.carriage.witnessSet,
      witnessSetHash: shape.carriage.witnessSetHash,
    };
    const chunkUtxos = await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `witness field ${shape.label}`,
    });
    expect(chunkUtxos).toHaveLength(3);
    await expectOnchainRefusal(() =>
      certifyFaultProofFieldCarriage({
        lucid,
        network,
        signer,
        planned,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        certificateMintingScript:
          contracts.fieldPreimageCertificateMintingScript,
        certificateReferenceScriptUtxo: requireCertificateReference(),
        chunkUtxos,
        compactCbor: shape.carriage.compactCbor,
        witnessSetCompactCbor: shape.carriage.witnessSetCompactCbor,
      }),
    );
    coverage.adjacentOverBoundRefused();
  };

  const step02Raw = (
    threadOutRef: string,
    shape: Shape,
    evidence: WitnessScriptDecodingEvidence,
    published: Awaited<ReturnType<typeof publishField>>,
    patch: Partial<
      Parameters<typeof submitWitnessScriptDecodingStep02Raw>[0]
    > = {},
  ) =>
    submitWitnessScriptDecodingStep02Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      anchorTxId: shape.txId,
      anchorWitnessSetHash: shape.carriage.witnessSetHash,
      carriage: shape.carriage,
      scriptWitnessItems: shape.items,
      carriageUtxos: published.carriageUtxos,
      certificateUtxo: published.certificateUtxo,
      referenceScriptUtxo: ref(1),
      nextState: exactScanState(evidence),
      ...patch,
    });

  /** The exact step-02 successor state for an evidence value. */
  const exactScanState = (
    evidence: WitnessScriptDecodingEvidence,
  ): SDK.WitnessScriptDecodingScanState => ({
    bound: {
      subject: evidence.finding.subject,
      witness_set_hash: evidence.finding.witnessSetHash,
      script_index: BigInt(evidence.finding.scriptIndex),
      accused_class: BigInt(evidence.finding.accusedClass),
    },
    total_length: BigInt(evidence.itemLength),
    item_commitment: evidence.itemCommitmentHex,
    control_cbor: evidence.initialControlCbor,
    next_expected_script_hash: contracts.steps[2].spendingScriptHash,
    checkpoint_hash: witnessScriptDecodingCheckpoint({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash: contracts.steps[2].spendingScriptHash,
    }),
    result_class: BigInt(evidence.initialResultClass),
  });

  const step03Raw = (
    threadOutRef: string,
    transition: ReturnType<typeof planWitnessScriptDecodingStep03Transition>,
    patch: Partial<
      Parameters<typeof submitWitnessScriptDecodingStep03Raw>[0]
    > = {},
  ) =>
    submitWitnessScriptDecodingStep03Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      args: transition.args,
      nextState: transition.nextState,
      nextStepIndex: transition.nextStepIndex,
      referenceScriptUtxo: ref(2),
      ...patch,
    });

  /** The scan state the thread carries at `threadOutRef` (step 03 or 04). */
  const scanStateAt = async (
    threadOutRef: string,
    stepIndex: 2 | 3,
  ): Promise<SDK.WitnessScriptDecodingScanState> => {
    const [txHash, index] = threadOutRef.split("#");
    const [utxo] = await lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(index) },
    ]);
    if (utxo?.datum == null) throw new Error("thread datum absent");
    const datum = Data.from(
      utxo.datum,
      (stepIndex === 2
        ? SDK.WitnessScriptDecodingStep03DatumSchema
        : SDK.WitnessScriptDecodingStep04DatumSchema) as never,
    ) as { data: SDK.WitnessScriptDecodingScanState };
    return datum.data;
  };

  const scanToClose = async (
    threadOutRef: string,
    evidence: WitnessScriptDecodingEvidence,
    prefix: string | null,
    shape: string,
  ) => {
    let outRef = threadOutRef;
    let resumes = 0;
    let closed = false;
    let maximum: CompleteSignedTransactionMeasurement | undefined;
    let first: CompleteSignedTransactionMeasurement | undefined;
    let close: CompleteSignedTransactionMeasurement | undefined;
    while (!closed) {
      const { result, measurement } = await measured(null, shape, () =>
        submitWitnessScriptDecodingStep03({
          lucid,
          contracts,
          categoryId,
          signer,
          threadOutRef: outRef,
          evidence,
          referenceScriptUtxo: ref(2),
        }),
      );
      outRef = result.nextThreadOutRef;
      closed = result.closed;
      if (closed) {
        close = measurement;
      } else {
        resumes += 1;
        first ??= measurement;
        if (
          maximum === undefined ||
          measurement.executionMemory > maximum.executionMemory
        )
          maximum = measurement;
      }
    }
    if (prefix !== null) {
      if (first !== undefined)
        record(`${prefix}-step03-resume-first`, shape, first);
      if (maximum !== undefined && maximum !== first)
        record(`${prefix}-step03-resume-max`, shape, maximum);
      if (close !== undefined) record(`${prefix}-step03-close`, shape, close);
    }
    if (resumes > 0) coverage.resumed();
    return { threadOutRef: outRef, resumes };
  };

  const step04 = async (
    threadOutRef: string,
    evidence: WitnessScriptDecodingEvidence,
    name: string | null,
    shape: string,
  ) => {
    const { result } = await measured(name, shape, () =>
      submitWitnessScriptDecodingStep04({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        referenceScriptUtxo: ref(3),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    await expect(
      lucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        result.fraudProofUnit,
      ),
    ).resolves.toHaveLength(1);
    return result;
  };

  const step04Raw = (threadOutRef: string) =>
    submitWitnessScriptDecodingStep04Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      referenceScriptUtxo: ref(3),
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });

  const expectThreadsGone = async (headerHash: string) => {
    const threadUnit = toUnit(
      contracts.computationThread.policyId,
      `${categoryId}${headerHash}`,
    );
    for (const step of contracts.steps) {
      await expect(
        lucid.utxosAtWithUnit(step.spendingScriptAddress, threadUnit),
      ).resolves.toHaveLength(0);
    }
  };

  const remove = async (
    headerHash: string,
    name: string | null,
    shape: string,
  ) => {
    if (removalReferenceScripts === undefined)
      throw new Error("removal references not published");
    const publishedRemovalReferences = removalReferenceScripts.published;
    const removeNow = BigInt(harness.emulator.now());
    const { result } = await measured(name, shape, () =>
      submitRemoveFraudulentBlock({
        lucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: publishedRemovalReferences },
        ),
        network,
        signer,
        fraudCategory: "witnessScriptDecoding",
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    expect(result.transactions[0]?.kind).toBe("remove-target");
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  };

  return {
    harness,
    contracts,
    category,
    categoryId,
    lucid,
    signer,
    ref,
    requireCertificateReference,
    measured,
    acceptedBlock,
    forcedBlock,
    init,
    cancel,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    publishField,
    certifyOverBoundField,
    step02Raw,
    exactScanState,
    step03Raw,
    scanStateAt,
    scanToClose,
    step04,
    step04Raw,
    expectThreadsGone,
    remove,
  };
};
