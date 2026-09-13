import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCompact as forcedCompact,
  materializeMidgardForcedTxFromCanonical as forcedView,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import type { SubmitStep01TxInclusion } from "../src/submit-step-01.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  applyWitnessScriptDecodingScripts,
  deriveWitnessScriptDecodingEvidenceFromCanonicalBlock,
  detectWitnessScriptDecodingCompleteReplay,
  planWitnessScriptDecodingStep03Transition,
  prepareWitnessScriptDecodingEvidence,
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
  WitnessScriptDecodingResultClasses,
} from "../src/witness-script-decoding/index.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { runEmulatorLifecycleStage } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  buildDecodingBlockFixture,
  decodingMalformedMaximumItem,
  decodingPlutusItem,
} from "./support/native-script-decoding-emulator.js";
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
  DEEP_MAXIMUM_DEPTH,
  deepCanonicalItem,
  emptyPayloadItem,
  headerMalformedAdjacentItem,
  headerMalformedItem,
  headerMalformedMaximumItem,
  MAXIMUM_FIELD_BYTES,
  mutateWitnessCertifiedCarriage,
  mutateWitnessCompactSource,
  mutateWitnessRawUtxoCarriage,
  mutateWitnessSet,
  nativeTxWithScriptWitnesses,
  overBoundFieldCarriagePlan,
  scriptWitnessField,
  smallCanonicalItem,
  submitWitnessScriptDecodingStep01ForcedRaw,
  submitWitnessScriptDecodingStep02Raw,
  submitWitnessScriptDecodingStep03Raw,
  submitWitnessScriptDecodingStep04Raw,
  wideCanonicalMaximumItem,
  type WitnessSetCarriage,
  witnessSetCarriageOf,
} from "./support/witness-script-decoding-raw.js";

const CATEGORY_ID = "00000022";
const REASON_ARMS = [
  "WitnessScriptHeaderMalformed",
  "WitnessNativeScriptMalformed",
  "WitnessNativeScriptNodeLimit",
  "WitnessNativeScriptDepthLimit",
] as const;
type ReasonArm = (typeof REASON_ARMS)[number];

/**
 * Every place a prover-supplied value is authenticated on chain. Each is
 * mutated once against a real bound thread and must be refused by a
 * validator, not by a builder.
 */
const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "forced_leaf_header",
  "forced_leaf_membership",
  "forced_leaf_reason",
  "forced_direction",
  "bound_witness_set_hash",
  "bound_accused_class",
  "script_coordinate",
  "successor_script",
  "native_tx_source",
  "witness_set",
  "field_raw_utxo",
  "field_certificate",
  "field_chunks",
  "item_commitment",
  "scan_chunk",
  "scan_control",
  "scan_frame",
  "scan_budget",
  "scan_checkpoint",
] as const;
const CANCELLABLE_STEPS = ["step-01", "step-02", "step-03", "step-04"] as const;

/**
 * The deep shape is 1,369 sixteen-step scan transactions at the field
 * bound; a shallower depth can be requested for a quick local pass, but the
 * ledger is written only at the maximum.
 */
const DEEP_DEPTH = Number(
  process.env.MIDGARD_WSD_DEEP_DEPTH ?? DEEP_MAXIMUM_DEPTH,
);
const WRITE_LEDGER = process.env.MIDGARD_WRITE_FIT_LEDGER === "1";

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/witness-script-decoding-v1-fit-ledger.json",
    import.meta.url,
  ),
);

const coverage = createLifecycleCoverageRecorder();
const measurements: VanRossemFitMeasurement[] = [];
let publicationsRecorded = false;

const record = (
  name: string,
  maximumShape: string,
  measurement: CompleteSignedTransactionMeasurement,
  kind: VanRossemFitMeasurement["kind"] = "lifecycle",
): void => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  if (measurement.redeemerCount > 0) {
    expect(measurement.executionMemory, name).toBeGreaterThan(0n);
    expect(measurement.executionSteps, name).toBeGreaterThan(0n);
  }
  expect(measurement.executionMemory, name).toBeLessThanOrEqual(16_500_000n);
  expect(measurement.executionSteps, name).toBeLessThanOrEqual(10_000_000_000n);
  measurements.push({
    name,
    kind,
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

type Shape = Readonly<{
  label: string;
  item: Buffer;
  nativeTx: MidgardNativeTxFull;
  txId: string;
  carriage: WitnessSetCarriage;
  fieldBytes: number;
  certified: boolean;
}>;

const shapeOf = (label: string, item: Buffer, fee: bigint): Shape => {
  const nativeTx = nativeTxWithScriptWitnesses([item], fee);
  const fieldBytes = scriptWitnessField([item]).length;
  return {
    label,
    item,
    nativeTx,
    txId: computeMidgardNativeTxId(nativeTx).toString("hex"),
    carriage: witnessSetCarriageOf(nativeTx),
    fieldBytes,
    certified: fieldBytes > 15_148,
  };
};

const headerMaximumShape = () =>
  shapeOf(
    "32,768-byte field 6; [1, 32,762 bytes]: undecodable wrapper over nine bounded-item chunks (Certified)",
    headerMalformedMaximumItem(),
    1_000n,
  );
const headerAdjacentShape = () =>
  shapeOf(
    "32,769-byte field 6; [1, 32,763 bytes]: one byte past the aggregate field bound (Certified)",
    headerMalformedAdjacentItem(),
    1_008n,
  );
const nativeMaximumShape = () =>
  shapeOf(
    "32,768-byte field 6; tag-0 payload refused at its fourth primitive step over nine bounded-item chunks (Certified)",
    decodingMalformedMaximumItem(),
    1_001n,
  );
const emptyPayloadShape = () =>
  shapeOf(
    "[0, h'']: decodable wrapper, empty payload (Inline)",
    emptyPayloadItem(),
    1_002n,
  );
const smallCanonicalShape = (fee = 1_003n) =>
  shapeOf(
    "all[sig]: canonical, four primitive steps (Inline)",
    smallCanonicalItem(),
    fee,
  );
const plutusShape = (fee = 1_004n) =>
  shapeOf(
    "[3, h'01020304']: non-native language (Inline)",
    decodingPlutusItem(),
    fee,
  );
const headerSmallShape = (fee = 1_005n) =>
  shapeOf(
    "[1, h'0a']: undecodable wrapper (Inline)",
    headerMalformedItem(),
    fee,
  );
const wideMaximumShape = () =>
  shapeOf(
    "32,768-byte field 6; all[1,020 sig, 38 after] = 1,059 nodes, 2,119 primitive steps over nine bounded-item chunks (Certified)",
    wideCanonicalMaximumItem(),
    1_006n,
  );
const deepShape = () =>
  shapeOf(
    `${scriptWitnessField([deepCanonicalItem(DEEP_DEPTH)]).length.toString()}-byte field 6; ${DEEP_DEPTH.toString()} nested all containers over one sig, ${(2 * DEEP_DEPTH + 2).toString()} primitive steps (Certified)`,
    deepCanonicalItem(DEEP_DEPTH),
    1_007n,
  );

const reasonOf = (arm: ReasonArm, scriptIndex: bigint): SDK.RejectionReason =>
  ({ [arm]: { script_index: scriptIndex } }) as SDK.RejectionReason;

const acceptedEvidence = (shape: Shape, scriptIndex = 0) =>
  prepareWitnessScriptDecodingEvidence({
    finding: {
      subject: SDK.acceptedVerdictSubject(shape.txId),
      witnessSetHash: shape.carriage.witnessSetHash,
      scriptIndex,
    },
    fieldPreimage: scriptWitnessField([shape.item]),
    committedFieldHashHex: shape.carriage.witnessSet.script_tx_wits_hash,
  });

const forcedEvidence = (
  shape: Shape,
  orderKey: SDK.OutputReference,
  reason: SDK.RejectionReason,
  scriptIndex = 0,
) =>
  prepareWitnessScriptDecodingEvidence({
    finding: {
      subject: SDK.forcedVerdictSubject({
        transactionId: shape.txId,
        sourceKey: orderKey,
        rejectionReason: reason,
      }),
      witnessSetHash: shape.carriage.witnessSetHash,
      scriptIndex,
    },
    fieldPreimage: scriptWitnessField([shape.item]),
    committedFieldHashHex: shape.carriage.witnessSet.script_tx_wits_hash,
  });

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

const makeHarness = async () => {
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
          scriptWitnessItems: [shape.item],
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
      itemCbors: [shape.item],
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
    const preimage = scriptWitnessField([shape.item]);
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
      scriptWitnessItems: [shape.item],
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

type Harness = Awaited<ReturnType<typeof makeHarness>>;

// ---------------------------------------------------------------------------
// Replay detection over the committed block
// ---------------------------------------------------------------------------

const expectReplayDetection = (
  block: Awaited<ReturnType<Harness["acceptedBlock"]>>["block"],
  transactions: readonly Shape[],
  evidence: WitnessScriptDecodingEvidence,
  violationIds: readonly string[],
) => {
  const replay = {
    headerHash: block.headerHash,
    transactions: transactions.map((shape) => ({
      nodeTxId: shape.txId,
      txCbor: encodeMidgardNativeTxCanonical(shape.nativeTx).toString("hex"),
    })),
    reconstruction: block.reconstruction,
  } as never;
  expect(
    detectWitnessScriptDecodingCompleteReplay(replay).map(
      (finding) => finding.violationId,
    ),
  ).toEqual([...violationIds]);
  expect(
    deriveWitnessScriptDecodingEvidenceFromCanonicalBlock(replay)
      .itemCommitmentHex,
  ).toBe(evidence.itemCommitmentHex);
};

/**
 * Drive an accepted thread's scan with the arguments a direction-B twin of
 * the same item plans (the arguments do not depend on the direction), so an
 * honest accepted block's canonical native script reaches the exact terminal
 * on chain and closes with no fault.
 */
const scanHonestAcceptedToClose = async (
  h: Harness,
  threadOutRef: string,
  accepted: WitnessScriptDecodingEvidence,
  twin: WitnessScriptDecodingEvidence,
) => {
  let outRef = threadOutRef;
  for (;;) {
    const state = await h.scanStateAt(outRef, 2);
    const transition = planWitnessScriptDecodingStep03Transition({
      state,
      evidence: twin,
      contracts: h.contracts,
    });
    const nextExpectedScriptHash =
      h.contracts.steps[transition.nextStepIndex].spendingScriptHash;
    const nextState: SDK.WitnessScriptDecodingScanState = {
      ...state,
      control_cbor: transition.nextState.control_cbor,
      next_expected_script_hash: nextExpectedScriptHash,
      checkpoint_hash: witnessScriptDecodingCheckpoint({
        evidence: accepted,
        controlCbor: transition.nextState.control_cbor,
        nextExpectedScriptHash,
      }),
      result_class: transition.nextState.result_class,
    };
    const submitted = await h.step03Raw(outRef, transition, { nextState });
    outRef = submitted.nextThreadOutRef;
    if (transition.closes) return outRef;
  }
};

// ---------------------------------------------------------------------------
// Scenarios
// ---------------------------------------------------------------------------

describe("witnessScriptDecoding registered-chain lifecycle", () => {
  it("convicts the accepted undecodable wrapper at the maximum item, cancels every step, refuses the membership, coordinate and honest accepted polarities, then mints and removes", async () => {
    const h = await makeHarness();
    const shape = headerMaximumShape();
    const honestNative = smallCanonicalShape();
    const honestPlutus = plutusShape();
    const adjacent = headerAdjacentShape();
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape, [
      honestNative,
      honestPlutus,
      adjacent,
    ]);
    const evidence = acceptedEvidence(shape);
    expect(scriptWitnessField([shape.item])).toHaveLength(MAXIMUM_FIELD_BYTES);
    expectReplayDetection(
      block,
      [shape, honestNative, honestPlutus],
      evidence,
      ["witness-script-header-malformed"],
    );
    const published = await h.publishField(shape, "accepted-header");

    // The consensus bound, one byte over. Off chain the field view refuses
    // the preimage before any evidence exists; on chain the certificate
    // policy refuses the length of the real chunk carriage, so the exact
    // 32,768-byte shape above is the last one any door can open.
    expect(() => acceptedEvidence(adjacent)).toThrow(/aggregate bound/u);
    await h.certifyOverBoundField(adjacent);

    // Cancel from every physical step.
    await h.cancel(
      await h.init(setup.fraudulentBlockOutRef, null, shape.label),
      0,
      "accepted-header-cancel-step01",
      shape.label,
    );
    const atStep02 = await h.step01Accepted(
      await h.init(setup.fraudulentBlockOutRef, null, shape.label),
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
    );
    await h.cancel(atStep02, 1, "accepted-header-cancel-step02", shape.label);
    const atStep03 = (
      await h.step02(
        await h.step01Accepted(
          await h.init(setup.fraudulentBlockOutRef, null, shape.label),
          inclusionOf(shape),
          setup.fraudulentBlockOutRef,
          0n,
        ),
        shape,
        evidence,
        null,
        published.carriageUtxos,
        published.certificateUtxo,
      )
    ).nextThreadOutRef;
    await h.cancel(atStep03, 2, "accepted-header-cancel-step03", shape.label);
    const atStep04 = (
      await h.scanToClose(
        (
          await h.step02(
            await h.step01Accepted(
              await h.init(setup.fraudulentBlockOutRef, null, shape.label),
              inclusionOf(shape),
              setup.fraudulentBlockOutRef,
              0n,
            ),
            shape,
            evidence,
            null,
            published.carriageUtxos,
            published.certificateUtxo,
          )
        ).nextThreadOutRef,
        evidence,
        null,
        shape.label,
      )
    ).threadOutRef;
    await h.cancel(atStep04, 3, "accepted-header-cancel-step04", shape.label);

    // Step-01 seam: the transaction's membership in the committed block.
    const membershipThread = await h.init(
      setup.fraudulentBlockOutRef,
      null,
      shape.label,
    );
    await expectOnchainRefusal(() =>
      h.step01Accepted(
        membershipThread,
        { ...inclusionOf(shape), transactionsPhasRoot: "ff".repeat(32) },
        setup.fraudulentBlockOutRef,
        0n,
      ),
    );
    coverage.seamMutated("tx_membership");
    await h.cancel(membershipThread, 0);

    // Step-02 seam: a coordinate past the field's item count. Step 01 binds
    // any non-negative ordinal; the field door refuses the opening.
    const outOfRange = await h.step01Accepted(
      await h.init(setup.fraudulentBlockOutRef, null, shape.label),
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      1n,
    );
    const outOfRangeState = h.exactScanState(evidence);
    await expectOnchainRefusal(() =>
      h.step02Raw(outOfRange, shape, evidence, published, {
        nextState: {
          ...outOfRangeState,
          bound: { ...outOfRangeState.bound, script_index: 1n },
        },
      }),
    );
    coverage.seamMutated("script_coordinate");
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await h.cancel(outOfRange, 1);

    // Honest accepted polarity, native: the canonical script reaches the exact
    // terminal through the resumable scan and step 04 refuses the no-fault
    // close.
    const honestEvidence = acceptedEvidence(honestNative);
    const honestTwin = forcedEvidence(
      honestNative,
      { transactionId: "ee".repeat(32), outputIndex: 0n },
      reasonOf("WitnessNativeScriptNodeLimit", 0n),
    );
    const honestBound = (
      await h.step02(
        await h.step01Accepted(
          await h.init(setup.fraudulentBlockOutRef, null, honestNative.label),
          inclusionOf(honestNative),
          setup.fraudulentBlockOutRef,
          0n,
        ),
        honestNative,
        honestEvidence,
      )
    ).nextThreadOutRef;
    const honestClosed = await scanHonestAcceptedToClose(
      h,
      honestBound,
      honestEvidence,
      honestTwin,
    );
    expect((await h.scanStateAt(honestClosed, 3)).result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NoFault),
    );
    await expectOnchainRefusal(() => h.step04Raw(honestClosed));
    coverage.scenario("honest_accepted_block_refusal");
    await h.cancel(honestClosed, 3);

    // Honest accepted polarity, non-native: a decodable Plutus wrapper is a
    // successful decoder result and closes at step 02.
    const plutusEvidence = acceptedEvidence(honestPlutus);
    const plutusClosed = (
      await h.scanToClose(
        (
          await h.step02(
            await h.step01Accepted(
              await h.init(
                setup.fraudulentBlockOutRef,
                null,
                honestPlutus.label,
              ),
              inclusionOf(honestPlutus),
              setup.fraudulentBlockOutRef,
              0n,
            ),
            honestPlutus,
            plutusEvidence,
          )
        ).nextThreadOutRef,
        plutusEvidence,
        null,
        honestPlutus.label,
      )
    ).threadOutRef;
    await expectOnchainRefusal(() => h.step04Raw(plutusClosed));
    await h.cancel(plutusClosed, 3);

    // The real lifecycle: Init, bind, open the maximum certified field, close
    // the header class through step 03, mint the proof, remove the block.
    const init = await h.init(
      setup.fraudulentBlockOutRef,
      "accepted-header-init",
      shape.label,
    );
    const bound = await h.step01Accepted(
      init,
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
      "accepted-header-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "accepted-header-step02",
      published.carriageUtxos,
      published.certificateUtxo,
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.HeaderMalformed),
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "accepted-header",
      shape.label,
    );
    expect(closed.resumes).toBe(0);
    await h.step04(
      closed.threadOutRef,
      evidence,
      "accepted-header-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(setup.headerHash);
    coverage.reason("WitnessScriptHeaderMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");
    await h.remove(setup.headerHash, "accepted-header-remove", shape.label);
  }, 900_000);

  it("convicts the accepted malformed payload at the maximum item through the resumable scan, refusing every field-opening and scan seam first", async () => {
    const h = await makeHarness();
    const shape = nativeMaximumShape();
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape);
    const evidence = acceptedEvidence(shape);
    expect(scriptWitnessField([shape.item])).toHaveLength(MAXIMUM_FIELD_BYTES);
    expect(evidence.chunkProofCount).toBe(9);
    expectReplayDetection(block, [shape], evidence, [
      "witness-native-script-malformed",
    ]);
    const published = await h.publishField(shape, "accepted-native");
    const other = plutusShape(1_100n);

    // Step-02 seams on one bound thread.
    const bound = await h.step01Accepted(
      await h.init(setup.fraudulentBlockOutRef, null, shape.label),
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        mutateOpening: (opening) =>
          mutateWitnessCertifiedCarriage(opening, (carriage) => ({
            ...carriage,
            cert_ref_input_index: carriage.chunk_ref_input_indices[0]!,
          })),
      }),
    );
    coverage.seamMutated("field_certificate");
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        mutateOpening: (opening) =>
          mutateWitnessCertifiedCarriage(opening, (carriage) => ({
            ...carriage,
            chunk_ref_input_indices: [
              carriage.chunk_ref_input_indices[1]!,
              carriage.chunk_ref_input_indices[0]!,
              ...carriage.chunk_ref_input_indices.slice(2),
            ],
          })),
      }),
    );
    coverage.seamMutated("field_chunks");
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        mutateOpening: (opening) =>
          mutateWitnessCompactSource(opening, other.carriage.compactCbor),
      }),
    );
    coverage.seamMutated("native_tx_source");
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        mutateOpening: (opening) =>
          mutateWitnessSet(opening, other.carriage.witnessSet),
      }),
    );
    coverage.seamMutated("witness_set");
    const exact = h.exactScanState(evidence);
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        nextState: { ...exact, item_commitment: "ab".repeat(32) },
      }),
    );
    coverage.seamMutated("item_commitment");
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, { nextStepIndex: 3 }),
    );
    coverage.seamMutated("successor_script");
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      null,
      published.carriageUtxos,
      published.certificateUtxo,
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.Pending),
    );

    // Step-03 seams on the pending scan state. The first segment of this
    // shape consumes one frame witness (container, leaf, frame pop) and stops
    // before the refusing token.
    const state = await h.scanStateAt(opened.nextThreadOutRef, 2);
    const transition = planWitnessScriptDecodingStep03Transition({
      state,
      evidence,
      contracts: h.contracts,
    });
    expect(transition.route).toBe("segment");
    expect(transition.args.frames).toHaveLength(1);
    expect(transition.args.next_chunk_proof).not.toBeNull();
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        args: {
          ...transition.args,
          chunk_proof: transition.args.next_chunk_proof,
          next_chunk_proof: transition.args.chunk_proof,
        },
      }),
    );
    coverage.seamMutated("scan_chunk");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        args: {
          ...transition.args,
          control_cbor: transition.nextState.control_cbor,
        },
      }),
    );
    coverage.seamMutated("scan_control");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        args: {
          ...transition.args,
          frames: transition.args.frames.map((frame) => ({
            ...frame,
            remaining: frame.remaining + 1n,
          })),
        },
      }),
    );
    coverage.seamMutated("scan_frame");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        args: { ...transition.args, step_budget: 0n },
      }),
    );
    coverage.seamMutated("scan_budget");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, {
        nextState: {
          ...transition.nextState,
          checkpoint_hash: "cd".repeat(32),
        },
      }),
    );
    coverage.seamMutated("scan_checkpoint");
    await expectOnchainRefusal(() =>
      h.step03Raw(opened.nextThreadOutRef, transition, { nextStepIndex: 3 }),
    );
    await h.cancel(opened.nextThreadOutRef, 2);

    // The real lifecycle on a fresh thread.
    const init = await h.init(
      setup.fraudulentBlockOutRef,
      "accepted-native-init",
      shape.label,
    );
    const rebound = await h.step01Accepted(
      init,
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
      "accepted-native-step01",
      shape.label,
    );
    const reopened = await h.step02(
      rebound,
      shape,
      evidence,
      "accepted-native-step02",
      published.carriageUtxos,
      published.certificateUtxo,
    );
    const closed = await h.scanToClose(
      reopened.nextThreadOutRef,
      evidence,
      "accepted-native",
      shape.label,
    );
    expect(closed.resumes).toBeGreaterThan(0);
    expect((await h.scanStateAt(closed.threadOutRef, 3)).result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NativeMalformed),
    );
    await h.step04(
      closed.threadOutRef,
      evidence,
      "accepted-native-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(setup.headerHash);
    coverage.reason("WitnessNativeScriptMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");
    await h.remove(setup.headerHash, "accepted-native-remove", shape.label);
  }, 900_000);

  it("convicts the accepted empty tag-0 payload, which step 02 closes as the structural class", async () => {
    const h = await makeHarness();
    const shape = emptyPayloadShape();
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape);
    const evidence = acceptedEvidence(shape);
    expect(evidence.initialResultClass).toBe(
      WitnessScriptDecodingResultClasses.NativeMalformed,
    );
    expectReplayDetection(block, [shape], evidence, [
      "witness-native-script-malformed",
    ]);
    const init = await h.init(
      setup.fraudulentBlockOutRef,
      "accepted-empty-init",
      shape.label,
    );
    const bound = await h.step01Accepted(
      init,
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
      "accepted-empty-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "accepted-empty-step02",
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NativeMalformed),
    );
    expect(opened.scanState.control_cbor).toBe("");
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "accepted-empty",
      shape.label,
    );
    expect(closed.resumes).toBe(0);
    await h.step04(
      closed.threadOutRef,
      evidence,
      "accepted-empty-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(setup.headerHash);
    coverage.reason("WitnessNativeScriptMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    await h.remove(setup.headerHash, "accepted-empty-remove", shape.label);
  }, 600_000);

  it("contradicts a wrongful header-malformed rejection of a decodable script, refusing every forced-door seam first", async () => {
    const h = await makeHarness();
    const shape = smallCanonicalShape();
    const reason = reasonOf("WitnessScriptHeaderMalformed", 0n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    expectReplayDetection(forced.block, [], evidence, [
      "witness-script-header-malformed",
    ]);

    const seams: readonly [
      (typeof AUTHENTICATION_SEAMS)[number],
      Partial<Parameters<typeof submitWitnessScriptDecodingStep01ForcedRaw>[0]>,
    ][] = [
      [
        "forced_leaf_header",
        {
          header: {
            ...forced.block.header,
            forcedTransactionsRoot: "ff".repeat(32),
          },
        },
      ],
      [
        "forced_leaf_membership",
        {
          membership: {
            ...forced.membership,
            key: {
              ...forced.membership.key,
              outputIndex: forced.membership.key.outputIndex + 1n,
            },
          },
        },
      ],
      ["forced_direction", { direction: 0n }],
      ["script_coordinate", { scriptIndex: 1n }],
      ["bound_witness_set_hash", { witnessSetHash: "ab".repeat(32) }],
      ["bound_accused_class", { accusedClass: 1n }],
      ["successor_script", { nextStepIndex: 2 }],
    ];
    for (const [seam, patch] of seams) {
      const thread = await h.init(
        forced.setup.fraudulentBlockOutRef,
        null,
        shape.label,
      );
      await expectOnchainRefusal(() =>
        h.step01ForcedRaw(thread, forced, evidence, patch),
      );
      coverage.seamMutated(seam);
      await h.cancel(thread, 0);
    }
    coverage.scenario("reason_or_subject_coordinate_mutation");

    // Step-02 seam on the published (RawUtxo) tier: the same small field,
    // published whole, opened against a reference input that is not its
    // publication. The door refuses the carriage on chain.
    const published = await h.publishField(shape, null);
    expect(published.certificateUtxo).toBeUndefined();
    const rawThread = await h.step01Forced(
      await h.init(forced.setup.fraudulentBlockOutRef, null, shape.label),
      forced,
      evidence.finding.witnessSetHash,
      0n,
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(rawThread, shape, evidence, published, {
        mutateOpening: (opening) => mutateWitnessRawUtxoCarriage(opening, 1n),
      }),
    );
    coverage.seamMutated("field_raw_utxo");
    await h.cancel(rawThread, 1);

    const init = await h.init(
      forced.setup.fraudulentBlockOutRef,
      "forced-header-init",
      shape.label,
    );
    const bound = await h.step01Forced(
      init,
      forced,
      evidence.finding.witnessSetHash,
      0n,
      "forced-header-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "forced-header-step02",
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-header",
      shape.label,
    );
    expect((await h.scanStateAt(closed.threadOutRef, 3)).result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NoFault),
    );
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-header-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    coverage.reason("WitnessScriptHeaderMalformed", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    await h.remove(
      forced.setup.headerHash,
      "forced-header-remove",
      shape.label,
    );
  }, 600_000);

  it("contradicts a wrongful native-malformed rejection of a non-native script, which closes at step 02", async () => {
    const h = await makeHarness();
    const shape = plutusShape();
    const reason = reasonOf("WitnessNativeScriptMalformed", 0n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    const init = await h.init(
      forced.setup.fraudulentBlockOutRef,
      "forced-native-init",
      shape.label,
    );
    const bound = await h.step01Forced(
      init,
      forced,
      evidence.finding.witnessSetHash,
      0n,
      "forced-native-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "forced-native-step02",
    );
    expect(opened.scanState.result_class).toBe(
      BigInt(WitnessScriptDecodingResultClasses.NoFault),
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-native",
      shape.label,
    );
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-native-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    coverage.reason("WitnessNativeScriptMalformed", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    await h.remove(
      forced.setup.headerHash,
      "forced-native-remove",
      shape.label,
    );
  }, 600_000);

  it("contradicts a wrongful node-limit rejection of the widest canonical script the field bound admits", async () => {
    const h = await makeHarness();
    const shape = wideMaximumShape();
    expect(scriptWitnessField([shape.item])).toHaveLength(MAXIMUM_FIELD_BYTES);
    const reason = reasonOf("WitnessNativeScriptNodeLimit", 0n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    const published = await h.publishField(shape, "forced-node-wide");
    const init = await h.init(
      forced.setup.fraudulentBlockOutRef,
      "forced-node-wide-init",
      shape.label,
    );
    const bound = await h.step01Forced(
      init,
      forced,
      evidence.finding.witnessSetHash,
      0n,
      "forced-node-wide-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "forced-node-wide-step02",
      published.carriageUtxos,
      published.certificateUtxo,
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-node-wide",
      shape.label,
    );
    // 2,119 primitive steps in sixteen-step segments, plus one segment cut
    // at a bounded-item chunk window: 134 scan transactions, 133 of them
    // resumes, most opening on a frame step with their window supplied.
    expect(closed.resumes).toBe(133);
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-node-wide-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    coverage.reason("WitnessNativeScriptNodeLimit", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    coverage.scenario("maximum_supported_evidence");
    await h.remove(
      forced.setup.headerHash,
      "forced-node-wide-remove",
      shape.label,
    );
  }, 900_000);

  it("contradicts a wrongful depth-limit rejection of the deepest canonical script the field bound admits", async () => {
    const h = await makeHarness();
    const shape = deepShape();
    if (DEEP_DEPTH === DEEP_MAXIMUM_DEPTH)
      expect(scriptWitnessField([shape.item])).toHaveLength(
        MAXIMUM_FIELD_BYTES,
      );
    const reason = reasonOf("WitnessNativeScriptDepthLimit", 0n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    const published = await h.publishField(shape, "forced-depth-deep");
    const init = await h.init(
      forced.setup.fraudulentBlockOutRef,
      "forced-depth-deep-init",
      shape.label,
    );
    const bound = await h.step01Forced(
      init,
      forced,
      evidence.finding.witnessSetHash,
      0n,
      "forced-depth-deep-step01",
      shape.label,
    );
    const opened = await h.step02(
      bound,
      shape,
      evidence,
      "forced-depth-deep-step02",
      published.carriageUtxos,
      published.certificateUtxo,
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-depth-deep",
      shape.label,
    );
    // `2·depth + 2` primitive steps in sixteen-step segments, plus the cuts
    // the planner makes at the eight bounded-item chunk windows of the
    // maximum item: 1,369 scan transactions at the maximum depth, 1,368 of
    // them resumes through a real checkpoint.
    expect(closed.resumes).toBeGreaterThanOrEqual(
      Math.ceil((2 * DEEP_DEPTH + 2) / 16) - 1,
    );
    if (DEEP_DEPTH === DEEP_MAXIMUM_DEPTH) expect(closed.resumes).toBe(1_368);
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-depth-deep-step04-proof-mint",
      shape.label,
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    coverage.reason("WitnessNativeScriptDepthLimit", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    if (DEEP_DEPTH === DEEP_MAXIMUM_DEPTH)
      coverage.scenario("maximum_supported_evidence");
    await runEmulatorLifecycleStage("witness-depth.remove", () =>
      h.remove(
        forced.setup.headerHash,
        "forced-depth-deep-remove",
        shape.label,
      ),
    );
  }, 1_800_000);

  it("refuses to contradict honest forced rejections: the undecodable wrapper and the empty payload", async () => {
    for (const [shape, arm] of [
      [headerSmallShape(), "WitnessScriptHeaderMalformed"],
      [emptyPayloadShape(), "WitnessNativeScriptMalformed"],
    ] as const) {
      const h = await makeHarness();
      const reason = reasonOf(arm, 0n);
      const forced = await h.forcedBlock(shape, reason);
      const evidence = forcedEvidence(shape, forced.orderKey, reason);
      expect(evidence.resultClass).toBe(evidence.finding.accusedClass);
      const bound = await h.step01Forced(
        await h.init(forced.setup.fraudulentBlockOutRef, null, shape.label),
        forced,
        evidence.finding.witnessSetHash,
        0n,
      );
      const opened = await h.step02(bound, shape, evidence);
      const closed = await h.scanToClose(
        opened.nextThreadOutRef,
        evidence,
        null,
        shape.label,
      );
      await expectOnchainRefusal(() => h.step04Raw(closed.threadOutRef));
      coverage.reason(arm);
      coverage.scenario("honest_forced_rejection_refusal");
      await h.cancel(closed.threadOutRef, 3);
    }
  }, 600_000);

  it("refuses to bind a forced rejection whose authenticated leaf carries a sibling typed reason", async () => {
    const h = await makeHarness();
    const shape = smallCanonicalShape();
    const forced = await h.forcedBlock(shape, "NetworkIdMismatch");
    const thread = await h.init(
      forced.setup.fraudulentBlockOutRef,
      null,
      shape.label,
    );
    await expectOnchainRefusal(() =>
      submitWitnessScriptDecodingStep01ForcedRaw({
        lucid: h.lucid,
        contracts: h.contracts,
        categoryId: h.categoryId,
        signer: h.signer,
        threadOutRef: thread,
        header: forced.block.header,
        membership: forced.membership,
        direction: 1n,
        subject: SDK.forcedVerdictSubject({
          transactionId: shape.txId,
          sourceKey: forced.orderKey,
          rejectionReason: "NetworkIdMismatch",
        }),
        witnessSetHash: shape.carriage.witnessSetHash,
        scriptIndex: 0n,
        accusedClass: 0n,
        referenceScriptUtxo: h.ref(0),
      }),
    );
    coverage.seamMutated("forced_leaf_reason");
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await h.cancel(thread, 0);
  }, 600_000);

  it("closes the coverage gate and the Van Rossem fit ledger", async () => {
    // A successful proof mint is not a completed journey. Refuse to write
    // partial evidence if any preceding lifecycle failed during removal.
    expect(
      measurements
        .map((entry) => entry.name)
        .filter((name) => name.endsWith("-remove"))
        .sort(),
    ).toEqual([
      "accepted-empty-remove",
      "accepted-header-remove",
      "accepted-native-remove",
      "forced-depth-deep-remove",
      "forced-header-remove",
      "forced-native-remove",
      "forced-node-wide-remove",
    ]);
    // Recorded honestly: every arm, seam, cancel, resume and the adjacent
    // refusal at the aggregate field bound are reached. The two omissions
    // the gate reports are the wrongful-acceptance directions of the
    // node-limit and depth-limit arms, which no canonical field-6 item can
    // realise: the aggregate preimage is bounded at 32,768 bytes and a node
    // costs at least three bytes, so 16,385 nodes (or depth 16,385) need at
    // least 49,155 bytes. The widest and deepest shapes above stop at 1,059
    // nodes and depth 10,909; the exact-bound and adjacent-over-bound
    // node/depth refusals are engine facts pinned by the family's Aiken and
    // TypeScript selectors, not lifecycles.
    expect(() =>
      assertCompleteLifecycleCoverage({
        coverage: coverage.snapshot(),
        expectedReasonArms: [...REASON_ARMS],
        authenticationSeams: [...AUTHENTICATION_SEAMS],
        cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
        resumable: true,
        hasAdjacentConsensusBound: true,
      }),
    ).toThrow(
      "incomplete fault-proof lifecycle coverage: WitnessNativeScriptNodeLimit success directions: accepted_invalid; WitnessNativeScriptDepthLimit success directions: accepted_invalid",
    );
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: `witnessScriptDecoding:${CATEGORY_ID}:testnet`,
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: `aiken ${preamble.preamble?.compiler?.version ?? "unknown"}`,
      measurements,
    });
    for (const entry of ledger.entries) {
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication")
        expect(
          entry.publicationReserveMargin,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
    }
    if (WRITE_LEDGER) {
      if (DEEP_DEPTH !== DEEP_MAXIMUM_DEPTH)
        throw new Error("the ledger is written only at the maximum deep shape");
      await writeVanRossemFitLedger(ledgerPath, ledger);
      console.info(`[witness-script-decoding-fit-ledger] wrote ${ledgerPath}`);
    }
    console.info(
      `[witness-script-decoding-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
