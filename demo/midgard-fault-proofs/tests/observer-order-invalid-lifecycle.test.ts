import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { midgardFieldCommitment } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCompact as forcedCompact,
  materializeMidgardForcedTxFromCanonical as forcedView,
} from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  forcedVerdictSubject,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { advanceMissingNativeScriptTxSemanticCheckpoint } from "../src/missing-native-script-tx/staged-walk.js";
import { createObserverOrderInvalidActuator } from "../src/observer-order-invalid/actuator.js";
import { buildObserverOrderInvalidArtifact } from "../src/observer-order-invalid/artifact.js";
import {
  applyObserverOrderInvalidScripts,
  OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES,
  type ObserverOrderInvalidContracts,
} from "../src/observer-order-invalid/contracts.js";
import {
  classifyObserverOrderInvalidFinding,
  OBSERVER_ORDER_INVALID_ITEM_BUDGET,
  type ObserverOrderInvalidEvidence,
  observerOrderInvalidEvidenceCloses,
  type ObserverOrderInvalidFinding,
  prepareObserverOrderInvalidEvidence,
} from "../src/observer-order-invalid/family.js";
import {
  encodeObserverOrderWalkCheckpoint,
  hashObserverOrderWalkCheckpoint,
  type ObserverOrderInvalidStagedPlan,
  planObserverOrderInvalidStagedWalk,
} from "../src/observer-order-invalid/staged-plan.js";
import { submitObserverOrderInvalidCancel } from "../src/observer-order-invalid/submit-cancel.js";
import {
  submitObserverOrderInvalidStep01Accepted,
  submitObserverOrderInvalidStep01Forced,
} from "../src/observer-order-invalid/submit-step-01.js";
import { submitObserverOrderInvalidStep02 } from "../src/observer-order-invalid/submit-step-02.js";
import { submitObserverOrderInvalidStep03 } from "../src/observer-order-invalid/submit-step-03.js";
import { submitObserverOrderInvalidStep04 } from "../src/observer-order-invalid/submit-step-04.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import type { SubmitStep01TxInclusion } from "../src/step-support.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  ascendingObservers,
  buildAcceptedObserverInclusions,
  buildForcedObserverLeaf,
  compactCborHex,
  type ForcedObserverLeaf,
  mutateCertifiedCarriage,
  mutateCompactSource,
  mutateRawUtxoCarriage,
  observerAt,
  type ObserverFieldShape,
  observerFieldShape,
  submitObserverOrderInvalidStep01ForcedRaw,
  submitObserverOrderInvalidStep02Raw,
  submitObserverOrderInvalidStep03Raw,
  submitObserverOrderInvalidStep04Raw,
  transactionIdOf,
  witnessSetCompactCborHex,
} from "./support/observer-order-invalid-raw.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const REASON_ARM = "ObserverOrderInvalid";
const CATEGORY_ID = "00000025";
/**
 * The largest field 3 the §5.4 aggregate field bound admits: a three-byte
 * array header plus 1,092 fixed-stride 30-byte items is 32,763 of the
 * 32,768 admissible bytes, carried as three certified chunks. One more item
 * is not an encodable transaction.
 */
const MAXIMUM_OBSERVERS = 1092;
const MAXIMUM_FIELD_BYTES = 32_763;
const MAXIMUM_CHUNKS = 3;
const LAST_ORDINAL = MAXIMUM_OBSERVERS - 1;
const MAXIMUM_SCANS = Math.ceil(
  MAXIMUM_OBSERVERS / OBSERVER_ORDER_INVALID_ITEM_BUDGET,
);

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
  "forced_reason_coordinate",
  "forced_subject_transaction",
  "forced_direction",
  "successor_script",
  "native_tx_source",
  "field_raw_utxo",
  "field_certificate",
  "field_chunks",
  "scan_checkpoint",
  "scan_successor_state",
  "scan_wrong_successor",
  "scan_budget",
  "premature_decision",
  "decision_polarity",
] as const;
type Seam = (typeof AUTHENTICATION_SEAMS)[number];
const CANCELLABLE_STEPS = ["step-01", "step-02", "step-03", "step-04"] as const;

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/observer-order-invalid-v1-fit-ledger.json",
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
  // Chunk publications and reference-script publications run no script; every
  // transaction that does must have been evaluated locally.
  if (measurement.redeemerCount > 0) {
    expect(measurement.executionMemory, name).toBeGreaterThan(0n);
    expect(measurement.executionSteps, name).toBeGreaterThan(0n);
  }
  measurements.push({
    name,
    kind,
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

const scanLabel = (prefix: string, ordinal: number) =>
  `${prefix}-step03-scan${(ordinal + 1).toString().padStart(2, "0")}`;

// ---------------------------------------------------------------------------
// Shapes: every ordering polarity at first, middle, last and duplicate ordinals
// ---------------------------------------------------------------------------

/** Ascending, with the last adjacent pair swapped: the offence is ordinal 1091. */
const maximumLastViolationShape = () => {
  const observers = ascendingObservers(MAXIMUM_OBSERVERS);
  observers[LAST_ORDINAL - 1] = observerAt(LAST_ORDINAL);
  observers[LAST_ORDINAL] = observerAt(LAST_ORDINAL - 1);
  return observerFieldShape({
    label: `${MAXIMUM_OBSERVERS.toString()} observers, last pair descending (${MAXIMUM_FIELD_BYTES.toString()}-byte certified field)`,
    observers,
  });
};
const maximumOrderedShape = () =>
  observerFieldShape({
    label: `${MAXIMUM_OBSERVERS.toString()} strictly ascending observers (${MAXIMUM_FIELD_BYTES.toString()}-byte certified field)`,
    observers: ascendingObservers(MAXIMUM_OBSERVERS),
  });
const firstPairDescendingShape = () =>
  observerFieldShape({
    label: "2 observers, first pair descending (published inline field)",
    observers: [observerAt(1), observerAt(0)],
    fee: 11n,
  });
const middleDuplicateShape = () =>
  observerFieldShape({
    label: "5 observers, duplicate at ordinal 2 (published inline field)",
    observers: [
      observerAt(0),
      observerAt(1),
      observerAt(1),
      observerAt(2),
      observerAt(3),
    ],
    fee: 13n,
  });
const smallOrderedShape = () =>
  observerFieldShape({
    label: "5 strictly ascending observers (published inline field)",
    observers: ascendingObservers(5),
    fee: 17n,
  });
const twoOrderedShape = () =>
  observerFieldShape({
    label: "2 strictly ascending observers (published inline field)",
    observers: ascendingObservers(2),
  });
const emptyShape = () =>
  observerFieldShape({
    label: "0 observers (published inline field)",
    observers: [],
  });
const singleShape = () =>
  observerFieldShape({
    label: "1 observer (published inline field)",
    observers: [observerAt(3)],
  });
const duplicateFirstShape = () =>
  observerFieldShape({
    label: "3 observers, duplicate at ordinal 1 (published inline field)",
    observers: [observerAt(0), observerAt(0), observerAt(1)],
  });
const earlierViolationShape = () =>
  observerFieldShape({
    label: "3 observers, descending at ordinal 1, ascending at ordinal 2",
    observers: [observerAt(1), observerAt(0), observerAt(2)],
  });

const reasonAt = (observerIndex: number): RejectionReason => ({
  ObserverOrderInvalid: { observer_index: BigInt(observerIndex) },
});

const acceptedFinding = (
  shape: ObserverFieldShape,
  observerIndex: number,
): ObserverOrderInvalidFinding => ({
  subject: acceptedVerdictSubject(transactionIdOf(shape)),
  observerIndex,
});

const evidenceOf = (
  shape: ObserverFieldShape,
  finding: ObserverOrderInvalidFinding,
): ObserverOrderInvalidEvidence =>
  prepareObserverOrderInvalidEvidence({
    finding,
    fieldPreimage: shape.fieldPreimage,
    committedFieldHashHex: midgardFieldCommitment(shape.fieldPreimage).toString(
      "hex",
    ),
  });

const stagedOf = (
  shape: ObserverFieldShape,
  observerIndex: number,
): ObserverOrderInvalidStagedPlan =>
  planObserverOrderInvalidStagedWalk({
    transactionId: transactionIdOf(shape),
    fieldPreimageCbor: Buffer.from(shape.fieldPreimage).toString("hex"),
    observerIndex,
  });

const forcedFinding = (
  leaf: ForcedObserverLeaf,
  sourceKey: { transactionId: string; outputIndex: bigint },
  observerIndex: number,
  rejectionReason: RejectionReason = reasonAt(observerIndex),
): ObserverOrderInvalidFinding => ({
  subject: forcedVerdictSubject({
    transactionId: leaf.transactionId,
    sourceKey,
    rejectionReason,
  }),
  observerIndex,
});

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realObserverOrderInvalid: true,
      alwaysFraudProofCatalogue: true,
      alwaysStateQueue: true,
    },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered = harness.contracts.fraudProofContracts.observerOrderInvalid;
  const category = harness.catalogue.categories.observerOrderInvalid;
  if (category === undefined) throw new Error("observer order category absent");
  expect(category.categoryId).toBe(CATEGORY_ID);
  expectRegisteredChainParity({
    registered,
    applied: applyObserverOrderInvalidScripts({
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
    OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES,
  );
  const contracts: ObserverOrderInvalidContracts = {
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
  const catalogue = harness.catalogue;
  const references: UTxO[] = [];
  let certificateReference: UTxO | undefined;
  /**
   * Published only after the block setup: the setup mint policies are
   * parameterized on the funder's nonce UTxO, which any earlier funder
   * transaction would consume.
   */
  const publishReferences = async () => {
    if (references.length > 0) return;
    for (const [index, step] of steps.entries()) {
      const published = await captureEmulatorSubmission(harness.emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `observer-order-step-${(index + 1).toString()}`,
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
        label: "observer-order-certificate",
      })
    ).utxo;
  };
  const requireCertificateReference = (): UTxO => {
    if (certificateReference === undefined)
      throw new Error("references not published");
    return certificateReference;
  };
  const stepReferences = () =>
    references as unknown as readonly [UTxO, UTxO, UTxO, UTxO];

  /**
   * Publish (and certify, when the tier requires it) the field-3 carriage
   * the way the production builder expects to resolve it. Returns the chunk
   * and certificate measurements of a certified publication.
   */
  const publishField = async (shape: ObserverFieldShape) => {
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: 3,
      anchorTxId: transactionIdOf(shape),
      nativeTxCompactCbor: compactCborHex(shape.nativeTx),
      itemCbors: shape.observers,
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
      label: `observer order field ${shape.label}`,
    });
    if (planned.plan.tier !== "Certified") {
      await publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `observer order field ${shape.label}`,
      });
      return { tier: planned.plan.tier, chunks: [], certificate: undefined };
    }
    const carriage = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `observer order field ${shape.label}`,
      }),
    );
    const certificate = await captureEmulatorSubmission(harness.emulator, () =>
      certifyFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
        certificateReferenceScriptUtxo: requireCertificateReference(),
        chunkUtxos: carriage.result,
        compactCbor: compactCborHex(shape.nativeTx),
        witnessSetCompactCbor: witnessSetCompactCborHex(shape.nativeTx),
      }),
    );
    return {
      tier: planned.plan.tier,
      chunks: carriage.measurements,
      certificate: certificate.measurement,
    };
  };
  const recordCarriage = (
    prefix: string,
    shape: ObserverFieldShape,
    published: Awaited<ReturnType<typeof publishField>>,
  ) => {
    expect(published.tier).toBe("Certified");
    expect(published.chunks).toHaveLength(MAXIMUM_CHUNKS);
    published.chunks.forEach((measurement, index) =>
      record(
        `${prefix}-carriage-chunk0${(index + 1).toString()}`,
        shape.label,
        measurement,
      ),
    );
    record(
      `${prefix}-carriage-certificate`,
      shape.label,
      published.certificate!,
    );
  };

  const init = (fraudulentBlockOutRef: string, fraudulentHeaderHash: string) =>
    captureEmulatorSubmission(harness.emulator, () =>
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
        fraudulentBlockOutRef,
        fraudulentHeaderHash,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  type Initialized = Awaited<ReturnType<typeof init>>["result"];
  const threadOf = (initialized: Initialized) =>
    `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`;
  const threadUtxoOf = async (initialized: Initialized) => {
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: initialized.txHash,
        outputIndex: initialized.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    return threadUtxo;
  };
  /** The thread output a raw continuation left at `address`. */
  const threadAfter = async (txHash: string, address: string) => {
    const next = (await harness.proverLucid.utxosAt(address)).find(
      (utxo) => utxo.txHash === txHash,
    );
    if (next === undefined) throw new Error("raw continuation output absent");
    return `${next.txHash}#${next.outputIndex.toString()}`;
  };
  const step01Accepted = async (
    initialized: Initialized,
    finding: ObserverOrderInvalidFinding,
    txInclusion: SubmitStep01TxInclusion,
    stateQueueBlockOutRef: string,
  ) =>
    captureEmulatorSubmission(harness.emulator, async () =>
      submitObserverOrderInvalidStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding,
        threadUtxo: await threadUtxoOf(initialized),
        threadToken: {
          unit: initialized.computationThreadUnit,
          fraudulentHeaderHash: initialized.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step01Forced = (
    threadOutRef: string,
    finding: ObserverOrderInvalidFinding,
    forcedSource: Readonly<Record<string, unknown>>,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitObserverOrderInvalidStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        finding,
        forcedSource,
        referenceScriptUtxo: references[0]!,
      }),
    );
  const step01ForcedRaw = (
    threadOutRef: string,
    finding: ObserverOrderInvalidFinding,
    forcedSource: Readonly<Record<string, unknown>>,
    nextStepIndex: 0 | 1 | 2 | 3,
  ) =>
    submitObserverOrderInvalidStep01ForcedRaw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      finding,
      forcedSource,
      referenceScriptUtxo: references[0]!,
      nextStepIndex,
    });
  const step02 = (
    threadOutRef: string,
    evidence: ObserverOrderInvalidEvidence,
    shape: ObserverFieldShape,
    staged: ObserverOrderInvalidStagedPlan,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitObserverOrderInvalidStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        nativeTxCompactCbor:
          evidence.subject.source_kind === 1n
            ? forcedCompact(forcedView(shape.nativeTx).compact).toString("hex")
            : compactCborHex(shape.nativeTx),
        staged,
        action: { kind: "authenticate" },
        referenceScriptUtxo: references[1]!,
      }),
    );
  const step02Raw = (
    threadOutRef: string,
    evidence: ObserverOrderInvalidEvidence,
    shape: ObserverFieldShape,
    staged: ObserverOrderInvalidStagedPlan,
    mutateOpening?: Parameters<
      typeof submitObserverOrderInvalidStep02Raw
    >[0]["mutateOpening"],
  ) =>
    submitObserverOrderInvalidStep02Raw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      evidence,
      nativeTxCompactCbor:
        evidence.subject.source_kind === 1n
          ? forcedCompact(forcedView(shape.nativeTx).compact).toString("hex")
          : compactCborHex(shape.nativeTx),
      staged,
      referenceScriptUtxo: references[1]!,
      mutateOpening,
    });
  const step03 = (
    threadOutRef: string,
    evidence: ObserverOrderInvalidEvidence,
    shape: ObserverFieldShape,
    staged: ObserverOrderInvalidStagedPlan,
    walkOrdinal: number,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitObserverOrderInvalidStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        nativeTxCompactCbor:
          evidence.subject.source_kind === 1n
            ? forcedCompact(forcedView(shape.nativeTx).compact).toString("hex")
            : compactCborHex(shape.nativeTx),
        staged,
        walkOrdinal,
        referenceScriptUtxo: references[2]!,
      }),
    );
  type Step03Exposed = Omit<
    Parameters<typeof submitObserverOrderInvalidStep03Raw>[0],
    | "lucid"
    | "contracts"
    | "categoryId"
    | "signer"
    | "threadOutRef"
    | "evidence"
    | "nativeTxCompactCbor"
    | "staged"
    | "walkOrdinal"
    | "referenceScriptUtxo"
  >;
  const step03Raw = (
    threadOutRef: string,
    evidence: ObserverOrderInvalidEvidence,
    shape: ObserverFieldShape,
    staged: ObserverOrderInvalidStagedPlan,
    walkOrdinal: number,
    exposed: Step03Exposed = {},
  ) =>
    submitObserverOrderInvalidStep03Raw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      evidence,
      nativeTxCompactCbor:
        evidence.subject.source_kind === 1n
          ? forcedCompact(forcedView(shape.nativeTx).compact).toString("hex")
          : compactCborHex(shape.nativeTx),
      staged,
      walkOrdinal,
      referenceScriptUtxo: references[2]!,
      ...exposed,
    });
  /** Every scan of the plan through the production builder, recorded. */
  const scanAll = async (
    threadOutRef: string,
    evidence: ObserverOrderInvalidEvidence,
    shape: ObserverFieldShape,
    staged: ObserverOrderInvalidStagedPlan,
    prefix: string,
  ) => {
    let cursor = threadOutRef;
    for (let ordinal = 0; ordinal < staged.walk.length; ordinal += 1) {
      const scanned = await step03(cursor, evidence, shape, staged, ordinal);
      record(scanLabel(prefix, ordinal), shape.label, scanned.measurement);
      cursor = scanned.result.nextThreadOutRef;
    }
    if (staged.walk.length > 1) coverage.resumed();
    return cursor;
  };
  const step04 = (
    threadOutRef: string,
    evidence: ObserverOrderInvalidEvidence,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitObserverOrderInvalidStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step04Raw = (threadOutRef: string) =>
    submitObserverOrderInvalidStep04Raw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      referenceScriptUtxo: references[3]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = async (threadOutRef: string, stepIndex: 0 | 1 | 2 | 3) => {
    const cancelled = await captureEmulatorSubmission(harness.emulator, () =>
      submitObserverOrderInvalidCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    coverage.cancelled(CANCELLABLE_STEPS[stepIndex]);
    return cancelled;
  };
  const removalDeploymentInfo = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue:
    // the manifest's fraudProofObserverOrderInvalid entries carry the
    // registered chain the harness built.
    return buildRemovalDeploymentInfo(harness.contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
  };
  const removal = async (fraudulentHeaderHash: string) => {
    const deploymentInfo = await removalDeploymentInfo();
    const now = BigInt(harness.emulator.now());
    const removed = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "observerOrderInvalid",
        fraudulentHeaderHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removed.result.fraudCategoryId).toBe(CATEGORY_ID);
    expect(removed.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-target",
    ]);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
    return removed;
  };
  return {
    harness,
    contracts,
    catalogue,
    category,
    publishReferences,
    requireCertificateReference,
    stepReferences,
    publishField,
    recordCarriage,
    init,
    threadOf,
    threadAfter,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    step03,
    step03Raw,
    scanAll,
    step04,
    step04Raw,
    cancel,
    removalDeploymentInfo,
    removal,
  };
};

type Harness = Awaited<ReturnType<typeof makeHarness>>;

const acceptedBlock = async (
  h: Harness,
  shapes: readonly ObserverFieldShape[],
) => {
  const block = await buildAcceptedObserverInclusions(
    shapes.map((shape) => shape.nativeTx),
  );
  const setup = await setupFraudulentBlock({
    funderLucid: h.harness.funderLucid,
    emulator: h.harness.emulator,
    contracts: h.harness.contracts,
    catalogue: h.catalogue,
    fixture: {
      transactionsRoot: block.transactionsRoot,
      l2TransactionCount: block.l2TransactionCount,
    },
  });
  await h.publishReferences();
  return { setup, inclusions: block.inclusions };
};

/**
 * One rejected forced leaf typed with `rejectionReason` under a header the
 * committed block carries; the prover's finding claims this family's reason
 * at `observerIndex`.
 */
const forcedBlock = async (
  h: Harness,
  shape: ObserverFieldShape,
  observerIndex: number,
  rejectionReason: RejectionReason = reasonAt(observerIndex),
) => {
  const credential = getAddressDetails(
    await h.harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("forced funder key absent");
  const baseFixture = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.harness.funderLucid,
        h.harness.emulator.now() + 120_000,
      ) - 1,
  });
  const sourceKey = baseFixture.eventKey.ForcedTransactionEventKey.tx_order_id;
  const leaf = await buildForcedObserverLeaf({
    shape,
    sourceKey,
    rejectionReason,
  });
  const header = {
    ...baseFixture.header,
    forcedTransactionsRoot: leaf.root.root,
  };
  const setup = await submitSetupTx({
    lucid: h.harness.funderLucid,
    contracts: h.harness.contracts,
    nonceUtxo: h.harness.nonceUtxo,
    catalogue: h.catalogue,
    header,
  });
  await h.publishReferences();
  const finding = forcedFinding(leaf, sourceKey, observerIndex);
  const source = { header, membership: leaf.membership, direction: 1n };
  return { setup, header, leaf, sourceKey, finding, source };
};

type ForcedContext = Harness &
  Awaited<ReturnType<typeof forcedBlock>> & {
    readonly shape: ObserverFieldShape;
    readonly evidence: ObserverOrderInvalidEvidence;
    readonly staged: ObserverOrderInvalidStagedPlan;
  };

/**
 * Init -> forced step 01 -> step 02 -> every scan -> step 04 proof mint ->
 * removal, every transaction recorded under `prefix`.
 */
const forcedSuccess = async (
  prefix: string,
  shape: ObserverFieldShape,
  observerIndex: number,
  beforeRemoval: (context: ForcedContext) => Promise<void> = async () => {},
) => {
  const h = await makeHarness();
  const block = await forcedBlock(h, shape, observerIndex);
  const { setup, finding, source } = block;
  const evidence = evidenceOf(shape, finding);
  const staged = stagedOf(shape, observerIndex);
  expect(evidence.violation).toBe(false);
  expect(observerOrderInvalidEvidenceCloses(evidence)).toBe(true);
  const initialized = await h.init(
    setup.fraudulentBlockOutRef,
    setup.headerHash,
  );
  record(`${prefix}-init`, shape.label, initialized.measurement);
  const bound = await h.step01Forced(
    h.threadOf(initialized.result),
    finding,
    source,
  );
  record(`${prefix}-step01`, shape.label, bound.measurement);
  const published = await h.publishField(shape);
  if (published.tier === "Certified")
    h.recordCarriage(prefix, shape, published);
  const opened = await h.step02(
    bound.result.nextThreadOutRef,
    evidence,
    shape,
    staged,
  );
  record(`${prefix}-step02`, shape.label, opened.measurement);
  const decided = await h.scanAll(
    opened.result.nextThreadOutRef,
    evidence,
    shape,
    staged,
    prefix,
  );
  const proven = await h.step04(decided, evidence);
  expect(proven.result.fraudProofUnit).toBeTruthy();
  record(`${prefix}-step04-proof-mint`, shape.label, proven.measurement);
  coverage.reason(REASON_ARM, "forced_rejection_wrong");
  coverage.scenario("wrongful_forced_rejection_success");
  await beforeRemoval({ ...h, ...block, shape, evidence, staged });
  record(
    `${prefix}-remove`,
    shape.label,
    (await h.removal(setup.headerHash)).measurement,
  );
};

/** Init -> accepted step 01 -> step 02 -> every scan -> step 04 -> removal. */
const acceptedSuccess = async (
  prefix: string,
  shape: ObserverFieldShape,
  observerIndex: number,
) => {
  const h = await makeHarness();
  const { setup, inclusions } = await acceptedBlock(h, [shape]);
  const finding = acceptedFinding(shape, observerIndex);
  const evidence = evidenceOf(shape, finding);
  const staged = stagedOf(shape, observerIndex);
  expect(evidence.violation).toBe(true);
  expect(observerOrderInvalidEvidenceCloses(evidence)).toBe(true);
  await h.publishField(shape);
  const initialized = await h.init(
    setup.fraudulentBlockOutRef,
    setup.headerHash,
  );
  record(`${prefix}-init`, shape.label, initialized.measurement);
  const bound = await h.step01Accepted(
    initialized.result,
    finding,
    inclusions[0]!,
    setup.fraudulentBlockOutRef,
  );
  record(`${prefix}-step01`, shape.label, bound.measurement);
  const opened = await h.step02(
    bound.result.nextThreadOutRef,
    evidence,
    shape,
    staged,
  );
  record(`${prefix}-step02`, shape.label, opened.measurement);
  const decided = await h.scanAll(
    opened.result.nextThreadOutRef,
    evidence,
    shape,
    staged,
    prefix,
  );
  const proven = await h.step04(decided, evidence);
  expect(proven.result.fraudProofUnit).toBeTruthy();
  record(`${prefix}-step04-proof-mint`, shape.label, proven.measurement);
  coverage.reason(REASON_ARM, "accepted_invalid");
  coverage.scenario("wrongful_acceptance_success");
  record(
    `${prefix}-remove`,
    shape.label,
    (await h.removal(setup.headerHash)).measurement,
  );
};

describe("observerOrderInvalid registered-chain lifecycle", () => {
  it("convicts the maximum accepted field at its last ordinal through the production actuator, cancels every step, refuses every accepted seam and the honest ordered field, then mints and removes", async () => {
    const h = await makeHarness();
    const maximum = maximumLastViolationShape();
    const honest = smallOrderedShape();
    expect(maximum.fieldPreimage).toHaveLength(MAXIMUM_FIELD_BYTES);
    // Adjacent over the §5.4 aggregate field bound: the field has no carriage
    // tier, so this family can neither prepare evidence for it nor open it;
    // the shared field door refuses its length on chain, and the fault it
    // carries belongs to the committed-field-shape families.
    const overBound = observerFieldShape({
      label: "over bound",
      observers: ascendingObservers(MAXIMUM_OBSERVERS + 1),
    });
    expect(() => evidenceOf(overBound, acceptedFinding(overBound, 1))).toThrow(
      /aggregate bound/u,
    );
    const { setup, inclusions } = await acceptedBlock(h, [maximum, honest]);
    const maximumInclusion = inclusions[0]!;
    const finding = acceptedFinding(maximum, LAST_ORDINAL);
    const evidence = evidenceOf(maximum, finding);
    const staged = stagedOf(maximum, LAST_ORDINAL);
    expect(evidence.carriage).toBe("Certified");
    expect(evidence.violation).toBe(true);
    expect(staged.walk).toHaveLength(MAXIMUM_SCANS);

    const published = await h.publishField(maximum);
    h.recordCarriage("accepted", maximum, published);
    const artifact = buildObserverOrderInvalidArtifact({
      headerHash: setup.headerHash,
      detectionId: `${transactionIdOf(maximum)}:accepted:${LAST_ORDINAL.toString()}`,
      position: 0n,
      evidence,
      nativeTxCompactCbor: maximumInclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: witnessSetCompactCborHex(maximum.nativeTx),
      l2TransactionSourceCbor: maximumInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: maximumInclusion.transactionsPhasRoot,
      transactionMembershipCbor: maximumInclusion.txMembershipProofCbor,
    });
    const actuator = (deploymentInfo: unknown) =>
      createObserverOrderInvalidActuator({
        binding: {
          definition: { headerHash: setup.headerHash },
          resolvedContracts: {
            category: { categoryId: h.category.categoryId },
            contracts: {
              fraudProof: {
                spendingScriptHash:
                  h.harness.contracts.fraudProof.spendingScriptHash,
              },
            },
          },
          network,
          blueprint: h.harness.realBlueprint,
          deploymentInfo,
          releaseEconomics: {
            policy: { fraudProverRewardLovelace: "400000000" },
          },
        } as never,
        lucid: h.harness.proverLucid,
        signer: h.harness.proverSigner,
        contracts: h.contracts,
        references: {
          steps: h.stepReferences(),
          witnesses: h.harness.witnessReferenceScripts as never,
          fieldPreimageCertificateMint: h.requireCertificateReference(),
        },
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "observer-order-emulator",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
      });
    const proofActuator = actuator({});
    const drive = async (
      action: Parameters<typeof proofActuator.capture>[0]["action"],
      nextAddress: string,
    ) =>
      captureEmulatorSubmission(h.harness.emulator, async () => {
        const captured = await proofActuator.capture({ action, artifact });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await h.harness.proverLucid.awaitTx(txHash);
        const next = (await h.harness.proverLucid.utxosAt(nextAddress)).find(
          (utxo) => utxo.txHash === txHash,
        );
        if (next === undefined)
          throw new Error("actuator omitted its next thread output");
        return {
          txHash,
          nextThreadOutRef: `${next.txHash}#${next.outputIndex.toString()}`,
          fraudProofUnit: Object.keys(next.assets).find(
            (unit) => unit !== "lovelace" && next.assets[unit] === 1n,
          ),
        };
      });

    // Cancel from the first three physical steps, on production threads.
    const cancelAt01 = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    record(
      "accepted-cancel-step01",
      maximum.label,
      (await h.cancel(h.threadOf(cancelAt01.result), 0)).measurement,
    );
    const cancelAt02 = await h.step01Accepted(
      (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      finding,
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    record(
      "accepted-cancel-step02",
      maximum.label,
      (await h.cancel(cancelAt02.result.nextThreadOutRef, 1)).measurement,
    );
    const cancelAt03 = await h.step02(
      (
        await h.step01Accepted(
          (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
          finding,
          maximumInclusion,
          setup.fraudulentBlockOutRef,
        )
      ).result.nextThreadOutRef,
      evidence,
      maximum,
      staged,
    );
    record(
      "accepted-cancel-step03",
      maximum.label,
      (await h.cancel(cancelAt03.result.nextThreadOutRef, 2)).measurement,
    );

    // The restarted thread is driven by the production actuator: the same
    // artifact a fresh process would admit from its journal.
    const restarted = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    record("accepted-init", maximum.label, restarted.measurement);
    const step01 = await drive(
      {
        stage: "step_01",
        threadOutRef: h.threadOf(restarted.result),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      },
      h.contracts.steps[1].spendingScriptAddress,
    );
    record("accepted-step01", maximum.label, step01.measurement);
    const step02 = await drive(
      {
        stage: "step_02",
        threadOutRef: step01.result.nextThreadOutRef,
        action: { kind: "authenticate" },
      },
      h.contracts.steps[2].spendingScriptAddress,
    );
    record("accepted-step02", maximum.label, step02.measurement);
    let cursor = step02.result.nextThreadOutRef;
    for (let ordinal = 0; ordinal < staged.walk.length; ordinal += 1) {
      const terminal = ordinal === staged.walk.length - 1;
      const scanned = await drive(
        { stage: "step_03", threadOutRef: cursor, walkOrdinal: ordinal },
        h.contracts.steps[terminal ? 3 : 2].spendingScriptAddress,
      );
      record(
        scanLabel("accepted", ordinal),
        maximum.label,
        scanned.measurement,
      );
      cursor = scanned.result.nextThreadOutRef;
    }
    coverage.resumed();
    const proven = await drive(
      { stage: "step_04", threadOutRef: cursor },
      h.harness.contracts.fraudProof.spendingScriptAddress,
    );
    expect(proven.result.fraudProofUnit).toBeTruthy();
    record("accepted-step04-proof-mint", maximum.label, proven.measurement);
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");

    const seam = async (name: Seam, build: () => Promise<unknown>) => {
      await expectOnchainRefusal(build);
      coverage.seamMutated(name);
    };

    // Step-01 seam: the transaction's membership in the committed block.
    const membershipThread = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    await seam("tx_membership", () =>
      h.step01Accepted(
        membershipThread.result,
        finding,
        { ...maximumInclusion, transactionsPhasRoot: "ff".repeat(32) },
        setup.fraudulentBlockOutRef,
      ),
    );
    await h.cancel(h.threadOf(membershipThread.result), 0);

    // Step-02 seams against one bound thread; a refused spend leaves it bound.
    const seamThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        finding,
        maximumInclusion,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await seam("native_tx_source", () =>
      h.step02Raw(seamThread, evidence, maximum, staged, (opening) =>
        mutateCompactSource(opening, compactCborHex(honest.nativeTx)),
      ),
    );
    await seam("field_certificate", () =>
      h.step02Raw(seamThread, evidence, maximum, staged, (opening) =>
        mutateCertifiedCarriage(opening, (carriage) => ({
          ...carriage,
          cert_ref_input_index: carriage.chunk_ref_input_indices[0]!,
        })),
      ),
    );
    await seam("field_chunks", () =>
      h.step02Raw(seamThread, evidence, maximum, staged, (opening) =>
        mutateCertifiedCarriage(opening, (carriage) => ({
          ...carriage,
          chunk_ref_input_indices: [
            ...carriage.chunk_ref_input_indices,
          ].reverse(),
        })),
      ),
    );
    await h.cancel(seamThread, 1);

    // Step-03 seams against a thread that already holds one real checkpoint:
    // the resume is what every mutation below must break.
    const scanThread = (
      await h.step03(
        (
          await h.step02(
            (
              await h.step01Accepted(
                (await h.init(setup.fraudulentBlockOutRef, setup.headerHash))
                  .result,
                finding,
                maximumInclusion,
                setup.fraudulentBlockOutRef,
              )
            ).result.nextThreadOutRef,
            evidence,
            maximum,
            staged,
          )
        ).result.nextThreadOutRef,
        evidence,
        maximum,
        staged,
        0,
      )
    ).result.nextThreadOutRef;
    await seam("scan_budget", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        itemBudget: BigInt(OBSERVER_ORDER_INVALID_ITEM_BUDGET + 1),
      }),
    );
    // Checkpoint bytes that do not hash to the committed checkpoint.
    const priorBytes = encodeObserverOrderWalkCheckpoint(staged.walk[0]!);
    priorBytes[priorBytes.length - 1] = priorBytes[priorBytes.length - 1]! ^ 1;
    await seam("scan_checkpoint", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        checkpointBytesHex: priorBytes.toString("hex"),
      }),
    );
    // A successor accumulator the engine never produced.
    const second = staged.walk[1]!;
    await seam("scan_successor_state", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        successor: {
          kind: "scan",
          checkpointHash: hashObserverOrderWalkCheckpoint(second),
          seen: BigInt(second.nextItemIndex + 1),
          previousObserver: observerAt(second.nextItemIndex).toString("hex"),
        },
      }),
    );
    // The right scanning state sent to the terminal script.
    await seam("scan_wrong_successor", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        nextStepIndex: 3,
      }),
    );
    // Deciding before the walk reaches the cited ordinal.
    await seam("premature_decision", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        successor: { kind: "decision", violation: true },
      }),
    );
    await h.cancel(scanThread, 2);

    // Honest accepted block: a strictly ordered field cited at ordinal 1.
    // Step 01 binds it, the scan decides `ordered`, and the terminal step
    // must refuse to convict.
    const honestFinding = acceptedFinding(honest, 1);
    const honestEvidence = evidenceOf(honest, honestFinding);
    const honestStaged = stagedOf(honest, 1);
    expect(honestEvidence.violation).toBe(false);
    expect(observerOrderInvalidEvidenceCloses(honestEvidence)).toBe(false);
    await h.publishField(honest);
    const honestBound = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        honestFinding,
        inclusions[1]!,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    // A published small field rides a RawUtxo carriage: naming the next
    // reference input (the step's own reference script) as the carriage
    // substitutes the bytes the door commits against the body.
    await seam("field_raw_utxo", () =>
      h.step02Raw(
        honestBound,
        honestEvidence,
        honest,
        honestStaged,
        (opening) => mutateRawUtxoCarriage(opening, 1n),
      ),
    );
    const honestOpened = await h.step02(
      honestBound,
      honestEvidence,
      honest,
      honestStaged,
    );
    const honestDecided = await h.step03(
      honestOpened.result.nextThreadOutRef,
      honestEvidence,
      honest,
      honestStaged,
      0,
    );
    await expectOnchainRefusal(() =>
      h.step04Raw(honestDecided.result.nextThreadOutRef),
    );
    coverage.scenario("honest_accepted_block_refusal");
    record(
      "accepted-cancel-step04",
      honest.label,
      (await h.cancel(honestDecided.result.nextThreadOutRef, 3)).measurement,
    );

    // Removal last, through the actuator's mutation-leased stage: it consumes
    // the fraudulent block every thread above bound.
    const removalActuator = actuator(await h.removalDeploymentInfo());
    vi.setSystemTime(h.harness.emulator.now());
    const removal = await captureEmulatorSubmission(
      h.harness.emulator,
      async () => {
        const captured = await removalActuator.capture({
          action: {
            stage: "remove",
            nextRemovalOutRef: setup.fraudulentBlockOutRef,
            fraudProofOutRef: proven.result.nextThreadOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await h.harness.proverLucid.awaitTx(txHash);
        return { txHash };
      },
    );
    record("accepted-remove", maximum.label, removal.measurement);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 1_800_000);

  it("convicts an accepted field whose first adjacent pair descends", async () => {
    await acceptedSuccess("accepted-first", firstPairDescendingShape(), 1);
  }, 900_000);

  it("convicts an accepted field with a duplicate at a middle ordinal", async () => {
    await acceptedSuccess("accepted-duplicate", middleDuplicateShape(), 2);
  }, 900_000);

  it("contradicts a wrongful forced rejection of the maximum ordered field at its last ordinal, refusing every forced-door seam and the flipped decision first", async () => {
    await forcedSuccess(
      "forced-maximum",
      maximumOrderedShape(),
      LAST_ORDINAL,
      async (h) => {
        const {
          setup,
          leaf,
          sourceKey,
          finding,
          source,
          header,
          shape,
          evidence,
          staged,
        } = h;
        const thread = h.threadOf(
          (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        );
        const door = async (
          seam: Seam,
          mutatedFinding: ObserverOrderInvalidFinding,
          patch: Partial<typeof source>,
        ) => {
          await expectOnchainRefusal(() =>
            h.step01Forced(thread, mutatedFinding, { ...source, ...patch }),
          );
          coverage.seamMutated(seam);
        };
        await door("forced_leaf_header", finding, {
          header: { ...header, validationTracesRoot: "ff".repeat(32) },
        });
        await door("forced_leaf_membership", finding, {
          membership: { ...leaf.membership, root: "ee".repeat(32) },
        });
        // The subject is bound from the authenticated leaf, never the finding.
        await door(
          "forced_subject_transaction",
          {
            ...finding,
            subject: forcedVerdictSubject({
              transactionId: "dd".repeat(32),
              sourceKey,
              rejectionReason: reasonAt(LAST_ORDINAL),
            }),
          },
          {},
        );
        // The leaf names ordinal 1091; a finding naming 1090 carries a
        // consistent reason of its own and is refused only by the exact
        // typed-reason binding.
        await door(
          "forced_reason_coordinate",
          forcedFinding(leaf, sourceKey, LAST_ORDINAL - 1),
          {},
        );
        coverage.scenario("reason_or_subject_coordinate_mutation");
        await door(
          "forced_direction",
          { ...finding, subject: acceptedVerdictSubject(leaf.transactionId) },
          { direction: 0n },
        );
        await expectOnchainRefusal(() =>
          h.step01ForcedRaw(thread, finding, source, 0),
        );
        coverage.seamMutated("successor_script");
        await h.cancel(thread, 0);

        // The terminal scan must carry the engine's decision, not the
        // prover's: flipping it to `violation` is refused on chain.
        const bound = await h.step01Forced(
          h.threadOf(
            (await h.init(setup.fraudulentBlockOutRef, setup.headerHash))
              .result,
          ),
          finding,
          source,
        );
        const opened = await h.step02(
          bound.result.nextThreadOutRef,
          evidence,
          shape,
          staged,
        );
        let cursor = opened.result.nextThreadOutRef;
        for (let ordinal = 0; ordinal < staged.walk.length - 1; ordinal += 1)
          cursor = (await h.step03(cursor, evidence, shape, staged, ordinal))
            .result.nextThreadOutRef;
        await expectOnchainRefusal(() =>
          h.step03Raw(cursor, evidence, shape, staged, staged.walk.length - 1, {
            successor: { kind: "decision", violation: true },
          }),
        );
        coverage.seamMutated("decision_polarity");
        await h.cancel(cursor, 2);
      },
    );
  }, 1_800_000);

  it("contradicts a wrongful forced rejection at a middle ordinal of a small ordered field", async () => {
    await forcedSuccess("forced-middle", smallOrderedShape(), 2);
  }, 900_000);

  it("contradicts a wrongful forced rejection naming an ordinal past the field's end", async () => {
    await forcedSuccess("forced-past-end", twoOrderedShape(), 7);
  }, 900_000);

  it("contradicts a wrongful forced rejection of the empty observer field", async () => {
    await forcedSuccess("forced-empty", emptyShape(), 1);
  }, 900_000);

  it("contradicts a wrongful forced rejection naming ordinal 0", async () => {
    await forcedSuccess("forced-zero", singleShape(), 0);
  }, 900_000);

  it("refuses to contradict an honest forced rejection of a duplicate observer", async () => {
    const shape = duplicateFirstShape();
    const h = await makeHarness();
    const { setup, finding, source } = await forcedBlock(h, shape, 1);
    const evidence = evidenceOf(shape, finding);
    const staged = stagedOf(shape, 1);
    expect(evidence.violation).toBe(true);
    expect(observerOrderInvalidEvidenceCloses(evidence)).toBe(false);
    const bound = await h.step01Forced(
      h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      ),
      finding,
      source,
    );
    await h.publishField(shape);
    const opened = await h.step02(
      bound.result.nextThreadOutRef,
      evidence,
      shape,
      staged,
    );
    const decided = await h.step03(
      opened.result.nextThreadOutRef,
      evidence,
      shape,
      staged,
      0,
    );
    await expectOnchainRefusal(() =>
      h.step04Raw(decided.result.nextThreadOutRef),
    );
    coverage.scenario("honest_forced_rejection_refusal");
    await h.cancel(decided.result.nextThreadOutRef, 3);
  }, 900_000);

  it("refuses to walk past an earlier offending pair toward a later cited ordinal", async () => {
    // The leaf names ordinal 2 of a field whose ordinal 1 already descends:
    // the rejection is inexact, the transaction is invalid, and the family
    // must not convict. The off-chain twin refuses to prepare evidence; the
    // hand-built plan reaches the scan validator, which refuses at item 1.
    const shape = earlierViolationShape();
    const h = await makeHarness();
    const { setup, finding, source } = await forcedBlock(h, shape, 2);
    expect(() => evidenceOf(shape, finding)).toThrow(/earlier/u);
    const evidence: ObserverOrderInvalidEvidence = {
      ...classifyObserverOrderInvalidFinding(finding),
      violation: false,
      previousObserverHex: observerAt(0).toString("hex"),
      observerHex: observerAt(2).toString("hex"),
      fieldPreimageHex: Buffer.from(shape.fieldPreimage).toString("hex"),
      fieldCommitmentHex: midgardFieldCommitment(shape.fieldPreimage).toString(
        "hex",
      ),
      carriage: "Inline",
    };
    // Ordinal 1 is a legitimate plan over the same field; only the walk
    // length differs from the cited ordinal's.
    const base = stagedOf(shape, 1);
    const staged: ObserverOrderInvalidStagedPlan = {
      ...base,
      walk: [
        {
          ...advanceMissingNativeScriptTxSemanticCheckpoint({
            checkpoint: { ...base.initialWalk, fieldIndex: 6 },
            txId: transactionIdOf(shape),
            items: base.items,
            budget: 3,
          }),
          fieldIndex: 3 as const,
        },
      ],
      violation: false,
    };
    const bound = await h.step01Forced(
      h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      ),
      finding,
      source,
    );
    await h.publishField(shape);
    const opened = await h.threadAfter(
      await h.step02Raw(bound.result.nextThreadOutRef, evidence, shape, staged),
      h.contracts.steps[2].spendingScriptAddress,
    );
    await expectOnchainRefusal(() =>
      h.step03Raw(opened, evidence, shape, staged, 0, {
        successor: { kind: "decision", violation: false },
      }),
    );
    await h.cancel(opened, 2);
  }, 900_000);

  it("refuses to bind a forced rejection whose authenticated leaf carries the sibling observer reason", async () => {
    const shape = smallOrderedShape();
    const h = await makeHarness();
    // The leaf is typed ObserversForbiddenOnUntaggedNetwork, the other reason
    // the machine maps to the same rejection code; the prover claims this
    // family's coordinate for the same transaction.
    const { setup, finding, source } = await forcedBlock(
      h,
      shape,
      2,
      "ObserversForbiddenOnUntaggedNetwork",
    );
    const thread = h.threadOf(
      (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
    );
    await expectOnchainRefusal(() => h.step01Forced(thread, finding, source));
    coverage.seamMutated("forced_leaf_reason");
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await h.cancel(thread, 0);
  }, 600_000);

  it("closes the coverage gate and the Van Rossem fit ledger", async () => {
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON_ARM],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
      resumable: true,
      // The decisive predicate has no numeric bound of its own. The only
      // bound on field 3 is the §5.4 aggregate field bound, which makes the
      // adjacent 1,093-observer field unencodable (asserted above) and is
      // owned by the shared field door and its own families.
      hasAdjacentConsensusBound: false,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: `observerOrderInvalid:${CATEGORY_ID}:testnet`,
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
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
      await writeVanRossemFitLedger(ledgerPath, ledger);
      console.info(`[observer-order-invalid-fit-ledger] wrote ${ledgerPath}`);
    }
    console.info(
      `[observer-order-invalid-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
