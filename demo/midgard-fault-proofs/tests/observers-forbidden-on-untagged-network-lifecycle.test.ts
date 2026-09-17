import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { midgardFieldCommitment } from "@al-ft/midgard-core";
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
import { createObserversForbiddenActuator } from "../src/observers-forbidden-on-untagged-network/actuator.js";
import { buildObserversForbiddenArtifact } from "../src/observers-forbidden-on-untagged-network/artifact.js";
import {
  applyObserversForbiddenScripts,
  OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES,
  type ObserversForbiddenContracts,
} from "../src/observers-forbidden-on-untagged-network/contracts.js";
import {
  MIDGARD_ABSENT_SCRIPT_INTEGRITY_HASH,
  type ObserversForbiddenEvidence,
  observersForbiddenEvidenceCloses,
  type ObserversForbiddenFinding,
  prepareObserversForbiddenEvidence,
} from "../src/observers-forbidden-on-untagged-network/family.js";
import { submitObserversForbiddenCancel } from "../src/observers-forbidden-on-untagged-network/submit-cancel.js";
import {
  submitObserversForbiddenStep01Accepted,
  submitObserversForbiddenStep01Forced,
} from "../src/observers-forbidden-on-untagged-network/submit-step-01.js";
import { submitObserversForbiddenStep02 } from "../src/observers-forbidden-on-untagged-network/submit-step-02.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import type { SubmitStep01TxInclusion } from "../src/submit-step-01.js";
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
  buildAcceptedObserverInclusions,
  buildForcedObserverLeaf,
  compactCborHex,
  type ForcedObserverLeaf,
  mutateCertifiedCarriage,
  mutateCompactSource,
  mutateRawUtxoCarriage,
  observerHashes,
  type ObserverShape,
  observerShape,
  submitObserversForbiddenStep01ForcedRaw,
  submitObserversForbiddenStep02Raw,
  transactionIdOf,
  witnessSetCompactCborHex,
} from "./support/observers-forbidden-on-untagged-network-raw.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const REASON_ARM = "ObserversForbiddenOnUntaggedNetwork";
const CATEGORY_ID = "00000024";
/** A present integrity hash: the phase-A observer arm is reachable. */
const PRESENT_HASH = "ab".repeat(32);
const ABSENT_HASH = MIDGARD_ABSENT_SCRIPT_INTEGRITY_HASH;
/**
 * The observer frontier the shared bounded field surface accepts: a
 * three-byte array header plus 505 canonical 28-byte hashes, 15,153 bytes,
 * which forces certified carriage over two chunks.
 */
const MAXIMUM_OBSERVERS = 505;
const MAXIMUM_FIELD_BYTES = 15_153;
const MAXIMUM_CHUNKS = 2;

/**
 * Every place a prover-supplied value is authenticated on chain. Each is
 * mutated once against a real bound thread and must be refused by a
 * validator, not by a builder.
 */
const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "accepted_network_scalar",
  "forced_leaf_header",
  "forced_leaf_membership",
  "forced_leaf_reason",
  "forced_subject_transaction",
  "forced_direction",
  "successor_script",
  "native_tx_source",
  "field_raw_utxo",
  "field_certificate",
  "field_chunks",
] as const;
const CANCELLABLE_STEPS = ["step-01", "step-02"] as const;

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/observers-forbidden-on-untagged-network-v1-fit-ledger.json",
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

// ---------------------------------------------------------------------------
// Shapes: every polarity of observer count, network scalar, integrity hash
// ---------------------------------------------------------------------------

const maximumUntaggedShape = () =>
  observerShape({
    label: `${MAXIMUM_OBSERVERS.toString()} observers on scalar 255 under a present integrity hash (${MAXIMUM_FIELD_BYTES.toString()}-byte certified field)`,
    observerCount: MAXIMUM_OBSERVERS,
    networkId: 255,
    scriptIntegrityHash: PRESENT_HASH,
  });
/** Accepted by the machine: no Plutus evaluation, so the arm is unreachable. */
const nativeOnlyShape = (fee = 11n) =>
  observerShape({
    label:
      "1 observer on scalar 255 under the absent integrity hash (native-only, inline field)",
    observerCount: 1,
    networkId: 255,
    scriptIntegrityHash: ABSENT_HASH,
    fee,
  });
const emptyUntaggedShape = (fee = 13n) =>
  observerShape({
    label:
      "0 observers on scalar 255 under a present integrity hash (inline field)",
    observerCount: 0,
    networkId: 255,
    scriptIntegrityHash: PRESENT_HASH,
    fee,
  });
const maximumTaggedShape = () =>
  observerShape({
    label: `${MAXIMUM_OBSERVERS.toString()} observers on scalar 1 under a present integrity hash (${MAXIMUM_FIELD_BYTES.toString()}-byte certified field)`,
    observerCount: MAXIMUM_OBSERVERS,
    networkId: 1,
    scriptIntegrityHash: PRESENT_HASH,
  });
const honestForcedShape = () =>
  observerShape({
    label:
      "1 observer on scalar 255 under a present integrity hash (inline field)",
    observerCount: 1,
    networkId: 255,
    scriptIntegrityHash: PRESENT_HASH,
  });

const acceptedFinding = (
  shape: ObserverShape,
  patch: Partial<ObserversForbiddenFinding> = {},
): ObserversForbiddenFinding => ({
  subject: acceptedVerdictSubject(transactionIdOf(shape)),
  networkId: shape.networkId,
  scriptIntegrityHash: shape.scriptIntegrityHash,
  ...patch,
});

const evidenceOf = (
  shape: ObserverShape,
  finding: ObserversForbiddenFinding,
): ObserversForbiddenEvidence =>
  prepareObserversForbiddenEvidence({
    finding,
    observerFieldPreimage: shape.fieldPreimage,
    committedFieldHashHex: midgardFieldCommitment(shape.fieldPreimage).toString(
      "hex",
    ),
  });

const acceptedEvidence = (shape: ObserverShape) =>
  evidenceOf(shape, acceptedFinding(shape));

const forcedFinding = (
  shape: ObserverShape,
  leaf: ForcedObserverLeaf,
  sourceKey: { transactionId: string; outputIndex: bigint },
  rejectionReason: RejectionReason = REASON_ARM,
): ObserversForbiddenFinding => ({
  subject: forcedVerdictSubject({
    transactionId: leaf.transactionId,
    sourceKey,
    rejectionReason,
  }),
  networkId: shape.networkId,
  scriptIntegrityHash: shape.scriptIntegrityHash,
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
      realObserversForbiddenOnUntaggedNetwork: true,
      alwaysFraudProofCatalogue: true,
      alwaysStateQueue: true,
    },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.observersForbiddenOnUntaggedNetwork;
  const category =
    harness.catalogue.categories.observersForbiddenOnUntaggedNetwork;
  if (category === undefined) throw new Error("observers category absent");
  expect(category.categoryId).toBe(CATEGORY_ID);
  expectRegisteredChainParity({
    registered,
    applied: applyObserversForbiddenScripts({
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
    OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES,
  );
  const contracts: ObserversForbiddenContracts = {
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
          label: `observers-step-${(index + 1).toString()}`,
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
        label: "observers-certificate",
      })
    ).utxo;
  };
  const requireCertificateReference = (): UTxO => {
    if (certificateReference === undefined)
      throw new Error("references not published");
    return certificateReference;
  };
  const stepReferences = () => references as unknown as readonly [UTxO, UTxO];

  /**
   * Publish (and certify, when the tier requires it) the field-3 carriage
   * the way the production builder expects to resolve it. Returns the chunk
   * and certificate measurements of a certified publication.
   */
  const publishField = async (
    shape: ObserverShape,
    sourceKind: 0n | 1n = 0n,
  ) => {
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: sourceKind,
      fieldIndex: 3,
      anchorTxId: transactionIdOf(shape),
      nativeTxCompactCbor: compactCborHex(shape.nativeTx, sourceKind),
      itemCbors: observerHashes(shape.observerCount),
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
      label: `observers field ${shape.label}`,
    });
    if (planned.plan.tier !== "Certified") {
      await publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `observers field ${shape.label}`,
      });
      return { tier: planned.plan.tier, chunks: [], certificate: undefined };
    }
    const carriage = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `observers field ${shape.label}`,
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
        compactCbor: compactCborHex(shape.nativeTx, sourceKind),
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
    shape: ObserverShape,
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
  const step01Accepted = async (
    initialized: Initialized,
    finding: ObserversForbiddenFinding,
    txInclusion: SubmitStep01TxInclusion,
    stateQueueBlockOutRef: string,
  ) =>
    captureEmulatorSubmission(harness.emulator, async () =>
      submitObserversForbiddenStep01Accepted({
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
    finding: ObserversForbiddenFinding,
    forcedSource: Readonly<Record<string, unknown>>,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitObserversForbiddenStep01Forced({
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
    finding: ObserversForbiddenFinding,
    forcedSource: Readonly<Record<string, unknown>>,
    nextStepIndex: 0 | 1,
  ) =>
    submitObserversForbiddenStep01ForcedRaw({
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
    evidence: ObserversForbiddenEvidence,
    shape: ObserverShape,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitObserversForbiddenStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: compactCborHex(
          shape.nativeTx,
          evidence.subject.source_kind,
        ),
        referenceScriptUtxo: references[1]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step02Raw = (
    threadOutRef: string,
    evidence: ObserversForbiddenEvidence,
    shape: ObserverShape,
    mutateOpening?: Parameters<
      typeof submitObserversForbiddenStep02Raw
    >[0]["mutateOpening"],
  ) =>
    submitObserversForbiddenStep02Raw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      evidence,
      nativeTxCompactCbor: compactCborHex(
        shape.nativeTx,
        evidence.subject.source_kind,
      ),
      referenceScriptUtxo: references[1]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      mutateOpening,
    });
  const cancel = async (threadOutRef: string, stepIndex: 0 | 1) => {
    const cancelled = await captureEmulatorSubmission(harness.emulator, () =>
      submitObserversForbiddenCancel({
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
    // the manifest's fraudProofObserversForbiddenOnUntaggedNetwork entries
    // carry the registered chain the harness built.
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
        fraudCategory: "observersForbiddenOnUntaggedNetwork",
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
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    cancel,
    removalDeploymentInfo,
    removal,
  };
};

type Harness = Awaited<ReturnType<typeof makeHarness>>;

const acceptedBlock = async (h: Harness, shapes: readonly ObserverShape[]) => {
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
 * committed block carries; the prover's finding claims this family's reason.
 */
const forcedBlock = async (
  h: Harness,
  shape: ObserverShape,
  rejectionReason: RejectionReason = REASON_ARM,
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
  const finding = forcedFinding(shape, leaf, sourceKey);
  const evidence = evidenceOf(shape, finding);
  const source = { header, membership: leaf.membership, direction: 1n };
  return { setup, header, leaf, sourceKey, finding, evidence, source };
};

/** Init -> forced step 01 -> step 02 proof mint, every step recorded. */
const forcedSuccess = async (
  prefix: string,
  shape: ObserverShape,
  beforeRemoval: (
    context: Harness & Awaited<ReturnType<typeof forcedBlock>>,
  ) => Promise<void> = async () => {},
) => {
  const h = await makeHarness();
  const block = await forcedBlock(h, shape);
  const { setup, evidence, source } = block;
  expect(observersForbiddenEvidenceCloses(evidence)).toBe(true);
  const initialized = await h.init(
    setup.fraudulentBlockOutRef,
    setup.headerHash,
  );
  record(`${prefix}-init`, shape.label, initialized.measurement);
  const bound = await h.step01Forced(
    h.threadOf(initialized.result),
    evidence,
    source,
  );
  record(`${prefix}-step01`, shape.label, bound.measurement);
  const published = await h.publishField(shape, 1n);
  if (published.tier === "Certified")
    h.recordCarriage(prefix, shape, published);
  const proven = await h.step02(bound.result.nextThreadOutRef, evidence, shape);
  expect(proven.result.fraudProofUnit).toBeTruthy();
  record(`${prefix}-step02-proof-mint`, shape.label, proven.measurement);
  coverage.reason(REASON_ARM, "forced_rejection_wrong");
  coverage.scenario("wrongful_forced_rejection_success");
  await beforeRemoval({ ...h, ...block });
  record(
    `${prefix}-remove`,
    shape.label,
    (await h.removal(setup.headerHash)).measurement,
  );
};

describe("observersForbiddenOnUntaggedNetwork registered-chain lifecycle", () => {
  it("convicts the maximum accepted observer field on scalar 255 through the production actuator, cancels both steps, refuses every accepted seam and both honest accepted polarities, then mints and removes", async () => {
    const h = await makeHarness();
    const maximum = maximumUntaggedShape();
    const honestNative = nativeOnlyShape();
    const honestEmpty = emptyUntaggedShape();
    const { setup, inclusions } = await acceptedBlock(h, [
      maximum,
      honestNative,
      honestEmpty,
    ]);
    const maximumInclusion = inclusions[0]!;
    const evidence = acceptedEvidence(maximum);
    expect(evidence.observerFieldPreimageCbor).toHaveLength(
      MAXIMUM_FIELD_BYTES * 2,
    );
    expect(evidence.observerCount).toBe(MAXIMUM_OBSERVERS);
    expect(evidence.carriage).toBe("Certified");
    expect(observersForbiddenEvidenceCloses(evidence)).toBe(true);

    const published = await h.publishField(maximum);
    h.recordCarriage("accepted", maximum, published);
    const artifact = buildObserversForbiddenArtifact({
      headerHash: setup.headerHash,
      detectionId: `${transactionIdOf(maximum)}:accepted`,
      position: 0n,
      evidence,
      nativeTxCompactCbor: maximumInclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: witnessSetCompactCborHex(maximum.nativeTx),
      l2TransactionSourceCbor: maximumInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: maximumInclusion.transactionsPhasRoot,
      transactionMembershipCbor: maximumInclusion.txMembershipProofCbor,
    });
    const actuator = (deploymentInfo: unknown) =>
      createObserversForbiddenActuator({
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
            token: "observers-forbidden-emulator",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
      });
    const proofActuator = actuator({});

    // Cancel from every nonterminal physical step.
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
      evidence,
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    record(
      "accepted-cancel-step02",
      maximum.label,
      (await h.cancel(cancelAt02.result.nextThreadOutRef, 1)).measurement,
    );

    // The restarted thread is driven by the production actuator: the same
    // artifact a fresh process would admit from its journal.
    const restarted = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    record("accepted-init", maximum.label, restarted.measurement);
    const step01 = await captureEmulatorSubmission(
      h.harness.emulator,
      async () => {
        const captured = await proofActuator.capture({
          action: {
            stage: "step_01",
            threadOutRef: h.threadOf(restarted.result),
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await h.harness.proverLucid.awaitTx(txHash);
        const next = (
          await h.harness.proverLucid.utxosAt(
            h.contracts.steps[1].spendingScriptAddress,
          )
        ).find((utxo) => utxo.txHash === txHash);
        if (next === undefined)
          throw new Error("actuator step-01 output absent");
        return {
          txHash,
          nextThreadOutRef: `${next.txHash}#${next.outputIndex.toString()}`,
        };
      },
    );
    record("accepted-step01", maximum.label, step01.measurement);
    const proven = await captureEmulatorSubmission(
      h.harness.emulator,
      async () => {
        const captured = await proofActuator.capture({
          action: {
            stage: "step_02",
            threadOutRef: step01.result.nextThreadOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await h.harness.proverLucid.awaitTx(txHash);
        const proof = (
          await h.harness.proverLucid.utxosAt(
            h.harness.contracts.fraudProof.spendingScriptAddress,
          )
        ).find((utxo) => utxo.txHash === txHash);
        if (proof === undefined)
          throw new Error("actuator proof output absent");
        return {
          txHash,
          fraudProofOutRef: `${proof.txHash}#${proof.outputIndex.toString()}`,
          fraudProofUnit: Object.keys(proof.assets).find(
            (unit) => unit !== "lovelace" && proof.assets[unit] === 1n,
          ),
        };
      },
    );
    expect(proven.result.fraudProofUnit).toBeTruthy();
    record("accepted-step02-proof-mint", maximum.label, proven.measurement);
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");

    // Step-01 seam: the transaction's membership in the committed block.
    const membershipThread = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    await expectOnchainRefusal(() =>
      h.step01Accepted(
        membershipThread.result,
        evidence,
        { ...maximumInclusion, transactionsPhasRoot: "ff".repeat(32) },
        setup.fraudulentBlockOutRef,
      ),
    );
    coverage.seamMutated("tx_membership");
    await h.cancel(h.threadOf(membershipThread.result), 0);

    // Step-01 seam: the bound scalar must be the compact body's, not the
    // prover's. Scalar 1 is a canonical value, so only the validator refuses.
    const scalarThread = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    await expectOnchainRefusal(() =>
      h.step01Accepted(
        scalarThread.result,
        acceptedFinding(maximum, { networkId: 1 }),
        maximumInclusion,
        setup.fraudulentBlockOutRef,
      ),
    );
    coverage.seamMutated("accepted_network_scalar");
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await h.cancel(h.threadOf(scalarThread.result), 0);

    // Step-02 seams against one bound thread; a refused spend leaves it bound.
    const seamThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        evidence,
        maximumInclusion,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, (opening) =>
        mutateCompactSource(opening, compactCborHex(honestNative.nativeTx)),
      ),
    );
    coverage.seamMutated("native_tx_source");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, (opening) =>
        mutateCertifiedCarriage(opening, (carriage) => ({
          ...carriage,
          cert_ref_input_index: carriage.chunk_ref_input_indices[0]!,
        })),
      ),
    );
    coverage.seamMutated("field_certificate");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, (opening) =>
        mutateCertifiedCarriage(opening, (carriage) => ({
          ...carriage,
          chunk_ref_input_indices: [
            ...carriage.chunk_ref_input_indices,
          ].reverse(),
        })),
      ),
    );
    coverage.seamMutated("field_chunks");
    await h.cancel(seamThread, 1);

    // Honest accepted block, native-only polarity: observers on scalar 255
    // under the absent integrity hash need no Plutus evaluation, so the
    // machine accepts the transaction. Step 01 binds it; the terminal step
    // must refuse to convict.
    const honestNativeEvidence = acceptedEvidence(honestNative);
    expect(honestNativeEvidence.carriage).toBe("Inline");
    expect(observersForbiddenEvidenceCloses(honestNativeEvidence)).toBe(false);
    await h.publishField(honestNative);
    const honestNativeThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        honestNativeEvidence,
        inclusions[1]!,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await expectOnchainRefusal(() =>
      h.step02Raw(honestNativeThread, honestNativeEvidence, honestNative),
    );
    coverage.scenario("honest_accepted_block_refusal");
    await h.cancel(honestNativeThread, 1);

    // Honest accepted block, empty polarity: no observers on scalar 255.
    const honestEmptyEvidence = acceptedEvidence(honestEmpty);
    expect(honestEmptyEvidence.observerCount).toBe(0);
    expect(observersForbiddenEvidenceCloses(honestEmptyEvidence)).toBe(false);
    await h.publishField(honestEmpty);
    const honestEmptyThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        honestEmptyEvidence,
        inclusions[2]!,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await expectOnchainRefusal(() =>
      h.step02Raw(honestEmptyThread, honestEmptyEvidence, honestEmpty),
    );
    await h.cancel(honestEmptyThread, 1);

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
            fraudProofOutRef: proven.result.fraudProofOutRef,
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
  }, 900_000);

  it("contradicts a wrongful forced rejection of the empty observer field on scalar 255, refusing every forced-door seam and a substituted published carriage first", async () => {
    const shape = emptyUntaggedShape();
    await forcedSuccess("forced-empty", shape, async (h) => {
      const { setup, leaf, sourceKey, finding, evidence, source, header } = h;
      expect(evidence.carriage).toBe("Inline");
      const thread = h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      );
      const door = async (
        seam: (typeof AUTHENTICATION_SEAMS)[number],
        mutatedFinding: ObserversForbiddenFinding,
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
            rejectionReason: REASON_ARM,
          }),
        },
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

      // A published small field rides a RawUtxo carriage: naming the next
      // reference input (the step's own reference script) as the carriage
      // substitutes the bytes the door commits against the body.
      const bytesThread = (
        await h.step01Forced(
          h.threadOf(
            (await h.init(setup.fraudulentBlockOutRef, setup.headerHash))
              .result,
          ),
          evidence,
          source,
        )
      ).result.nextThreadOutRef;
      await expectOnchainRefusal(() =>
        h.step02Raw(bytesThread, evidence, shape, (opening) =>
          mutateRawUtxoCarriage(opening, 1n),
        ),
      );
      coverage.seamMutated("field_raw_utxo");
      await h.cancel(bytesThread, 1);
    });
  }, 900_000);

  it("contradicts a wrongful forced rejection of the maximum observer field on a tagged scalar", async () => {
    await forcedSuccess("forced-tagged", maximumTaggedShape());
  }, 900_000);

  it("contradicts a wrongful forced rejection of a native-only transaction carrying observers on scalar 255", async () => {
    await forcedSuccess("forced-native", nativeOnlyShape());
  }, 900_000);

  it("refuses to contradict an honest forced rejection of observers on scalar 255 under a present integrity hash", async () => {
    const shape = honestForcedShape();
    const h = await makeHarness();
    const { setup, evidence, source } = await forcedBlock(h, shape);
    expect(observersForbiddenEvidenceCloses(evidence)).toBe(false);
    const bound = await h.step01Forced(
      h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      ),
      evidence,
      source,
    );
    await h.publishField(shape, 1n);
    await expectOnchainRefusal(() =>
      h.step02Raw(bound.result.nextThreadOutRef, evidence, shape),
    );
    coverage.scenario("honest_forced_rejection_refusal");
    await h.cancel(bound.result.nextThreadOutRef, 1);
  }, 900_000);

  it("refuses to bind a forced rejection whose authenticated leaf carries a sibling tx-global reason", async () => {
    const shape = honestForcedShape();
    const h = await makeHarness();
    // The leaf is typed NetworkIdMismatch; the prover claims this family's
    // reason for the same transaction. Both are tx-global with no
    // coordinates, so only the exact typed-reason binding separates them.
    const { setup, leaf, sourceKey, source } = await forcedBlock(
      h,
      shape,
      "NetworkIdMismatch",
    );
    const claimed = forcedFinding(shape, leaf, sourceKey);
    const thread = h.threadOf(
      (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
    );
    await expectOnchainRefusal(() => h.step01Forced(thread, claimed, source));
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
      // Two physical steps: the family closes in one step-02 transaction and
      // declares no checkpoint to resume from.
      resumable: false,
      // The decisive predicate has no numeric bound of its own; the observer
      // count bound belongs to the shared field door and its own families.
      hasAdjacentConsensusBound: false,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: `observersForbiddenOnUntaggedNetwork:${CATEGORY_ID}:testnet`,
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
      console.info(
        `[observers-forbidden-on-untagged-network-fit-ledger] wrote ${ledgerPath}`,
      );
    }
    console.info(
      `[observers-forbidden-on-untagged-network-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
