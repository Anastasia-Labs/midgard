import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  computeMidgardNativeTxId,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  type FieldOpening,
  forcedVerdictSubject,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  applyFieldItemWidthIllegalScripts,
  FIELD_ITEM_WIDTH_ILLEGAL_BLUEPRINT_TITLES,
  type FieldItemWidthEvidence,
  type FieldItemWidthFinding,
  type FieldItemWidthIllegalContracts,
  prepareFieldItemWidthEvidence,
  submitFieldItemWidthIllegalCancel,
  submitFieldItemWidthIllegalStep01Accepted,
  submitFieldItemWidthIllegalStep01Forced,
  submitFieldItemWidthIllegalStep02,
  submitFieldItemWidthIllegalStep03,
} from "../src/field-item-width-illegal/index.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import type { SubmitStep01TxInclusion } from "../src/step-support.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
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
import {
  boundaryLegalOutputShape,
  buildAcceptedWidthInclusions,
  buildWidthForcedFixture,
  compactCborHex,
  emptyMintItemShape,
  forcedIllegalOutputShape,
  forcedMaximumLegalOutputShape,
  MAXIMUM_FIELD_BYTES,
  maximumIllegalOutputShape,
  mintFieldTx,
  nonEmptyMintItemShape,
  submitWidthStep02Raw,
  submitWidthStep03Raw,
  type WidthShape,
  witnessSetCompactCborHex,
} from "./support/field-item-width-illegal-shapes.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const REASON_ARM = "FieldItemWidthIllegal";
const CATEGORY_ID = "00000021";
/**
 * Every place a prover-supplied value is authenticated on chain. Each is
 * mutated once against a real bound thread and must be refused by a
 * validator, not by a builder.
 */
const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "forced_leaf_header",
  "forced_leaf_membership",
  "forced_leaf_reason_coordinate",
  "forced_direction",
  "native_tx_source",
  "field_preimage_bytes",
  "field_certificate",
  "field_chunks",
  "successor_script",
] as const;
const CANCELLABLE_STEPS = ["step-01", "step-02", "step-03"] as const;

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/field-item-width-illegal-v1-fit-ledger.json",
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

const widthReason = (fieldIndex: number, itemIndex: number): RejectionReason =>
  ({
    FieldItemWidthIllegal: {
      field_index: BigInt(fieldIndex),
      item_index: BigInt(itemIndex),
    },
  }) as const;

const acceptedEvidence = (shape: WidthShape): FieldItemWidthEvidence =>
  prepareFieldItemWidthEvidence({
    finding: {
      subject: acceptedVerdictSubject(
        computeMidgardNativeTxId(shape.nativeTx).toString("hex"),
      ),
      fieldIndex: shape.fieldIndex,
      itemIndex: shape.itemIndex,
    },
    fieldPreimage: shape.fieldPreimage,
    committedFieldHashHex: midgardFieldCommitment(shape.fieldPreimage).toString(
      "hex",
    ),
  });

type CertifiedCarriage = {
  cert_ref_input_index: bigint;
  chunk_ref_input_indices: bigint[];
};

const mutateCertifiedCarriage = (
  opening: FieldOpening,
  patch: (carriage: CertifiedCarriage) => CertifiedCarriage,
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  const carriage = opening.BodyFieldOpening.carriage;
  if (!("Certified" in carriage))
    throw new Error("certified carriage expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      carriage: {
        Certified: patch({
          cert_ref_input_index: carriage.Certified.cert_ref_input_index,
          chunk_ref_input_indices: [
            ...carriage.Certified.chunk_ref_input_indices,
          ],
        }),
      },
    },
  };
};

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realFieldItemWidthIllegal: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.fieldItemWidthIllegal;
  const category = harness.catalogue.categories.fieldItemWidthIllegal;
  if (category === undefined) throw new Error("width category absent");
  expect(category.categoryId).toBe(CATEGORY_ID);
  expectRegisteredChainParity({
    registered,
    applied: applyFieldItemWidthIllegalScripts({
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
    Object.values(FIELD_ITEM_WIDTH_ILLEGAL_BLUEPRINT_TITLES),
  );
  const contracts: FieldItemWidthIllegalContracts = {
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
          label: `width-step-${(index + 1).toString()}`,
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
        label: "width-certificate",
      })
    ).utxo;
  };
  const requireCertificateReference = (): UTxO => {
    if (certificateReference === undefined)
      throw new Error("references not published");
    return certificateReference;
  };

  const init = (fraudulentBlockOutRef: string) =>
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
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  type Initialized = Awaited<ReturnType<typeof init>>["result"];
  const threadOf = (initialized: Initialized) =>
    `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`;
  const step01Accepted = async (
    initialized: Initialized,
    finding: FieldItemWidthFinding,
    txInclusion: SubmitStep01TxInclusion,
    stateQueueBlockOutRef: string,
  ) => {
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: initialized.txHash,
        outputIndex: initialized.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    return captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding,
        threadUtxo,
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
  };
  const step01Forced = (
    threadOutRef: string,
    finding: FieldItemWidthFinding,
    forcedSource: Readonly<Record<string, unknown>>,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep01Forced({
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
  const step02 = (
    threadOutRef: string,
    evidence: FieldItemWidthEvidence,
    shape: WidthShape,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep02({
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
        witnessSetCompactCbor: witnessSetCompactCborHex(shape.nativeTx),
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: requireCertificateReference(),
      }),
    );
  const step02Raw = (
    threadOutRef: string,
    evidence: FieldItemWidthEvidence,
    shape: WidthShape,
    options: {
      readonly mutateOpening?: (
        opening: FieldOpening,
        referenceInputs: readonly UTxO[],
      ) => FieldOpening;
      readonly nextStepIndex?: 1 | 2;
    },
  ) =>
    submitWidthStep02Raw({
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
      witnessSetCompactCbor: witnessSetCompactCborHex(shape.nativeTx),
      referenceScriptUtxo: references[1]!,
      certificateReferenceScriptUtxo: requireCertificateReference(),
      ...options,
    });
  const step03 = (threadOutRef: string, evidence: FieldItemWidthEvidence) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[2]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step03Raw = (threadOutRef: string) =>
    submitWidthStep03Raw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      referenceScriptUtxo: references[2]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = async (threadOutRef: string, stepIndex: 0 | 1 | 2) => {
    const cancelled = await captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalCancel({
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
  const removal = async (fraudulentHeaderHash: string) => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue.
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    const removed = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "fieldItemWidthIllegal",
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
    init,
    threadOf,
    step01Accepted,
    step01Forced,
    step02,
    step02Raw,
    step03,
    step03Raw,
    cancel,
    removal,
  };
};

type Harness = Awaited<ReturnType<typeof makeHarness>>;

/** The chunk publications, the certificate, then the step: recorded apart. */
const recordCarriage = (
  prefix: string,
  shape: string,
  captured: Awaited<ReturnType<Harness["step02"]>>,
  expectedChunks: number,
): void => {
  const all = captured.measurements;
  if (expectedChunks === 0) {
    expect(all).toHaveLength(1);
  } else {
    expect(all).toHaveLength(expectedChunks + 2);
    for (let index = 0; index < expectedChunks; index += 1)
      record(
        `${prefix}-carriage-chunk0${(index + 1).toString()}`,
        shape,
        all[index]!,
      );
    record(`${prefix}-carriage-certificate`, shape, all[expectedChunks]!);
  }
  record(`${prefix}-step02`, shape, captured.measurement);
};

const acceptedBlock = async (h: Harness, transactions: WidthShape[]) => {
  const block = await buildAcceptedWidthInclusions(
    transactions.map((shape) => shape.nativeTx),
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

const forcedBlock = async (h: Harness, shape: WidthShape) => {
  const credential = getAddressDetails(
    await h.harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("missing funder key");
  const forced = await buildWidthForcedFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.harness.funderLucid,
        h.harness.emulator.now() + 120_000,
      ) - 1,
    nativeTx: shape.nativeTx,
    rejectionReason: widthReason(shape.fieldIndex, shape.itemIndex),
  });
  const setup = await submitSetupTx({
    lucid: h.harness.funderLucid,
    contracts: h.harness.contracts,
    nonceUtxo: h.harness.nonceUtxo,
    catalogue: h.catalogue,
    header: forced.header,
  });
  await h.publishReferences();
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forced.reconstruction,
    eventKey: forced.eventKey,
  });
  const evidence = prepareFieldItemWidthEvidence({
    finding: {
      subject: forcedVerdictSubject({
        transactionId: forced.transaction.tx_id,
        sourceKey: membership.key,
        rejectionReason: forced.rejectionReason,
      }),
      fieldIndex: shape.fieldIndex,
      itemIndex: shape.itemIndex,
    },
    fieldPreimage: shape.fieldPreimage,
    committedFieldHashHex: midgardFieldCommitment(shape.fieldPreimage).toString(
      "hex",
    ),
  });
  expect(evidence.decisiveFaultHolds).toBe(shape.illegal);
  const source = { header: forced.header, membership, direction: 1n };
  return { forced, setup, membership, evidence, source };
};

/** Init → forced step 01 → step 02 → proof mint → removal, all recorded. */
const forcedSuccess = async (
  prefix: string,
  shape: WidthShape,
  expectedChunks: number,
  beforeRemoval: (
    context: Harness & Awaited<ReturnType<typeof forcedBlock>>,
  ) => Promise<void> = async () => {},
) => {
  const h = await makeHarness();
  const block = await forcedBlock(h, shape);
  const { setup, evidence, source } = block;
  const initialized = await h.init(setup.fraudulentBlockOutRef);
  record(`${prefix}-init`, shape.label, initialized.measurement);
  const bound = await h.step01Forced(
    h.threadOf(initialized.result),
    evidence,
    source,
  );
  record(`${prefix}-step01`, shape.label, bound.measurement);
  const authenticated = await h.step02(
    bound.result.nextThreadOutRef,
    evidence,
    shape,
  );
  recordCarriage(prefix, shape.label, authenticated, expectedChunks);
  const proven = await h.step03(
    authenticated.result.nextThreadOutRef,
    evidence,
  );
  expect(proven.result.fraudProofUnit).toBeTruthy();
  record(`${prefix}-step03-proof-mint`, shape.label, proven.measurement);
  coverage.reason(REASON_ARM, "forced_rejection_wrong");
  coverage.scenario("wrongful_forced_rejection_success");
  await beforeRemoval({ ...h, ...block });
  record(
    `${prefix}-remove`,
    shape.label,
    (await h.removal(setup.headerHash)).measurement,
  );
};

/** Init → forced step 01 → step 02, then the terminal step must refuse. */
const forcedHonestRefusal = async (shape: WidthShape) => {
  const h = await makeHarness();
  const { setup, evidence, source } = await forcedBlock(h, shape);
  const bound = await h.step01Forced(
    h.threadOf((await h.init(setup.fraudulentBlockOutRef)).result),
    evidence,
    source,
  );
  const authenticated = await h.step02(
    bound.result.nextThreadOutRef,
    evidence,
    shape,
  );
  await expectOnchainRefusal(() =>
    h.step03Raw(authenticated.result.nextThreadOutRef),
  );
  coverage.scenario("honest_forced_rejection_refusal");
};

describe("fieldItemWidthIllegal registered-chain lifecycle", () => {
  it("convicts the widest accepted output a maximum field can carry: cancels every step, refuses every step-02 seam, the adjacent item coordinate and the honest bound, then mints and removes", async () => {
    const h = await makeHarness();
    const maximum = maximumIllegalOutputShape();
    const legal = boundaryLegalOutputShape();
    const { setup, inclusions } = await acceptedBlock(h, [maximum, legal]);
    const maximumInclusion = inclusions[0]!;
    const legalInclusion = inclusions[1]!;
    const evidence = acceptedEvidence(maximum);
    expect(evidence.fieldPreimageHex).toHaveLength(MAXIMUM_FIELD_BYTES * 2);
    expect(evidence.carriage).toBe("Certified");
    expect(evidence.decisiveFaultHolds).toBe(true);
    const legalEvidence = acceptedEvidence(legal);
    expect(legalEvidence.itemWidth).toBe(16_384);
    expect(legalEvidence.decisiveFaultHolds).toBe(false);

    const initialized = await h.init(setup.fraudulentBlockOutRef);
    record("accepted-init", maximum.label, initialized.measurement);
    const bound = await h.step01Accepted(
      initialized.result,
      evidence,
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    record("accepted-step01", maximum.label, bound.measurement);
    const authenticated = await h.step02(
      bound.result.nextThreadOutRef,
      evidence,
      maximum,
    );
    expect(authenticated.result.carriageTier).toBe("Certified");
    recordCarriage("accepted", maximum.label, authenticated, 3);
    const proven = await h.step03(
      authenticated.result.nextThreadOutRef,
      evidence,
    );
    expect(proven.result.fraudProofUnit).toBeTruthy();
    record("accepted-step03-proof-mint", maximum.label, proven.measurement);
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");
    // Cancel from every nonterminal physical step.
    const cancelAt01 = await h.init(setup.fraudulentBlockOutRef);
    record(
      "accepted-cancel-step01",
      maximum.label,
      (await h.cancel(h.threadOf(cancelAt01.result), 0)).measurement,
    );
    const cancelAt02 = await h.step01Accepted(
      (await h.init(setup.fraudulentBlockOutRef)).result,
      evidence,
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
          (await h.init(setup.fraudulentBlockOutRef)).result,
          evidence,
          maximumInclusion,
          setup.fraudulentBlockOutRef,
        )
      ).result.nextThreadOutRef,
      evidence,
      maximum,
    );
    record(
      "accepted-cancel-step03",
      maximum.label,
      (await h.cancel(cancelAt03.result.nextThreadOutRef, 2)).measurement,
    );

    // Step-01 seam: the transaction's membership in the committed block.
    const membershipThread = await h.init(setup.fraudulentBlockOutRef);
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

    // Step-02 seams against one bound thread; a refused spend leaves it bound.
    const seamThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef)).result,
        evidence,
        maximumInclusion,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    const otherCompact = compactCborHex(legal.nativeTx, 0n);
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, {
        mutateOpening: (opening) => {
          if (!("BodyFieldOpening" in opening))
            throw new Error("body opening expected");
          return {
            BodyFieldOpening: {
              ...opening.BodyFieldOpening,
              native_tx_compact_cbor: otherCompact,
            },
          };
        },
      }),
    );
    coverage.seamMutated("native_tx_source");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, {
        mutateOpening: (opening) =>
          mutateCertifiedCarriage(opening, (carriage) => ({
            ...carriage,
            cert_ref_input_index: carriage.chunk_ref_input_indices[0]!,
          })),
      }),
    );
    coverage.seamMutated("field_certificate");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, {
        mutateOpening: (opening) =>
          mutateCertifiedCarriage(opening, (carriage) => ({
            ...carriage,
            chunk_ref_input_indices: [
              ...carriage.chunk_ref_input_indices,
            ].reverse(),
          })),
      }),
    );
    coverage.seamMutated("field_chunks");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, { nextStepIndex: 1 }),
    );
    coverage.seamMutated("successor_script");
    await h.cancel(seamThread, 1);

    // Adjacent-over-bound coordinate: the field has one item, the thread binds
    // index 1, and the door refuses the read at the item-count bound.
    const overBound = await h.step01Accepted(
      (await h.init(setup.fraudulentBlockOutRef)).result,
      { subject: evidence.subject, fieldIndex: 2, itemIndex: 1 },
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(overBound.result.nextThreadOutRef, evidence, maximum, {}),
    );
    coverage.adjacentOverBoundRefused();
    await h.cancel(overBound.result.nextThreadOutRef, 1);

    // Honest accepted block: the adjacent legal width, exactly at the bound,
    // authenticates but the terminal step refuses to convict it.
    const honest = await h.step02(
      (
        await h.step01Accepted(
          (await h.init(setup.fraudulentBlockOutRef)).result,
          legalEvidence,
          legalInclusion,
          setup.fraudulentBlockOutRef,
        )
      ).result.nextThreadOutRef,
      legalEvidence,
      legal,
    );
    await expectOnchainRefusal(() =>
      h.step03Raw(honest.result.nextThreadOutRef),
    );
    coverage.scenario("honest_accepted_block_refusal");
    await h.cancel(honest.result.nextThreadOutRef, 2);

    // Removal last: it consumes the fraudulent block every thread above bound.
    record(
      "accepted-remove",
      maximum.label,
      (await h.removal(setup.headerHash)).measurement,
    );
  }, 600_000);

  it("convicts an accepted empty mint-policy item through inline carriage, refuses substituted field bytes and the honest non-empty item, then mints and removes", async () => {
    const h = await makeHarness();
    const nativeTx = mintFieldTx();
    const empty = emptyMintItemShape(nativeTx);
    const nonEmpty = nonEmptyMintItemShape(nativeTx);
    const { setup, inclusions } = await acceptedBlock(h, [empty]);
    const inclusion = inclusions[0]!;
    const evidence = acceptedEvidence(empty);
    expect(evidence.carriage).toBe("Inline");
    expect(evidence.itemWidth).toBe(0);
    expect(evidence.decisiveFaultHolds).toBe(true);

    const initialized = await h.init(setup.fraudulentBlockOutRef);
    record("accepted-mint-init", empty.label, initialized.measurement);
    const bound = await h.step01Accepted(
      initialized.result,
      evidence,
      inclusion,
      setup.fraudulentBlockOutRef,
    );
    record("accepted-mint-step01", empty.label, bound.measurement);
    const authenticated = await h.step02(
      bound.result.nextThreadOutRef,
      evidence,
      empty,
    );
    expect(authenticated.result.carriageTier).toBe("Inline");
    recordCarriage("accepted-mint", empty.label, authenticated, 0);
    const proven = await h.step03(
      authenticated.result.nextThreadOutRef,
      evidence,
    );
    expect(proven.result.fraudProofUnit).toBeTruthy();
    record("accepted-mint-step03-proof-mint", empty.label, proven.measurement);
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    // Inline carriage exposes the bytes themselves: one flipped byte breaks
    // the field commitment and the door refuses the opening.
    const seamThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef)).result,
        evidence,
        inclusion,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, empty, {
        mutateOpening: (opening) => {
          if (!("BodyFieldOpening" in opening))
            throw new Error("body opening expected");
          const carriage = opening.BodyFieldOpening.carriage;
          if (!("Inline" in carriage)) throw new Error("inline expected");
          const bytes = Buffer.from(carriage.Inline.preimage, "hex");
          bytes[bytes.length - 1] = bytes[bytes.length - 1]! ^ 0x01;
          return {
            BodyFieldOpening: {
              ...opening.BodyFieldOpening,
              carriage: { Inline: { preimage: bytes.toString("hex") } },
            },
          };
        },
      }),
    );
    coverage.seamMutated("field_preimage_bytes");
    await h.cancel(seamThread, 1);

    // Honest accepted block on field 5: the 29-byte item is legal.
    const honestEvidence = acceptedEvidence(nonEmpty);
    expect(honestEvidence.decisiveFaultHolds).toBe(false);
    const honest = await h.step02(
      (
        await h.step01Accepted(
          (await h.init(setup.fraudulentBlockOutRef)).result,
          honestEvidence,
          inclusion,
          setup.fraudulentBlockOutRef,
        )
      ).result.nextThreadOutRef,
      honestEvidence,
      nonEmpty,
    );
    await expectOnchainRefusal(() =>
      h.step03Raw(honest.result.nextThreadOutRef),
    );
    coverage.scenario("honest_accepted_block_refusal");
    await h.cancel(honest.result.nextThreadOutRef, 2);

    record(
      "accepted-mint-remove",
      empty.label,
      (await h.removal(setup.headerHash)).measurement,
    );
  }, 600_000);

  it("contradicts a wrongful forced rejection of the maximum legal output field, refusing every forced-door seam first", async () => {
    const shape = forcedMaximumLegalOutputShape();
    await forcedSuccess("forced", shape, 3, async (h) => {
      const { forced, setup, membership, evidence, source } = h;
      const thread = h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef)).result,
      );
      const door = async (
        seam: (typeof AUTHENTICATION_SEAMS)[number],
        finding: FieldItemWidthFinding,
        patch: Partial<typeof source>,
      ) => {
        await expectOnchainRefusal(() =>
          h.step01Forced(thread, finding, { ...source, ...patch }),
        );
        coverage.seamMutated(seam);
      };
      // The leaf rejects item 1; the thread binds item 0 of the same field.
      await door(
        "forced_leaf_reason_coordinate",
        {
          subject: forcedVerdictSubject({
            transactionId: forced.transaction.tx_id,
            sourceKey: membership.key,
            rejectionReason: widthReason(2, 0),
          }),
          fieldIndex: 2,
          itemIndex: 0,
        },
        {},
      );
      coverage.scenario("reason_or_subject_coordinate_mutation");
      await door("forced_leaf_header", evidence, {
        header: { ...forced.header, validationTracesRoot: "ff".repeat(32) },
      });
      await door("forced_leaf_membership", evidence, {
        membership: { ...membership, root: "ee".repeat(32) },
      });
      await door(
        "forced_direction",
        {
          subject: acceptedVerdictSubject(forced.transaction.tx_id),
          fieldIndex: 2,
          itemIndex: 1,
        },
        { direction: 0n },
      );
      await h.cancel(thread, 0);
    });
  }, 600_000);

  it("contradicts a wrongful forced rejection of a non-empty mint-policy item", async () => {
    await forcedSuccess("forced-mint", nonEmptyMintItemShape(), 0);
  }, 600_000);

  it("refuses to contradict an honest forced rejection of an output one byte over the bound", async () => {
    await forcedHonestRefusal(forcedIllegalOutputShape());
  }, 600_000);

  it("refuses to contradict an honest forced rejection of an empty mint-policy item", async () => {
    await forcedHonestRefusal(emptyMintItemShape());
  }, 600_000);

  it("closes the coverage gate and the Van Rossem fit ledger", async () => {
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON_ARM],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
      resumable: false,
      hasAdjacentConsensusBound: true,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: "fieldItemWidthIllegal:00000021:testnet",
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
      console.info(`[field-item-width-illegal-fit-ledger] wrote ${ledgerPath}`);
    }
    console.info(
      `[field-item-width-illegal-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
