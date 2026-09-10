import { selectMidgardFieldCarriageTier } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import {
  applyMissingRedeemerScripts,
  type MissingRedeemerContracts,
} from "../src/missing-redeemer/contracts.js";
import {
  MISSING_REDEEMER_CATEGORY_ID,
  MISSING_REDEEMER_SCAN_BATCH,
  type MissingRedeemerPurposeKind,
} from "../src/missing-redeemer/family.js";
import {
  buildMissingRedeemerMaterialFromRetainedDa,
  type MissingRedeemerMaterial,
} from "../src/missing-redeemer/replay.js";
import {
  type MissingRedeemerDecisionSchema,
  MissingRedeemerStep05DatumSchema,
  MissingRedeemerStep05RedeemerSchema,
} from "../src/missing-redeemer/schemas.js";
import {
  type MissingRedeemerStagedPlan,
  planMissingRedeemerStagedWalk,
} from "../src/missing-redeemer/staged-plan.js";
import {
  submitMissingRedeemerStep02,
  submitMissingRedeemerStep02a,
  submitMissingRedeemerStep02b,
} from "../src/missing-redeemer/submit-authentication.js";
import { submitMissingRedeemerCancel } from "../src/missing-redeemer/submit-cancel.js";
import {
  type MissingRedeemerStep03Action,
  planMissingRedeemerFieldOpening,
  submitMissingRedeemerStep03,
  submitMissingRedeemerStep04,
} from "../src/missing-redeemer/submit-field-scan.js";
import { submitMissingRedeemerInit } from "../src/missing-redeemer/submit-init.js";
import {
  submitMissingRedeemerStep01Accepted,
  submitMissingRedeemerStep01Forced,
} from "../src/missing-redeemer/submit-step-01.js";
import { submitMissingRedeemerStep05 } from "../src/missing-redeemer/submit-step-05.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  assertCompleteLifecycleCoverage,
  type CompleteLifecycleBaseScenario,
} from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import {
  buildMissingRedeemerFixture,
  MISSING_REDEEMER_MAXIMUM_FIELD_BYTES,
  MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
  type MissingRedeemerFixture,
  type MissingRedeemerFixtureShape,
} from "./support/missing-redeemer-emulator.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  emulatorSuccessorHeaderStart,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
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

const AddressDataLocal = Data.Object({
  paymentCredential: Data.Enum([
    Data.Object({ PublicKeyCredential: Data.Tuple([Data.Bytes()]) }),
    Data.Object({ ScriptCredential: Data.Tuple([Data.Bytes()]) }),
  ]),
  stakeCredential: Data.Nullable(Data.Any()),
});

const PURPOSE_KINDS = [0, 1, 2, 3] as const;
const PHYSICAL_STEPS = [
  "step01",
  "step02",
  "step02a",
  "step02b",
  "step03",
  "step04",
  "step05",
] as const;
const AUTHENTICATION_SEAMS = [
  "reference-script",
  "trace-root",
  "trace-state",
  "purpose-selection",
  "source-selection",
  "field-commitment",
  "walk-checkpoint",
] as const;
const measuredFit = createMeasuredFitRecorder(
  "missing-redeemer",
  "lifecycle",
  "all purpose kinds and source locations in both directions; exact 32,768-byte certified field with 17 redeemers and resumed grammar/walk",
);

const FAMILY = "missing-redeemer";

type Row = {
  readonly label: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
};
const printedFit = (rows: readonly Row[]): string =>
  JSON.stringify(rows, (_key, value: unknown) =>
    typeof value === "bigint" ? value.toString() : value,
  );
const outRefUtxo = async (
  lucid: Harness["harness"]["proverLucid"],
  outRef: string,
) =>
  (
    await lucid.utxosByOutRef([
      { txHash: outRef.slice(0, 64), outputIndex: Number(outRef.slice(65)) },
    ])
  )[0];

const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const paymentCredential = getAddressDetails(
    harness.contracts.fraudProof.spendingScriptAddress,
  ).paymentCredential!;
  const addressData = Data.from(
    Data.to(
      {
        paymentCredential:
          paymentCredential.type === "Key"
            ? { PublicKeyCredential: [paymentCredential.hash] }
            : { ScriptCredential: [paymentCredential.hash] },
        stakeCredential: null,
      } as never,
      AddressDataLocal as never,
    ),
  );
  const steps = applyMissingRedeemerScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    hubOracleScriptHash: harness.contracts.hubOracle.policyId,
  });
  const contracts: MissingRedeemerContracts = {
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
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    missingRedeemer: {
      ...harness.contracts.fraudProofs.missingRedeemer,
      spendingScriptHash: steps[0].spendingScriptHash,
    },
  });
  const category = catalogue.categories.missingRedeemer;
  expect(category.categoryId).toBe(MISSING_REDEEMER_CATEGORY_ID);
  const publications: Row[] = [];
  const references: UTxO[] = [];
  // Published from the prover wallet: the funder's first UTxO is the
  // deployment nonce the block setup spends later.
  for (const [index, step] of steps.entries()) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.proverLucid,
      script: step.spendingScript,
      label: `missing-redeemer-${PHYSICAL_STEPS[index]!}`,
    });
    references.push(published.utxo);
    publications.push({
      label: `reference-${PHYSICAL_STEPS[index]!}`,
      measurement: published.publicationMeasurement,
    });
    expect(
      published.publicationMeasurement.completeSignedBytes,
    ).toBeLessThanOrEqual(15_872);
  }
  return {
    harness,
    steps,
    contracts,
    catalogue,
    category,
    references,
    publications,
  };
};
type Harness = Awaited<ReturnType<typeof makeHarness>>;

/** Commits the fixture's block plus one successor and publishes its carriage. */
let measuredScenario = 0;
const makeStage = async (bundle: Harness, fixture: MissingRedeemerFixture) => {
  const measuredCase = `${measuredScenario++}-${fixture.shape.direction}-kind${fixture.shape.purposeKind}-${fixture.shape.sourceLocation}-field${fixture.fieldBytes}`;
  const { harness, contracts, catalogue, category, references, steps } = bundle;
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot: "00".repeat(32),
    subject:
      fixture.shape.direction === "accepted"
        ? { kind: "normal", nativeTx: fixture.transaction.tx }
        : {
            kind: "forced",
            nativeTx: fixture.transaction.tx,
            orderKey: fixture.orderKey,
            verdict: { ForcedTxInvalid: { reason: fixture.rejectionReason } },
          },
  });
  expect(block.nativeTxId).toBe(fixture.subject.transaction_id);
  const header = {
    ...block.header,
    endTime: block.header.endTime + 60_000n,
    validationTracesRoot: fixture.validationTracesRoot,
    validationTraceCount: fixture.validationTraceCount,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const successorStart = emulatorSuccessorHeaderStart({
    predecessorEndTime: header.endTime,
    emulator: harness.emulator,
  });
  const successor = await submitSuccessorBlockTx({
    lucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    anchorBlockUnit: setup.stateQueueBlockUnit,
    header: {
      ...header,
      startTime: BigInt(successorStart),
      endTime: BigInt(successorStart + 60_000),
      prevHeaderHash: setup.headerHash,
    },
    hubOracle: setup.hubOracle,
    scheduler: setup.scheduler,
    activeOperatorNode: setup.activeOperatorNode,
    activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
  });
  const rows: Row[] = [];
  const measured = async <T>(
    label: string,
    operation: () => Promise<T>,
  ): Promise<T> => {
    try {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      // Carriage publication submits one transaction per chunk; every one
      // of them is a lifecycle transaction the ledger must fit.
      if (captured.measurements.length === 1)
        rows.push({ label, measurement: captured.measurement });
      else
        captured.measurements.forEach((measurement, index) =>
          rows.push({ label: `${label}-${index.toString()}`, measurement }),
        );
      captured.measurements.forEach((measurement, index) =>
        measuredFit.record(
          `${measuredCase}/${rows.length - captured.measurements.length + index}-${label}`,
          measurement,
          measurement.executionMemory === 0n ? "publication" : "lifecycle",
        ),
      );
      return captured.result;
    } catch (error) {
      console.error(`[missing-redeemer-lifecycle] ${label} failed`);
      throw error;
    }
  };
  const material = fixture.material;
  const staged = planMissingRedeemerStagedWalk({
    transactionId: material.evidence.subject.transaction_id,
    fieldPreimageCbor: material.evidence.fieldPreimageHex,
  });
  const opening = planMissingRedeemerFieldOpening({
    ...material,
    staged,
    owner: harness.proverSigner.paymentKeyHash,
  });
  // Tier-2/3 carriage is published once per stage; every thread on this
  // block resolves the same content-addressed chunks and certificate.
  if (opening.plan.tier !== "Inline") {
    const chunks = await measured("publish-field-8-carriage", () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned: opening,
        publisherAddress: harness.proverSigner.address,
        label: "missing-redeemer field 8 carriage",
      }),
    );
    if (opening.plan.tier === "Certified") {
      const certificateReference = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "missing-redeemer field certificate mint",
      });
      await measured("certify-field-8", () =>
        certifyFaultProofFieldCarriage({
          lucid: harness.proverLucid,
          network,
          signer: harness.proverSigner,
          planned: opening,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            harness.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: certificateReference.utxo,
          chunkUtxos: chunks,
          compactCbor: material.nativeTxCompactCbor,
          witnessSetCompactCbor: material.witnessSetCompactCbor,
        }),
      );
    }
  }
  const common = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  } as const;
  const initialize = async (label = "init") =>
    (
      await measured(label, () =>
        submitMissingRedeemerInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: successor.continuedAnchorOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      )
    ).nextThreadOutRef;
  const bind = async (
    threadOutRef: string,
    options: {
      purposeKind?: MissingRedeemerPurposeKind;
      purposeIndex?: number;
      referenceScriptUtxo?: UTxO;
      label?: string;
    } = {},
  ) => {
    const purposeKind = options.purposeKind ?? fixture.shape.purposeKind;
    const purposeIndex = options.purposeIndex ?? 0;
    const referenceScriptUtxo = options.referenceScriptUtxo ?? references[0];
    const result = await measured(options.label ?? "step01", async () =>
      fixture.shape.direction === "accepted"
        ? submitMissingRedeemerStep01Accepted({
            ...common,
            blueprint: harness.realBlueprint,
            network,
            threadOutRef,
            stateQueueBlockOutRef: successor.continuedAnchorOutRef,
            txInclusion: block.txInclusion!,
            header,
            purposeKind,
            purposeIndex,
            referenceScriptUtxo,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          })
        : submitMissingRedeemerStep01Forced({
            ...common,
            threadOutRef,
            header,
            membership: (await buildForcedTransactionLeafMembershipProof({
              reconstruction: block.reconstruction,
              eventKey: fixture.eventKey,
            }))!,
            purposeKind,
            purposeIndex,
            referenceScriptUtxo,
          }),
    );
    return result.nextThreadOutRef;
  };
  const authenticate = async (
    threadOutRef: string,
    step: 1 | 2 | 3,
    authentication: MissingRedeemerMaterial["authentication"] = material.authentication,
    label?: string,
  ) => {
    const submit = [
      submitMissingRedeemerStep02,
      submitMissingRedeemerStep02a,
      submitMissingRedeemerStep02b,
    ][step - 1]!;
    const result = await measured(label ?? PHYSICAL_STEPS[step], () =>
      submit({
        ...common,
        threadOutRef,
        authentication,
        referenceScriptUtxo: references[step]!,
      }),
    );
    return result.nextThreadOutRef;
  };
  const fieldCommon = (plan: MissingRedeemerStagedPlan = staged) => ({
    ...common,
    evidence: material.evidence,
    nativeTxCompactCbor: material.nativeTxCompactCbor,
    witnessSetCompactCbor: material.witnessSetCompactCbor,
    staged: plan,
  });
  const open = async (
    threadOutRef: string,
    action: MissingRedeemerStep03Action,
    label?: string,
    plan?: MissingRedeemerStagedPlan,
  ) => {
    const result = await measured(label ?? `step03-${action.kind}`, () =>
      submitMissingRedeemerStep03({
        ...fieldCommon(plan),
        threadOutRef,
        action,
        referenceScriptUtxo: references[4]!,
      }),
    );
    return result.nextThreadOutRef;
  };
  /** Every step-03 transaction the carriage tier needs, in order. */
  const openField = async (threadOutRef: string) => {
    if (opening.plan.tier !== "Certified")
      return await open(threadOutRef, { kind: "direct" });
    let current = await open(threadOutRef, { kind: "grammar_start" });
    for (let ordinal = 1; ordinal < staged.grammar.length; ordinal += 1)
      current = await open(
        current,
        { kind: "grammar_resume", ordinal },
        `step03-grammar_resume-${ordinal.toString()}`,
      );
    return await open(current, { kind: "grammar_finish" });
  };
  const scanBatch = async (
    threadOutRef: string,
    label?: string,
    plan?: MissingRedeemerStagedPlan,
  ) => {
    const result = await measured(label ?? "step04", () =>
      submitMissingRedeemerStep04({
        ...fieldCommon(plan),
        threadOutRef,
        referenceScriptUtxo: references[5]!,
      }),
    );
    return result.nextThreadOutRef;
  };
  const batches = Math.max(
    1,
    Math.ceil(material.evidence.itemCount / MISSING_REDEEMER_SCAN_BATCH),
  );
  /** Runs the walk to its decision, one real batch per transaction. */
  const scan = async (threadOutRef: string) => {
    let current = threadOutRef;
    for (let batch = 0; batch < batches; batch += 1) {
      current = await scanBatch(current, `step04-batch-${batch.toString()}`);
      if (material.evidence.checkpoints[batch]?.found === true) break;
      if (batch + 1 < batches) {
        // Still at step 04 with a live checkpoint until the field is exhausted.
        const utxo = await outRefUtxo(harness.proverLucid, current);
        expect(utxo?.address).toBe(steps[5].spendingScriptAddress);
      }
    }
    return current;
  };
  const finalize = (threadOutRef: string, label = "step05") =>
    measured(label, () =>
      submitMissingRedeemerStep05({
        ...common,
        threadOutRef,
        evidence: material.evidence,
        referenceScriptUtxo: references[6]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const cancel = (threadOutRef: string, stepIndex: number, label: string) =>
    measured(label, () =>
      submitMissingRedeemerCancel({
        ...common,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const names = [
      "fraudProofMissingRedeemer",
      "fraudProofMissingRedeemerStep02",
      "fraudProofMissingRedeemerStep02a",
      "fraudProofMissingRedeemerStep02b",
      "fraudProofMissingRedeemerStep03",
      "fraudProofMissingRedeemerStep04",
      "fraudProofMissingRedeemerStep05",
    ] as const;
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        ...Object.fromEntries(
          steps.map((step, index) => [
            names[index]!,
            {
              scriptHash: step.spendingScriptHash,
              contract: {
                type: step.spendingScript.type,
                cborHex: step.spendingScript.script,
              },
            },
          ]),
        ),
      },
    };
    let leaseReleased = false;
    const now = BigInt(harness.emulator.now());
    const removal = await measured("remove-leased", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "missingRedeemer",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "missing-redeemer-lifecycle",
            source: "emulator",
            renew: async () => undefined,
            release: async () => {
              leaseReleased = true;
            },
            fail: async () => undefined,
          }),
        },
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.fraudulentHeaderHash).toBe(setup.headerHash);
    expect(leaseReleased).toBe(true);
  };
  /** A fresh process recovers the thread from the chain alone. */
  const restart = async (threadOutRef: string, stepIndex: number) => {
    const found = (
      await harness.proverLucid.utxosAt(steps[stepIndex]!.spendingScriptAddress)
    ).find(
      (utxo) =>
        `${utxo.txHash}#${utxo.outputIndex.toString()}` === threadOutRef,
    );
    expect(found, `thread at ${PHYSICAL_STEPS[stepIndex]!}`).toBeDefined();
    return threadOutRef;
  };
  const assertFit = () => {
    for (const { label, measurement } of rows) {
      expect(measurement.l1ByteMargin, label).toBeGreaterThan(0);
      // Raw carriage publication runs no script; every other row does.
      if (label.startsWith("publish-")) continue;
      expect(measurement.executionMemory, label).toBeGreaterThan(0n);
      expect(measurement.executionSteps, label).toBeGreaterThan(0n);
    }
  };
  /** Runs Init through the exhausted or matched walk, restarting at each step. */
  const runToDecision = async () => {
    let thread = await initialize();
    thread = await restart(await bind(thread), 1);
    thread = await restart(await authenticate(thread, 1), 2);
    thread = await restart(await authenticate(thread, 2), 3);
    thread = await restart(await authenticate(thread, 3), 4);
    thread = await restart(await openField(thread), 5);
    return await restart(await scan(thread), 6);
  };
  /** Spends the decision straight into the terminal validator, bypassing the off-chain guard. */
  const finalizeDirectly = async (threadOutRef: string) => {
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      family: FAMILY,
      stepIndex: 6,
      threadOutRef,
    });
    requireLinearFaultStepState<
      Data.Static<typeof MissingRedeemerDecisionSchema>
    >({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingRedeemerStep05DatumSchema as never,
      family: FAMILY,
      stepIndex: 6,
    });
    return await submitLinearFaultFinalize({
      lucid: harness.proverLucid,
      family: FAMILY,
      stepIndex: 6,
      step: contracts.steps[6],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer: harness.proverSigner,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: MissingRedeemerStep05RedeemerSchema,
      buildFamilyArgs: (layout) => ({
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: references[6]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };
  return {
    block,
    header,
    setup,
    opening,
    staged,
    rows,
    initialize,
    bind,
    authenticate,
    open,
    openField,
    scanBatch,
    scan,
    finalize,
    finalizeDirectly,
    cancel,
    remove,
    restart,
    assertFit,
    runToDecision,
  };
};

const shapeFor = (
  direction: "accepted" | "forced",
  purposeKind: MissingRedeemerPurposeKind,
): MissingRedeemerFixtureShape => ({
  direction,
  purposeKind,
  // Both source locations in both directions: half the kinds keep their
  // script inline and the other half resolve it from a reference input,
  // swapped between directions; the MidgardV1 receive script is inline by
  // construction.
  sourceLocation:
    purposeKind === 3
      ? "inline"
      : (direction === "accepted") === (purposeKind % 2 === 0)
        ? "reference"
        : "inline",
});

describe("missingRedeemer concrete retained lifecycle material", () => {
  it.each(PURPOSE_KINDS)(
    "derives the accepted absence and forced presence for purpose kind %d",
    async (purposeKind) => {
      const accepted = await buildMissingRedeemerFixture(
        shapeFor("accepted", purposeKind),
      );
      expect(accepted.material.evidence.redeemerMissing).toBe(true);
      expect(accepted.material.evidence.purposeKind).toBe(purposeKind);
      expect(accepted.material.evidence.purpose.sourceLanguageTag).toBe(
        purposeKind === 3 ? 128 : 3,
      );
      expect(accepted.material.evidence.purpose.source).toBe(
        accepted.shape.sourceLocation === "inline"
          ? "witness"
          : "resolved-reference",
      );
      expect(accepted.material.authentication.control.stage).toBe(10n);
      const forced = await buildMissingRedeemerFixture(
        shapeFor("forced", purposeKind),
      );
      expect(forced.material.evidence.redeemerMissing).toBe(false);
      expect(forced.material.evidence.purpose.source).toBe(
        forced.shape.sourceLocation === "inline"
          ? "witness"
          : "resolved-reference",
      );
      expect(forced.material.authentication.control.stage).toBe(10n);
      expect(
        forced.material.authentication.control.discovery.current_purpose_kind,
      ).toBe(BigInt(purposeKind));
    },
    120_000,
  );

  it("builds the exact maximum certified field and refuses the adjacent over-bound field", async () => {
    const maximum = await buildMissingRedeemerFixture({
      ...shapeFor("accepted", 0),
      decoyRedeemers: MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
      fieldBytes: MISSING_REDEEMER_MAXIMUM_FIELD_BYTES,
    });
    expect(maximum.fieldBytes).toBe(MISSING_REDEEMER_MAXIMUM_FIELD_BYTES);
    expect(maximum.material.evidence.carriage).toBe("Certified");
    expect(maximum.material.evidence.itemCount).toBe(
      MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
    );
    expect(
      maximum.material.evidence.checkpoints.map(({ cursor }) => cursor),
    ).toEqual([16, 17]);
    expect(() =>
      selectMidgardFieldCarriageTier(MISSING_REDEEMER_MAXIMUM_FIELD_BYTES + 1),
    ).toThrow(/aggregate bound/u);
    await expect(
      buildMissingRedeemerFixture({
        ...shapeFor("accepted", 0),
        decoyRedeemers: MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
        fieldBytes: MISSING_REDEEMER_MAXIMUM_FIELD_BYTES + 1,
      }),
    ).rejects.toThrow();
  }, 180_000);

  it("refuses a substituted trace root and an omitted purpose witness", async () => {
    const fixture = await buildMissingRedeemerFixture(shapeFor("accepted", 1));
    const rebuild = (
      overrides: Partial<
        Parameters<typeof buildMissingRedeemerMaterialFromRetainedDa>[0]
      >,
    ) =>
      buildMissingRedeemerMaterialFromRetainedDa({
        eventKey: fixture.eventKey,
        subject: fixture.subject,
        purposeKind: 1,
        purposeIndex: 0,
        txCbor: fixture.transaction.txCbor,
        authenticatedValidationTraceEntries: fixture.descriptorEntries,
        retainedValidationWitnessEntries: fixture.retainedEntries,
        expectedValidationTracesRoot: fixture.validationTracesRoot,
        ...overrides,
      });
    await expect(
      rebuild({ expectedValidationTracesRoot: "ff".repeat(32) }),
    ).rejects.toThrow(/validation root changed/u);
    const withoutPurpose = fixture.retainedEntries.filter(({ value }) => {
      const auxiliary = SDK.decodeRetainedValidationWitness(value).auxiliary;
      return !(
        typeof auxiliary === "object" && "ScriptPurposeScanWitness" in auxiliary
      );
    });
    await expect(
      rebuild({ retainedValidationWitnessEntries: withoutPurpose }),
    ).rejects.toThrow(/purpose membership witness/u);
    await expect(rebuild({ purposeIndex: 1 })).rejects.toThrow(
      /purpose selection is absent/u,
    );
  }, 120_000);
});

describe("missingRedeemer real Lucid lifecycle", () => {
  const directions: ("accepted_invalid" | "forced_rejection_wrong")[] = [];
  const scenarios: CompleteLifecycleBaseScenario[] = [];
  const seams: string[] = [];
  const cancelled: string[] = [];
  let resumedAfterCheckpoint = false;
  let adjacentOverBoundRefused = false;
  const publications: Row[] = [];

  it.each(
    PURPOSE_KINDS.flatMap((purposeKind) =>
      (["accepted", "forced"] as const).map((direction) => ({
        direction,
        purposeKind,
      })),
    ),
  )(
    "convicts the $direction direction for purpose kind $purposeKind from Init through the permanent mint and removal",
    async ({ direction, purposeKind }) => {
      const bundle = await makeHarness();
      if (publications.length === 0) publications.push(...bundle.publications);
      const fixture = await buildMissingRedeemerFixture(
        shapeFor(direction, purposeKind),
      );
      const stage = await makeStage(bundle, fixture);
      // Every field-8 opening reads published carriage, so the smallest
      // field is demoted from inline to one raw carriage UTxO.
      expect(stage.opening.plan.tier).toBe("RawUtxo");
      const decision = await stage.runToDecision();
      const final = await stage.finalize(decision);
      expect(final.fraudProofUnit).toBeTruthy();
      const [permanentProof] = await bundle.harness.proverLucid.utxosAtWithUnit(
        bundle.harness.contracts.fraudProof.spendingScriptAddress,
        final.fraudProofUnit,
      );
      expect(permanentProof?.txHash).toBe(final.txHash);
      await stage.remove();
      stage.assertFit();
      directions.push(
        direction === "accepted"
          ? "accepted_invalid"
          : "forced_rejection_wrong",
      );
      scenarios.push(
        direction === "accepted"
          ? "wrongful_acceptance_success"
          : "wrongful_forced_rejection_success",
        "permanent_proof_token_and_descendant_removal",
      );
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info(
          `[missing-redeemer-fit ${direction} kind ${purposeKind.toString()} ${fixture.shape.sourceLocation}] ${printedFit(stage.rows)}`,
        );
    },
    900_000,
  );

  it("scans the exact 32,768-byte certified field in resumed batches, cancels the grammar and mid-walk states, mints, and removes", async () => {
    const bundle = await makeHarness();
    const fixture = await buildMissingRedeemerFixture({
      ...shapeFor("accepted", 0),
      decoyRedeemers: MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
      fieldBytes: MISSING_REDEEMER_MAXIMUM_FIELD_BYTES,
    });
    expect(fixture.fieldBytes).toBe(MISSING_REDEEMER_MAXIMUM_FIELD_BYTES);
    const stage = await makeStage(bundle, fixture);
    expect(stage.opening.plan.tier).toBe("Certified");
    expect(stage.staged.grammar).toHaveLength(2);
    expect(stage.staged.walk.map((point) => point.nextItemIndex)).toEqual([
      16, 17,
    ]);
    const authenticated = async () => {
      let thread = await stage.initialize();
      thread = await stage.bind(thread);
      thread = await stage.authenticate(thread, 1);
      thread = await stage.authenticate(thread, 2);
      return await stage.authenticate(thread, 3);
    };
    // Certified carriage has a provisional item count: the direct opening
    // is refused before anything is signed.
    let thread = await authenticated();
    await expect(stage.open(thread, { kind: "direct" })).rejects.toThrow(
      /requires grammar/u,
    );
    // Item 7: cancel from the grammar state and from the mid-walk state.
    const grammar = await stage.open(thread, { kind: "grammar_start" });
    await stage.cancel(grammar, 4, "cancel-step03-grammar");
    cancelled.push("step03");
    thread = await stage.restart(
      await stage.openField(await authenticated()),
      5,
    );
    const midWalk = await stage.restart(
      await stage.scanBatch(thread, "step04-batch-0"),
      5,
    );
    // Item 6: a walk checkpoint bound to another transaction is refused.
    const foreign = planMissingRedeemerStagedWalk({
      transactionId: "ff".repeat(32),
      fieldPreimageCbor: fixture.material.evidence.fieldPreimageHex,
    });
    await expect(
      stage.scanBatch(midWalk, "step04-foreign-checkpoint", foreign),
    ).rejects.toThrow(/checkpoint is unreachable/u);
    seams.push("walk-checkpoint");
    // Item 8: the second batch is rebuilt from the on-chain checkpoint alone.
    const decision = await stage.restart(
      await stage.scanBatch(midWalk, "step04-batch-1"),
      6,
    );
    resumedAfterCheckpoint = true;
    const anotherMidWalk = await stage.scanBatch(
      await stage.openField(await authenticated()),
      "step04-batch-0-again",
    );
    await stage.cancel(anotherMidWalk, 5, "cancel-step04-mid-walk");
    cancelled.push("step04");
    const final = await stage.finalize(decision);
    expect(final.fraudProofUnit).toBeTruthy();
    await stage.remove();
    stage.assertFit();
    scenarios.push("maximum_supported_evidence");
    adjacentOverBoundRefused = true;
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[missing-redeemer-fit maximum certified ${MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT.toString()} items] ${printedFit(stage.rows)}`,
      );
  }, 1_800_000);

  it("refuses honest blocks, mutated coordinates, and every substituted authentication seam, and cancels every other physical step", async () => {
    const bundle = await makeHarness();
    const wrongful = await buildMissingRedeemerFixture(shapeFor("forced", 0));
    const stage = await makeStage(bundle, wrongful);
    // Item 5: the forced door binds the exact typed reason; another
    // coordinate is refused before anything is signed.
    let thread = await stage.initialize();
    await expect(stage.bind(thread, { purposeKind: 1 })).rejects.toThrow(
      /coordinate changed/u,
    );
    await expect(stage.bind(thread, { purposeIndex: 1 })).rejects.toThrow(
      /coordinate changed/u,
    );
    // Item 6: reference-script substitution at the first seam.
    await expect(
      stage.bind(thread, { referenceScriptUtxo: bundle.references[1] }),
    ).rejects.toThrow(/reference script/iu);
    seams.push("reference-script");
    scenarios.push("reason_or_subject_coordinate_mutation");
    // Item 7: cancel from step 01, then from every authenticated step.
    await stage.cancel(thread, 0, "cancel-step01");
    cancelled.push("step01");
    thread = await stage.bind(await stage.initialize());
    const authentication = wrongful.material.authentication;
    // Item 6: a substituted trace root fails step 02 on chain.
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 1, {
        ...authentication,
        traceMembership: {
          ...authentication.traceMembership,
          root: "ff".repeat(32),
        },
      }),
    );
    seams.push("trace-root");
    await stage.cancel(thread, 1, "cancel-step02");
    cancelled.push("step02");
    thread = await stage.authenticate(
      await stage.bind(await stage.initialize()),
      1,
    );
    // Item 6: a substituted stage-10 control fails the work root on chain,
    // and a trace proof for another state index fails the state binding.
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 2, {
        ...authentication,
        control: {
          ...authentication.control,
          discovery: {
            ...authentication.control.discovery,
            current_purpose_index: 1n,
          },
        },
      }),
    );
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 2, {
        ...authentication,
        traceProof: {
          ...authentication.traceProof,
          state_index: authentication.traceProof.state_index + 1n,
        },
      }),
    );
    seams.push("trace-state");
    await stage.cancel(thread, 2, "cancel-step02a");
    cancelled.push("step02a");
    thread = await stage.authenticate(
      await stage.authenticate(await stage.bind(await stage.initialize()), 1),
      2,
    );
    // Item 6: alternate purpose and alternate source substitutions fail 02b.
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 3, {
        ...authentication,
        purposeSiblings: [...authentication.purposeSiblings, "00".repeat(32)],
      }),
    );
    seams.push("purpose-selection");
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 3, {
        ...authentication,
        sourceItemCommitment: "ee".repeat(32),
      }),
    );
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 3, {
        ...authentication,
        sourceLanguageTag: authentication.sourceLanguageTag === 3n ? 128n : 3n,
      }),
    );
    seams.push("source-selection");
    await stage.cancel(thread, 3, "cancel-step02b");
    cancelled.push("step02b");
    thread = await stage.authenticate(
      await stage.authenticate(
        await stage.authenticate(await stage.bind(await stage.initialize()), 1),
        2,
      ),
      3,
    );
    // Item 6: a truncated item set cannot open as the committed field 8.
    const truncated = planMissingRedeemerStagedWalk({
      transactionId: wrongful.material.evidence.subject.transaction_id,
      fieldPreimageCbor: wrongful.material.evidence.fieldPreimageHex,
    });
    await expect(
      stage.open(thread, { kind: "direct" }, "step03-truncated-field", {
        ...truncated,
        items: truncated.items.slice(0, 1),
      }),
    ).rejects.toThrow(/commits to/u);
    seams.push("field-commitment");
    const decision = await stage.restart(
      await stage.scan(await stage.openField(thread)),
      6,
    );
    await stage.cancel(decision, 6, "cancel-step05");
    cancelled.push("step05");
    // Item 4: an honest forced rejection of a genuinely redeemer-less
    // transaction proves absence, and the terminal step refuses it.
    const honestForced = await buildMissingRedeemerFixture({
      ...shapeFor("forced", 2),
      targetRedeemerPresent: false,
    });
    expect(honestForced.material.evidence.redeemerMissing).toBe(true);
    // Each committed block needs its own deployment nonce: a fresh harness.
    const honestStage = await makeStage(await makeHarness(), honestForced);
    const honestDecision = await honestStage.runToDecision();
    await expect(honestStage.finalize(honestDecision)).rejects.toThrow(
      /terminal decision differs/u,
    );
    await expectOnchainRefusal(() =>
      honestStage.finalizeDirectly(honestDecision),
    );
    scenarios.push("honest_forced_rejection_refusal");
    // Item 3: an accepted block whose transaction carries the redeemer
    // proves presence, and the terminal step refuses it.
    const honestAccepted = await buildMissingRedeemerFixture({
      ...shapeFor("accepted", 3),
      targetRedeemerPresent: true,
    });
    expect(honestAccepted.material.evidence.redeemerMissing).toBe(false);
    const acceptedStage = await makeStage(await makeHarness(), honestAccepted);
    const acceptedDecision = await acceptedStage.runToDecision();
    await expect(acceptedStage.finalize(acceptedDecision)).rejects.toThrow(
      /terminal decision differs/u,
    );
    await expectOnchainRefusal(() =>
      acceptedStage.finalizeDirectly(acceptedDecision),
    );
    scenarios.push("honest_accepted_block_refusal");
    stage.assertFit();
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(`[missing-redeemer-fit cancels] ${printedFit(stage.rows)}`);
  }, 1_800_000);

  it("covers every §5.3 item", () => {
    assertCompleteLifecycleCoverage({
      coverage: {
        reasonArms: ["RedeemerMissing"],
        successfulDirectionByReason: { RedeemerMissing: directions },
        scenarios,
        authenticatedSeamsMutated: seams,
        cancelledPhysicalSteps: cancelled,
        resumedAfterCheckpoint,
        adjacentOverBoundRefused,
      },
      expectedReasonArms: ["RedeemerMissing"],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...PHYSICAL_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: true,
    });
    for (const row of publications)
      expect(row.measurement.l1ByteMargin, row.label).toBeGreaterThan(0);
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[missing-redeemer-fit publications] ${printedFit(publications)}`,
      );
  });
});
