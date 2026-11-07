/** Real-contract emulator fixtures for the `distinctAssetAccumulationLimit`
 * family: the harness, the published role scripts, field carriage, and a
 * driver from a genuine Init through step-01 and the planned custody to the
 * terminal. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS,
  distinctAssetAccumulationLimitContractRoles,
  type DistinctAssetAccumulationLimitCustody,
  type DistinctAssetAccumulationLimitProofPlan,
  type DistinctAssetAccumulationLimitRole,
  distinctAssetAccumulationLimitStep01Datum,
  distinctAssetAccumulationLimitTransitionAt,
  type DistinctAssetAccumulationLimitWalkBatch,
  fitDistinctAssetAccumulationLimitPage,
  isDistinctAssetAccumulationLimitPagedRole,
  prepareDistinctAssetAccumulationLimitProof,
  submitDistinctAssetAccumulationLimitInit,
  submitDistinctAssetAccumulationLimitStep01Accepted,
  submitDistinctAssetAccumulationLimitStep01Forced,
  submitDistinctAssetAccumulationLimitTerminal,
  submitDistinctAssetAccumulationLimitTransition,
  submitDistinctAssetAccumulationLimitTransitions,
} from "../../src/distinct-asset-accumulation-limit/index.js";
import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import type { DistinctAssetScenario } from "./distinct-asset-accumulation-limit-emulator.subject.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  makeFaultProofEmulatorHarness,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
} from "./submit-init-emulator-shared.js";

export const makeDistinctAssetAccumulationLimitHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realDistinctAssetAccumulationLimit: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const distinctAssetAccumulationLimit =
    harness.contracts.distinctAssetAccumulationLimit;
  const category = harness.catalogue.categories.distinctAssetAccumulationLimit;
  if (distinctAssetAccumulationLimit === undefined || category === undefined)
    throw new Error(
      "distinct-asset-accumulation-limit harness contracts/category were omitted",
    );
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.distinctAssetAccumulationLimit
  )
    throw new Error("unexpected distinct-asset-accumulation-limit category id");
  return { ...harness, distinctAssetAccumulationLimit, category };
};

export type DistinctAssetAccumulationLimitHarness = Awaited<
  ReturnType<typeof makeDistinctAssetAccumulationLimitHarness>
>;

export type RoleReference = {
  readonly role: DistinctAssetAccumulationLimitRole;
  readonly scriptHash: string;
  readonly utxo: UTxO;
  readonly publication: CompleteSignedTransactionMeasurement;
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishDistinctAssetAccumulationLimitRoles = async (
  harness: DistinctAssetAccumulationLimitHarness,
): Promise<ReadonlyMap<DistinctAssetAccumulationLimitRole, RoleReference>> => {
  const roles = distinctAssetAccumulationLimitContractRoles(
    harness.distinctAssetAccumulationLimit,
  );
  const byHash = new Map<string, RoleReference>();
  const out = new Map<DistinctAssetAccumulationLimitRole, RoleReference>();
  for (const role of SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER) {
    const validator = roles[role];
    let reference = byHash.get(validator.spendingScriptHash);
    if (reference === undefined) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: validator.spendingScript,
        label: `distinct-asset-accumulation-limit ${role}`,
      });
      reference = {
        role,
        scriptHash: validator.spendingScriptHash,
        utxo: published.utxo,
        publication: published.publicationMeasurement,
      };
      byHash.set(validator.spendingScriptHash, reference);
    }
    out.set(role, { ...reference, role });
  }
  return out;
};

/** Publishes (and, at tier 3, certifies) every planned field that does not
 * travel inline. */
export const publishDistinctAssetAccumulationLimitCarriage = async ({
  harness,
  scenario,
  plan,
}: {
  readonly harness: DistinctAssetAccumulationLimitHarness;
  readonly scenario: DistinctAssetScenario;
  readonly plan: DistinctAssetAccumulationLimitProofPlan["plan"];
}): Promise<{
  readonly carriage: ReadonlyMap<number, readonly UTxO[]>;
  readonly measurements: readonly {
    readonly field: number;
    readonly tier: string;
    readonly measurement: CompleteSignedTransactionMeasurement;
  }[];
}> => {
  const carriage = new Map<number, readonly UTxO[]>();
  const measurements: {
    field: number;
    tier: string;
    measurement: CompleteSignedTransactionMeasurement;
  }[] = [];
  const ownOutputs = createOwnOutputJournal();
  let certificateReference: UTxO | undefined;
  for (const field of DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS) {
    const planned = plan(field).openingPlan;
    const tier = planned.plan.tier;
    if (tier === "Inline") continue;
    const published = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriage({
        ownOutputs,
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `distinct-asset-accumulation-limit field ${field.toString()}`,
      }),
    );
    for (const measurement of published.measurements)
      measurements.push({ field, tier, measurement });
    const references = [...published.result];
    if (tier === "Certified") {
      certificateReference ??= (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.proverLucid,
          script: harness.contracts.fieldPreimageCertificate.mintingScript,
          label: "distinct-asset-accumulation-limit field certificate",
        })
      ).utxo;
      const certified = await captureEmulatorSubmission(harness.emulator, () =>
        certifyFaultProofFieldCarriage({
          ownOutputs,
          lucid: harness.proverLucid,
          network,
          signer: harness.proverSigner,
          planned,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            harness.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: certificateReference!,
          chunkUtxos: references,
          compactCbor: scenario.material.bound.compact_cbor,
          witnessSetCompactCbor:
            scenario.material.material.proofSource.witnessSetCompactCbor.toString(
              "hex",
            ),
        }),
      );
      measurements.push({
        field,
        tier: "certificate",
        measurement: certified.measurement,
      });
      references.push(certified.result.certificateUtxo);
    }
    carriage.set(field, references);
  }
  return { carriage, measurements };
};

export type StepMeasurement = {
  readonly label: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
};

/** Continues the thread role by role from the thread datum to the
 * terminal's input, as the workflow does: each page that opens MPF proofs is
 * sized to the transaction budget. */
const continueToTerminal = async (
  custody: DistinctAssetAccumulationLimitCustody,
  prepared: DistinctAssetAccumulationLimitProofPlan,
  start: UTxO,
  scenario: DistinctAssetScenario,
): Promise<UTxO> => {
  const transitionAt = distinctAssetAccumulationLimitTransitionAt(
    prepared,
    distinctAssetAccumulationLimitStep01Datum(
      scenario.material,
      custody.signer.paymentKeyHash,
    ),
  );
  let thread = start;
  let role = prepared.transitions[0]!.role;
  while (role !== "terminal") {
    const transition = isDistinctAssetAccumulationLimitPagedRole(role)
      ? await fitDistinctAssetAccumulationLimitPage({
          custody,
          walk: prepared.walk,
          role,
          thread,
        })
      : transitionAt(role, thread.datum!);
    thread = (
      await submitDistinctAssetAccumulationLimitTransition(
        custody,
        thread,
        transition,
      )
    ).thread;
    role = transition.next;
  }
  return thread;
};

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveDistinctAssetAccumulationLimitThread = async ({
  harness,
  scenario,
  references,
  batch,
  transitions,
  beforeStep01,
}: {
  readonly harness: DistinctAssetAccumulationLimitHarness;
  readonly scenario: DistinctAssetScenario;
  readonly references: ReadonlyMap<
    DistinctAssetAccumulationLimitRole,
    RoleReference
  >;
  readonly batch?: DistinctAssetAccumulationLimitWalkBatch;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: DistinctAssetAccumulationLimitProofPlan,
  ) => DistinctAssetAccumulationLimitProofPlan["transitions"];
  /** Runs against the Init thread before step-01 spends it (e.g. a forged
   * step-01 the validator must refuse). */
  readonly beforeStep01?: (args: {
    readonly thread: UTxO;
    readonly unit: string;
    readonly prepared: DistinctAssetAccumulationLimitProofPlan;
  }) => Promise<void>;
}) => {
  const { proverLucid, distinctAssetAccumulationLimit, proverSigner } = harness;
  const steps: StepMeasurement[] = [];
  const prepared = await prepareDistinctAssetAccumulationLimitProof({
    material: scenario.material,
    contracts: distinctAssetAccumulationLimit,
    prover: proverSigner.paymentKeyHash,
    ...(batch === undefined ? {} : { batch }),
  });
  const { carriage, measurements: carriageMeasurements } =
    await publishDistinctAssetAccumulationLimitCarriage({
      harness,
      scenario,
      plan: prepared.plan,
    });
  const reference = (role: DistinctAssetAccumulationLimitRole) => {
    const found = references.get(role);
    if (found === undefined) throw new Error(`${role} reference absent`);
    return found.utxo;
  };
  const init = await captureEmulatorSubmission(harness.emulator, () =>
    submitDistinctAssetAccumulationLimitInit({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: distinctAssetAccumulationLimit,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  steps.push({ label: "init", measurement: init.measurement });
  if (beforeStep01 !== undefined) {
    const [txHash, outputIndex] = init.result.nextThreadOutRef.split("#");
    const [thread] = await proverLucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (thread === undefined) throw new Error("Init thread output absent");
    await beforeStep01({
      thread,
      unit: init.result.computationThreadUnit,
      prepared,
    });
  }
  const common = {
    lucid: proverLucid,
    contracts: distinctAssetAccumulationLimit,
    categoryId: harness.category.categoryId,
    signer: proverSigner,
    threadOutRef: init.result.nextThreadOutRef,
    material: scenario.material,
    referenceScriptUtxo: reference("step01"),
  };
  const step01 = await captureEmulatorSubmission(harness.emulator, () =>
    scenario.block.txInclusion === null
      ? submitDistinctAssetAccumulationLimitStep01Forced({
          ...common,
          reconstruction: scenario.block.reconstruction,
        })
      : submitDistinctAssetAccumulationLimitStep01Accepted({
          ...common,
          blueprint: harness.realBlueprint,
          network,
          stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
          txInclusion: scenario.block.txInclusion,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
  );
  steps.push({ label: "step01", measurement: step01.measurement });
  const unit = init.result.computationThreadUnit;
  const custody: DistinctAssetAccumulationLimitCustody = {
    lucid: proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: distinctAssetAccumulationLimit,
    signer: proverSigner,
    unit,
    references: reference,
    carriage: (field) => carriage.get(field) ?? [],
    plan: prepared.plan,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    onSigned: (label, signed) =>
      steps.push({
        label,
        measurement: measureCompleteSignedTransaction(signed.toCBOR()),
      }),
  };
  const [start] = await proverLucid.utxosByOutRef([
    {
      txHash: step01.result.txHash,
      outputIndex: Number(step01.result.nextThreadOutRef.split("#")[1]),
    },
  ]);
  if (start === undefined) throw new Error("step-01 thread output absent");
  const thread =
    transitions === undefined
      ? await continueToTerminal(custody, prepared, start, scenario)
      : await submitDistinctAssetAccumulationLimitTransitions(
          custody,
          start,
          transitions(prepared),
        );
  /** `at` is the terminal's input when the caller advanced the thread. */
  const terminal = (requireConviction = true, at: UTxO = thread) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitDistinctAssetAccumulationLimitTerminal({
        lucid: proverLucid,
        contracts: distinctAssetAccumulationLimit,
        material: scenario.material,
        categoryId: harness.category.categoryId,
        signer: proverSigner,
        threadOutRef: `${at.txHash}#${at.outputIndex.toString()}`,
        referenceScriptUtxo: reference("terminal"),
        witnessReferenceScripts: harness.witnessReferenceScripts,
        requireConviction,
      }),
    ).then((submitted) => {
      steps.push({ label: "terminal", measurement: submitted.measurement });
      return submitted.result;
    });
  return {
    prepared,
    custody,
    thread,
    unit,
    steps,
    carriageMeasurements,
    terminal,
  };
};

/** Per-step ex-unit and size bounds every thread transaction must meet. */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS = {
  memory: 13_200_000n,
  steps: 8_000_000_000n,
  bytes: 16_384,
} as const;

export const stepTable = (steps: readonly StepMeasurement[]) =>
  steps.map(({ label, measurement }) => ({
    label,
    bytes: measurement.completeSignedBytes,
    memory: measurement.executionMemory.toString(),
    cpu: measurement.executionSteps.toString(),
  }));
