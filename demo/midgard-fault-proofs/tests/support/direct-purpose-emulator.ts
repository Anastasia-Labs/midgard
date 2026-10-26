/** Real-contract emulator fixtures shared by the direct purpose families: the
 * harness, the published role scripts, field carriage, and a driver from a
 * genuine Init through step-01 and a planned custody to the terminal. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  DIRECT_PURPOSE_FIELDS,
  type DirectPurposeContracts,
  type DirectPurposeCustody,
  type DirectPurposeFamily,
  directPurposeFieldPlans,
  type DirectPurposeRole,
  type DirectPurposeTransition,
  submitDirectPurposeInit,
  submitDirectPurposeStep01Accepted,
  submitDirectPurposeStep01Forced,
  submitDirectPurposeTerminal,
  submitDirectPurposeTransitions,
} from "../../src/direct-purpose-family/index.js";
import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import type { MissingRedeemerScenario as DirectPurposeScenario } from "./missing-redeemer-emulator.subject.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  makeFaultProofEmulatorHarness,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
} from "./submit-init-emulator-shared.js";

export type DirectPurposeCategory = "missingRedeemer" | "missingScriptSource";

/** The emulator harness with one direct purpose family's real contracts. */
export const makeDirectPurposeHarness = async (key: DirectPurposeCategory) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      ...(key === "missingRedeemer"
        ? { realMissingRedeemer: true }
        : { realMissingScriptSource: true }),
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const direct = harness.contracts[key];
  const category = harness.catalogue.categories[key];
  if (direct === undefined || category === undefined)
    throw new Error(`${key} harness contracts/category were omitted`);
  if (category.categoryId !== SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[key])
    throw new Error(`unexpected ${key} category id`);
  return { ...harness, direct, category };
};

export type DirectPurposeHarness = Awaited<
  ReturnType<typeof makeDirectPurposeHarness>
>;

export type RoleReference = {
  readonly role: DirectPurposeRole;
  readonly scriptHash: string;
  readonly utxo: UTxO;
  readonly publication: CompleteSignedTransactionMeasurement;
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishDirectPurposeRoles = async (
  harness: DirectPurposeHarness,
  family: DirectPurposeFamily<unknown>,
): Promise<ReadonlyMap<DirectPurposeRole, RoleReference>> => {
  const roles = family.roles(harness.direct);
  const byHash = new Map<string, RoleReference>();
  const out = new Map<DirectPurposeRole, RoleReference>();
  for (const role of family.roleOrder) {
    const validator = roles[role];
    if (validator === undefined)
      throw new Error(`${family.label}: ${role} has no validator`);
    let reference = byHash.get(validator.spendingScriptHash);
    if (reference === undefined) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: validator.spendingScript,
        label: `${family.label} ${role}`,
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
export const publishDirectPurposeCarriage = async ({
  harness,
  label,
  scenario,
  plan,
}: {
  readonly harness: DirectPurposeHarness;
  readonly label: string;
  readonly scenario: DirectPurposeScenario;
  readonly plan: DirectPurposePlan["plan"];
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
  for (const field of DIRECT_PURPOSE_FIELDS) {
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
        label: `${label} field ${field.toString()}`,
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
          label: `${label} field certificate`,
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

/** What a family's prover hands the driver: field plans and the custody from
 * `bindEvent` to the terminal's input. */
export type DirectPurposePlan = {
  readonly plan: ReturnType<typeof directPurposeFieldPlans>;
  readonly transitions: readonly DirectPurposeTransition[];
};

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveDirectPurposeThread = async <Plan extends DirectPurposePlan>({
  harness,
  family,
  scenario,
  references,
  prepare,
  transitions,
}: {
  readonly harness: DirectPurposeHarness;
  readonly family: DirectPurposeFamily<unknown>;
  readonly scenario: DirectPurposeScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  /** The family's prover over the scenario's material. */
  readonly prepare: (args: {
    readonly material: DirectPurposeScenario["material"];
    readonly contracts: DirectPurposeContracts;
    readonly prover: string;
  }) => Promise<Plan>;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (prepared: Plan) => Plan["transitions"];
}) => {
  const { proverLucid, direct, proverSigner } = harness;
  const steps: StepMeasurement[] = [];
  const prepared = await prepare({
    material: scenario.material,
    contracts: direct,
    prover: proverSigner.paymentKeyHash,
  });
  const { carriage, measurements: carriageMeasurements } =
    await publishDirectPurposeCarriage({
      harness,
      label: family.label,
      scenario,
      plan: prepared.plan,
    });
  const reference = (role: DirectPurposeRole) => {
    const found = references.get(role);
    if (found === undefined) throw new Error(`${role} reference absent`);
    return found.utxo;
  };
  const init = await captureEmulatorSubmission(harness.emulator, () =>
    submitDirectPurposeInit({
      family,
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: direct,
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
  const common = {
    family,
    lucid: proverLucid,
    contracts: direct,
    categoryId: harness.category.categoryId,
    signer: proverSigner,
    threadOutRef: init.result.nextThreadOutRef,
    material: scenario.material,
    referenceScriptUtxo: reference("step01"),
  };
  const step01 = await captureEmulatorSubmission(harness.emulator, () =>
    scenario.block.txInclusion === null
      ? submitDirectPurposeStep01Forced({
          ...common,
          reconstruction: scenario.block.reconstruction,
        })
      : submitDirectPurposeStep01Accepted({
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
  const custody: DirectPurposeCustody = {
    family,
    lucid: proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: direct,
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
  const thread = await submitDirectPurposeTransitions(
    custody,
    start,
    transitions?.(prepared) ?? prepared.transitions,
  );
  /** `at` is the terminal's input when the caller advanced the thread. */
  const terminal = (requireConviction = true, at: UTxO = thread) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitDirectPurposeTerminal({
        family,
        lucid: proverLucid,
        contracts: direct,
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
export const DIRECT_PURPOSE_STEP_BOUNDS = {
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
