/** Real-contract emulator fixtures for the `missingRedeemer` family: the
 * harness, the 39 published role scripts, field carriage, and a driver from
 * a genuine Init through step-01 and the planned custody to the terminal. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  MISSING_REDEEMER_FIELDS,
  missingRedeemerContractRoles,
  type MissingRedeemerCustody,
  type MissingRedeemerProofPlan,
  type MissingRedeemerRole,
  prepareMissingRedeemerProof,
  submitMissingRedeemerInit,
  submitMissingRedeemerStep01Accepted,
  submitMissingRedeemerStep01Forced,
  submitMissingRedeemerTerminal,
  submitMissingRedeemerTransitions,
} from "../../src/missing-redeemer/index.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  makeFaultProofEmulatorHarness,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
} from "./submit-init-emulator-shared.js";

export const makeMissingRedeemerHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMissingRedeemer: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const missingRedeemer = harness.contracts.missingRedeemer;
  const category = harness.catalogue.categories.missingRedeemer;
  if (missingRedeemer === undefined || category === undefined)
    throw new Error("missing-redeemer harness contracts/category were omitted");
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingRedeemer
  )
    throw new Error("unexpected missing-redeemer category id");
  return { ...harness, missingRedeemer, category };
};

export type MissingRedeemerHarness = Awaited<
  ReturnType<typeof makeMissingRedeemerHarness>
>;

export type RoleReference = {
  readonly role: MissingRedeemerRole;
  readonly scriptHash: string;
  readonly utxo: UTxO;
  readonly publication: CompleteSignedTransactionMeasurement;
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishMissingRedeemerRoles = async (
  harness: MissingRedeemerHarness,
): Promise<ReadonlyMap<MissingRedeemerRole, RoleReference>> => {
  const roles = missingRedeemerContractRoles(harness.missingRedeemer);
  const byHash = new Map<string, RoleReference>();
  const out = new Map<MissingRedeemerRole, RoleReference>();
  for (const role of SDK.MISSING_REDEEMER_ROLE_ORDER) {
    const validator = roles[role];
    let reference = byHash.get(validator.spendingScriptHash);
    if (reference === undefined) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: validator.spendingScript,
        label: `missing-redeemer ${role}`,
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
export const publishMissingRedeemerCarriage = async ({
  harness,
  scenario,
  plan,
}: {
  readonly harness: MissingRedeemerHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly plan: MissingRedeemerProofPlan["plan"];
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
  let certificateReference: UTxO | undefined;
  for (const field of MISSING_REDEEMER_FIELDS) {
    const planned = plan(field).openingPlan;
    const tier = planned.plan.tier;
    if (tier === "Inline") continue;
    const published = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `missing-redeemer field ${field.toString()}`,
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
          label: "missing-redeemer field certificate",
        })
      ).utxo;
      const certified = await captureEmulatorSubmission(harness.emulator, () =>
        certifyFaultProofFieldCarriage({
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

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveMissingRedeemerThread = async ({
  harness,
  scenario,
  references,
  observed,
  transitions,
}: {
  readonly harness: MissingRedeemerHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly references: ReadonlyMap<MissingRedeemerRole, RoleReference>;
  readonly observed?: SDK.DirectPurposeCoordinate;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: MissingRedeemerProofPlan,
  ) => MissingRedeemerProofPlan["transitions"];
}) => {
  const { proverLucid, missingRedeemer, proverSigner } = harness;
  const steps: StepMeasurement[] = [];
  const prepared = await prepareMissingRedeemerProof({
    material: scenario.material,
    contracts: missingRedeemer,
    prover: proverSigner.paymentKeyHash,
    ...(observed === undefined ? {} : { observed }),
  });
  const { carriage, measurements: carriageMeasurements } =
    await publishMissingRedeemerCarriage({
      harness,
      scenario,
      plan: prepared.plan,
    });
  const reference = (role: MissingRedeemerRole) => {
    const found = references.get(role);
    if (found === undefined) throw new Error(`${role} reference absent`);
    return found.utxo;
  };
  const init = await captureEmulatorSubmission(harness.emulator, () =>
    submitMissingRedeemerInit({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: missingRedeemer,
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
    lucid: proverLucid,
    contracts: missingRedeemer,
    categoryId: harness.category.categoryId,
    signer: proverSigner,
    threadOutRef: init.result.nextThreadOutRef,
    material: scenario.material,
    referenceScriptUtxo: reference("step01"),
  };
  const step01 = await captureEmulatorSubmission(harness.emulator, () =>
    scenario.block.txInclusion === null
      ? submitMissingRedeemerStep01Forced({
          ...common,
          reconstruction: scenario.block.reconstruction,
        })
      : submitMissingRedeemerStep01Accepted({
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
  const custody: MissingRedeemerCustody = {
    lucid: proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: missingRedeemer,
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
  const thread = await submitMissingRedeemerTransitions(
    custody,
    start,
    transitions?.(prepared) ?? prepared.transitions,
  );
  /** `at` is the terminal's input when the caller advanced the thread. */
  const terminal = (requireConviction = true, at: UTxO = thread) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitMissingRedeemerTerminal({
        lucid: proverLucid,
        contracts: missingRedeemer,
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
export const MISSING_REDEEMER_STEP_BOUNDS = {
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
