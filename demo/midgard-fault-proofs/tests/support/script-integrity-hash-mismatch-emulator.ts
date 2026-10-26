/** Real-contract emulator fixtures for the `scriptIntegrityHashMismatch` family: the
 * harness, the 21 published role scripts, field carriage, and a driver from
 * a genuine Init through step-01 and the planned custody to the terminal. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  prepareScriptIntegrityHashMismatchProof,
  SCRIPT_INTEGRITY_HASH_MISMATCH_FIELDS,
  scriptIntegrityHashMismatchContractRoles,
  type ScriptIntegrityHashMismatchCustody,
  type ScriptIntegrityHashMismatchProofPlan,
  type ScriptIntegrityHashMismatchRole,
  type ScriptIntegrityHashMismatchWalkBatch,
  submitScriptIntegrityHashMismatchInit,
  submitScriptIntegrityHashMismatchStep01Accepted,
  submitScriptIntegrityHashMismatchStep01Forced,
  submitScriptIntegrityHashMismatchTerminal,
  submitScriptIntegrityHashMismatchTransitions,
} from "../../src/script-integrity-hash-mismatch/index.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import type { IntegrityScenario } from "./script-integrity-hash-mismatch-emulator.subject.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  makeFaultProofEmulatorHarness,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
} from "./submit-init-emulator-shared.js";

export const makeScriptIntegrityHashMismatchHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realScriptIntegrityHashMismatch: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const scriptIntegrityHashMismatch =
    harness.contracts.scriptIntegrityHashMismatch;
  const category = harness.catalogue.categories.scriptIntegrityHashMismatch;
  if (scriptIntegrityHashMismatch === undefined || category === undefined)
    throw new Error(
      "script-integrity-hash-mismatch harness contracts/category were omitted",
    );
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.scriptIntegrityHashMismatch
  )
    throw new Error("unexpected script-integrity-hash-mismatch category id");
  return { ...harness, scriptIntegrityHashMismatch, category };
};

export type ScriptIntegrityHashMismatchHarness = Awaited<
  ReturnType<typeof makeScriptIntegrityHashMismatchHarness>
>;

export type RoleReference = {
  readonly role: ScriptIntegrityHashMismatchRole;
  readonly scriptHash: string;
  readonly utxo: UTxO;
  readonly publication: CompleteSignedTransactionMeasurement;
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishScriptIntegrityHashMismatchRoles = async (
  harness: ScriptIntegrityHashMismatchHarness,
): Promise<ReadonlyMap<ScriptIntegrityHashMismatchRole, RoleReference>> => {
  const roles = scriptIntegrityHashMismatchContractRoles(
    harness.scriptIntegrityHashMismatch,
  );
  const byHash = new Map<string, RoleReference>();
  const out = new Map<ScriptIntegrityHashMismatchRole, RoleReference>();
  for (const role of SDK.SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER) {
    const validator = roles[role];
    let reference = byHash.get(validator.spendingScriptHash);
    if (reference === undefined) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: validator.spendingScript,
        label: `script-integrity-hash-mismatch ${role}`,
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
export const publishScriptIntegrityHashMismatchCarriage = async ({
  harness,
  scenario,
  plan,
}: {
  readonly harness: ScriptIntegrityHashMismatchHarness;
  readonly scenario: IntegrityScenario;
  readonly plan: ScriptIntegrityHashMismatchProofPlan["plan"];
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
  for (const field of SCRIPT_INTEGRITY_HASH_MISMATCH_FIELDS) {
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
        label: `script-integrity-hash-mismatch field ${field.toString()}`,
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
          label: "script-integrity-hash-mismatch field certificate",
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

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveScriptIntegrityHashMismatchThread = async ({
  harness,
  scenario,
  references,
  batch,
  transitions,
}: {
  readonly harness: ScriptIntegrityHashMismatchHarness;
  readonly scenario: IntegrityScenario;
  readonly references: ReadonlyMap<
    ScriptIntegrityHashMismatchRole,
    RoleReference
  >;
  readonly batch?: ScriptIntegrityHashMismatchWalkBatch;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: ScriptIntegrityHashMismatchProofPlan,
  ) => ScriptIntegrityHashMismatchProofPlan["transitions"];
}) => {
  const { proverLucid, scriptIntegrityHashMismatch, proverSigner } = harness;
  const steps: StepMeasurement[] = [];
  const prepared = await prepareScriptIntegrityHashMismatchProof({
    material: scenario.material,
    contracts: scriptIntegrityHashMismatch,
    prover: proverSigner.paymentKeyHash,
    ...(batch === undefined ? {} : { batch }),
  });
  const { carriage, measurements: carriageMeasurements } =
    await publishScriptIntegrityHashMismatchCarriage({
      harness,
      scenario,
      plan: prepared.plan,
    });
  const reference = (role: ScriptIntegrityHashMismatchRole) => {
    const found = references.get(role);
    if (found === undefined) throw new Error(`${role} reference absent`);
    return found.utxo;
  };
  const init = await captureEmulatorSubmission(harness.emulator, () =>
    submitScriptIntegrityHashMismatchInit({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: scriptIntegrityHashMismatch,
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
    contracts: scriptIntegrityHashMismatch,
    categoryId: harness.category.categoryId,
    signer: proverSigner,
    threadOutRef: init.result.nextThreadOutRef,
    material: scenario.material,
    referenceScriptUtxo: reference("step01"),
  };
  const step01 = await captureEmulatorSubmission(harness.emulator, () =>
    scenario.block.txInclusion === null
      ? submitScriptIntegrityHashMismatchStep01Forced({
          ...common,
          reconstruction: scenario.block.reconstruction,
        })
      : submitScriptIntegrityHashMismatchStep01Accepted({
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
  const custody: ScriptIntegrityHashMismatchCustody = {
    lucid: proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: scriptIntegrityHashMismatch,
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
  const thread = await submitScriptIntegrityHashMismatchTransitions(
    custody,
    start,
    transitions?.(prepared) ?? prepared.transitions,
  );
  /** `at` is the terminal's input when the caller advanced the thread. */
  const terminal = (requireConviction = true, at: UTxO = thread) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitScriptIntegrityHashMismatchTerminal({
        lucid: proverLucid,
        contracts: scriptIntegrityHashMismatch,
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
export const SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS = {
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
