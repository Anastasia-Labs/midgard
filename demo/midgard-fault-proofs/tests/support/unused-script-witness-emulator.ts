/** Real-contract emulator fixtures for the `unusedScriptWitness` family: the
 * harness, the 24 published role scripts, field carriage, and a driver from
 * a genuine Init through step-01 and the planned custody to the terminal. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  directPurposePaging,
  type DirectPurposeRole,
} from "../../src/direct-purpose-family/index.js";
import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  fitUnusedScriptWitnessPage,
  fitUnusedScriptWitnessSourceTransition,
  isUnusedScriptWitnessPagedRole,
  prepareUnusedScriptWitnessProof,
  submitUnusedScriptWitnessInit,
  submitUnusedScriptWitnessStep01Accepted,
  submitUnusedScriptWitnessStep01Forced,
  submitUnusedScriptWitnessTerminal,
  submitUnusedScriptWitnessTransition,
  submitUnusedScriptWitnessTransitions,
  UNUSED_SCRIPT_WITNESS_FIELDS,
  unusedScriptWitnessContractRoles,
  type UnusedScriptWitnessCustody,
  type UnusedScriptWitnessProofPlan,
  type UnusedScriptWitnessRole,
  unusedScriptWitnessStep01Datum,
  unusedScriptWitnessTransitionAt,
  type UnusedScriptWitnessWalkBatch,
} from "../../src/unused-script-witness/index.js";
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

export const makeUnusedScriptWitnessHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realUnusedScriptWitness: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const unusedScriptWitness = harness.contracts.unusedScriptWitness;
  const category = harness.catalogue.categories.unusedScriptWitness;
  if (unusedScriptWitness === undefined || category === undefined)
    throw new Error(
      "unused-script-witness harness contracts/category were omitted",
    );
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.unusedScriptWitness
  )
    throw new Error("unexpected unused-script-witness category id");
  return { ...harness, unusedScriptWitness, category };
};

export type UnusedScriptWitnessHarness = Awaited<
  ReturnType<typeof makeUnusedScriptWitnessHarness>
>;

export type RoleReference = {
  readonly role: UnusedScriptWitnessRole;
  readonly scriptHash: string;
  readonly utxo: UTxO;
  readonly publication: CompleteSignedTransactionMeasurement;
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishUnusedScriptWitnessRoles = async (
  harness: UnusedScriptWitnessHarness,
): Promise<ReadonlyMap<UnusedScriptWitnessRole, RoleReference>> => {
  const roles = unusedScriptWitnessContractRoles(harness.unusedScriptWitness);
  const byHash = new Map<string, RoleReference>();
  const out = new Map<UnusedScriptWitnessRole, RoleReference>();
  for (const role of SDK.UNUSED_SCRIPT_WITNESS_ROLE_ORDER) {
    const validator = roles[role];
    let reference = byHash.get(validator.spendingScriptHash);
    if (reference === undefined) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: validator.spendingScript,
        label: `unused-script-witness ${role}`,
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
const publishCarriage = async ({
  harness,
  scenario,
  plan,
}: {
  readonly harness: UnusedScriptWitnessHarness;
  readonly scenario: IntegrityScenario;
  readonly plan: UnusedScriptWitnessProofPlan["plan"];
}) => {
  const carriage = new Map<number, readonly UTxO[]>();
  const measurements: {
    field: number;
    tier: string;
    measurement: CompleteSignedTransactionMeasurement;
  }[] = [];
  const ownOutputs = createOwnOutputJournal();
  let certificateReference: UTxO | undefined;
  for (const field of UNUSED_SCRIPT_WITNESS_FIELDS) {
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
        label: `unused-script-witness field ${field.toString()}`,
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
          label: "unused-script-witness field certificate",
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

/** Continues the thread role by role to the terminal's input, as the
 * workflow does: each page that opens MPF proofs, in the shared source walk
 * and in the all-purposes walk, is sized to the transaction budget. */
const continueToTerminal = async (
  custody: UnusedScriptWitnessCustody,
  prepared: UnusedScriptWitnessProofPlan,
  start: UTxO,
  scenario: IntegrityScenario,
): Promise<UTxO> => {
  const step01Datum = unusedScriptWitnessStep01Datum(
    scenario.material,
    custody.signer.paymentKeyHash,
  );
  const paging = directPurposePaging({
    label: "unusedScriptWitness",
    transitions: prepared.source,
    first: step01Datum,
  });
  const sourceRoles = new Set<string>(prepared.source.map(({ role }) => role));
  const isSourceRole = (
    role: UnusedScriptWitnessRole,
  ): role is UnusedScriptWitnessRole & DirectPurposeRole =>
    sourceRoles.has(role);
  const transitionAt = unusedScriptWitnessTransitionAt(
    prepared,
    step01Datum,
    paging,
    isSourceRole,
  );
  let thread = start;
  let role = prepared.transitions[0]!.role;
  while (role !== "terminal") {
    const transition = isSourceRole(role)
      ? await fitUnusedScriptWitnessSourceTransition({
          custody,
          paging,
          role,
          thread,
        })
      : isUnusedScriptWitnessPagedRole(role)
        ? await fitUnusedScriptWitnessPage({
            custody,
            walk: prepared.walk,
            role,
            thread,
          })
        : transitionAt(role, thread.datum!);
    thread = (
      await submitUnusedScriptWitnessTransition(custody, thread, transition)
    ).thread;
    role = transition.next;
  }
  return thread;
};

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveUnusedScriptWitnessThread = async ({
  harness,
  scenario,
  references,
  scriptIndex,
  batch,
  transitions,
  beforeStep01,
}: {
  readonly harness: UnusedScriptWitnessHarness;
  readonly scenario: IntegrityScenario;
  readonly references: ReadonlyMap<UnusedScriptWitnessRole, RoleReference>;
  /** The witness to walk; the first convicting one otherwise. */
  readonly scriptIndex?: bigint;
  readonly batch?: UnusedScriptWitnessWalkBatch;
  /** Replaces the planned custody (e.g. an empty prefix for forged rows). */
  readonly transitions?: (
    prepared: UnusedScriptWitnessProofPlan,
  ) => UnusedScriptWitnessProofPlan["transitions"];
  /** Runs against the Init thread before step-01 spends it. */
  readonly beforeStep01?: (args: {
    readonly thread: UTxO;
    readonly unit: string;
    readonly prepared: UnusedScriptWitnessProofPlan;
  }) => Promise<void>;
}) => {
  const { proverLucid, unusedScriptWitness, proverSigner } = harness;
  const steps: StepMeasurement[] = [];
  const prepared = await prepareUnusedScriptWitnessProof({
    material: scenario.material,
    contracts: unusedScriptWitness,
    prover: proverSigner.paymentKeyHash,
    ...(scriptIndex === undefined ? {} : { scriptIndex }),
    ...(batch === undefined ? {} : { batch }),
  });
  const { carriage, measurements: carriageMeasurements } =
    await publishCarriage({ harness, scenario, plan: prepared.plan });
  const reference = (role: UnusedScriptWitnessRole) => {
    const found = references.get(role);
    if (found === undefined) throw new Error(`${role} reference absent`);
    return found.utxo;
  };
  const init = await captureEmulatorSubmission(harness.emulator, () =>
    submitUnusedScriptWitnessInit({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: unusedScriptWitness,
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
    contracts: unusedScriptWitness,
    categoryId: harness.category.categoryId,
    signer: proverSigner,
    threadOutRef: init.result.nextThreadOutRef,
    material: scenario.material,
    referenceScriptUtxo: reference("step01"),
  };
  const step01 = await captureEmulatorSubmission(harness.emulator, () =>
    scenario.block.txInclusion === null
      ? submitUnusedScriptWitnessStep01Forced({
          ...common,
          reconstruction: scenario.block.reconstruction,
        })
      : submitUnusedScriptWitnessStep01Accepted({
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
  const custody: UnusedScriptWitnessCustody = {
    lucid: proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: unusedScriptWitness,
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
      : await submitUnusedScriptWitnessTransitions(
          custody,
          start,
          transitions(prepared),
        );
  /** `at` is the terminal's input when the caller advanced the thread. */
  const terminal = (requireConviction = true, at: UTxO = thread) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitUnusedScriptWitnessTerminal({
        lucid: proverLucid,
        contracts: unusedScriptWitness,
        selection: prepared.selection,
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
export const UNUSED_SCRIPT_WITNESS_STEP_BOUNDS = {
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
