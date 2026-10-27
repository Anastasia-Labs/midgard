/** Real-contract emulator fixtures for the `executionNativeScriptInvalid` family: the
 * harness, the 28 published role scripts, field carriage, and a driver from
 * a genuine Init through step-01 and the planned custody to the terminal. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  directPurposePaging,
  type DirectPurposeRole,
} from "../../src/direct-purpose-family/index.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS,
  executionNativeScriptInvalidContractRoles,
  type ExecutionNativeScriptInvalidCustody,
  type ExecutionNativeScriptInvalidEntry,
  type ExecutionNativeScriptInvalidProofPlan,
  type ExecutionNativeScriptInvalidRole,
  executionNativeScriptInvalidStep01Datum,
  executionNativeScriptInvalidTransitionAt,
  type ExecutionNativeScriptInvalidWalkBatch,
  fitExecutionNativeScriptInvalidSourceTransition,
  fitExecutionNativeScriptInvalidWalkPage,
  isExecutionNativeScriptInvalidWalkRole,
  prepareExecutionNativeScriptInvalidProof,
  submitExecutionNativeScriptInvalidInit,
  submitExecutionNativeScriptInvalidStep01Accepted,
  submitExecutionNativeScriptInvalidStep01Forced,
  submitExecutionNativeScriptInvalidTerminal,
  submitExecutionNativeScriptInvalidTransition,
  submitExecutionNativeScriptInvalidTransitions,
} from "../../src/execution-native-script-invalid/index.js";
import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import type { NativeScenario } from "./execution-native-script-invalid-emulator.subject.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  makeFaultProofEmulatorHarness,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
} from "./submit-init-emulator-shared.js";

export const makeExecutionNativeScriptInvalidHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realExecutionNativeScriptInvalid: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const executionNativeScriptInvalid =
    harness.contracts.executionNativeScriptInvalid;
  const category = harness.catalogue.categories.executionNativeScriptInvalid;
  if (executionNativeScriptInvalid === undefined || category === undefined)
    throw new Error(
      "execution-native-script-invalid harness contracts/category were omitted",
    );
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.executionNativeScriptInvalid
  )
    throw new Error("unexpected execution-native-script-invalid category id");
  return { ...harness, executionNativeScriptInvalid, category };
};

export type ExecutionNativeScriptInvalidHarness = Awaited<
  ReturnType<typeof makeExecutionNativeScriptInvalidHarness>
>;

export type RoleReference = {
  readonly role: ExecutionNativeScriptInvalidRole;
  readonly scriptHash: string;
  readonly utxo: UTxO;
  readonly publication: CompleteSignedTransactionMeasurement;
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishExecutionNativeScriptInvalidRoles = async (
  harness: ExecutionNativeScriptInvalidHarness,
): Promise<ReadonlyMap<ExecutionNativeScriptInvalidRole, RoleReference>> => {
  const roles = executionNativeScriptInvalidContractRoles(
    harness.executionNativeScriptInvalid,
  );
  const byHash = new Map<string, RoleReference>();
  const out = new Map<ExecutionNativeScriptInvalidRole, RoleReference>();
  for (const role of SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER) {
    const validator = roles[role];
    let reference = byHash.get(validator.spendingScriptHash);
    if (reference === undefined) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: validator.spendingScript,
        label: `execution-native-script-invalid ${role}`,
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
  readonly harness: ExecutionNativeScriptInvalidHarness;
  readonly scenario: NativeScenario;
  readonly plan: ExecutionNativeScriptInvalidProofPlan["plan"];
}) => {
  const carriage = new Map<number, readonly UTxO[]>();
  const measurements: {
    field: number;
    tier: string;
    measurement: CompleteSignedTransactionMeasurement;
  }[] = [];
  const ownOutputs = createOwnOutputJournal();
  let certificateReference: UTxO | undefined;
  for (const field of EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS) {
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
        label: `execution-native-script-invalid field ${field.toString()}`,
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
          label: "execution-native-script-invalid field certificate",
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

/** A fitted page: its role, its first transition's label and its size. */
export type PageSize = {
  readonly role: ExecutionNativeScriptInvalidRole;
  readonly label: string;
  readonly size: number;
};

export type StepMeasurement = {
  readonly label: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
};

/** Continues the thread role by role to the terminal's input, as the
 * workflow does: each page that opens MPF proofs, in the shared source walk
 * and in the all-purposes walk, is sized to the transaction budget. */
const continueToTerminal = async (
  custody: ExecutionNativeScriptInvalidCustody,
  prepared: ExecutionNativeScriptInvalidProofPlan,
  start: UTxO,
  scenario: NativeScenario,
  pages: PageSize[],
): Promise<UTxO> => {
  const step01Datum = executionNativeScriptInvalidStep01Datum(
    scenario.material,
    custody.signer.paymentKeyHash,
  );
  const paging = directPurposePaging({
    label: "executionNativeScriptInvalid",
    transitions: prepared.source,
    first: step01Datum,
  });
  const sourceRoles = new Set<string>(prepared.source.map(({ role }) => role));
  const isSourceRole = (
    role: ExecutionNativeScriptInvalidRole,
  ): role is ExecutionNativeScriptInvalidRole & DirectPurposeRole =>
    sourceRoles.has(role);
  const transitionAt = executionNativeScriptInvalidTransitionAt(
    prepared,
    step01Datum,
    paging,
    isSourceRole,
  );
  // The size of the last page each fit built: the page it submits.
  let fitted: number | undefined;
  const sizedPaging = {
    ...paging,
    page: (...args: Parameters<typeof paging.page>) => {
      const page = paging.page(...args);
      fitted = page.size;
      return page;
    },
  };
  const walk = prepared.walk;
  const sizedWalk =
    walk === null
      ? null
      : {
          ...walk,
          page: (...args: Parameters<typeof walk.page>) => {
            const page = walk.page(...args);
            fitted = page.size;
            return page;
          },
        };
  let thread = start;
  let role = prepared.transitions[0]!.role;
  while (role !== "terminal") {
    fitted = undefined;
    const transition = isSourceRole(role)
      ? await fitExecutionNativeScriptInvalidSourceTransition({
          custody,
          paging: sizedPaging,
          role,
          thread,
        })
      : isExecutionNativeScriptInvalidWalkRole(role) && sizedWalk !== null
        ? await fitExecutionNativeScriptInvalidWalkPage({
            custody,
            walk: sizedWalk,
            role,
            thread,
          })
        : transitionAt(role, thread.datum!);
    if (fitted !== undefined)
      pages.push({ role, label: transition.label, size: fitted });
    thread = (
      await submitExecutionNativeScriptInvalidTransition(
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
export const driveExecutionNativeScriptInvalidThread = async ({
  harness,
  scenario,
  references,
  entry,
  batch,
  transitions,
  beforeStep01,
}: {
  readonly harness: ExecutionNativeScriptInvalidHarness;
  readonly scenario: NativeScenario;
  readonly references: ReadonlyMap<
    ExecutionNativeScriptInvalidRole,
    RoleReference
  >;
  /** The route to take; the first convicting one otherwise. */
  readonly entry?: ExecutionNativeScriptInvalidEntry;
  readonly batch?: ExecutionNativeScriptInvalidWalkBatch;
  /** Replaces the planned custody (e.g. an empty prefix for forged rows). */
  readonly transitions?: (
    prepared: ExecutionNativeScriptInvalidProofPlan,
  ) => ExecutionNativeScriptInvalidProofPlan["transitions"];
  /** Runs against the Init thread before step-01 spends it. */
  readonly beforeStep01?: (args: {
    readonly thread: UTxO;
    readonly unit: string;
    readonly prepared: ExecutionNativeScriptInvalidProofPlan;
  }) => Promise<void>;
}) => {
  const { proverLucid, executionNativeScriptInvalid, proverSigner } = harness;
  const steps: StepMeasurement[] = [];
  const pages: PageSize[] = [];
  const prepared = await prepareExecutionNativeScriptInvalidProof({
    material: scenario.material,
    contracts: executionNativeScriptInvalid,
    prover: proverSigner.paymentKeyHash,
    ...(entry === undefined ? {} : { entry }),
    ...(batch === undefined ? {} : { batch }),
  });
  const { carriage, measurements: carriageMeasurements } =
    await publishCarriage({ harness, scenario, plan: prepared.plan });
  const reference = (role: ExecutionNativeScriptInvalidRole) => {
    const found = references.get(role);
    if (found === undefined) throw new Error(`${role} reference absent`);
    return found.utxo;
  };
  const init = await captureEmulatorSubmission(harness.emulator, () =>
    submitExecutionNativeScriptInvalidInit({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: executionNativeScriptInvalid,
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
    contracts: executionNativeScriptInvalid,
    categoryId: harness.category.categoryId,
    signer: proverSigner,
    threadOutRef: init.result.nextThreadOutRef,
    material: scenario.material,
    referenceScriptUtxo: reference("step01"),
  };
  const step01 = await captureEmulatorSubmission(harness.emulator, () =>
    scenario.block.txInclusion === null
      ? submitExecutionNativeScriptInvalidStep01Forced({
          ...common,
          reconstruction: scenario.block.reconstruction,
        })
      : submitExecutionNativeScriptInvalidStep01Accepted({
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
  const custody: ExecutionNativeScriptInvalidCustody = {
    lucid: proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: executionNativeScriptInvalid,
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
      ? await continueToTerminal(custody, prepared, start, scenario, pages)
      : await submitExecutionNativeScriptInvalidTransitions(
          custody,
          start,
          transitions(prepared),
        );
  /** `at` is the terminal's input when the caller advanced the thread. */
  const terminal = (requireConviction = true, at: UTxO = thread) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitExecutionNativeScriptInvalidTerminal({
        lucid: proverLucid,
        contracts: executionNativeScriptInvalid,
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
    pages,
    carriageMeasurements,
    terminal,
  };
};

/** Per-step ex-unit and size bounds every thread transaction must meet. */
export const EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS = {
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
