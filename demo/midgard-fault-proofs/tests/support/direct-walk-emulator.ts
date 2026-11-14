/** Real-contract emulator fixtures shared by the direct-walk families, keyed
 * by the family's walk: the harness, the published role scripts, field
 * carriage, and a driver from a genuine Init through step-01 and the planned
 * custody to the terminal. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  DIRECT_PURPOSE_FIELDS,
  directPurposeArtifactPaging,
  type DirectPurposeContracts,
  type DirectPurposeCustody,
  type DirectPurposeCustodyFamily,
  type DirectPurposeFieldPlan,
  type DirectPurposePaging,
  type DirectPurposeReplay,
  type DirectPurposeRole,
  directPurposeStep01Datum,
  type DirectPurposeTransition,
  type DirectWalkFamily,
  type DirectWalkProof,
  directWalkTerminalFamily,
  fitDirectPurposeTransition,
  submitDirectPurposeInit,
  submitDirectPurposeStep01Accepted,
  submitDirectPurposeStep01Forced,
  submitDirectPurposeTerminal,
  submitDirectPurposeTransition,
  submitDirectPurposeTransitions,
} from "../../src/direct-purpose-family/index.js";
import type { DirectScriptMaterial } from "../../src/direct-script-derivation/material.js";
import { publishDirectWalkCarriage } from "./direct-walk-emulator.carriage.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  makeFaultProofEmulatorHarness,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
} from "./submit-init-emulator-shared.js";

/** The direct-walk families' catalogue categories. */
export type DirectWalkCategory = Extract<
  SDK.FraudProofCatalogueCategoryName,
  | "missingRedeemer"
  | "missingScriptSource"
  | "receivePurposeLanguage"
  | "unusedRedeemer"
  | "executionSourceScriptDecoding"
  | "distinctAssetAccumulationLimit"
  | "executionNativeScriptInvalid"
  | "scriptIntegrityHashMismatch"
  | "unusedScriptWitness"
>;

/** A walk of any step count, its finding and proof types inferred. */
type Walk<
  Prepared extends DirectPurposeReplay,
  Proof extends DirectWalkProof,
> = DirectWalkFamily<DirectWalkCategory, number, Prepared, Proof>;

/** The subject the driver proves: its material, its block's fraudulent
 * output and the inclusion (accepted) or reconstruction (forced) step-01
 * binds. */
export type DirectWalkScenario = Readonly<{
  material: DirectScriptMaterial;
  setup: Readonly<{ fraudulentBlockOutRef: string }>;
  block: Readonly<{
    txInclusion:
      | Parameters<typeof submitDirectPurposeStep01Accepted>[0]["txInclusion"]
      | null;
    reconstruction: Parameters<
      typeof submitDirectPurposeStep01Forced
    >[0]["reconstruction"];
  }>;
}>;

type ContractOptions = NonNullable<
  Parameters<typeof makeFaultProofEmulatorHarness>[0]
>["contractOptions"];

/** The emulator harness with the walk's real contracts: its category's
 * `real…` contract flag, checked against the contracts and the catalogue
 * category it builds. */
export const makeDirectWalkHarness = async (walk: {
  readonly spec: { readonly category: DirectWalkCategory };
}) => {
  const key = walk.spec.category;
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      ...({
        [`real${key.charAt(0).toUpperCase()}${key.slice(1)}`]: true,
      } as ContractOptions),
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const direct: DirectPurposeContracts | undefined = harness.contracts[key];
  const category = harness.catalogue.categories[key];
  if (direct === undefined || category === undefined)
    throw new Error(`${key} harness contracts/category were omitted`);
  if (category.categoryId !== SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[key])
    throw new Error(`unexpected ${key} category id`);
  return { ...harness, direct, category };
};

export type DirectWalkHarness = Awaited<
  ReturnType<typeof makeDirectWalkHarness>
>;

export type RoleReference = {
  readonly role: DirectPurposeRole;
  readonly scriptHash: string;
  readonly utxo: UTxO;
  readonly publication: CompleteSignedTransactionMeasurement;
};

/** Publishes every role script of the walk once through the signed
 * publication path, which refuses any publication above 15,872 bytes. */
export const publishDirectWalkRoles = async (
  harness: DirectWalkHarness,
  { family }: { readonly family: DirectPurposeCustodyFamily },
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

/** A fitted page: its role, its first transition's label and its size. */
export type PageSize = {
  readonly role: DirectPurposeRole;
  readonly label: string;
  readonly size: number;
};

export type StepMeasurement = {
  readonly label: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
};

/** What a family's prover hands the driver: field plans and the custody from
 * `bindEvent` to the terminal's input. */
export type DirectWalkPlan = {
  readonly plan: (field: number) => DirectPurposeFieldPlan;
  readonly transitions: readonly DirectPurposeTransition[];
};

/** Continues the thread role by role from step-01 to the terminal's input,
 * as the workflow does: each page that opens MPF proofs is sized to the
 * transaction budget, and the size of every fitted page is kept. The walk
 * ends where the planned custody does: at its last transition's successor
 * role, holding that transition's datum. */
const continueToTerminalInput = async (
  custody: DirectPurposeCustody,
  paging: DirectPurposePaging,
  planned: readonly DirectPurposeTransition[],
  start: UTxO,
  pages: PageSize[],
): Promise<UTxO> => {
  const last = planned.at(-1)!;
  // The size of the last page each fit built: the page it submits.
  let fitted: number | undefined;
  const sized: DirectPurposePaging = {
    ...paging,
    page: (...args) => {
      const page = paging.page(...args);
      fitted = page.size;
      return page;
    },
  };
  let thread = start;
  let role = planned[0]!.role;
  while (role !== last.next || thread.datum !== last.datum) {
    fitted = undefined;
    const transition = await fitDirectPurposeTransition({
      custody,
      paging: sized,
      role,
      thread,
    });
    if (fitted !== undefined)
      pages.push({ role, label: transition.label, size: fitted });
    thread = (await submitDirectPurposeTransition(custody, thread, transition))
      .thread;
    role = transition.next;
  }
  return thread;
};

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveDirectWalkThread = async <
  Prepared extends DirectPurposeReplay,
  Proof extends DirectWalkProof,
  Plan extends Proof & DirectWalkPlan,
>({
  harness,
  walk,
  scenario,
  references,
  prepare,
  transitions,
  beforeStep01,
}: {
  readonly harness: DirectWalkHarness;
  readonly walk: Walk<Prepared, Proof>;
  readonly scenario: DirectWalkScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  /** The family's prover over the scenario's material. */
  readonly prepare: (args: {
    readonly material: DirectScriptMaterial;
    readonly contracts: DirectPurposeContracts;
    readonly prover: string;
  }) => Plan | Promise<Plan>;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (prepared: Plan) => Plan["transitions"];
  /** Runs against the Init thread before step-01 spends it (e.g. a forged
   * step-01 the validator must refuse). */
  readonly beforeStep01?: (args: {
    readonly thread: UTxO;
    readonly unit: string;
    readonly prepared: Plan;
  }) => Promise<void>;
}) => {
  const { proverLucid, direct, proverSigner } = harness;
  const { family } = walk;
  const steps: StepMeasurement[] = [];
  const pages: PageSize[] = [];
  const prepared = await prepare({
    material: scenario.material,
    contracts: direct,
    prover: proverSigner.paymentKeyHash,
  });
  const { carriage, measurements: carriageMeasurements } =
    await publishDirectWalkCarriage({
      harness,
      label: family.label,
      fields: walk.fields ?? DIRECT_PURPOSE_FIELDS,
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
  if (beforeStep01 !== undefined) {
    const [txHash, outputIndex] = init.result.nextThreadOutRef.split("#");
    const [initThread] = await proverLucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (initThread === undefined) throw new Error("Init thread output absent");
    await beforeStep01({
      thread: initThread,
      unit: init.result.computationThreadUnit,
      prepared,
    });
  }
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
  const thread =
    transitions === undefined
      ? await continueToTerminalInput(
          custody,
          directPurposeArtifactPaging(
            walk.artifact,
            prepared,
            directPurposeStep01Datum(
              scenario.material,
              proverSigner.paymentKeyHash,
            ),
          ),
          prepared.transitions,
          start,
          pages,
        )
      : await submitDirectPurposeTransitions(
          custody,
          start,
          transitions(prepared),
        );
  const terminalFamily = directWalkTerminalFamily(walk, {
    admitted: { material: scenario.material },
    proof: prepared,
  });
  /** `at` is the terminal's input when the caller advanced the thread. */
  const terminal = (requireConviction = true, at: UTxO = thread) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitDirectPurposeTerminal({
        family: terminalFamily,
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
    pages,
    carriageMeasurements,
    terminal,
  };
};

/** Per-step ex-unit and size bounds every thread transaction must meet. */
export const DIRECT_WALK_STEP_BOUNDS = {
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
