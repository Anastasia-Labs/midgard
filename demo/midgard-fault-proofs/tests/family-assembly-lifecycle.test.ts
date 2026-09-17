import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import { RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../src/workflow/cursor-family-adapter.js";
import * as deployment from "../src/workflow/deployment-manifest-binding.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflowConfig,
} from "../src/workflow/family-definition.js";
import * as observations from "../src/workflow/family-l1-observation.js";
import * as carriage from "../src/workflow/field-carriage-prerequisite.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../src/workflow/manifest-bound-family-assembly.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../src/workflow/raw-l1-snapshot.js";

afterEach(() => vi.restoreAllMocks());

const category = "receivePurposeLanguage";
const headerHash = "aa".repeat(28);
const owner = "11".repeat(28);
const script = { type: "PlutusV3", script: "49480100002221200101" } as const;
type Runtime = Readonly<{ label: string }>;
type Context = FamilyAssemblyContext<
  typeof category,
  "computationThreadMint",
  false,
  1,
  Runtime
>;

const fixture = () => {
  const contexts: Context[] = [];
  const requirements: string[] = [];
  const address = credentialToAddress("Preprod", { type: "Key", hash: owner });
  const references = [0, 1, 2].map(
    (outputIndex): UTxO => ({
      txHash: "22".repeat(32),
      outputIndex,
      address,
      assets: { lovelace: 2_000_000n },
      scriptRef: script,
    }),
  );
  const names = ["familyStep", "computationThreadMint", "correctionLockSpend"];
  const binding = {
    deploymentFingerprint: "44".repeat(32),
    network: "Preprod",
    definition: { category, headerHash },
    fieldPreimageCertificate: null,
    releaseFinality: {},
    releaseEconomics: {},
    cardanoProtocolParameters: { maxTxSize: 16384 },
    referenceScriptsByContract: Object.fromEntries(
      names.map((name, index) => [
        name,
        {
          outRef: `${references[index]!.txHash}#${index}`,
          scriptHash: validatorToScriptHash(script),
        },
      ]),
    ),
  };
  const bind = vi
    .spyOn(deployment, "bindFraudProofWorkflowDeployment")
    .mockResolvedValue(binding as never);
  const l1 = {
    portVersion: observations.FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category,
    rawL1: { authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY },
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: vi.fn(),
    },
    observeHeader: vi.fn(),
    transactionConfirmed: vi.fn(async () => false),
    observe: vi.fn(async () => ({
      provenance: {
        trustClass: "authenticated_cardano_l1",
        sourceId: "fixture",
        grade: "security",
      },
      stage: {
        kind: "not_started",
        stateQueueBlockOutRef: `${"33".repeat(32)}#0`,
      },
    })),
  };
  const observe = vi
    .spyOn(observations, "createFraudProofFamilyLocalKupmiosL1ObservationPort")
    .mockReturnValue(l1 as never);
  const decorate = vi.spyOn(carriage, "withFieldCarriagePrerequisite");
  const arm = {
    kind: "cursor" as const,
    spec: {
      category,
      stepCount: 1,
      successors: { 1: ["proof_token"] },
    } as const,
    stepContractNames: ["familyStep"] as const,
    createRefineAction: (context: Context) => async () => ({
      runLabel: context.runtime.label,
    }),
    transactionPort: (
      context: Context,
    ): CursorFamilyTransactionPort<typeof category> => {
      contexts.push(context);
      let prepared = 0;
      return {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category,
        prepare: async () => ({
          label: context.runtime.label,
          prepared: ++prepared,
        }),
        capture: async (): Promise<never> => {
          throw new Error("Lifecycle wiring must not submit");
        },
      };
    },
  };
  const definition = defineFamily<
    typeof category,
    "computationThreadMint",
    false,
    1,
    Runtime
  >({
    category,
    stepDatumSchemas: [FraudProofComputationThreadStepDatum],
    witnessRoles: ["computationThreadMint"],
    fieldPreimageCertificate: false,
    auxiliaryReferenceScripts: { removal: "correctionLockSpend" },
    replayer: () => RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
    adapter: arm,
    fieldCarriage: [
      {
        requirementForAction: (context) => {
          requirements.push(context.runtime.label);
          return null;
        },
      },
    ],
  });
  const config: ManifestBoundFamilyWorkflowConfig<
    typeof category,
    "computationThreadMint",
    false,
    1
  > = {
    manifest: {},
    blueprintJson: "{}",
    deploymentInfo: {},
    headerHash,
    lucid: {} as never,
    signer: { address, paymentKeyHash: owner } as never,
    source: {} as never,
    referenceScripts: {
      steps: [references[0]!],
      witnesses: { computationThreadMint: references[1]! },
    },
    auxiliaryReferenceScripts: { removal: references[2]! },
    stateQueueMutationLeaseCoordinator: { acquire: vi.fn() },
  };
  return {
    definition,
    config,
    arm,
    contexts,
    requirements,
    bind,
    observe,
    decorate,
    references,
    l1,
  };
};

it("binds once, then isolates transaction caches and invocation refiners for each run", async () => {
  const f = fixture();
  const bound = await bindManifestBoundFamilyWorkflow(f.definition, f.config);
  expect(f.contexts).toHaveLength(0);
  const first = assembleBoundManifestBoundFamilyWorkflow(f.definition, bound, {
    label: "first",
  });
  const second = assembleBoundManifestBoundFamilyWorkflow(f.definition, bound, {
    label: "second",
  });
  expect(f.bind).toHaveBeenCalledOnce();
  expect(f.observe).toHaveBeenCalledOnce();
  expect(first.l1).toBe(second.l1);
  expect(first.transactions).not.toBe(second.transactions);
  const prepareInput = { evidence: {} as never, classification: {} as never };
  expect(await first.transactions.prepare(prepareInput)).toEqual({
    label: "first",
    prepared: 1,
  });
  expect(await second.transactions.prepare(prepareInput)).toEqual({
    label: "second",
    prepared: 1,
  });
  expect(await first.transactions.prepare(prepareInput)).toEqual({
    label: "first",
    prepared: 2,
  });
  const observationInput: Parameters<typeof first.adapter.observe>[0] = {
    identity: {
      schemaVersion: "midgard-fraud-proof-workflow-identity-v1" as const,
      deploymentFingerprint: "44".repeat(32),
      category,
      target: { kind: "state_queue_header" as const, headerHash },
    },
    workflowId: "fixture",
    artifact: {},
    entries: [],
  };
  expect(await first.adapter.observe(observationInput)).toMatchObject({
    kind: "action_required",
    action: { input: { runLabel: "first" } },
  });
  expect(await second.adapter.observe(observationInput)).toMatchObject({
    kind: "action_required",
    action: { input: { runLabel: "second" } },
  });
});

it("uses the identical field-carriage handle for capture and adapter decoration", async () => {
  const f = fixture();
  const bound = await bindManifestBoundFamilyWorkflow(f.definition, f.config);
  assembleBoundManifestBoundFamilyWorkflow(f.definition, bound, {
    label: "shared",
  });
  const context = f.contexts[0]!;
  const port = context.fieldCarriagePrerequisites[0]!;
  expect(f.decorate.mock.calls[0]![0].prerequisite).toBe(port);
  expect(Object.isFrozen(context)).toBe(true);
  expect(
    await port.resolveAuthenticated({
      headerHash,
      action: { actionId: "fixture", input: {} },
      artifact: {},
    }),
  ).toEqual({ publications: [], requirement: null });
  expect(f.requirements).toEqual(["shared"]);
});

it("rejects simultaneous static and context-bound action refiners before loading the manifest", async () => {
  const f = fixture();
  const ambiguous = defineFamily({
    ...f.definition,
    adapter: { ...f.arm, refineAction: async () => ({}) },
  });
  await expect(
    bindManifestBoundFamilyWorkflow(ambiguous, f.config),
  ).rejects.toThrow("declares two action refiners");
  expect(f.bind).not.toHaveBeenCalled();
});

it.each(["missing", "different_output", "different_script"] as const)(
  "rejects %s auxiliary references before opening L1",
  async (fault) => {
    const f = fixture();
    const reference = f.references[2]!;
    const auxiliaryReferenceScripts: Readonly<Record<string, UTxO>> =
      fault === "missing"
        ? {}
        : {
            removal:
              fault === "different_output"
                ? { ...reference, outputIndex: 99 }
                : {
                    ...reference,
                    scriptRef: {
                      type: "Native" as const,
                      script: `8200581c${"44".repeat(28)}`,
                    },
                  },
          };
    await expect(
      bindManifestBoundFamilyWorkflow(f.definition, {
        ...f.config,
        auxiliaryReferenceScripts,
      }),
    ).rejects.toThrow(
      fault === "missing"
        ? "omitted auxiliary reference script removal"
        : fault === "different_output"
          ? "differs from finalized manifest identity"
          : "script differs from finalized manifest identity",
    );
    expect(f.observe).not.toHaveBeenCalled();
  },
);

it("rejects structural copies and a different definition for the same category", async () => {
  const f = fixture();
  const bound = await bindManifestBoundFamilyWorkflow(f.definition, f.config);
  const other = defineFamily({
    ...f.definition,
    auxiliaryReferenceScripts: { other: "stateQueueSpend" },
  });
  expect(() =>
    assembleBoundManifestBoundFamilyWorkflow(
      f.definition,
      { ...bound },
      { label: "clone" },
    ),
  ).toThrow("deployment was not bound for this definition");
  expect(() =>
    assembleBoundManifestBoundFamilyWorkflow(other, bound, { label: "other" }),
  ).toThrow("deployment was not bound for this definition");
  expect(f.contexts).toHaveLength(0);
});
