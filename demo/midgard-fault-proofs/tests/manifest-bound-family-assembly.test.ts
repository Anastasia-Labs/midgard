import {
  credentialToAddress,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import * as settlementAuthority from "../src/cross-block-duplicate-event/settlement-authority.js";
import * as inspection from "../src/inspect-contracts.js";
import * as historyRoster from "../src/missing-native-script-tx/historical-script.js";
import * as deployment from "../src/workflow/deployment-manifest-binding.js";
import { familyStepContractNames } from "../src/workflow/family-definition.js";
import {
  type AssembledFamilyCategory,
  FAMILY_DEFINITIONS,
} from "../src/workflow/family-definitions.js";
import * as observations from "../src/workflow/family-l1-observation.js";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "../src/workflow/family-l1-observation.js";
import * as fieldCarriage from "../src/workflow/field-carriage-prerequisite.js";
import * as historyAuthority from "../src/workflow/historical-native-script-corpus.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "../src/workflow/manifest-bound-family-assembly.js";
import * as orchestrator from "../src/workflow/orchestrator.js";
import * as proofChunks from "../src/workflow/proof-chunk-prerequisite.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../src/workflow/raw-l1-snapshot.js";
import { cursorAssemblyRuntimeFixture } from "./support/cursor-assembly-runtime-fixtures.js";
import { createAuthenticatedFamilyAssemblyRuntimeFixture } from "./support/family-assembly-runtime-fixtures.js";

afterEach(() => vi.restoreAllMocks());

const categories = Object.keys(FAMILY_DEFINITIONS) as AssembledFamilyCategory[];

const OWNER = "11".repeat(28);
const HEADER = "aa".repeat(28);
const SCRIPT = { type: "PlutusV3", script: "49480100002221200101" } as const;

/**
 * A fake deployment binding that resolves every reference script the
 * definition can ask for, plus a fake L1 port whose provider never starts.
 * Verified-manifest loading and provider startup are isolated; the assembly,
 * the exact manifest out-ref/script checks and the family's own port factory
 * stay real. Step contract names come from whichever spec the definition's
 * arm uses.
 */
const fixture = (category: AssembledFamilyCategory) => {
  const definition = FAMILY_DEFINITIONS[category];
  const stepNames = familyStepContractNames(definition);
  const address = credentialToAddress("Preprod", { type: "Key", hash: OWNER });
  const names = [
    ...stepNames,
    ...definition.witnessRoles,
    "fieldPreimageCertificateMint",
    ...Object.values(definition.auxiliaryReferenceScripts ?? {}),
  ];
  const utxoAt = (index: number): UTxO => ({
    txHash: "22".repeat(32),
    outputIndex: index,
    address,
    assets: { lovelace: 2_000_000n },
    scriptRef: SCRIPT,
  });
  const references = new Map(names.map((name, index) => [name, utxoAt(index)]));
  const steps = stepNames.map((name) => references.get(name)!);
  const witnesses = Object.fromEntries(
    definition.witnessRoles.map((role) => [role, references.get(role)!]),
  );
  const contractSteps = steps.map((_step, index) => ({
    spendingScriptAddress: address,
    spendingScriptHash: `${index.toString().padStart(2, "0")}`.repeat(28),
  }));
  const binding = {
    deploymentFingerprint: "44".repeat(32),
    network: "Preprod",
    blueprint: {},
    deploymentInfo: {},
    releaseFinality: { policy: { confirmationDepth: 1 } },
    releaseEconomics: { policy: { fraudProverRewardLovelace: "1000000" } },
    cardanoProtocolParameters: { maxTxSize: 16384 },
    fieldPreimageCertificate: {
      policyId: "55".repeat(28),
      mintingScript: SCRIPT,
    },
    referenceScriptsByContract: Object.fromEntries(
      names.map((name) => {
        const utxo = references.get(name)!;
        return [
          name,
          {
            outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
            scriptHash: validatorToScriptHash(SCRIPT),
          },
        ];
      }),
    ),
    definition: { category, headerHash: HEADER, proverCredential: OWNER },
    contractEntries: {
      hubOracleMint: { scriptHash: "88".repeat(28) },
      stateQueueMint: { scriptHash: "33".repeat(28) },
    },
    resolvedContracts: {
      contracts: {
        [category]: {
          steps: contractSteps,
          firstStep: contractSteps[0],
          acceptedStep02: contractSteps[1],
          forcedStep02: contractSteps[2],
          certificateStep03: contractSteps[3],
        },
        computationThread: { policyId: "66".repeat(28) },
        fraudProof: {
          policyId: "77".repeat(28),
          mintingScript: SCRIPT,
          spendingScriptAddress: address,
        },
      },
      stateQueuePolicyId: "33".repeat(28),
      hubOraclePolicyId: "88".repeat(28),
      category: { categoryId: "0000000b" },
    },
  };
  vi.spyOn(deployment, "bindFraudProofWorkflowDeployment").mockResolvedValue(
    binding as unknown as Awaited<
      ReturnType<typeof deployment.bindFraudProofWorkflowDeployment>
    >,
  );
  const bindReference = vi.spyOn(
    deployment,
    "requireManifestBoundReferenceScriptUtxo",
  );
  const observation = { fake: "observation" };
  const l1 = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category,
    rawL1: { authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY },
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: vi.fn(),
    },
    observeHeader: vi.fn(async () => observation),
    observeBoundary: vi.fn(),
    transactionConfirmed: vi.fn(async () => false),
    observe: vi.fn(),
  };
  const observe = vi
    .spyOn(observations, "createFraudProofFamilyLocalKupmiosL1ObservationPort")
    .mockReturnValue(l1 as never);
  const withField = vi.spyOn(fieldCarriage, "withFieldCarriagePrerequisite");
  const withChunks = vi.spyOn(proofChunks, "withProofChunkPrerequisite");
  const config = {
    manifest: {},
    blueprintJson: "{}",
    deploymentInfo: {},
    headerHash: HEADER,
    lucid: { fake: "lucid" },
    signer: { address, paymentKeyHash: OWNER },
    referenceScripts: {
      steps,
      witnesses,
      fieldPreimageCertificateMint: references.get(
        "fieldPreimageCertificateMint",
      )!,
    },
    source: {},
    auxiliaryReferenceScripts: Object.fromEntries(
      Object.entries(definition.auxiliaryReferenceScripts ?? {}).map(
        ([role, contractName]) => [role, references.get(contractName)!],
      ),
    ),
    stateQueueMutationLeaseCoordinator: { acquire: vi.fn() },
  };
  // Historical providers are external authorities, isolated alongside L1 here.
  // Family contract mapping, transaction ports and prerequisite wiring stay real.
  vi.spyOn(
    historyAuthority,
    "requireHistoricalNativeScriptHistoryAuthority",
  ).mockReturnValue({ providerRosterDigest: "99".repeat(32) });
  vi.spyOn(
    historyRoster,
    "requireHistoricalNativeScriptSourceRoster",
  ).mockImplementation(() => undefined as never);
  vi.spyOn(
    settlementAuthority,
    "createCrossBlockSettlementAuthority",
  ).mockReturnValue({
    deploymentFingerprint: binding.deploymentFingerprint,
    capture: vi.fn(),
  });
  vi.spyOn(
    inspection,
    "parseContractDeploymentReferenceScriptAuthPolicyId",
  ).mockReturnValue("ab".repeat(28));
  const historical = {
    historicalNativeScriptCheckpointStore: {},
    historicalNativeScriptHistorySource: {
      providerRosterDigest: "99".repeat(32),
    },
    historicalNativeScriptL1Roster: {
      applicationOverlayDigest: "99".repeat(32),
    },
  };
  const runtime =
    createAuthenticatedFamilyAssemblyRuntimeFixture(category, {
      binding,
      config,
    }) ??
    cursorAssemblyRuntimeFixture({ category, binding, l1, config }) ??
    (category === "minAda" || category === "missingNativeScriptTx"
      ? { ...historical, corpusCell: {} }
      : category === "transitionTrace"
        ? { config: { ...config, ...historical }, cell: {} }
        : category === "crossBlockDuplicateEvent"
          ? {
              historySource: historical.historicalNativeScriptHistorySource,
              checkpointStore: {},
            }
          : undefined);
  const assemble = () =>
    assembleManifestBoundFamilyWorkflow(
      definition as never,
      config as never,
      runtime,
    );
  return {
    definition,
    runtime,
    stepNames,
    binding,
    config,
    references,
    l1,
    observation,
    assemble,
    bindReference,
    observe,
    withField,
    withChunks,
  };
};

describe("manifest-bound family assembly", () => {
  it("covers every migrated definition, linear and cursor", () => {
    expect(categories).toHaveLength(49);
    expect(
      categories.filter(
        (category) => FAMILY_DEFINITIONS[category].adapter.kind === "linear",
      ),
    ).toHaveLength(18);
    expect(
      categories.filter(
        (category) => FAMILY_DEFINITIONS[category].adapter.kind === "cursor",
      ),
    ).toHaveLength(31);
    for (const category of categories) {
      expect(FAMILY_DEFINITIONS[category].category).toBe(category);
    }
    expect(
      new Set(categories.map((c) => FAMILY_DEFINITIONS[c].adapter.kind)),
    ).toEqual(new Set(["linear", "cursor"]));
  });

  describe.each(categories)("%s", (category) => {
    it("assembles a workflow whose adapter carries the category", async () => {
      const f = fixture(category);
      const workflow = await f.assemble();
      expect(workflow.definition).toBe(f.definition);
      expect(workflow.binding).toBe(f.binding);
      expect(workflow.l1).toBe(f.l1);
      expect(workflow.adapter.category).toBe(category);
      expect(workflow.transactions.category).toBe(category);
      expect(workflow.replayer.launchScope).toEqual([category]);
      expect(Object.isFrozen(workflow)).toBe(true);
    });

    it("binds every spec step and declared witness reference before provider startup", async () => {
      const f = fixture(category);
      await f.assemble();
      const bound = f.bindReference.mock.calls.map(
        ([input]) => input.contractName,
      );
      const expected = [
        ...f.stepNames,
        ...f.definition.witnessRoles,
        ...(f.definition.fieldPreimageCertificate
          ? ["fieldPreimageCertificateMint"]
          : []),
        ...Object.values(f.definition.auxiliaryReferenceScripts ?? {}),
      ];
      expect(bound).toEqual(expected);
      for (const name of expected) {
        expect(f.bindReference).toHaveBeenCalledWith({
          binding: f.binding,
          contractName: name,
          utxo: f.references.get(name),
        });
      }
      const [referenceOrder] = f.bindReference.mock.invocationCallOrder;
      const [observeOrder] = f.observe.mock.invocationCallOrder;
      expect(referenceOrder).toBeLessThan(observeOrder!);
    });

    describe.each(FAMILY_DEFINITIONS[category].witnessRoles)(
      "witness %s",
      (role) => {
        it("refuses a missing reference before provider startup", async () => {
          const f = fixture(category);
          Reflect.deleteProperty(f.config.referenceScripts.witnesses, role);
          await expect(f.assemble()).rejects.toThrow(
            `${category} workflow config omitted witness reference script ${role}`,
          );
          expect(f.observe).not.toHaveBeenCalled();
        });

        it("refuses a reference from a different output", async () => {
          const f = fixture(category);
          f.config.referenceScripts.witnesses[role] = {
            ...f.references.get(role)!,
            outputIndex: 99,
          };
          await expect(f.assemble()).rejects.toThrow(
            `${role} reference UTxO differs from finalized manifest identity`,
          );
          expect(f.observe).not.toHaveBeenCalled();
        });

        it("refuses a different script at the published output", async () => {
          const f = fixture(category);
          f.config.referenceScripts.witnesses[role] = {
            ...f.references.get(role)!,
            scriptRef: { type: "Native", script: `8200581c${"44".repeat(28)}` },
          };
          await expect(f.assemble()).rejects.toThrow(
            `${role} reference UTxO script differs from finalized manifest identity`,
          );
          expect(f.observe).not.toHaveBeenCalled();
        });
      },
    );

    it("binds the field-preimage certificate mint only when the definition declares it", async () => {
      const f = fixture(category);
      if (f.definition.fieldPreimageCertificate) {
        // The assembly refuses a manifest without the certificate policy
        // before any provider observation.
        f.binding.fieldPreimageCertificate = null as never;
        await expect(f.assemble()).rejects.toThrow(
          `${category} manifest omitted the field-preimage certificate policy`,
        );
        expect(f.observe).not.toHaveBeenCalled();
        return;
      }
      // A definition that does not declare the certificate never binds the
      // minting reference script. Its transaction port may still read the
      // certificate policy id from the manifest (committed-field-shape and
      // zero-input bind it into a step verdict), so the manifest stays whole.
      const workflow = await f.assemble();
      expect(workflow.adapter.category).toBe(category);
      expect(f.bindReference).not.toHaveBeenCalledWith(
        expect.objectContaining({
          contractName: "fieldPreimageCertificateMint",
        }),
      );
    });

    it("refuses an L1 port without raw-L1 and publication authorities", async () => {
      const f = fixture(category);
      Reflect.deleteProperty(f.l1, "rawL1");
      await expect(f.assemble()).rejects.toThrow(
        `${category} requires authenticated raw L1 and publication authorities`,
      );
    });

    it("applies exactly the declared prerequisites, field carriage before proof chunks", async () => {
      const f = fixture(category);
      const workflow = await f.assemble();
      const fieldEntries = f.definition.fieldCarriage ?? [];
      expect(f.withField).toHaveBeenCalledTimes(fieldEntries.length);
      expect(f.withChunks).toHaveBeenCalledTimes(
        f.definition.proofChunk === undefined ? 0 : 1,
      );
      const decorations = [
        ...f.withField.mock.results.map((result, index) => ({
          kind: "field" as const,
          base: f.withField.mock.calls[index]![0].base,
          rawDatum: f.withField.mock.calls[index]![0].rawDatum,
          result: result.value as unknown,
        })),
        ...f.withChunks.mock.results.map((result, index) => ({
          kind: "chunk" as const,
          base: f.withChunks.mock.calls[index]![0].base,
          rawDatum: undefined,
          result: result.value as unknown,
        })),
      ];
      // Each decorator wraps the previous one's result; the last is the
      // workflow's adapter. With no prerequisites the adapter is undecorated.
      let previous: unknown = undefined;
      decorations.forEach((decoration, index) => {
        if (index > 0) expect(decoration.base).toBe(previous);
        expect(decoration.base).toMatchObject({ category });
        previous = decoration.result;
      });
      if (decorations.length === 0) {
        expect(workflow.adapter).toMatchObject({ category });
      } else {
        expect(workflow.adapter).toBe(previous);
      }
      fieldEntries.forEach((entry, index) => {
        expect(decorations[index]!.rawDatum).toBe(entry.rawDatum);
      });
    });

    it("runs through the retained-DA runner with its replayer and single-category scope", async () => {
      const f = fixture(category);
      const workflow = await f.assemble();
      const result = { fake: "run-result" };
      const run = vi
        .spyOn(orchestrator, "runFraudProofWorkflowFromRetainedDa")
        .mockResolvedValue(result as never);
      const registry = vi.spyOn(
        orchestrator,
        "createFraudProofWorkflowRegistry",
      );
      const sources = [{ fake: "source" }] as never;
      const journal = { fake: "journal" } as never;
      await expect(
        runOrResumeManifestBoundFamilyWorkflow({ workflow, sources, journal }),
      ).resolves.toBe(result);
      expect(f.l1.observeHeader).toHaveBeenCalledWith({ headerHash: HEADER });
      expect(run).toHaveBeenCalledTimes(1);
      const [input] = run.mock.calls[0]!;
      expect(input).toMatchObject({
        deploymentFingerprint: f.binding.deploymentFingerprint,
        observation: f.observation,
        sources,
        journal,
      });
      expect(input.replayer).toBe(workflow.replayer);
      expect(input.terminalVerifier).toBe(workflow.terminalVerifier);
      expect(input.releaseFinalityAuthority).toBe(
        workflow.releaseFinalityAuthority,
      );
      // The registry freezes each adapter behind a wrapper, so the adapter
      // identity is checked at the registry's input.
      expect(registry).toHaveBeenCalledTimes(1);
      expect(registry.mock.calls[0]![0]).toEqual({
        adapters: [workflow.adapter],
        launchScope: [category],
      });
      expect(registry.mock.calls[0]![0].adapters[0]).toBe(workflow.adapter);
      expect(input.registry).toBe(registry.mock.results[0]!.value);
      expect([...input.registry.keys()]).toEqual([category]);
      expect("replayContext" in input).toBe(false);
    });

    it("forwards a supplied replay context to the runner", async () => {
      const f = fixture(category);
      const replayContext = { fake: "replay-context" };
      const workflow = await assembleManifestBoundFamilyWorkflow(
        f.definition as never,
        { ...f.config, replayContext } as never,
        f.runtime,
      );
      expect(workflow.replayContext).toBe(replayContext);
      const run = vi
        .spyOn(orchestrator, "runFraudProofWorkflowFromRetainedDa")
        .mockResolvedValue({} as never);
      await runOrResumeManifestBoundFamilyWorkflow({
        workflow,
        sources: [],
        journal: {} as never,
      });
      expect(run.mock.calls[0]![0].replayContext).toBe(replayContext);
    });
  });

  it("refuses a definition that is not a frozen product of defineFamily", async () => {
    const [category] = categories;
    const f = fixture(category!);
    for (const definition of [
      { ...f.definition },
      Object.freeze({ ...f.definition, definitionVersion: "other" }),
    ]) {
      await expect(
        assembleManifestBoundFamilyWorkflow(
          definition as never,
          f.config as never,
        ),
      ).rejects.toThrow(`${category!} family definition changed identity`);
    }
    expect(f.observe).not.toHaveBeenCalled();
  });

  it("refuses a definition whose step datum schemas disagree with its spec", async () => {
    const [category] = categories;
    const f = fixture(category!);
    const definition = Object.freeze({
      ...f.definition,
      stepDatumSchemas: f.definition.stepDatumSchemas.slice(1),
    });
    await expect(
      assembleManifestBoundFamilyWorkflow(
        definition as never,
        f.config as never,
      ),
    ).rejects.toThrow(
      `${category!} definition declares ${(f.definition.stepDatumSchemas.length - 1).toString()} step datum schemas for a ${f.stepNames.length.toString()}-step spec`,
    );
    expect(f.observe).not.toHaveBeenCalled();
  });
});
