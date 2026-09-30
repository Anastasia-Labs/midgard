import "node:fs/promises";
import "node:os";
import "node:path";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "vitest";
import "../../src/fault-proofs/fault-proof-application.js";
import "../../src/funding/workflow-funding-profile-overlay.js";
import "../../src/runtime/config.js";
import "../../src/storage/public-da-libp2p-transport.js";
import "../funding/funding-handoff-fixture.js";
import "../support/deployment-authority-fixture.js";
import "./fault-proof-application.raw-config.js";

import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  applyFamilyApplicationRecord,
  FAMILY_APPLICATION_REGISTRY,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  PREDECESSOR_LEDGER_PROOF_CATEGORIES,
  TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES,
  type WorkflowAdapterReadinessInput,
  workflowFundingRequirementsForRunner,
  workflowReadinessReport,
} from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import {
  unsafeCreateWatcherFaultProofApplicationForTest,
  WATCHER_FAULT_PROOF_APPLICATION,
  WATCHER_FAULT_PROOF_STARTUP_READINESS,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_MISSING_WORKFLOW_CATEGORIES,
  WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES,
} from "../../src/fault-proofs/fault-proof-application.js";
import {
  createWatcherWorkflowFundingProfileBundle,
  loadWatcherWorkflowFundingProfileOverlay,
} from "../../src/funding/workflow-funding-profile-overlay.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import {
  AUTHORITY,
  BLUEPRINT_PATH,
  contractNameForOutRef,
  dependencies,
  DEPLOYMENT,
  DEPLOYMENT_INFO_PATH,
  HEADER,
  hostileStructuralExecutionInvocation,
  infrastructure,
  invocation,
  MANIFEST_PATH,
  rawConfig,
  SHARED_THREAD_REFERENCE_KEYS,
  TEST_HISTORY_STORE,
  transportFactory,
} from "./fault-proof-application.raw-config.js";

describe("watcher production fault-proof application V1", () => {
  it("installs every runner with an empty signed funding bundle while funding requests stay gated", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-empty-funding-"));
    try {
      const bundle = createWatcherWorkflowFundingProfileBundle({
        profiles: [],
      });
      const authority = makeWatcherDeploymentAuthorityFixture({
        fundingProfileBundleDigest: bundle.fundingProfileBundleDigest,
      });
      const bundlePath = join(directory, "funding.json");
      await writeFile(bundlePath, bundle.fundingProfileBundleBytes);
      const fundingProfileOverlay =
        await loadWatcherWorkflowFundingProfileOverlay({
          bundlePath,
          deploymentIdentity: authority.result,
        });
      const application = unsafeCreateWatcherFaultProofApplicationForTest(
        {
          deploymentIdentity: authority.result,
          infrastructure: infrastructure(),
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          unsafeTransportFactoryForTest: transportFactory().factory,
          fundingProfileOverlay,
        },
        dependencies(),
        {
          MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
        },
      );
      expect(application.installedCategories).toEqual([
        ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      ]);
      for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
        expect(() =>
          workflowFundingRequirementsForRunner({
            category,
            runner: application.runners[category],
          }),
        ).toThrow("has no admitted measured funding profile");
      }
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("installs every compiled manifest-bound runner and preflights every reference roster read-only", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-proof-app-"));
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const transport = transportFactory();
    const deps = dependencies();
    try {
      const application = unsafeCreateWatcherFaultProofApplicationForTest(
        {
          deploymentIdentity: AUTHORITY.result,
          infrastructure: infrastructure(),
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          unsafeTransportFactoryForTest: transport.factory,
        },
        deps,
        {
          MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
        },
      );

      expect(application.schemaVersion).toBe(WATCHER_FAULT_PROOF_APPLICATION);
      // The catalogue in the SDK is the independent authority on which
      // families a watcher must be able to prove, and in which order.
      expect(application.installedCategories).toEqual([
        ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      ]);
      expect(WATCHER_INSTALLED_WORKFLOW_CATEGORIES).toEqual([
        ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      ]);
      expect(WATCHER_MISSING_WORKFLOW_CATEGORIES).toEqual([]);
      expect(
        workflowReadinessReport(
          WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
          application.applicationRegistry,
        ),
      ).toMatchObject({
        deploymentFingerprint: DEPLOYMENT,
        installedCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
        requestedCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
        readyCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
        missingCategoryCount: 0,
      });

      for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
        const resolvedBefore = vi.mocked(deps.resolveReferenceScript).mock.calls
          .length;
        const readiness = await application.assertStartupReady(
          invocation(configPath, category),
        );
        const resolvedContractNames = vi
          .mocked(deps.resolveReferenceScript)
          .mock.calls.slice(resolvedBefore)
          .map(([input]) => input.contractName);

        const { referenceScriptOutRefs, ...envelope } = readiness;
        expect(envelope).toEqual({
          schemaVersion: WATCHER_FAULT_PROOF_STARTUP_READINESS,
          ready: true,
          category,
          deploymentFingerprint: DEPLOYMENT,
          headerHash: HEADER,
        });

        const rosterKeys = Object.keys(referenceScriptOutRefs);
        const rosterOutRefs = Object.values(referenceScriptOutRefs);
        // Every rostered entry is a real resolved reference UTxO, not a
        // placeholder left behind by a reference script the workflow failed to
        // bind (which surfaces as `undefined#undefined`).
        for (const rosterOutRef of rosterOutRefs) {
          expect(rosterOutRef).toMatch(/^[0-9a-f]{64}#\d+$/u);
        }
        // One distinct deployment contract per rostered role, and the roster
        // accounts for exactly the reference scripts this workflow resolved:
        // nothing loaded but unreported, nothing reported but never resolved.
        expect(new Set(rosterOutRefs).size).toBe(rosterOutRefs.length);
        expect(rosterOutRefs.map(contractNameForOutRef).sort()).toEqual(
          [...resolvedContractNames].sort(),
        );
        // Readiness reports one out-ref per role of the family's registry
        // roster, and the resolver was asked for exactly that roster's
        // contracts: the registry record is the single source of truth.
        const { roster } = FAMILY_APPLICATION_REGISTRY[category];
        expect(rosterKeys).toEqual(Object.keys(roster));
        expect([...resolvedContractNames].sort()).toEqual(
          Object.values(roster).sort(),
        );
        // Every family authenticates catalogue membership during init, including
        // the fabricated-event families whose later evidence comes from L1.
        expect(rosterKeys).toEqual(
          expect.arrayContaining([...SHARED_THREAD_REFERENCE_KEYS]),
        );
        expect(rosterKeys).toContain("phasMembershipWithdraw");
        expect(rosterKeys.length).toBeGreaterThan(
          SHARED_THREAD_REFERENCE_KEYS.length,
        );

        // A physical step may appear once, or once per committed polarity
        // (`step02Accepted` / `step02Forced`); either way its ordinal is bound.
        const stepOrdinals = [
          ...new Set(
            rosterKeys
              .map((key) => /^step(\d{2})(?:[A-Z]\w*)?$/u.exec(key)?.[1])
              .filter((ordinal): ordinal is string => ordinal !== undefined)
              .map(Number),
          ),
        ].sort((left, right) => left - right);
        if (category === "transitionTrace") {
          // The workflow module exports its own reference roster, an authority
          // independent of the registry: the contracts readiness resolved are
          // exactly the ones that module names.
          expect([...resolvedContractNames].sort()).toEqual(
            [...TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES].sort(),
          );
        }
        if (category === "validationTraceDispute") {
          // The interactive dispute game names its scripts by role in the
          // game, not by a physical step ordinal.
          expect(stepOrdinals).toEqual([]);
          expect(rosterKeys).toEqual(
            expect.arrayContaining([
              "opener",
              "source",
              "game",
              "boundary",
              "timeout",
              "award",
            ]),
          );
        } else {
          // The physical steps of a thread are contiguous from step01: a
          // dropped or renamed intermediate step leaves a gap here.
          expect(stepOrdinals.length).toBeGreaterThan(0);
          expect(stepOrdinals).toEqual(
            Array.from(
              { length: stepOrdinals.length },
              (_unused, index) => index + 1,
            ),
          );
        }
      }
      // The roster is complete, so this element type is `never` and the body
      // is unreachable. The guard stays so that re-opening a residue entry is
      // still required to fail closed at startup.
      for (const category of WATCHER_MISSING_WORKFLOW_CATEGORIES) {
        await expect(
          application.assertStartupReady(invocation(configPath, category)),
        ).rejects.toThrow(
          `watcher has no installed production workflow for ${String(category)}`,
        );
      }

      // Each readiness preflight keeps an isolated Lucid instance. Readiness
      // binds the deployment and resolves the roster only: it opens no
      // retained-DA transport, reads no secret and acquires no lease.
      expect(deps.makeLucid).toHaveBeenCalledTimes(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      );
      expect(transport.factory).not.toHaveBeenCalled();
      expect(application.retainedDaTransportStatus()).toEqual({
        state: "idle",
        failure: null,
      });
      expect(deps.resolveSigner).not.toHaveBeenCalled();
      expect(deps.createLeaseCoordinator).not.toHaveBeenCalled();
      await expect(
        application.runOrResume(
          hostileStructuralExecutionInvocation(configPath, "doubleSpend"),
        ),
      ).rejects.toThrow(
        "unsafe watcher fault-proof test application cannot execute transactions",
      );
      await application.close();
      await application.close();
      expect(transport.stop).not.toHaveBeenCalled();
      expect(application.retainedDaTransportStatus()).toEqual({
        state: "closed",
        failure: null,
      });
      await expect(
        application.assertStartupReady(invocation(configPath, "doubleSpend")),
      ).rejects.toThrow("watcher fault-proof application is closed");
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("installs nativeScriptDecoding with all six manifest-bound physical steps", async () => {
    const directory = await mkdtemp(join(tmpdir(), "native-decoding-app-"));
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const transport = transportFactory();
    const deps = dependencies();
    try {
      const application = unsafeCreateWatcherFaultProofApplicationForTest(
        {
          deploymentIdentity: AUTHORITY.result,
          infrastructure: infrastructure(),
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          unsafeTransportFactoryForTest: transport.factory,
        },
        deps,
        {
          MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
        },
      );
      expect(application.installedCategories).toContain("nativeScriptDecoding");
      expect(WATCHER_MISSING_WORKFLOW_CATEGORIES).not.toContain(
        "nativeScriptDecoding",
      );
      expect(WATCHER_MISSING_WORKFLOW_CATEGORIES).not.toContain(
        "valueNotPreserved",
      );
      const readiness = await application.assertStartupReady(
        invocation(configPath, "nativeScriptDecoding"),
      );
      expect(readiness).toMatchObject({
        ready: true,
        category: "nativeScriptDecoding",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: HEADER,
      });
      // Six physical steps, each bound to a distinct deployment contract of
      // this family — a roster entry pointing at another family's script, or
      // two steps sharing one script, fails here.
      const stepEntries = Object.entries(
        readiness.referenceScriptOutRefs,
      ).filter(([key]) => /^step\d{2}$/u.test(key));
      expect(stepEntries.map(([key]) => key)).toEqual([
        "step01",
        "step02",
        "step03",
        "step04",
        "step05",
        "step06",
      ]);
      const stepContractNames = stepEntries.map(([, rosterOutRef]) =>
        contractNameForOutRef(rosterOutRef),
      );
      expect(new Set(stepContractNames).size).toBe(6);
      for (const contractName of stepContractNames) {
        expect(contractName).toMatch(/^fraudProofNativeScriptDecoding/u);
      }
      await application.close();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("fails closed on application/category, deployment, authority, and secret substitution", async () => {
    expect(() =>
      unsafeCreateWatcherFaultProofApplicationForTest(
        {
          deploymentIdentity: AUTHORITY.result,
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          infrastructure: {
            ...infrastructure(),
            privateEvidenceUrl: "https://operator-private.example",
          } as never,
        },
        dependencies(),
      ),
    ).toThrow("unknown or missing fields");

    // The Midgard node lease coordination keys are gone; a caller still
    // supplying one is refused like any other unknown field.
    for (const deleted of [
      { midgardNodeUrl: "http://127.0.0.1:3000" },
      {
        midgardNodeAdminKeySource: {
          kind: "environment",
          variable: "MIDGARD_NODE_ADMIN_KEY",
        },
      },
      { stateQueueLeaseTtlMs: 30_000 },
    ]) {
      expect(() =>
        unsafeCreateWatcherFaultProofApplicationForTest(
          {
            deploymentIdentity: AUTHORITY.result,
            historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
            infrastructure: { ...infrastructure(), ...deleted } as never,
          },
          dependencies(),
        ),
      ).toThrow("unknown or missing fields");
    }

    expect(() =>
      unsafeCreateWatcherFaultProofApplicationForTest(
        {
          deploymentIdentity: AUTHORITY.result,
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          infrastructure: {
            ...infrastructure(),
            historicalNativeScriptHistory: {
              ...infrastructure().historicalNativeScriptHistory,
              providers: [
                ...infrastructure().historicalNativeScriptHistory.providers,
                {
                  sourceId: "history-provider-c",
                  operatorIdentitySha256: "71".repeat(32),
                  authorityEndpoint: "https://history-c.example.test",
                },
              ],
            },
          },
        },
        dependencies(),
      ),
    ).toThrow(
      "historical native-script providers must have distinct canonical identities and endpoints",
    );

    const directory = await mkdtemp(
      join(tmpdir(), "midgard-proof-app-hostile-"),
    );
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    try {
      const admitted = unsafeCreateWatcherFaultProofApplicationForTest(
        {
          deploymentIdentity: AUTHORITY.result,
          historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
          infrastructure: infrastructure(),
          unsafeTransportFactoryForTest: transportFactory().factory,
        },
        dependencies(),
        {
          MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
        },
      );
      // Every catalogue category now has an installed workflow (54/54), so the
      // uninstalled-category guard is exercised with a forged non-catalogue
      // category string.
      await expect(
        admitted.assertStartupReady(
          invocation(configPath, "doubleSpend", {
            category:
              "forgedUninstalledCategory" as WorkflowAdapterReadinessInput["category"],
          }),
        ),
      ).rejects.toThrow("no installed production workflow");
      await expect(
        admitted.assertStartupReady(
          invocation(configPath, "doubleSpend", {
            deploymentFingerprint: "ff".repeat(32),
          }),
        ),
      ).rejects.toThrow("differs from verified watcher authority");
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("derives the predecessor-ledger set from the classifier's replay requirement", () => {
    // The four families whose proof opens prev_utxos_root: the same set the
    // classifier reads when it decides `predecessor_context_unavailable`.
    // The decision-time check re-checks that invariant; it is not the
    // records' `requires.replayContext` set, which the loop enforces at load.
    expect([...WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES].sort()).toEqual(
      [...PREDECESSOR_LEDGER_PROOF_CATEGORIES].sort(),
    );
    expect([...WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES].sort()).toEqual([
      "minAda",
      "missingNativeScriptUtxo",
      "noReferenceInput",
      "nonExistentInput",
    ]);
  });

  it("reads no secret on the readiness path, and refuses secret substitution only when acting", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-readiness-secrets-"),
    );
    const configPath = join(directory, "watcher.json");
    await writeFile(
      configPath,
      JSON.stringify({
        ...rawConfig(),
        proverWallet: {
          keySource: { kind: "file", path: "/etc/midgard/prover.key" },
        },
      }),
    );
    const deps = dependencies();
    const readText = vi.mocked(deps.readText).getMockImplementation()!;
    vi.mocked(deps.readText).mockImplementation(async (path) => {
      if (path === "/etc/midgard/prover.key")
        throw new Error(`secret ${path} was read`);
      return await readText(path);
    });
    const application = unsafeCreateWatcherFaultProofApplicationForTest(
      {
        deploymentIdentity: AUTHORITY.result,
        historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
        infrastructure: infrastructure(),
        unsafeTransportFactoryForTest: transportFactory().factory,
      },
      deps,
      {},
    );
    try {
      const readiness = await application.assertStartupReady(
        invocation(configPath, "doubleSpend"),
      );
      expect(readiness.ready).toBe(true);
      expect(Object.keys(readiness.referenceScriptOutRefs)).toEqual(
        Object.keys(FAMILY_APPLICATION_REGISTRY.doubleSpend.roster),
      );
      expect(deps.resolveSigner).not.toHaveBeenCalled();
      expect(deps.createLeaseCoordinator).not.toHaveBeenCalled();
      expect(
        vi
          .mocked(deps.readText)
          .mock.calls.map(([path]) => path)
          .filter((path) => path.endsWith(".key")),
      ).toEqual([]);

      // The acting path reads the prover secret and fails closed when it is
      // missing.
      await expect(
        application.unsafeLoadRuntimeForTest({
          runtimeConfigPath: configPath,
          invocation: hostileStructuralExecutionInvocation(
            configPath,
            "doubleSpend",
          ),
        }),
      ).rejects.toThrow("secret /etc/midgard/prover.key was read");
    } finally {
      await application.close();
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("supplies a family exactly the common infrastructure the watcher holds, so a record requiring a replay context the classifier never captured refuses with the shared error", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-requires-"));
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const deps = dependencies();
    const application = unsafeCreateWatcherFaultProofApplicationForTest(
      {
        deploymentIdentity: AUTHORITY.result,
        historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
        infrastructure: infrastructure(),
        unsafeTransportFactoryForTest: transportFactory().factory,
      },
      deps,
      {
        MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim(),
      },
    );
    const applied = async <
      Category extends keyof typeof FAMILY_APPLICATION_REGISTRY,
    >(
      category: Category,
      stubConstruction = false,
    ) => {
      const execution = hostileStructuralExecutionInvocation(
        configPath,
        category,
      );
      const loaded = await application.unsafeLoadRuntimeForTest({
        runtimeConfigPath: configPath,
        invocation: execution,
      });
      const resolvedBefore = vi.mocked(deps.resolveReferenceScript).mock.calls
        .length;
      // The registry record decides what it needs; the watcher only supplies.
      // A refusing family is applied through its real record. For the family
      // that is admitted, construction is stubbed so the test crosses the
      // requirement gate and the roster resolution, and nothing
      // family-internal.
      // The registry erases each entry's config type (TS7056), so the record
      // is re-widened here exactly as the runtime's factory table does.
      const entry = FAMILY_APPLICATION_REGISTRY[category];
      const record = (stubConstruction
        ? {
            ...entry,
            bindConfig: () => undefined,
            constructWorkflow: async () => ({
              binding: {
                deploymentFingerprint: DEPLOYMENT,
                definition: { category, headerHash: HEADER },
              },
              decisionDigest: execution.decisionDigest,
            }),
            execute: async () => undefined,
          }
        : entry) as unknown as FamilyApplicationRecord<
        Category,
        unknown,
        FamilyApplicationWorkflowIdentity<Category>
      >;
      try {
        return await applyFamilyApplicationRecord({
          record,
          infrastructure: loaded.infrastructure,
          resolveReferenceScript: loaded.resolveReferenceScript,
          invocation: {
            deploymentFingerprint: DEPLOYMENT,
            category,
            headerHash: HEADER,
          },
        });
      } finally {
        await loaded.close();
        vi.mocked(deps.resolveReferenceScript).mock.calls.splice(
          resolvedBefore,
        );
      }
    };
    try {
      const replayContextFamilies =
        WATCHER_INSTALLED_WORKFLOW_CATEGORIES.filter((category) =>
          FAMILY_APPLICATION_REGISTRY[category].requires.includes(
            "replayContext",
          ),
        );
      expect(replayContextFamilies).toHaveLength(5);
      for (const category of replayContextFamilies) {
        await expect(applied(category)).rejects.toThrow(
          `${category} application requires replayContext, which the host did not supply`,
        );
      }
      expect(deps.resolveReferenceScript).not.toHaveBeenCalled();

      // A family that requires only what the watcher always supplies (the
      // historical authority, the validation-challenge port) is applied:
      // the resolver is asked for exactly its roster.
      const { referenceScriptOutRefs } = await applied("minAda", true);
      expect(Object.keys(referenceScriptOutRefs)).toEqual(
        Object.keys(FAMILY_APPLICATION_REGISTRY.minAda.roster),
      );
      // Non-tail removal is coordinated locally: the acting path builds its
      // lease coordinator from nothing, and the only secret it reads is the
      // prover wallet supplied above (no Midgard node admin key exists).
      expect(deps.createLeaseCoordinator).toHaveBeenCalled();
      for (const call of vi.mocked(deps.createLeaseCoordinator).mock.calls) {
        expect(call).toEqual([]);
      }
      expect(
        vi
          .mocked(deps.readText)
          .mock.calls.map(([path]) => path)
          .filter((path) => path.endsWith(".key")),
      ).toEqual([]);
    } finally {
      await application.close();
      await rm(directory, { recursive: true, force: true });
    }
  });
});

it("verifies completion deployment metadata without resolving wallet secrets", async () => {
  const deps = dependencies();
  vi.mocked(deps.readText).mockImplementation(async (path) => {
    if (path === "/etc/midgard/watcher.json")
      return JSON.stringify(rawConfig());
    if (path === BLUEPRINT_PATH) return "tampered-blueprint";
    if (path === MANIFEST_PATH)
      return JSON.stringify(AUTHORITY.signedIdentity.manifest);
    if (path === DEPLOYMENT_INFO_PATH)
      return JSON.stringify({
        referenceScriptAuthPolicy: "02".repeat(28),
        contracts: AUTHORITY.contracts,
      });
    throw new Error(`unexpected read ${path}`);
  });
  const application = unsafeCreateWatcherFaultProofApplicationForTest(
    {
      deploymentIdentity: AUTHORITY.result,
      infrastructure: infrastructure(),
      historicalNativeScriptCheckpointStore: TEST_HISTORY_STORE,
    },
    deps,
    {},
  );
  try {
    await expect(
      application.verifyCompleted({
        runtimeConfigPath: "/etc/midgard/watcher.json",
        category: "doubleSpend",
        headerHash: HEADER,
        decisionDigest: "cd".repeat(32),
        entries: [],
        terminal: fundingTerminal(HEADER, "aa".repeat(32), "bb".repeat(32)),
      }),
    ).rejects.toThrow(/blueprint/u);
    expect(deps.resolveSigner).not.toHaveBeenCalled();
    expect(deps.makeLucid).not.toHaveBeenCalled();
    expect(deps.createLeaseCoordinator).not.toHaveBeenCalled();
    expect(deps.resolveReferenceScript).not.toHaveBeenCalled();
  } finally {
    await application.close();
  }
});
