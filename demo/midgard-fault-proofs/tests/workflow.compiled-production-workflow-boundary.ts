import "./workflow.q51-w-o4-resumable-workflow.js";

import * as SDK from "@al-ft/midgard-sdk/core";
import {
  credentialToAddress,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { parseArgs } from "../src/bin.js";
import { proofMaterialsAddress } from "../src/funding/proof-materials-address.js";
import { sealManifestBoundNetworkIdRuntime } from "../src/network-id/workflow-adapter.seal-manifest-bound-network-id-runtime.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { WORKFLOW_ACTUATION_PERMIT } from "../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import {
  MissingWorkflowAdaptersError,
  validateWorkflowAdapterCoverage,
  WORKFLOW_ADAPTER_REGISTRATIONS,
  WORKFLOW_ADAPTER_RUNNER,
} from "../src/workflow/adapters.js";
import {
  runFraudProofWorkflowCli,
  workflowReadinessReport,
} from "../src/workflow/cli.js";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.detect-double-withdraws.js";
import { requireManifestBoundReferenceScriptUtxo } from "../src/workflow/deployment-manifest-binding.assert-deployment-info-matches-manifest.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runFraudProofWorkflowFromRetainedDa } from "../src/workflow/orchestrator.run-fraud-proof-workflow-from-retained-da.js";
import { WORKFLOW_RUNNER_FACTORIES } from "../src/workflow/runtime.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
} from "./helpers/canonical-block-evidence-fixture.js";
import { WALLET_ONLY_SEND_ALWAYS_OPEN } from "./support/wallet-only-send-always-open.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  DEPLOYMENT_FINGERPRINT,
  makeAdapter,
  releaseFinalityAuthority,
  terminalVerifier,
} from "./workflow.make-adapter.js";

describe("compiled production workflow boundary", () => {
  // The signer's identities: proof-materials-address.test.ts.
  it("rejects substituted reference-script identities", () => {
    const paymentKeyHash = "a7".repeat(28);
    const address = credentialToAddress("Preview", {
      type: "Key",
      hash: paymentKeyHash,
    });

    const scriptRef = {
      type: "Native" as const,
      script: `8200581c${"b9".repeat(28)}`,
    };
    const exact = {
      txHash: "a8".repeat(32),
      outputIndex: 2,
      address,
      assets: { lovelace: 2_000_000n },
      scriptRef,
    } satisfies UTxO;
    const binding = {
      referenceScriptsByContract: {
        fraudProofNetworkId: {
          outRef: `${exact.txHash}#2`,
          scriptHash: validatorToScriptHash(scriptRef),
        },
      },
    };
    expect(() =>
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofNetworkId",
        utxo: exact,
      }),
    ).not.toThrow();
    expect(() =>
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofNetworkId",
        utxo: { ...exact, outputIndex: 3 },
      }),
    ).toThrow("differs from finalized manifest identity");
    expect(() =>
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "unpublishedSharedWitness",
        utxo: exact,
      }),
    ).toThrow("has no published reference-script identity");
  });

  it("seals every network-id runtime reference role and overrides hostile inline removal", () => {
    const paymentKeyHash = "a7".repeat(28);
    const address = credentialToAddress("Preview", {
      type: "Key",
      hash: paymentKeyHash,
    });
    const scriptRef = {
      type: "Native" as const,
      script: `8200581c${"b9".repeat(28)}`,
    };
    const roleNames = [
      "fraudProofNetworkId",
      "fraudProofNetworkIdStep02",
      "fieldPreimageCertificateMint",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "chunkedVerifyWithdraw",
      "pexcludesWithdraw",
    ] as const;
    const references = Object.fromEntries(
      roleNames.map((role, index) => [
        role,
        {
          txHash: (index + 1).toString(16).padStart(64, "0"),
          outputIndex: index,
          address,
          assets: { lovelace: 2_000_000n },
          scriptRef,
        } satisfies UTxO,
      ]),
    ) as unknown as Record<(typeof roleNames)[number], UTxO>;
    const binding = {
      network: "Preview" as const,
      referenceScriptsByContract: Object.fromEntries(
        roleNames.map((role) => [
          role,
          {
            outRef: `${references[role].txHash}#${references[role].outputIndex.toString()}`,
            scriptHash: validatorToScriptHash(scriptRef),
          },
        ]),
      ),
    };
    const base = {
      binding,
      signer: {
        source: "test",
        address,
        proofMaterialsAddress: proofMaterialsAddress("Preview", paymentKeyHash),
        paymentKeyHash,
        selectWallet: () => {},
      },
      stepReferenceScripts: [
        references.fraudProofNetworkId,
        references.fraudProofNetworkIdStep02,
      ] as const,
      fieldPreimageCertificateReferenceScript:
        references.fieldPreimageCertificateMint,
      witnessReferenceScripts: {
        computationThreadMint: references.computationThreadMint,
        fraudProofMint: references.fraudProofMint,
        phasMembershipWithdraw: references.phasMembershipWithdraw,
        chunkedVerifyWithdraw: references.chunkedVerifyWithdraw,
        pexcludesWithdraw: references.pexcludesWithdraw,
      },
      removal: {
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => {
            throw new Error("not called by pure runtime seal");
          },
        },
      },
    };
    const hostileRemoval = {
      ...base.removal,
      requireReferenceScripts: false,
    } as typeof base.removal;
    const sealed = sealManifestBoundNetworkIdRuntime({
      ...base,
      removal: hostileRemoval,
    });
    expect(sealed.removal.requireReferenceScripts).toBe(true);
    expect([
      ...sealed.stepReferenceScripts,
      sealed.fieldPreimageCertificateReferenceScript,
      sealed.witnessReferenceScripts.computationThreadMint,
      sealed.witnessReferenceScripts.fraudProofMint,
      sealed.witnessReferenceScripts.phasMembershipWithdraw,
      sealed.witnessReferenceScripts.chunkedVerifyWithdraw,
      sealed.witnessReferenceScripts.pexcludesWithdraw,
    ]).toEqual(roleNames.map((role) => references[role]));

    const substituted = {
      ...references.fraudProofNetworkId,
      outputIndex: 99,
    };
    const hostileInputs = [
      {
        ...base,
        stepReferenceScripts: [
          substituted,
          base.stepReferenceScripts[1],
        ] as const,
      },
      {
        ...base,
        stepReferenceScripts: [
          base.stepReferenceScripts[0],
          substituted,
        ] as const,
      },
      { ...base, fieldPreimageCertificateReferenceScript: substituted },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          computationThreadMint: substituted,
        },
      },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          fraudProofMint: substituted,
        },
      },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          phasMembershipWithdraw: substituted,
        },
      },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          chunkedVerifyWithdraw: substituted,
        },
      },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          pexcludesWithdraw: substituted,
        },
      },
    ] as const;
    for (const hostile of hostileInputs) {
      expect(() => sealManifestBoundNetworkIdRuntime(hostile)).toThrow(
        "differs from finalized manifest identity",
      );
    }
  });

  it("rejects omitted, duplicate, and unknown production registrations", () => {
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.slice(0, -1),
      ),
    ).toThrow("cardinality mismatch");
    expect(() =>
      validateWorkflowAdapterCoverage([
        ...WORKFLOW_ADAPTER_REGISTRATIONS.slice(0, -1),
        WORKFLOW_ADAPTER_REGISTRATIONS[0]!,
      ]),
    ).toThrow("duplicates doubleSpend");
    expect(() =>
      validateWorkflowAdapterCoverage([
        ...WORKFLOW_ADAPTER_REGISTRATIONS.slice(0, -1),
        { category: "forgedFamily" },
      ]),
    ).toThrow("actual=forgedFamily");
    expect(() =>
      validateWorkflowAdapterCoverage([
        {
          ...WORKFLOW_ADAPTER_REGISTRATIONS[0]!,
          status: "ready",
        },
        ...WORKFLOW_ADAPTER_REGISTRATIONS.slice(1),
      ]),
    ).toThrow("has no compiled executable runner");
  });

  it("seals registry keys and adapter methods against post-construction mutation", () => {
    const original = makeAdapter();
    const registry = createFraudProofWorkflowRegistry({
      adapters: [original],
      launchScope: ["doubleSpend"],
    });
    const admitted = registry.get("doubleSpend")!;
    expect(Object.isFrozen(registry)).toBe(true);
    expect(Object.isFrozen(admitted)).toBe(true);
    expect(Object.isFrozen(admitted.safety)).toBe(true);
    expect("set" in registry).toBe(false);
    expect("delete" in registry).toBe(false);

    const substitutedObserve = vi.fn(async () => ({
      kind: "conflict" as const,
      reason: "substituted",
    }));
    expect(Reflect.set(original, "category", "networkId")).toBe(true);
    expect(Reflect.set(original, "observe", substitutedObserve)).toBe(true);
    expect([...registry.keys()]).toEqual(["doubleSpend"]);
    expect(admitted.category).toBe("doubleSpend");
    expect(admitted.observe).not.toBe(substitutedObserve);
    expect(Reflect.set(admitted, "category", "networkId")).toBe(false);
  });

  it("cannot mutate the workflow registry during awaited retained-DA fetch", async () => {
    const fixture = await buildCanonicalBlockFixture({ transactions: [] });
    const original = makeAdapter();
    const registry = createFraudProofWorkflowRegistry({
      adapters: [original],
      launchScope: ["doubleSpend"],
    });
    const before = registry.get("doubleSpend")!;
    let mutationAttempted = false;
    const source: RetainedDaPayloadSource = {
      sourceId: "libp2p-hostile",
      fetchPayloadByHeaderHash: async () => {
        mutationAttempted = true;
        expect(() =>
          (
            registry as unknown as Map<
              SDK.FraudProofCatalogueCategoryName,
              FraudProofFamilyWorkflowAdapter
            >
          ).set("networkId", {
            ...makeAdapter(),
            category: "networkId",
          }),
        ).toThrow();
        Reflect.set(original, "observe", async () => ({
          kind: "conflict" as const,
          reason: "substituted during fetch",
        }));
        return {
          ok: true,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "libp2p-hostile/peer-hostile",
            grade: "security" as const,
          },
          sourceId: "libp2p-hostile",
          sourcePeerId: "peer-hostile",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        };
      },
    };
    await expect(
      runFraudProofWorkflowFromRetainedDa({
        walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        observation: authenticatedHeaderObservation(fixture),
        sources: [source],
        replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
        registry,
        run: createTestWorkflowProcess().run({
          category: "doubleSpend",
          headerHash: fixture.headerHash,
        }),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
      }),
    ).resolves.toMatchObject({ kind: "no_fault_detected" });
    expect(mutationAttempted).toBe(true);
    expect(registry.get("doubleSpend")).toBe(before);
  });

  it("deep-freezes registry rows and rejects forged or cross-category runners", () => {
    const first = WORKFLOW_ADAPTER_REGISTRATIONS[0]!;
    expect(Object.isFrozen(first)).toBe(true);
    expect(Reflect.set(first, "status", "ready")).toBe(false);
    expect(first.status).toBe("missing");
    expect(Object.isFrozen(first.existingSurface)).toBe(true);
    expect(Reflect.set(first.existingSurface, 0, "forged-surface")).toBe(false);

    const forgedRunner = {
      runnerVersion: WORKFLOW_ADAPTER_RUNNER,
      runOrResume: async () => "forged",
    };
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) =>
          registration.category === "doubleSpend"
            ? { ...registration, status: "ready", runner: forgedRunner }
            : registration,
        ),
      ),
    ).toThrow("no compiled executable runner admitted for its exact category");

    const admittedDoubleSpend = WORKFLOW_RUNNER_FACTORIES.doubleSpend(
      async () => {
        throw new Error("runner loader is not invoked during admission");
      },
    );
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) =>
          registration.category === "networkId"
            ? {
                ...registration,
                status: "ready",
                runner: admittedDoubleSpend,
              }
            : registration,
        ),
      ),
    ).toThrow("no compiled executable runner admitted for its exact category");
  });

  it("enumerates every registered category with an exact missing-adapter reason", () => {
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.map(
        (registration) => registration.category,
      ),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    expect(workflowReadinessReport()).toMatchObject({
      registeredCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      requestedCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      readyCategoryCount: 0,
      missingCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
        ({ category }) => category === "doubleSpend",
      ),
    ).toMatchObject({
      status: "missing",
      reason: "constrained_adapter_is_not_launch_scope_complete",
    });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
        ({ category }) => category === "nativeScriptDecoding",
      ),
    ).toMatchObject({
      reason: "manual_step_chain_has_no_atomic_driver",
    });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
        ({ category }) => category === "networkId",
      ),
    ).toMatchObject({
      status: "missing",
      reason: "constrained_adapter_is_not_launch_scope_complete",
    });
  });

  it("parses run/resume workflow identity flags in the compiled CLI, refusing the retired journal directory", () => {
    const argv = [
      "node",
      "midgard-fault-proofs",
      "resume-workflow",
      "--fraud-category",
      "doubleSpend",
      "--deployment-fingerprint",
      DEPLOYMENT_FINGERPRINT,
      "--header-hash",
      "f8".repeat(28),
      "--workflow-runtime-config",
      "/etc/midgard/fraud-proof-runtime-v1.json",
    ];
    expect(parseArgs(argv)).toMatchObject({
      command: "resume-workflow",
      fraudCategory: "doubleSpend",
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      headerHash: "f8".repeat(28),
      workflowRuntimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
    });
    expect(() =>
      parseArgs([...argv, "--workflow-journal-dir", "/tmp/midgard-workflow"]),
    ).toThrow("Unknown argument: --workflow-journal-dir");
  });

  it("fails closed on a missing adapter before accepting evidence", async () => {
    const headerHash = "f8".repeat(28);
    await expect(
      runFraudProofWorkflowCli({
        walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
        category: "invalidRange",
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        headerHash,
        decisionDigest: "f9".repeat(32),
        actuationPermit: {
          permitVersion: WORKFLOW_ACTUATION_PERMIT,
        },
        // The CLI demands the permit and the in-flight set before it looks
        // at adapters, so both are present: this test is about the
        // missing-adapter refusal, not the refusal that precedes it.
        sends: createTestWorkflowProcess().sends({
          category: "invalidRange",
          headerHash,
        }),
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      }),
    ).rejects.toBeInstanceOf(MissingWorkflowAdaptersError);
  });
});
