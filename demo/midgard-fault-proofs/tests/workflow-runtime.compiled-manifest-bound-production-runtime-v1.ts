import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import { describe, expect, it, vi } from "vitest";

import type { DaLibp2pRetainedDaSource } from "../src/transition-trace/fetch.da-libp2p-retained-da-source.js";
import { workflowActuationDecisionDigest } from "../src/workflow/actuation-permit.assert-workflow-actuation-permit-identity.js";
import {
  isWorkflowActuationRevokedError,
  type WorkflowActuationCheckpoint,
  WorkflowActuationRevokedError,
} from "../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import {
  assertWorkflowRunActuation,
  bindWorkflowRunActuation,
} from "../src/workflow/actuation-permit.js";
import {
  assertWorkflowApplicationRegistry,
  installWorkflowApplicationRegistry,
  validateWorkflowAdapterCoverage,
  WORKFLOW_ADAPTER_REGISTRATIONS,
  WORKFLOW_ADAPTER_RUNNER,
  workflowAdapterRunner,
} from "../src/workflow/adapters.js";
import { defineFamilyApplication } from "../src/workflow/family-application.assert-family-definition-roster.js";
import { FAMILY_APPLICATION_REGISTRY } from "../src/workflow/family-application-registry.family-application-registry.js";
import {
  createFamilyApplicationWorkflowRunner,
  createManifestBoundWorkflowRunner,
  type LoadedWorkflowRuntime,
  WORKFLOW_RUNNER_FACTORIES,
} from "../src/workflow/runtime.js";
import * as runtime from "../src/workflow/runtime.js";
import {
  createWorkflowObjectiveWallet,
  createWorkflowRun,
  type WorkflowRun,
} from "../src/workflow/workflow-run.js";
import { familyCommonInfrastructureForTest } from "./support/family-common-infrastructure.js";
import {
  admittedActuation,
  DEPLOYMENT,
} from "./workflow-runtime.admitted-actuation.js";
import {
  type DoubleSpendIdentity,
  doubleSpendRecord,
  loadedRuntime,
  recoverableDoubleSpendWorkflow,
} from "./workflow-runtime.loaded-runtime.js";

/** A fresh wallet wiring for `actuation`'s objective. */
const objectiveWalletOf = (actuation: Readonly<{ headerHash: string }>) =>
  createWorkflowObjectiveWallet({
    category: "doubleSpend",
    headerHash: actuation.headerHash,
  });

describe("compiled manifest-bound production runtime V1", () => {
  it("admits every fixed factory only for its exact application category", () => {
    const categories = Object.keys(
      WORKFLOW_RUNNER_FACTORIES,
    ) as (keyof typeof WORKFLOW_RUNNER_FACTORIES)[];
    for (const category of categories) {
      const runner = WORKFLOW_RUNNER_FACTORIES[category](async () => {
        throw new Error(`${category} loader is not invoked during admission`);
      });
      const registry = installWorkflowApplicationRegistry({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: [category],
        installations: [
          { category, deploymentFingerprint: DEPLOYMENT, runner },
        ],
      });
      expect(
        registry.registrations.find(
          (registration) => registration.category === category,
        ),
      ).toMatchObject({ category, status: "ready", runner });
      const otherCategory = categories.find(
        (candidate) => candidate !== category,
      )!;
      expect(() =>
        installWorkflowApplicationRegistry({
          deploymentFingerprint: DEPLOYMENT,
          requiredInstalledCategories: [otherCategory],
          installations: [
            {
              category: otherCategory,
              deploymentFingerprint: DEPLOYMENT,
              runner,
            },
          ],
        }),
      ).toThrow("module-admitted category-bound runner");
    }
  });

  it("derives one factory per registry record and exports no per-family constructor", () => {
    expect(Object.keys(WORKFLOW_RUNNER_FACTORIES).sort()).toEqual(
      Object.keys(FAMILY_APPLICATION_REGISTRY).sort(),
    );
    for (const factory of Object.values(WORKFLOW_RUNNER_FACTORIES)) {
      expect(typeof factory).toBe("function");
    }
    // The only runner constructors the module exports are the two generic
    // ones: the public non-admissible body and the record-typed admitted
    // one. A family is added to the table by registering its record.
    expect(
      Object.keys(runtime)
        .filter((name) => /^create\w+WorkflowRunner$/.test(name))
        .sort(),
    ).toEqual([
      "createFamilyApplicationWorkflowRunner",
      "createManifestBoundWorkflowRunner",
    ]);
  });

  it("gives every derived runner exactly the shared drive surface", () => {
    for (const [category, factory] of Object.entries(
      WORKFLOW_RUNNER_FACTORIES,
    )) {
      const runner = factory(async () => {
        throw new Error(`${category} loader is not invoked during admission`);
      });
      expect(runner.runnerVersion).toBe(WORKFLOW_ADAPTER_RUNNER);
      expect(Object.keys(runner).sort()).toEqual([
        "runOrResume",
        "runnerVersion",
      ]);
    }
  });

  it("wires every row to the record of its own category", async () => {
    // The table is keyed by the registry, and each row's body refuses a
    // foreign category naming its own: together these pin row X to record X.
    for (const [category, factory] of Object.entries(
      WORKFLOW_RUNNER_FACTORIES,
    )) {
      const loadRuntimeConfig = vi.fn();
      const runner = factory(loadRuntimeConfig);
      const foreign = category === "doubleSpend" ? "zeroInput" : "doubleSpend";
      await expect(
        runner.runOrResume({ category: foreign } as never),
      ).rejects.toThrow(
        `production workflow runner category mismatch: expected=${category} actual=${foreign}`,
      );
      expect(loadRuntimeConfig).not.toHaveBeenCalled();
    }
  });

  it("does not admit the public generic constructor as a production family runner", () => {
    const generic = createManifestBoundWorkflowRunner({
      record: doubleSpendRecord({
        bindConfig: () => undefined,
        constructWorkflow: async (): Promise<DoubleSpendIdentity> => {
          throw new Error("generic runner is not invoked during admission");
        },
        execute: async () => {
          throw new Error("generic runner is not invoked during admission");
        },
      }),
      loadRuntime: async () => {
        throw new Error("generic runner is not invoked during admission");
      },
    });
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) =>
          registration.category === "doubleSpend"
            ? { ...registration, status: "ready", runner: generic }
            : registration,
        ),
      ),
    ).toThrow("no compiled executable runner admitted for its exact category");
  });

  it("installs an immutable deployment-bound application overlay without mutating the static catalogue", () => {
    const runner = WORKFLOW_RUNNER_FACTORIES.daHashPreimage(async () => {
      throw new Error("installed Q44 loader reached");
    });
    const registry = installWorkflowApplicationRegistry({
      deploymentFingerprint: DEPLOYMENT,
      requiredInstalledCategories: ["daHashPreimage"],
      installations: [
        {
          category: "daHashPreimage",
          deploymentFingerprint: DEPLOYMENT,
          runner,
        },
      ],
    });
    expect(() => assertWorkflowApplicationRegistry(registry)).not.toThrow();
    expect(Object.isFrozen(registry)).toBe(true);
    expect(Object.isFrozen(registry.installedCategories)).toBe(true);
    expect(Object.isFrozen(registry.registrations)).toBe(true);
    expect(registry.registrations).toHaveLength(
      WORKFLOW_ADAPTER_REGISTRATIONS.length,
    );
    expect(
      registry.registrations.find(
        (registration) => registration.category === "daHashPreimage",
      ),
    ).toMatchObject({ status: "ready", runner });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
        (registration) => registration.category === "daHashPreimage",
      ),
    ).toMatchObject({ status: "missing" });
    expect(workflowAdapterRunner("daHashPreimage", registry)).toBe(runner);
  });

  it("rejects incomplete, duplicate, unrecognized, forged, and cross-category application installations", () => {
    const doubleSpend = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
      throw new Error("not invoked");
    });
    const daHashPreimage = WORKFLOW_RUNNER_FACTORIES.daHashPreimage(
      async () => {
        throw new Error("not invoked");
      },
    );
    const install = (
      input: Parameters<typeof installWorkflowApplicationRegistry>[0],
    ) => installWorkflowApplicationRegistry(input);

    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["doubleSpend", "daHashPreimage"],
        installations: [
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("installation cardinality mismatch");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["doubleSpend", "daHashPreimage"],
        installations: [
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("duplicates doubleSpend");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: "ff".repeat(32),
            runner: daHashPreimage,
          },
        ],
      }),
    ).toThrow("unrecognized deployment identity");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("module-admitted category-bound runner");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: DEPLOYMENT,
            runner: {
              runnerVersion: WORKFLOW_ADAPTER_RUNNER,
              runOrResume: async () => undefined,
            },
          },
        ],
      }),
    ).toThrow("module-admitted category-bound runner");
    expect(() =>
      assertWorkflowApplicationRegistry({
        schemaVersion: "midgard-production-fraud-proof-application-registry-v1",
        deploymentFingerprint: DEPLOYMENT,
        installedCategories: ["daHashPreimage"],
        registrations: WORKFLOW_ADAPTER_REGISTRATIONS,
      }),
    ).toThrow("not installed through the authenticated immutable boundary");
  });

  it("constructs the exact workflow and runs it over the invocation's objective wallet", async () => {
    const actuation = await admittedActuation();
    const close = vi.fn(async () => undefined);
    const loadRuntime = vi.fn(async () => loadedRuntime(actuation, { close }));
    const bindConfig = vi.fn(() => ({ releaseConfig: "manifest-bound" }));
    const constructWorkflow = vi.fn(async (_config: unknown) =>
      recoverableDoubleSpendWorkflow(actuation.headerHash),
    );
    const objectiveWallet = objectiveWalletOf(actuation);
    const execute = vi.fn(async ({ run }: { readonly run: WorkflowRun }) => {
      expect(run.objectiveWallet).toBe(objectiveWallet);
      expect(workflowActuationDecisionDigest(run)).toBe(
        actuation.decisionDigest,
      );
      return { kind: "executed" };
    });
    const runner = createManifestBoundWorkflowRunner({
      record: doubleSpendRecord({ bindConfig, constructWorkflow, execute }),
      loadRuntime,
    });
    expect(runner.runnerVersion).toBe(WORKFLOW_ADAPTER_RUNNER);
    await expect(
      runner.runOrResume({
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        objectiveWallet,
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      }),
    ).resolves.toStrictEqual({ kind: "executed" });
    expect(loadRuntime).toHaveBeenCalledWith({
      runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      invocation: expect.objectContaining({
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
      }),
    });
    // The config the record binds from the loaded infrastructure is what
    // its constructor receives: the loader never hands a family its config.
    expect(bindConfig).toHaveBeenCalledWith(
      expect.objectContaining({
        infrastructure: expect.objectContaining({
          headerHash: actuation.headerHash,
        }),
        references: {},
      }),
    );
    expect(constructWorkflow).toHaveBeenCalledWith({
      releaseConfig: "manifest-bound",
    });
    expect(execute).toHaveBeenCalledOnce();
    expect(close).toHaveBeenCalledOnce();
  });

  it("rejects a revoked decision permit before loading runtime infrastructure", async () => {
    const actuation = await admittedActuation();
    actuation.revoke("canonical rollback observed");
    const loadRuntime = vi.fn(async () => {
      throw new Error("revoked runner must not load infrastructure");
    });
    const runner = createManifestBoundWorkflowRunner({
      record: doubleSpendRecord({
        bindConfig: () => undefined,
        constructWorkflow: async (): Promise<DoubleSpendIdentity> => {
          throw new Error("revoked runner must not construct a workflow");
        },
        execute: async () => {
          throw new Error("revoked runner must not execute");
        },
      }),
      loadRuntime,
    });
    let rejected: unknown;
    try {
      await runner.runOrResume({
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        objectiveWallet: objectiveWalletOf(actuation),
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      });
    } catch (error) {
      rejected = error;
    }
    expect(rejected).toBeInstanceOf(WorkflowActuationRevokedError);
    expect(isWorkflowActuationRevokedError(rejected)).toBe(true);
    expect(
      isWorkflowActuationRevokedError(
        new WorkflowActuationRevokedError({
          decisionDigest: actuation.decisionDigest,
          rollbackGeneration: "7",
          checkpoint: "runner_start",
          revocationReason: "forged",
        }),
      ),
    ).toBe(false);
    expect(loadRuntime).not.toHaveBeenCalled();
  });

  it("checks the live permit at every shared workflow actuation boundary", async () => {
    const actuation = await admittedActuation();
    const run = bindWorkflowRunActuation({
      run: createWorkflowRun(objectiveWalletOf(actuation)),
      permit: actuation.actuationPermit,
      decisionDigest: actuation.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash: actuation.headerHash,
    });
    expect(workflowActuationDecisionDigest(run)).toBe(actuation.decisionDigest);
    const checkpoints: readonly WorkflowActuationCheckpoint[] = [
      "workflow_resume",
      "before_observe",
      "before_preflight",
      "before_submit",
      "before_terminal_verify",
    ];
    for (const checkpoint of checkpoints) {
      expect(() =>
        assertWorkflowRunActuation({
          run,
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
          checkpoint,
        }),
      ).not.toThrow();
    }
    actuation.revoke("canonical rollback observed");
    for (const checkpoint of checkpoints) {
      let rejected: unknown;
      try {
        assertWorkflowRunActuation({
          run,
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
          checkpoint,
        });
      } catch (error) {
        rejected = error;
      }
      expect(isWorkflowActuationRevokedError(rejected)).toBe(true);
      expect(rejected).toMatchObject({
        decisionDigest: actuation.decisionDigest,
        rollbackGeneration: "7",
        checkpoint,
      });
    }
  });

  /**
   * One refusal test per check of the generic run-or-resume body. Every row of
   * the runner table is this body applied to a registry record, so each check
   * is tested here once instead of once per family. The workflow under test is
   * built through the public generic constructor, whose body is the same
   * function the admitted rows run.
   */
  describe("generic run-or-resume refusals", () => {
    type Actuation = Awaited<ReturnType<typeof admittedActuation>>;
    const identityOf = (
      actuation: Actuation,
      deploymentFingerprint: string = DEPLOYMENT,
    ) =>
      recoverableDoubleSpendWorkflow(
        actuation.headerHash,
        deploymentFingerprint,
      );
    const invocationOf = (actuation: Actuation) => ({
      category: "doubleSpend" as const,
      deploymentFingerprint: DEPLOYMENT,
      headerHash: actuation.headerHash,
      decisionDigest: actuation.decisionDigest,
      actuationPermit: actuation.actuationPermit,
      objectiveWallet: objectiveWalletOf(actuation),
      runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
    });
    const driveGenericRunOrResume = async ({
      loaded,
      deploymentFingerprint,
      category,
      record = {},
    }: {
      readonly loaded: (
        actuation: Actuation,
        close: () => Promise<void>,
      ) => LoadedWorkflowRuntime;
      readonly deploymentFingerprint?: string;
      readonly category?: FraudProofCatalogueCategoryName;
      readonly record?: Partial<
        Pick<
          Parameters<typeof doubleSpendRecord>[0],
          "roster" | "requires" | "constructWorkflow"
        >
      >;
    }) => {
      const actuation = await admittedActuation();
      const execute = vi.fn(async () => ({ kind: "unexpected" }));
      const close = vi.fn(async () => undefined);
      const loadRuntime = vi.fn(async () => loaded(actuation, close));
      const runner = createManifestBoundWorkflowRunner({
        record: doubleSpendRecord({
          bindConfig: () => undefined,
          constructWorkflow: async () =>
            identityOf(actuation, deploymentFingerprint),
          execute,
          ...record,
        }),
        loadRuntime,
      });
      const invocation = invocationOf(actuation);
      const outcome = runner.runOrResume(
        category === undefined ? invocation : { ...invocation, category },
      );
      await outcome.catch(() => undefined);
      return { outcome, execute, close, loadRuntime };
    };
    const publicLoaded =
      (overrides: Partial<LoadedWorkflowRuntime> = {}) =>
      (
        actuation: Actuation,
        close: () => Promise<void>,
      ): LoadedWorkflowRuntime =>
        loadedRuntime(actuation, { close, ...overrides });

    it("refuses another category before loading any runtime configuration", async () => {
      const { outcome, loadRuntime, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded(),
        category: "zeroInput",
      });
      await expect(outcome).rejects.toThrow(
        "production workflow runner category mismatch: expected=doubleSpend actual=zeroInput",
      );
      expect(loadRuntime).not.toHaveBeenCalled();
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses a loaded runtime that omits its transport disposer", async () => {
      const { outcome, execute } = await driveGenericRunOrResume({
        loaded: (actuation) => {
          const { close: _close, ...withoutDisposer } = publicLoaded()(
            actuation,
            async () => undefined,
          );
          return withoutDisposer as never;
        },
      });
      await expect(outcome).rejects.toThrow(
        "production workflow runtime config omitted its transport disposer",
      );
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses an unsupported runtime config schema and still disposes the transport", async () => {
      const { outcome, close, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded({
          schemaVersion:
            "midgard-production-fraud-proof-runtime-config-v0" as never,
        }),
      });
      await expect(outcome).rejects.toThrow(
        "production workflow runtime config has an unsupported schema",
      );
      expect(close).toHaveBeenCalledOnce();
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses a retained-DA source that is not a public libp2p transport", async () => {
      const { outcome, close, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded({
          retainedDaSources: [
            {
              sourceId: "operator-private-file",
              fetchPayloadByHeaderHash: async () => ({
                ok: false as const,
                sourceId: "operator-private-file",
                attempts: [],
              }),
            } as unknown as DaLibp2pRetainedDaSource,
          ],
        }),
      });
      await expect(outcome).rejects.toThrow(
        "production workflow runtime requires concrete public retained-DA libp2p sources",
      );
      expect(close).toHaveBeenCalledOnce();
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses a constructed workflow whose manifest identity differs from the invocation", async () => {
      const { outcome, close, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded(),
        deploymentFingerprint: "ff".repeat(32),
      });
      await expect(outcome).rejects.toThrow(
        "manifest-bound workflow identity differs from the compiled CLI invocation",
      );
      expect(close).toHaveBeenCalledOnce();
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses loaded infrastructure that names another header or decision, and disposes it", async () => {
      for (const overrides of [
        { headerHash: "ee".repeat(32) },
        { decisionDigest: "ee".repeat(32) },
        { decisionDigest: undefined },
      ]) {
        const { outcome, close, execute } = await driveGenericRunOrResume({
          loaded: (actuation, close) =>
            loadedRuntime(actuation, {
              close,
              infrastructure: familyCommonInfrastructureForTest(
                actuation,
                overrides,
              ),
            }),
        });
        await expect(outcome).rejects.toThrow(
          "production workflow runtime infrastructure differs from the invocation",
        );
        expect(close).toHaveBeenCalledOnce();
        expect(execute).not.toHaveBeenCalled();
      }
    });

    it("applies the record inside the body: resolves its roster through the loaded resolver and refuses a missing requirement", async () => {
      const resolveReferenceScript = vi.fn(async () => {
        throw new Error("roster resolution reached the host resolver");
      });
      const missing = await driveGenericRunOrResume({
        loaded: (actuation, close) =>
          loadedRuntime(actuation, { close, resolveReferenceScript }),
        record: { requires: ["replayContext"] },
      });
      await expect(missing.outcome).rejects.toThrow(
        "doubleSpend application requires replayContext, which the host did not supply",
      );
      expect(resolveReferenceScript).not.toHaveBeenCalled();
      expect(missing.close).toHaveBeenCalledOnce();

      const rostered = await driveGenericRunOrResume({
        loaded: (actuation, close) =>
          loadedRuntime(actuation, { close, resolveReferenceScript }),
        record: { roster: { step01: "fraudProofDoubleSpend" } },
      });
      await expect(rostered.outcome).rejects.toThrow(
        "roster resolution reached the host resolver",
      );
      expect(resolveReferenceScript).toHaveBeenCalledWith({
        category: "doubleSpend",
        role: "step01",
        contractName: "fraudProofDoubleSpend",
      });
      expect(rostered.close).toHaveBeenCalledOnce();
      expect(rostered.execute).not.toHaveBeenCalled();
    });

    it("checks the constructed decision digest against the invocation only when the record binds it", async () => {
      const execute = vi.fn(async () => ({ kind: "executed" }));
      const run = async (bindsDecisionDigest: boolean) => {
        const actuation = await admittedActuation();
        const runner = createFamilyApplicationWorkflowRunner(
          defineFamilyApplication({
            category: "doubleSpend" as const,
            roster: {},
            requires: [],
            bindConfig: () => undefined,
            constructWorkflow: async () => ({
              ...recoverableDoubleSpendWorkflow(actuation.headerHash),
              decisionDigest: "ab".repeat(32),
            }),
            execute,
            bindsDecisionDigest,
          }),
          async () => loadedRuntime(actuation),
        );
        return await runner.runOrResume({
          category: "doubleSpend",
          deploymentFingerprint: DEPLOYMENT,
          headerHash: actuation.headerHash,
          decisionDigest: actuation.decisionDigest,
          actuationPermit: actuation.actuationPermit,
          objectiveWallet: objectiveWalletOf(actuation),
          runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
        });
      };
      await expect(run(true)).rejects.toThrow(
        "doubleSpend manifest-bound workflow decision digest differs from invocation",
      );
      expect(execute).not.toHaveBeenCalled();
      await run(false);
      expect(execute).toHaveBeenCalledOnce();
    });
  });
});
