import {
  credentialToAddress,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { WORKFLOW_ACTUATION_PERMIT } from "../src/workflow/actuation-permit.js";
import {
  applyFamilyApplicationRecord,
  assertFamilyDefinitionRoster,
  defineFamilyApplication,
  type FamilyApplicationInvocation,
  type FamilyCommonInfrastructure,
  familyDefinitionRoster,
  resolveFamilyApplicationReferences,
} from "../src/workflow/family-application.js";
import { isAdmittedWorkflowRunner } from "../src/workflow/runner-admission.js";
import {
  createFamilyApplicationWorkflowRunner,
  createManifestBoundWorkflowRunner,
} from "../src/workflow/runtime.js";

const category = "minFee" as const;
const headerHash = "aa".repeat(28);
const deploymentFingerprint = "44".repeat(32);
const decisionDigest = "55".repeat(32);
const script = { type: "PlutusV3", script: "49480100002221200101" } as const;
const address = credentialToAddress("Preprod", {
  type: "Key",
  hash: "11".repeat(28),
});

const reference = (outputIndex: number): UTxO => ({
  txHash: "22".repeat(32),
  outputIndex,
  address,
  assets: { lovelace: 2_000_000n },
  scriptRef: script,
});

const roster = Object.freeze({
  step01: "fraudProofMinFee",
  computationThreadMint: "computationThreadMint",
});

type Config = Readonly<{ roles: readonly string[] }>;
type Workflow = Readonly<{
  binding: Readonly<{
    deploymentFingerprint: string;
    definition: Readonly<{ category: typeof category; headerHash: string }>;
  }>;
  decisionDigest?: string;
}>;

const workflowOf = (
  overrides: Partial<Workflow["binding"]> = {},
): Workflow => ({
  binding: {
    deploymentFingerprint,
    definition: { category, headerHash },
    ...overrides,
  },
  decisionDigest,
});

const fixture = (
  overrides: Partial<
    Parameters<
      typeof defineFamilyApplication<typeof category, Config, Workflow>
    >[0]
  > = {},
) => {
  const resolved = new Map<string, UTxO>();
  const resolveReferenceScript = vi.fn(
    async (input: { readonly role: string }): Promise<UTxO> => {
      const utxo = reference(resolved.size);
      resolved.set(input.role, utxo);
      return utxo;
    },
  );
  const record = defineFamilyApplication<typeof category, Config, Workflow>({
    category,
    roster,
    requires: [],
    bindConfig: ({ references }) => ({ roles: Object.keys(references) }),
    constructWorkflow: async () => workflowOf(),
    execute: async () => ({}),
    bindsDecisionDigest: false,
    ...overrides,
  });
  const infrastructure = {
    manifest: {},
    blueprintJson: "{}",
    deploymentInfo: {},
    headerHash,
    lucid: {} as never,
    signer: {} as never,
    source: {} as never,
    stateQueueMutationLeaseCoordinator: {} as never,
  } satisfies FamilyCommonInfrastructure;
  const invocation: FamilyApplicationInvocation = {
    deploymentFingerprint,
    category,
    headerHash,
  };
  return {
    record,
    infrastructure,
    invocation,
    resolveReferenceScript,
    resolved,
  };
};

describe("shared family application loop", () => {
  it("resolves exactly the roster and derives out-refs from the resolved map", async () => {
    const f = fixture();
    const applied = await applyFamilyApplicationRecord({
      record: f.record,
      infrastructure: f.infrastructure,
      resolveReferenceScript: f.resolveReferenceScript,
      invocation: f.invocation,
    });
    expect(f.resolveReferenceScript.mock.calls.map(([input]) => input)).toEqual(
      Object.entries(roster).map(([role, contractName]) => ({
        category,
        role,
        contractName,
      })),
    );
    expect(Object.keys(applied.references)).toEqual(Object.keys(roster));
    expect(applied.config.roles).toEqual(Object.keys(roster));
    expect(applied.referenceScriptOutRefs).toEqual(
      Object.fromEntries(
        [...f.resolved].map(([role, utxo]) => [
          role,
          `${utxo.txHash}#${utxo.outputIndex}`,
        ]),
      ),
    );
    expect(validatorToScriptHash(script)).toBe(
      validatorToScriptHash(applied.references.step01!.scriptRef!),
    );
  });

  it("refuses once, naming the category and the missing part", async () => {
    const f = fixture({ requires: ["replayContext"] });
    await expect(
      applyFamilyApplicationRecord({
        record: f.record,
        infrastructure: f.infrastructure,
        resolveReferenceScript: f.resolveReferenceScript,
        invocation: f.invocation,
      }),
    ).rejects.toThrow(
      "minFee application requires replayContext, which the host did not supply",
    );
    expect(f.resolveReferenceScript).not.toHaveBeenCalled();
  });

  it.each([
    ["replayContext", { replayContext: { sentinel: "replay" } }],
    [
      "historicalNativeScriptAuthority",
      { historicalNativeScriptAuthority: { sentinel: "authority" } },
    ],
  ] as const)(
    "refuses a record requiring %s when the host omits it and binds it when supplied",
    async (requirement, supplied) => {
      const f = fixture({
        requires: [requirement],
        bindConfig: ({ infrastructure }) =>
          ({ roles: [], bound: infrastructure[requirement] }) as never,
      });
      await expect(
        applyFamilyApplicationRecord({
          record: f.record,
          infrastructure: f.infrastructure,
          resolveReferenceScript: f.resolveReferenceScript,
          invocation: f.invocation,
        }),
      ).rejects.toThrow(
        `minFee application requires ${requirement}, which the host did not supply`,
      );
      expect(f.resolveReferenceScript).not.toHaveBeenCalled();
      const applied = await applyFamilyApplicationRecord({
        record: f.record,
        infrastructure: {
          ...f.infrastructure,
          ...(supplied as Partial<FamilyCommonInfrastructure>),
        },
        resolveReferenceScript: f.resolveReferenceScript,
        invocation: f.invocation,
      });
      expect((applied.config as { bound?: unknown }).bound).toBe(
        Object.values(supplied)[0],
      );
    },
  );

  it("refuses a record requiring the challenge port when the host omits it and binds the port's challenge when supplied", async () => {
    const challenge = Object.freeze({ sentinel: "challenge" });
    const currentChallenge = vi.fn(async () => challenge as never);
    const f = fixture({
      requires: ["validationChallenge"],
      bindConfig: async ({ infrastructure }) =>
        ({
          roles: [],
          challenge: await infrastructure.validationChallenge?.currentChallenge(
            {
              headerHash: infrastructure.headerHash,
              decisionDigest: infrastructure.decisionDigest ?? "",
            },
          ),
        }) as never,
    });
    await expect(
      applyFamilyApplicationRecord({
        record: f.record,
        infrastructure: f.infrastructure,
        resolveReferenceScript: f.resolveReferenceScript,
        invocation: f.invocation,
      }),
    ).rejects.toThrow(
      "minFee application requires validationChallenge, which the host did not supply",
    );
    expect(f.resolveReferenceScript).not.toHaveBeenCalled();
    const applied = await applyFamilyApplicationRecord({
      record: f.record,
      infrastructure: {
        ...f.infrastructure,
        decisionDigest,
        validationChallenge: { currentChallenge },
      },
      resolveReferenceScript: f.resolveReferenceScript,
      invocation: f.invocation,
    });
    expect((applied.config as { challenge?: unknown }).challenge).toBe(
      challenge,
    );
    expect(currentChallenge).toHaveBeenCalledWith({
      headerHash,
      decisionDigest,
    });
  });

  it("refuses a reconciliation exemption that cannot prove itself", async () => {
    const f = fixture({ requires: ["validationChallenge"] });
    await expect(
      applyFamilyApplicationRecord({
        record: f.record,
        infrastructure: f.infrastructure,
        resolveReferenceScript: f.resolveReferenceScript,
        invocation: {
          ...f.invocation,
          reconciliationAuthority: {
            permitVersion: WORKFLOW_ACTUATION_PERMIT,
          },
        },
      }),
    ).rejects.toThrow("production workflow actuation permit was not admitted");
    expect(f.resolveReferenceScript).not.toHaveBeenCalled();
  });

  it("resolves a roster for readiness without binding or requiring anything", async () => {
    const f = fixture({ requires: ["validationChallenge", "replayContext"] });
    const resolved = await resolveFamilyApplicationReferences({
      record: f.record,
      resolveReferenceScript: f.resolveReferenceScript,
    });
    expect(Object.keys(resolved.references)).toEqual(Object.keys(roster));
    expect(Object.keys(resolved.referenceScriptOutRefs)).toEqual(
      Object.keys(roster),
    );
    expect(
      f.resolveReferenceScript.mock.calls.map(([input]) => input.role),
    ).toEqual(Object.keys(roster));
    expect(resolved).not.toHaveProperty("workflow");
    expect(resolved).not.toHaveProperty("config");
  });

  it("refuses a record that was not minted through the definer", async () => {
    const f = fixture();
    await expect(
      applyFamilyApplicationRecord({
        record: { ...f.record, recordVersion: "forged" as never },
        infrastructure: f.infrastructure,
        resolveReferenceScript: f.resolveReferenceScript,
        invocation: f.invocation,
      }),
    ).rejects.toThrow(
      "family application record for minFee has an unsupported schema",
    );
    expect(f.resolveReferenceScript).not.toHaveBeenCalled();
  });

  it("refuses an invocation for another category", async () => {
    const f = fixture();
    await expect(
      applyFamilyApplicationRecord({
        record: f.record,
        infrastructure: f.infrastructure,
        resolveReferenceScript: f.resolveReferenceScript,
        invocation: { ...f.invocation, category: "zeroInput" },
      }),
    ).rejects.toThrow(
      "family application refuses category zeroInput; expected minFee",
    );
    expect(f.resolveReferenceScript).not.toHaveBeenCalled();
  });

  it.each([
    ["deploymentFingerprint", { deploymentFingerprint: "99".repeat(32) }],
    ["headerHash", { definition: { category, headerHash: "bb".repeat(28) } }],
  ] as const)(
    "refuses a constructed workflow whose %s differs from the invocation",
    async (_label, overrides) => {
      const f = fixture({
        constructWorkflow: async () => workflowOf(overrides),
      });
      await expect(
        applyFamilyApplicationRecord({
          record: f.record,
          infrastructure: f.infrastructure,
          resolveReferenceScript: f.resolveReferenceScript,
          invocation: f.invocation,
        }),
      ).rejects.toThrow(
        "manifest-bound workflow identity differs from the compiled CLI invocation",
      );
    },
  );

  it("checks the decision digest only for a record that binds it", async () => {
    const unbound = fixture();
    await expect(
      applyFamilyApplicationRecord({
        record: unbound.record,
        infrastructure: unbound.infrastructure,
        resolveReferenceScript: unbound.resolveReferenceScript,
        invocation: unbound.invocation,
      }),
    ).resolves.toMatchObject({ workflow: { decisionDigest } });
    const bound = fixture({ bindsDecisionDigest: true });
    await expect(
      applyFamilyApplicationRecord({
        record: bound.record,
        infrastructure: bound.infrastructure,
        resolveReferenceScript: bound.resolveReferenceScript,
        invocation: bound.invocation,
      }),
    ).rejects.toThrow(
      "minFee manifest-bound workflow decision digest differs from invocation",
    );
    await expect(
      applyFamilyApplicationRecord({
        record: bound.record,
        infrastructure: { ...bound.infrastructure, decisionDigest },
        resolveReferenceScript: bound.resolveReferenceScript,
        invocation: bound.invocation,
      }),
    ).resolves.toMatchObject({ workflow: { decisionDigest } });
  });
});

describe("record-derived runner", () => {
  const loadRuntime = async (): Promise<never> => {
    throw new Error("unused");
  };

  it("admits a runner built from a record, and never the public constructor", () => {
    const f = fixture();
    const runner = createFamilyApplicationWorkflowRunner(f.record, loadRuntime);
    expect(isAdmittedWorkflowRunner({ category, runner })).toBe(true);
    expect(isAdmittedWorkflowRunner({ category: "zeroInput", runner })).toBe(
      false,
    );
    expect(
      isAdmittedWorkflowRunner({
        category,
        runner: createManifestBoundWorkflowRunner({
          record: f.record,
          loadRuntime,
        }),
      }),
    ).toBe(false);
  });
});

describe("definition-derived roster", () => {
  const definition = Object.freeze({
    category: "unusedRedeemer" as const,
    witnessRoles: ["computationThreadMint", "fraudProofMint"] as const,
    fieldPreimageCertificate: false,
    auxiliaryReferenceScripts: Object.freeze({
      stateQueueSpend: "stateQueueSpend",
      schedulerSpend: "schedulerSpend",
    }),
    adapter: Object.freeze({
      kind: "cursor" as const,
      stepContractNames: [
        "fraudProofUnusedRedeemer",
        "fraudProofUnusedRedeemerStep02",
      ] as const,
    }),
  });

  it("carries the definition's auxiliary reference scripts as roles", () => {
    expect(familyDefinitionRoster(definition)).toEqual({
      step01: "fraudProofUnusedRedeemer",
      step02: "fraudProofUnusedRedeemerStep02",
      computationThreadMint: "computationThreadMint",
      fraudProofMint: "fraudProofMint",
      stateQueueSpend: "stateQueueSpend",
      schedulerSpend: "schedulerSpend",
    });
    expect(() =>
      assertFamilyDefinitionRoster(
        definition,
        familyDefinitionRoster(definition),
      ),
    ).not.toThrow();
  });

  it("refuses a roster that drifts from the declared auxiliary set in either direction", () => {
    const { schedulerSpend: _omitted, ...withoutScheduler } =
      familyDefinitionRoster(definition);
    expect(() =>
      assertFamilyDefinitionRoster(definition, withoutScheduler),
    ).toThrow("unusedRedeemer derived roster omits schedulerSpend");
    expect(() =>
      assertFamilyDefinitionRoster(definition, {
        ...familyDefinitionRoster(definition),
        correctionLockSpend: "correctionLockSpend",
      }),
    ).toThrow("unusedRedeemer derived roster carries correctionLockSpend");
    expect(() =>
      assertFamilyDefinitionRoster(
        { ...definition, auxiliaryReferenceScripts: undefined },
        familyDefinitionRoster(definition),
      ),
    ).toThrow("unusedRedeemer derived roster carries stateQueueSpend");
  });
});
