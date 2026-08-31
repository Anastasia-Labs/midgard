import { Proof } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { splitProofIntoChunkDatums } from "../src/publish-proof-chunks.js";
import type { FraudProofWorkflowIdentityV1 } from "../src/workflow/journal-v1.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
} from "../src/workflow/orchestrator-v1.js";
import {
  PRODUCTION_PROOF_CHUNK_PREREQUISITE_V1,
  type ProductionProofChunkPrerequisitePortV1,
  resolveDirectFirstProofChunksV1,
  withProductionProofChunkPrerequisiteV1,
} from "../src/workflow/production-proof-chunk-prerequisite-v1.js";
import type { LocallyEvaluatedTransactionV1 } from "../src/workflow/transaction-boundary-v1.js";

const txHash = "11".repeat(32);
const headerHash = "22".repeat(28);
const baseAction: FraudProofWorkflowActionV1 = Object.freeze({
  actionId: `step_01:${"33".repeat(32)}#0:${"44".repeat(32)}#0`,
  input: Object.freeze({
    schemaVersion: "midgard-production-linear-family-action-v1",
    category: "invalidRange",
    stage: "step_01",
    ordinal: 1,
    threadOutRef: `${"33".repeat(32)}#0`,
    stateQueueBlockOutRef: `${"44".repeat(32)}#0`,
  }),
});
const publicationAction: FraudProofWorkflowActionV1 = Object.freeze({
  actionId: `publish-proof-chunks:${baseAction.actionId}:${"55".repeat(32)}`,
  input: Object.freeze({
    schemaVersion: PRODUCTION_PROOF_CHUNK_PREREQUISITE_V1,
    category: "invalidRange",
    stage: "direct_or_publish_proof",
    forAction: baseAction,
    proofCborSha256: "55".repeat(32),
    chunkDatumSha256s: ["66".repeat(32)],
  }),
});

const identity: FraudProofWorkflowIdentityV1 = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: "77".repeat(32),
  category: "invalidRange",
  target: { kind: "state_queue_header", headerHash },
};
const context = {
  identity,
  workflowId: "88".repeat(32),
  artifact: { proofCbor: "proof-from-public-da" },
  entries: [],
} as const;

const signed = (): LocallyEvaluatedTransactionV1["signed"] =>
  ({
    toHash: () => txHash,
    submit: async () => txHash,
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () => undefined,
        plutus_v1_scripts: () => undefined,
        plutus_v2_scripts: () => undefined,
        plutus_v3_scripts: () => undefined,
      }),
    }),
  }) as unknown as LocallyEvaluatedTransactionV1["signed"];

const transaction = (): LocallyEvaluatedTransactionV1 => ({
  txHash,
  signed: signed(),
  referenceScripts: [],
});

const base = ({
  preflightFailure,
}: {
  readonly preflightFailure?: Error;
} = {}): FraudProofFamilyWorkflowAdapterV1 => ({
  adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  category: "invalidRange",
  safety: FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  prepare: vi.fn(async () => ({ proofCbor: "proof-from-public-da" })),
  observe: vi.fn(async () => ({
    kind: "action_required" as const,
    action: baseAction,
  })),
  preflight: vi.fn(async () => {
    if (preflightFailure !== undefined) throw preflightFailure;
    return {
      actionId: baseAction.actionId,
      txHash: "99".repeat(32),
      scriptExecution: "reference_scripts" as const,
      localUplcEvaluation: {
        status: "passed" as const,
        evaluator: "base-local-uplc",
      },
      referenceScripts: [
        {
          role: "invalid-range step-01",
          outRef: `${"aa".repeat(32)}#0`,
          scriptHash: "bb".repeat(28),
        },
      ],
    };
  }),
  submit: vi.fn(async () => ({
    kind: "submitted" as const,
    txHash: "99".repeat(32),
  })),
  reconcile: vi.fn(async () => ({
    kind: "confirmed" as const,
    txHash: "99".repeat(32),
  })),
});

const prerequisite = ({
  satisfied = false,
  reconcile = async () => ({ kind: "confirmed" as const, txHash }),
}: {
  readonly satisfied?: boolean;
  readonly reconcile?: ProductionProofChunkPrerequisitePortV1<"invalidRange">["reconcile"];
} = {}): ProductionProofChunkPrerequisitePortV1<"invalidRange"> => ({
  portVersion: PRODUCTION_PROOF_CHUNK_PREREQUISITE_V1,
  category: "invalidRange",
  classifyDirectCapacityFailure: vi.fn((cause: unknown) => {
    if (
      !(cause instanceof Error) ||
      cause.message !== "Max transaction size of 16384 exceeded. Found: 16385"
    ) {
      throw cause;
    }
    return {
      kind: "max_tx_size" as const,
      maximumTransactionBytes: 16_384,
      actualTransactionBytes: 16_385,
      errorSha256: "77".repeat(32),
    };
  }),
  inspect: vi.fn(async () =>
    satisfied
      ? { kind: "satisfied" as const }
      : { kind: "required" as const, action: publicationAction },
  ),
  capture: vi.fn(async () => ({
    transaction: transaction(),
    durableRecovery: {
      proofChunkPublication: {
        schemaVersion: "midgard-production-proof-chunk-publication-recovery-v1",
        proofCborSha256: "55".repeat(32),
        outputs: [
          {
            outRef: `${txHash}#0`,
            datumCbor: "d87980",
          },
        ],
      },
    },
  })),
  reconcile,
});

describe("production proof-chunk prerequisite V1", () => {
  it("makes the publication a distinct durable action and forbids step bypass", async () => {
    const underlying = base({
      preflightFailure: new Error(
        "Max transaction size of 16384 exceeded. Found: 16385",
      ),
    });
    const publication = prerequisite();
    const adapter = withProductionProofChunkPrerequisiteV1({
      category: "invalidRange",
      base: underlying,
      prerequisite: publication,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: publicationAction,
    });
    await expect(
      adapter.preflight({ ...context, action: baseAction }),
    ).rejects.toThrow("cannot bypass its direct-first carriage decision");
    expect(underlying.preflight).not.toHaveBeenCalled();

    const preflight = await adapter.preflight({
      ...context,
      action: publicationAction,
    });
    expect(preflight).toMatchObject({
      actionId: publicationAction.actionId,
      txHash,
      scriptExecution: "none",
      localUplcEvaluation: { status: "passed" },
      referenceScripts: [],
    });
    expect(preflight.durableRecovery).toEqual(
      expect.objectContaining({ proofCarriage: expect.any(Object) }),
    );
    expect(publication.classifyDirectCapacityFailure).toHaveBeenCalledOnce();
    expect(publication.capture).toHaveBeenCalledOnce();
    await expect(
      adapter.submit({
        ...context,
        action: publicationAction,
        preflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash });
  });

  it("keeps a fitting proof on the exact direct transaction without publishing", async () => {
    const underlying = base();
    const publication = prerequisite();
    const adapter = withProductionProofChunkPrerequisiteV1({
      category: "invalidRange",
      base: underlying,
      prerequisite: publication,
    });
    const preflight = await adapter.preflight({
      ...context,
      action: publicationAction,
    });
    expect(preflight).toMatchObject({
      actionId: publicationAction.actionId,
      txHash: "99".repeat(32),
      scriptExecution: "reference_scripts",
    });
    expect(preflight.durableRecovery).toMatchObject({
      proofCarriage: { route: "direct", baseAction },
    });
    expect(publication.capture).not.toHaveBeenCalled();
    await expect(
      adapter.submit({
        ...context,
        action: publicationAction,
        preflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash: "99".repeat(32) });
    expect(underlying.submit).toHaveBeenCalledOnce();
  });

  it("fails closed on a non-capacity direct preflight error", async () => {
    const underlying = base({
      preflightFailure: new Error("validator identity changed"),
    });
    const publication = prerequisite();
    const adapter = withProductionProofChunkPrerequisiteV1({
      category: "invalidRange",
      base: underlying,
      prerequisite: publication,
    });
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("validator identity changed");
    expect(publication.capture).not.toHaveBeenCalled();
  });

  it("allows the exact base step only after authenticated publication", async () => {
    const underlying = base();
    const adapter = withProductionProofChunkPrerequisiteV1({
      category: "invalidRange",
      base: underlying,
      prerequisite: prerequisite({ satisfied: true }),
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    await adapter.preflight({ ...context, action: baseAction });
    expect(underlying.preflight).toHaveBeenCalledOnce();
  });

  it("reconciles a journaled publication through a fresh adapter process", async () => {
    const reconcile = vi.fn(async () => ({
      kind: "confirmed" as const,
      txHash,
    }));
    const publication = prerequisite({ reconcile });
    const original = withProductionProofChunkPrerequisiteV1({
      category: "invalidRange",
      base: base({
        preflightFailure: new Error(
          "Max transaction size of 16384 exceeded. Found: 16385",
        ),
      }),
      prerequisite: publication,
    });
    const preflight = await original.preflight({
      ...context,
      action: publicationAction,
    });
    const fresh = withProductionProofChunkPrerequisiteV1({
      category: "invalidRange",
      base: base(),
      prerequisite: publication,
    });
    await expect(
      fresh.reconcile({
        ...context,
        action: publicationAction,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    expect(reconcile).toHaveBeenCalledWith({
      headerHash,
      action: publicationAction,
      artifact: context.artifact,
      txHash,
      durableRecovery: expect.objectContaining({
        proofChunkPublication: expect.any(Object),
      }),
    });
  });

  it("rejects duplicate or substituted publication captures", async () => {
    const publication = prerequisite();
    const adapter = withProductionProofChunkPrerequisiteV1({
      category: "invalidRange",
      base: base({
        preflightFailure: new Error(
          "Max transaction size of 16384 exceeded. Found: 16385",
        ),
      }),
      prerequisite: publication,
    });
    await adapter.preflight({ ...context, action: publicationAction });
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("outstanding captured body");
    await expect(
      adapter.preflight({
        ...context,
        action: {
          ...publicationAction,
          input: {
            ...publicationAction.input,
            proofCborSha256: "ff".repeat(32),
          },
        },
      }),
    ).rejects.toThrow("differs from the current requirement");
    expect(publication.capture).toHaveBeenCalledOnce();
  });

  it("keeps noReferenceInput direct unless its exact body exceeds the release max", async () => {
    const proofCbor = Data.to([{ Branch: { skip: 0n, neighbors: "" } }], Proof);
    const [datumCbor] = splitProofIntoChunkDatums(proofCbor);
    if (datumCbor === undefined) throw new Error("fixture proof did not chunk");
    const preexistingChunk = {
      txHash: "cd".repeat(32),
      outputIndex: 0,
      address: "addr_test1_no_reference_input",
      assets: { lovelace: 2_000_000n },
      datum: datumCbor,
    } as UTxO;
    const lucid = {
      utxosAt: vi.fn(async () => [preexistingChunk]),
    } as unknown as LucidEvolution;
    const directAction: FraudProofWorkflowActionV1 = Object.freeze({
      actionId: `step_03:${"ab".repeat(32)}#0`,
      input: Object.freeze({
        schemaVersion: "midgard-production-linear-family-action-v1",
        category: "noReferenceInput",
        stage: "step_03",
        ordinal: 3,
        threadOutRef: `${"ab".repeat(32)}#0`,
      }),
    });
    const routeAction: FraudProofWorkflowActionV1 = Object.freeze({
      actionId: `publish-proof-chunks:${directAction.actionId}:${"55".repeat(32)}`,
      input: Object.freeze({
        schemaVersion: PRODUCTION_PROOF_CHUNK_PREREQUISITE_V1,
        category: "noReferenceInput",
        stage: "direct_or_publish_proof",
        forAction: directAction,
        proofCborSha256: "55".repeat(32),
        chunkDatumSha256s: ["66".repeat(32)],
      }),
    });
    const noReferenceContext = {
      ...context,
      identity: {
        ...identity,
        category: "noReferenceInput" as const,
      },
    };
    const observedChunkCounts: number[] = [];
    const makeBase = (failure?: Error): FraudProofFamilyWorkflowAdapterV1 => ({
      ...base(),
      category: "noReferenceInput",
      observe: vi.fn(async () => ({
        kind: "action_required" as const,
        action: directAction,
      })),
      preflight: vi.fn(async ({ action }) => {
        const chunks = await resolveDirectFirstProofChunksV1({
          action,
          lucid,
          address: preexistingChunk.address,
          proofCbor,
        });
        observedChunkCounts.push(chunks.length);
        if (failure !== undefined) throw failure;
        return {
          actionId: directAction.actionId,
          txHash: "99".repeat(32),
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: "no-reference-input-local-uplc",
          },
          referenceScripts: [],
        };
      }),
    });
    const makePrerequisite = (
      satisfied = false,
    ): ProductionProofChunkPrerequisitePortV1<"noReferenceInput"> => ({
      ...prerequisite(),
      category: "noReferenceInput",
      inspect: vi.fn(async () =>
        satisfied
          ? { kind: "satisfied" as const }
          : { kind: "required" as const, action: routeAction },
      ),
    });

    const fittingPort = makePrerequisite();
    const fitting = withProductionProofChunkPrerequisiteV1({
      category: "noReferenceInput",
      base: makeBase(),
      prerequisite: fittingPort,
    });
    await expect(
      fitting.preflight({
        ...noReferenceContext,
        action: routeAction,
      }),
    ).resolves.toMatchObject({
      txHash: "99".repeat(32),
      durableRecovery: { proofCarriage: { route: "direct" } },
    });
    expect(fittingPort.capture).not.toHaveBeenCalled();
    expect(observedChunkCounts).toEqual([0]);

    const maximumPort = makePrerequisite();
    const maximum = withProductionProofChunkPrerequisiteV1({
      category: "noReferenceInput",
      base: makeBase(
        new Error("Max transaction size of 16384 exceeded. Found: 16385"),
      ),
      prerequisite: maximumPort,
    });
    await expect(
      maximum.preflight({
        ...noReferenceContext,
        action: routeAction,
      }),
    ).resolves.toMatchObject({
      txHash,
      durableRecovery: {
        proofCarriage: {
          route: "publication",
          directCapacityFailure: {
            maximumTransactionBytes: 16_384,
            actualTransactionBytes: 16_385,
          },
        },
      },
    });
    expect(maximumPort.capture).toHaveBeenCalledOnce();
    expect(observedChunkCounts).toEqual([0, 0]);

    const published = withProductionProofChunkPrerequisiteV1({
      category: "noReferenceInput",
      base: makeBase(),
      prerequisite: makePrerequisite(true),
    });
    await expect(
      published.preflight({
        ...noReferenceContext,
        action: directAction,
      }),
    ).resolves.toMatchObject({ txHash: "99".repeat(32) });
    expect(observedChunkCounts).toEqual([0, 0, 1]);
  });
});
