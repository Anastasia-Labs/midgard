import { Proof } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { splitProofIntoChunkDatums } from "../src/publish-proof-chunks.js";
import type { FraudProofWorkflowIdentity } from "../src/workflow/journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
} from "../src/workflow/orchestrator.js";
import {
  PROOF_CHUNK_PREREQUISITE,
  type ProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "../src/workflow/proof-chunk-prerequisite.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";

const txHash = "11".repeat(32);
const headerHash = "22".repeat(28);
const baseAction: FraudProofWorkflowAction = Object.freeze({
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
const publicationAction: FraudProofWorkflowAction = Object.freeze({
  actionId: `publish-proof-chunks:${baseAction.actionId}:${"55".repeat(32)}`,
  input: Object.freeze({
    schemaVersion: PROOF_CHUNK_PREREQUISITE,
    category: "invalidRange",
    stage: "direct_or_publish_proof",
    forAction: baseAction,
    proofCborSha256: "55".repeat(32),
    chunkDatumSha256s: ["66".repeat(32)],
  }),
});

const identity: FraudProofWorkflowIdentity = {
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

const signed = (): LocallyEvaluatedTransaction["signed"] =>
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
  }) as unknown as LocallyEvaluatedTransaction["signed"];

const transaction = (): LocallyEvaluatedTransaction => ({
  txHash,
  signed: signed(),
  referenceScripts: [],
});

const base = ({
  preflightFailure,
}: {
  readonly preflightFailure?: Error;
} = {}): FraudProofFamilyWorkflowAdapter => ({
  adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
  category: "invalidRange",
  safety: FRAUD_PROOF_WORKFLOW_SAFETY,
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
  readonly reconcile?: ProofChunkPrerequisitePort<"invalidRange">["reconcile"];
} = {}): ProofChunkPrerequisitePort<"invalidRange"> => ({
  portVersion: PROOF_CHUNK_PREREQUISITE,
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
    const adapter = withProofChunkPrerequisite({
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
    const adapter = withProofChunkPrerequisite({
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
    const adapter = withProofChunkPrerequisite({
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
    const adapter = withProofChunkPrerequisite({
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
    const original = withProofChunkPrerequisite({
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
    const fresh = withProofChunkPrerequisite({
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
    const adapter = withProofChunkPrerequisite({
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
    const directAction: FraudProofWorkflowAction = Object.freeze({
      actionId: `step_03:${"ab".repeat(32)}#0`,
      input: Object.freeze({
        schemaVersion: "midgard-production-linear-family-action-v1",
        category: "noReferenceInput",
        stage: "step_03",
        ordinal: 3,
        threadOutRef: `${"ab".repeat(32)}#0`,
      }),
    });
    const routeAction: FraudProofWorkflowAction = Object.freeze({
      actionId: `publish-proof-chunks:${directAction.actionId}:${"55".repeat(32)}`,
      input: Object.freeze({
        schemaVersion: PROOF_CHUNK_PREREQUISITE,
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
    const makeBase = (failure?: Error): FraudProofFamilyWorkflowAdapter => ({
      ...base(),
      category: "noReferenceInput",
      observe: vi.fn(async () => ({
        kind: "action_required" as const,
        action: directAction,
      })),
      preflight: vi.fn(async ({ action }) => {
        const chunks = await resolveDirectFirstProofChunks({
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
    ): ProofChunkPrerequisitePort<"noReferenceInput"> => ({
      ...prerequisite(),
      category: "noReferenceInput",
      inspect: vi.fn(async () =>
        satisfied
          ? { kind: "satisfied" as const }
          : { kind: "required" as const, action: routeAction },
      ),
    });

    const fittingPort = makePrerequisite();
    const fitting = withProofChunkPrerequisite({
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
    const maximum = withProofChunkPrerequisite({
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

    const published = withProofChunkPrerequisite({
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
