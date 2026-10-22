import {
  createMissingSignatureWorkflowAdapter,
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowReconcileResult,
  MISSING_SIGNATURE_ACTION,
  MISSING_SIGNATURE_TRANSACTION_PORT,
  PROOF_CARRIAGE_RECOVERY,
  PROOF_CHUNK_PREREQUISITE,
  type StateQueueMutationLeaseCoordinator,
  withProofChunkPrerequisite,
} from "@al-ft/midgard-fault-proofs";
import { createValueConservationAdapter } from "@al-ft/midgard-fault-proofs/test-support/value-conservation-adapter";
import { afterEach, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { type Fixture } from "./superseded-attempt-replacement.js";
import {
  chainHolds,
  confirmReplacement,
  runMinutesApart,
  threadFamilyChain,
  walletAfter,
} from "./superseded-attempt-rollback.chain.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

type Attempts = Readonly<{ landed: string; replacement: string }>;

const unused = async (): Promise<never> => {
  throw new Error("not part of a reconcile");
};

/** The family answers for `replacement` with `answer`, and reads every
 * other attempt from the chain. */
const answering =
  (answer: () => FraudProofWorkflowReconcileResult) =>
  (fixture: Fixture, { landed, replacement }: Attempts) => {
    chainHolds(fixture, [landed]);
    vi.mocked(fixture.adapter.reconcile).mockImplementation(
      async ({ txHash }) =>
        txHash === replacement ? answer() : await fixture.chain(txHash!),
    );
  };

/** Value conservation reads an attempt as confirmed only when its exact
 * checkpoint is on chain, and otherwise as still pending. */
const valueConservation = (fixture: Fixture, { landed }: Attempts) => {
  chainHolds(fixture, [landed]);
  const family = createValueConservationAdapter({
    prepare: unused,
    current: unused,
    capture: unused,
    confirmed: async ({ txHash }: { readonly txHash: string }) =>
      txHash === landed,
    stateQueueMutationLeaseCoordinator: {
      acquire: unused,
    } as unknown as StateQueueMutationLeaseCoordinator,
  });
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async (input) => await family.reconcile(input),
  );
};

const queueOutRef = `${"10".repeat(32)}#0`;

/** Missing signature reads the attempt whose output is its computation
 * thread's first step as confirmed, and any other attempt of its start as a
 * chain state that changed without it. */
const missingSignature = (fixture: Fixture, { landed }: Attempts) => {
  chainHolds(fixture, [landed]);
  const family = createMissingSignatureWorkflowAdapter({
    l1: {
      portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
      category: "missingSignature",
      publications: {} as never,
      observeHeader: unused,
      transactionConfirmed: async ({ txHash }) => txHash === landed,
      observe: async () => ({
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId: "local-kupmios/kupo+ogmios",
          grade: "security",
        },
        stage: {
          kind: "step",
          step: 1,
          threadOutRef: `${landed}#0`,
          stateQueueBlockOutRef: queueOutRef,
        },
      }),
    },
    transactions: {
      portVersion: MISSING_SIGNATURE_TRANSACTION_PORT,
      category: "missingSignature",
      prepare: unused,
      capture: unused,
    },
    stateQueueMutationLeaseCoordinator: {
      acquire: unused,
    } as unknown as StateQueueMutationLeaseCoordinator,
  });
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async ({ durableRecovery: _, ...input }) =>
      await family.reconcile({
        ...input,
        identity: {
          ...input.identity,
          category: "missingSignature",
          target: { kind: "state_queue_header", headerHash: "ab".repeat(28) },
        },
        action: {
          actionId: `init:${queueOutRef}`,
          input: {
            schemaVersion: MISSING_SIGNATURE_ACTION,
            category: "missingSignature",
            stage: "init",
            stateQueueBlockOutRef: queueOutRef,
          },
        },
      }),
  );
};

/** A proof-chunk family whose attempt carried its proof directly: the
 * carriage reads it through its base family, here the double-spend thread. */
const directProofCarriage = (fixture: Fixture, { landed }: Attempts) => {
  const base = threadFamilyChain(fixture, {
    onChain: [landed],
    unreadable: [],
  });
  const carriage = withProofChunkPrerequisite({
    category: "doubleSpend",
    base,
    prerequisite: {
      portVersion: PROOF_CHUNK_PREREQUISITE,
      category: "doubleSpend",
      classifyDirectCapacityFailure: () => {
        throw new Error("not part of a reconcile");
      },
      inspect: unused,
      capture: unused,
      reconcile: unused,
    },
  });
  const proofCborSha256 = "5a".repeat(32);
  vi.mocked(fixture.adapter.reconcile).mockImplementation(async (input) => {
    const baseAction = {
      actionId: input.action.actionId,
      input: { ...input.action.input, stage: "init" },
    };
    return await carriage.reconcile({
      ...input,
      action: {
        actionId: input.action.actionId,
        input: {
          schemaVersion: PROOF_CHUNK_PREREQUISITE,
          category: "doubleSpend",
          stage: "direct_or_publish_proof",
          forAction: baseAction,
          proofCborSha256,
          chunkDatumSha256s: [proofCborSha256],
        },
      },
      durableRecovery: {
        proofCarriage: {
          schemaVersion: PROOF_CARRIAGE_RECOVERY,
          route: "direct",
          baseAction,
          proofCborSha256,
        },
      },
    });
  });
};

/**
 * Owner ruling (whichever lands wins): whether a rolled-back result is gone
 * is a question about its own signed bytes, so the workflow answers it the
 * same way for every family, whatever the family's own read of that attempt
 * says once its stage has moved on with the attempt that landed instead.
 */
const families: readonly (readonly [
  string,
  (fixture: Fixture, attempts: Attempts) => unknown,
])[] = [
  [
    "a family that reads its computation thread",
    (fixture, { landed }) =>
      threadFamilyChain(fixture, { onChain: [landed], unreadable: [] }),
  ],
  ["a value-conservation family", valueConservation],
  ["a missing-signature family", missingSignature],
  ["a proof-chunk family on its direct route", directProofCarriage],
  [
    "a family that reads the replacement as still pending",
    answering(() => ({ kind: "pending" })),
  ],
  [
    "a family that reads the replacement as confirmed",
    answering(() => ({ kind: "confirmed", txHash: "00".repeat(32) })),
  ],
  [
    "a family that cannot resolve the replacement",
    answering(() => ({ kind: "unknown", reason: "stage moved on" })),
  ],
  [
    "a family that reads the replacement as a conflict",
    answering(() => ({ kind: "conflict", reason: "stage moved on" })),
  ],
  [
    "a family whose read of the replacement fails",
    answering(() => {
      throw new Error("family read failed");
    }),
  ],
];

it.each(families)(
  "takes the expired attempt as the result when a rollback drops its confirmed replacement and lands it instead, in %s",
  async (_, install) => {
    const fixture = await setupFundingRecoveryFixture(
      false,
      false,
      false,
      true,
    );
    const { journal, replacement } = await confirmReplacement(fixture);
    const before = (await journal.load(fixture.initial.workflowId)).length;
    walletAfter(fixture, fixture.transactionHash);
    install(fixture, {
      landed: fixture.transactionHash,
      replacement: replacement.transactionHash,
    });
    const read = vi.mocked(fixture.adapter.reconcile).getMockImplementation()!;
    const asked: (string | undefined)[] = [];
    vi.mocked(fixture.adapter.reconcile).mockImplementation(
      async (
        input: Parameters<FraudProofFamilyWorkflowAdapter["reconcile"]>[0],
      ) => {
        asked.push(input.txHash);
        return await read(input);
      },
    );
    const events = (await runMinutesApart(fixture, journal, 4)).slice(before);
    expect(events.at(-1)).toEqual({
      kind: "confirmed",
      actionId: "init",
      txHash: fixture.transactionHash,
    });
    expect(events).toContainEqual({
      kind: "reobserved",
      actionId: "init",
      txHash: replacement.transactionHash,
    });
    expect(events.some(({ kind }) => kind === "stalled")).toBe(false);
    // The family is never asked whether the rolled-back result is gone.
    expect(asked).not.toContain(replacement.transactionHash);
    const [record] = await fixture.records();
    expect(record!.state).toBe("active");
    expect(record!.pendingTransition).toBeNull();
    expect(await fixture.store.readReservedOutRefs({})).toContain(
      `${fixture.transactionHash}#0`,
    );
  },
);
