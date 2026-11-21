/**
 * A family reads a recorded attempt that is not on chain from its own signed
 * bytes, as the linear and cursor families do. An attempt that lands failing
 * phase 2, or that expires, can never produce its stage's successor: the
 * workflow accounts for it and moves on with a replacement that spends one of
 * its funding inputs, instead of waiting on it for ever.
 */
import type { StateQueueMutationLeaseCoordinator } from "@al-ft/midgard-fault-proofs/remove-fraudulent-block";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "@al-ft/midgard-fault-proofs/test-support/family-l1-observation";
import { createValueConservationAdapter } from "@al-ft/midgard-fault-proofs/test-support/value-conservation-adapter";
import { FraudProofL1UnavailableError } from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import {
  createMissingSignatureWorkflowAdapter,
  MISSING_SIGNATURE_TRANSACTION_PORT,
} from "@al-ft/midgard-fault-proofs/workflow/missing-signature-adapter";
import { MISSING_SIGNATURE_ACTION } from "@al-ft/midgard-fault-proofs/workflow/missing-signature-state";
import type { FraudProofWorkflowReconcileResult } from "@al-ft/midgard-fault-proofs/workflow/orchestrator";
import type { SignedWorkflowTransaction } from "@al-ft/midgard-fault-proofs/workflow/signed-transaction-reconciliation";
import { afterEach, describe, expect, it, vi } from "vitest";

import { forfeitureOf, showLanding } from "../support/collateral-landing.js";
import { setupFundingRecoveryFixture } from "../support/fault-proof-funding-fixture.setup-funding-recovery-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.sources-for.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

type Fixture = Awaited<ReturnType<typeof setupFundingRecoveryFixture>>;

const unused = async (): Promise<never> => {
  throw new Error("not part of a reconcile");
};

const queueOutRef = `${"10".repeat(32)}#0`;

/** Sends the exact recorded bytes again once authorized, noting each. */
const rebroadcastInto =
  (sent: string[]) =>
  async (
    signed: SignedWorkflowTransaction & {
      readonly authorizeResubmission: (
        input: SignedWorkflowTransaction,
      ) => Promise<void>;
    },
  ): Promise<string> => {
    await signed.authorizeResubmission(signed);
    sent.push(signed.transactionHash);
    return signed.transactionHash;
  };

/** The production missing-signature family reads the recorded attempt: its
 * block is not yet started, so the attempt's stage is unchanged, and the
 * attempt is not on chain. */
const missingSignature = (fixture: Fixture, sent: string[]) => {
  const family = createMissingSignatureWorkflowAdapter({
    l1: {
      portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
      category: "missingSignature",
      publications: {} as never,
      observeHeader: unused,
      transactionConfirmed: async () => false,
      observe: async () => ({
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId: "local-kupmios/kupo+ogmios",
          grade: "security",
        },
        stage: { kind: "not_started", stateQueueBlockOutRef: queueOutRef },
      }),
      observeSignedTransaction: fixture.observeSignedTransaction,
      rebroadcastSignedTransaction: rebroadcastInto(sent),
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

/** The value-conservation family reads the recorded attempt: its exact
 * checkpoint is not on chain. */
const valueConservation = (fixture: Fixture, sent: string[]) => {
  const family = createValueConservationAdapter({
    prepare: unused,
    current: unused,
    capture: unused,
    confirmed: async () => false,
    signedRecovery: {
      observeSignedTransaction: fixture.observeSignedTransaction,
      rebroadcastSignedTransaction: rebroadcastInto(sent),
    },
    stateQueueMutationLeaseCoordinator: {
      acquire: unused,
    } as unknown as StateQueueMutationLeaseCoordinator,
  });
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async (input) => await family.reconcile(input),
  );
};

const families = [
  ["a missing-signature family", missingSignature],
  ["a value-conservation family", valueConservation],
] as const;

const rows: readonly (readonly [
  string,
  FraudProofWorkflowReconcileResult,
  boolean,
])[] = [
  ["lands failing phase 2", { kind: "not_found", landedInvalid: true }, true],
  ["expires", { kind: "not_found" }, false],
];

describe.each(families)("in %s", (_family, install) => {
  it.each(rows)(
    "supersedes an attempt that %s",
    async (_, chainRead, landedInvalid) => {
      const fixture = await setupFundingRecoveryFixture(
        false,
        true,
        false,
        20n,
        44,
        3,
        100n,
        true,
      );
      const sent: string[] = [];
      install(fixture, sent);
      // The attempt never landed valid, so its output does not exist and its
      // funding input is live; a failing landing took only its collateral.
      fixture.useUnspentPendingInputs();
      if (landedInvalid) showLanding(fixture, true);
      fixture.chain.mockResolvedValue(chainRead);
      const replacement = {
        actionId: "init",
        input: { actionKind: "proof.init" },
      };
      vi.mocked(fixture.adapter.observe).mockResolvedValue({
        kind: "action_required",
        action: replacement,
      });
      // The test ends at the replacement's preflight, once it is funded.
      const ended = new FraudProofL1UnavailableError("test ends at preflight");
      vi.mocked(fixture.adapter.preflight).mockRejectedValue(ended);

      await expect(fixture.run(await fixture.recover())).rejects.toBe(ended);
      expect(sent).toEqual([]);
      const events = (await fixture.journal.load(fixture.initial.workflowId))
        .slice(fixture.originalEntries.length)
        .map(({ event }) => event);
      expect(events).toEqual([
        ...(landedInvalid ? [forfeitureOf(fixture)] : []),
        {
          kind: "reconciled",
          actionId: "init",
          outcome: "not_found",
          txHash: fixture.transactionHash,
        },
      ]);
      expect(fixture.adapter.preflight).toHaveBeenCalledTimes(1);
      expect(
        vi.mocked(fixture.adapter.preflight).mock.calls[0]![0].action,
      ).toEqual(replacement);
      // Whichever lands wins: the replacement spends one of the attempt's
      // inputs.
      const funded = (await fixture.records())[0]!.activeInputs.map(
        ({ outRef }) => outRef,
      );
      expect(
        funded.filter((outRef) =>
          [
            fixture.fundingInput.outRef,
            fixture.collateralInput.outRef,
          ].includes(outRef),
        ),
      ).not.toEqual([]);
    },
  );
});
