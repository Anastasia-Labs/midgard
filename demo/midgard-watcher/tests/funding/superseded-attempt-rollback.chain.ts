import {
  computeFraudProofRawL1PointId,
  createDoubleSpendConstrainedWorkflowAdapter,
  type DoubleSpendConstrainedWorkflowAdapterConfig,
  type FraudProofWorkflowJournalStore,
  type SignedTransactionRecoveryObservation,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { expect, vi } from "vitest";

import { walletAddress } from "../support/fault-proof-funding-fixture.js";
import {
  type Fixture,
  fundingOf,
  replaceAfterExpiry,
} from "./superseded-attempt-replacement.js";

/** The wallet as the chain shows it once `landed` spent the fixture's
 * funding input: its change is the wallet's, and no other attempt's is. */
export const walletAfter = (fixture: Fixture, landed: string) => {
  const funding = fundingOf(fixture);
  const coins = fixture.walletUtxos.filter(
    ({ txHash, outputIndex }) =>
      txHash === "99".repeat(32) &&
      `${txHash}#${outputIndex}` !== funding.outRef,
  );
  fixture.walletUtxos.splice(0, fixture.walletUtxos.length, ...coins, {
    txHash: landed,
    outputIndex: 0,
    address: walletAddress,
    assets: { lovelace: BigInt(funding.lovelace) - 1_000_000n },
  });
};

/** The chain reports exactly `onChain` as included. */
export const chainHolds = (fixture: Fixture, onChain: readonly string[]) =>
  fixture.chain.mockImplementation(async (txHash) =>
    onChain.includes(txHash!)
      ? { kind: "confirmed", txHash: txHash! }
      : { kind: "not_found" },
  );

/** The replacement of the expired attempt lands and is the init result. */
export const confirmReplacement = async (fixture: Fixture) => {
  const { journal, replacement } = await replaceAfterExpiry(fixture);
  // The fixture signs the same attempts in every row: each row starts with
  // no reads of them behind it.
  for (const hash of [fixture.transactionHash, replacement.transactionHash])
    supersededAttemptReadSchedule.forget(hash);
  walletAfter(fixture, replacement.transactionHash);
  chainHolds(fixture, [replacement.transactionHash]);
  await fixture.run(journal);
  expect(
    (await journal.load(fixture.initial.workflowId)).at(-1)!.event,
  ).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: replacement.transactionHash,
  });
  return { journal, replacement };
};

/** The chain as a double-spend computation thread sees it: its init landed
 * as whichever attempt is on chain, and the thread moved on with it. The
 * family's own reconcile answers the family's reads; signed bytes, read by
 * the family and the workflow alike, are read against `onChain`, except
 * while `unreadable` holds them. Returns the family's adapter. */
export const threadFamilyChain = (
  fixture: Fixture,
  chain: { onChain: readonly string[]; unreadable: readonly string[] },
) => {
  const point = (blockNo: number) => {
    const value = {
      blockNo: String(blockNo),
      slot: String(blockNo * 20),
      blockHash: blockNo.toString(16).padStart(64, "0"),
    };
    return { ...value, pointId: computeFraudProofRawL1PointId(value) };
  };
  const observeSignedTransaction = async (input: {
    transactionHash: string;
    signedTransactionCborHex: string;
  }): Promise<SignedTransactionRecoveryObservation> => {
    if (chain.unreadable.includes(input.transactionHash))
      throw new Error("chain provider unavailable");
    const included = chain.onChain.includes(input.transactionHash);
    return {
      ...input,
      status: included ? "included" : "invalidated_at_tip",
      canonicalPoint: point(100),
      releaseFinalPoint: point(10),
      ...(included ? { inclusionPoint: point(99) } : {}),
      inputs: [],
      reason: "test chain",
    };
  };
  fixture.observeSignedTransaction.mockImplementation(observeSignedTransaction);
  const family = createDoubleSpendConstrainedWorkflowAdapter({
    l1: {
      observe: async () => ({
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId: "local-node-test",
          grade: "security",
        },
        stage: {
          kind: "step_01",
          threadOutRef: `${chain.onChain[0]}#0`,
          stateQueueBlockOutRef: `${"73".repeat(32)}#0`,
        },
      }),
      transactionConfirmed: async ({ txHash }: { txHash: string }) =>
        chain.onChain.includes(txHash),
      observeSignedTransaction,
    },
  } as unknown as DoubleSpendConstrainedWorkflowAdapterConfig);
  // The fixture's init action names no stage; the family reads it as init.
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async (input) =>
      await family.reconcile({
        ...input,
        action: {
          ...input.action,
          input: { ...input.action.input, stage: "init" },
        },
      }),
  );
  return family;
};

export const runMinutesApart = async (
  fixture: Fixture,
  journal: FraudProofWorkflowJournalStore,
  runs: number,
) => {
  for (let run = 1; run <= runs; run += 1)
    await fixture.run(journal, () => new Date(Date.now() + run * 60_000));
  return (await journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );
};

export const intentsOf = (
  events: readonly { kind: string; txHash?: string }[],
  txHash: string,
) =>
  events.filter(
    (event) => event.kind === "submission_intent" && event.txHash === txHash,
  ).length;
