/**
 * networkId over an accepted forced transaction — complete Lucid lifecycle.
 *
 * A `ForcedTxValid` leaf's body and outputs enter the ledger exactly as an
 * accepted normal transaction's do, so its foreign-network claims are
 * adjudicated by the same `TransactionNetwork` / `OutputNetwork` step-02 arms.
 * Every thread starts from the generic computation-thread `Init` and walks the
 * real applied reference scripts: step 01 (forced dispatch) → forced door (leaf
 * binding under the forced root key) → step 02 (permanent mint) → state-queue
 * removal. Both polarities: a real foreign network convicts; a claim against
 * an honest commitment binds at the door and is refused at the step-02
 * network predicate.
 */
import "@al-ft/midgard-sdk";
import "vitest";
import "../src/network-id/index.js";
import "./network-id-wrongful-rejection-lifecycle.commit-forced-block.js";
import "./network-id-wrongful-rejection-lifecycle.make-stages.js";

import { forcedVerdictSubject, type NetworkIdFault } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  type NetworkIdFaultClaim,
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdAcceptedForced,
} from "../src/network-id/index.js";
import { runAcceptedForcedAdapter } from "./network-id-accepted-forced-lifecycle.workflow.js";
import {
  commitForcedBlock,
  forcedTransactionAt,
} from "./network-id-wrongful-rejection-lifecycle.commit-forced-block.js";
import {
  makeHarness,
  makeStages,
} from "./network-id-wrongful-rejection-lifecycle.make-stages.js";

const FORCED_TX_VALID = "ForcedTxValid";

type ForcedTransaction = ReturnType<typeof forcedTransactionAt>;
type CommittedBlock = Awaited<ReturnType<typeof commitForcedBlock>>;

/** The prepared accepted-forced artifact, from the committed leaf alone. */
const preparedAcceptedFor = ({
  transaction,
  block,
  faultClaim,
  claimedBodyNetworkId = transaction.bodyNetworkId,
}: {
  readonly transaction: ForcedTransaction;
  readonly block: CommittedBlock;
  readonly faultClaim: NetworkIdFaultClaim;
  readonly claimedBodyNetworkId?: bigint;
}): PreparedNetworkIdAcceptedForced => {
  const membership = block.memberships[0]!;
  const fault: NetworkIdFault =
    faultClaim.kind === "transaction-network"
      ? "TransactionNetwork"
      : { OutputNetwork: { output_index: faultClaim.outputIndex } };
  return {
    headerHash: block.setup.headerHash,
    expectedNetworkId: 0n,
    badTxId: transaction.transactionId,
    nativeTxCompactCbor: transaction.source.compact_cbor,
    fullTransactionCbor: transaction.fullTransactionCbor,
    outputsItemCbors: transaction.outputs.map((output) =>
      output.toString("hex"),
    ),
    committedNetworkId: claimedBodyNetworkId,
    outputNetworkIds: transaction.outputNetworkIds.map(BigInt),
    faultClaim,
    fault,
    subject: forcedVerdictSubject({
      transactionId: transaction.transactionId,
      sourceKey: membership.key,
      rejectionReason: null,
    }),
    forcedSource: {
      header: block.header,
      membership,
    },
  };
};

const commitAccepted = async (
  harness: Awaited<ReturnType<typeof makeHarness>>["harness"],
  transaction: ForcedTransaction,
) =>
  commitForcedBlock(harness, [
    {
      outputIndex: 0n,
      value: {
        tx_id: transaction.transactionId,
        submitted_source: transaction.source,
        verdict: FORCED_TX_VALID,
      },
    },
  ]);

describe("networkId accepted forced transaction real lifecycle", () => {
  it.each([undefined, "00", "1800", "ff"])(
    "runs both accepted-forced claims through the real adapter and reopens each confirmed stage with datum %s",
    async (datumHex) => {
      for (const faultClaim of [
        { kind: "transaction-network" as const },
        { kind: "output-network" as const, outputIndex: 1n },
      ]) {
        const { harness, refs } = await makeHarness();
        const transaction = forcedTransactionAt({
          outputNetworkIds: [0, 1],
          bodyNetworkId: 1n,
          datumHex,
        });
        const block = await commitAccepted(harness, transaction);
        await runAcceptedForcedAdapter({
          harness,
          refs,
          prepared: preparedAcceptedFor({ transaction, block, faultClaim }),
        });
      }
    },
    600_000,
  );

  it("convicts an accepted forced transaction on a foreign network through mint and removal", async () => {
    const { harness, refs } = await makeHarness();
    const transaction = forcedTransactionAt({
      outputNetworkIds: [0, 0],
      bodyNetworkId: 1n,
    });
    const block = await commitAccepted(harness, transaction);
    const prepared = preparedAcceptedFor({
      transaction,
      block,
      faultClaim: { kind: "transaction-network" },
    });
    const stages = makeStages(harness, refs, block.setup);
    const bound = await stages.acceptedBind(
      await stages.dispatch(await stages.initialize(), prepared),
      prepared,
    );
    expect(bound.state.committed_tx_network_id).toBe(1n);
    expect(bound.state.forced_source_key).toBe(prepared.subject.source_key);
    const final = await stages.finalize(bound.nextThreadOutRef, prepared);
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
    stages.assertFit("accepted-transaction-network");
  }, 600_000);

  it("convicts an accepted forced output on a foreign network through mint and removal", async () => {
    const { harness, refs } = await makeHarness();
    const transaction = forcedTransactionAt({ outputNetworkIds: [0, 1, 0] });
    const block = await commitAccepted(harness, transaction);
    const prepared = preparedAcceptedFor({
      transaction,
      block,
      faultClaim: { kind: "output-network", outputIndex: 1n },
    });
    const stages = makeStages(harness, refs, block.setup);
    const bound = await stages.acceptedBind(
      await stages.dispatch(await stages.initialize(), prepared),
      prepared,
    );
    const opening = planNetworkIdOutputsOpening({
      prepared,
      owner: harness.proverSigner.paymentKeyHash,
    });
    expect(opening.sourceKind).toBe(1n);
    const final = await stages.finalize(
      bound.nextThreadOutRef,
      prepared,
      opening,
    );
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
    stages.assertFit("accepted-output-network");
  }, 600_000);

  it("refuses claims against an honest accepted forced transaction at the step-02 network predicate", async () => {
    const { harness, refs } = await makeHarness();
    const transaction = forcedTransactionAt({ outputNetworkIds: [0, 0] });
    const block = await commitAccepted(harness, transaction);
    const stages = makeStages(harness, refs, block.setup);

    // Off chain the bind builder refuses both honest claims outright.
    const honestBody = preparedAcceptedFor({
      transaction,
      block,
      faultClaim: { kind: "transaction-network" },
    });
    const honestOutput = preparedAcceptedFor({
      transaction,
      block,
      faultClaim: { kind: "output-network", outputIndex: 0n },
    });
    const dispatchedBody = await stages.dispatch(
      await stages.initialize(),
      honestBody,
    );
    await expect(
      stages.acceptedBind(dispatchedBody, honestBody),
    ).rejects.toThrow(/no foreign network/u);

    // On chain the door binds the honest body claim (it authenticates the
    // committed body network, not the fault), and step 02 refuses it.
    const boundBody = await stages.acceptedBind(
      dispatchedBody,
      honestBody,
      true,
    );
    expect(boundBody.state.committed_tx_network_id).toBe(0n);
    await expect(
      stages.finalize(boundBody.nextThreadOutRef, honestBody, undefined, true),
    ).rejects.toThrow(/failed script execution Spend\[\d+\]/u);
    await stages.cancel(boundBody.nextThreadOutRef, "step02");

    // A lie about the committed body network dies at the door instead.
    const lyingBody = preparedAcceptedFor({
      transaction,
      block,
      faultClaim: { kind: "transaction-network" },
      claimedBodyNetworkId: 1n,
    });
    const dispatchedLie = await stages.dispatch(
      await stages.initialize(),
      lyingBody,
    );
    await expect(stages.acceptedBind(dispatchedLie, lyingBody)).rejects.toThrow(
      /failed script execution Spend\[\d+\]/u,
    );
    await stages.cancel(dispatchedLie, "forcedStep");

    // The honest output claim binds and is refused when step 02 opens the
    // named output under the forced source.
    const boundOutput = await stages.acceptedBind(
      await stages.dispatch(await stages.initialize(), honestOutput),
      honestOutput,
      true,
    );
    await expect(
      stages.finalize(
        boundOutput.nextThreadOutRef,
        honestOutput,
        planNetworkIdOutputsOpening({
          prepared: honestOutput,
          owner: harness.proverSigner.paymentKeyHash,
        }),
      ),
    ).rejects.toThrow(/failed script execution Spend\[\d+\]/u);
    await stages.cancel(boundOutput.nextThreadOutRef, "step02");
    stages.assertFit("accepted-honest");
  }, 600_000);
});
