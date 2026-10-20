import { createHash } from "node:crypto";

import {
  beginWorkflowFundingReservationAction,
  type FraudProofWorkflowJournalStore,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import {
  type setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { signFundingRecoveryFixtureBody } from "../support/fault-proof-funding-fixture.sign-body.js";

export type Fixture = Awaited<ReturnType<typeof setupFundingRecoveryFixture>>;

export const fundingOf = (fixture: Fixture) =>
  fixture.plan.inputs.find(({ role }) => role === "funding")!;

/** A signed replacement spending `outRef` with `collateral`, as a builder
 * would produce it for the current action. `protocolInputs` are further
 * ordinary inputs the reservation does not own, such as a proof thread. */
export const signReplacement = (
  outRef: string,
  lovelace: bigint,
  ttl: bigint,
  collateral: readonly string[] = [],
  protocolInputs: readonly string[] = [],
) => {
  const inputs = CML.TransactionInputList.new();
  for (const value of [outRef, ...protocolInputs]) {
    const [hash, index] = value.split("#");
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(hash!),
        BigInt(index!),
      ),
    );
  }
  const outputs = CML.TransactionOutputList.new();
  const remaining = lovelace - 1_000_000n;
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(remaining),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
  body.set_ttl(ttl);
  if (collateral.length !== 0) {
    const list = CML.TransactionInputList.new();
    for (const value of collateral) {
      const [txHash, outputIndex] = value.split("#");
      list.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(outputIndex!),
        ),
      );
    }
    body.set_collateral_inputs(list);
  }
  const signed = signFundingRecoveryFixtureBody(body);
  return {
    ...signed,
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(body.to_cbor_hex(), "hex"))
      .digest("hex"),
    consumedOutRefs: [outRef],
    producedInputs: [
      {
        outRef: `${signed.transactionHash}#0`,
        role: "funding" as const,
        lovelace: remaining.toString(),
        assets: [],
      },
    ],
  };
};

/** Records a replacement for the init action exactly as a submission does:
 * the store handoff, then preflight, intent, submitted and pending. */
export const recordReplacement = async (
  fixture: Fixture,
  journal: FraudProofWorkflowJournalStore,
  replacement: ReturnType<typeof signReplacement>,
  attempt: number,
  { actionId, actionKind } = { actionId: "init", actionKind: "proof.init" },
) => {
  const entries = await journal.load(fixture.initial.workflowId);
  const [record] = await fixture.records();
  const handoff = {
    ...fixture.handoff,
    expectedJournalSequence: entries.length,
    preflight: {
      ...fixture.handoff.preflight,
      actionId,
      txHash: replacement.transactionHash,
    },
    submissionIntent: {
      ...fixture.handoff.submissionIntent,
      actionId,
      actionInput: { actionKind },
      attempt,
      txHash: replacement.transactionHash,
    },
  };
  await fixture.store.prepareTransition({
    handoff,
    plan: fixture.plan,
    expectedRevision: record!.revision,
    actionKind,
    ...replacement,
  });
  await fixture.append(handoff.preflight);
  await fixture.append(handoff.submissionIntent);
  await fixture.append({
    kind: "submitted",
    actionId,
    attempt,
    txHash: replacement.transactionHash,
  });
  await fixture.append({
    kind: "reconciled",
    actionId,
    outcome: "pending",
    txHash: replacement.transactionHash,
  });
};

/** The fixture's attempt expires at the tip: its funding input is unspent and
 * the reader reports absence without retirement. */
export const supersedeAtTip = async (fixture: Fixture) => {
  fixture.useUnspentPendingInputs();
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  const journal = await fixture.recover();
  await fixture.run(journal);
  return journal;
};

export const replaceAfterExpiry = async (fixture: Fixture) => {
  const funding = fundingOf(fixture);
  const journal = await supersedeAtTip(fixture);
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const collateral = (await fixture.records())[0]!.activeInputs
    .filter(({ role }) => role === "collateral")
    .map(({ outRef }) => outRef);
  const replacement = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    300n,
    collateral,
  );
  await recordReplacement(fixture, journal, replacement, 2);
  // The process restarts with the replacement in flight.
  return { journal: await fixture.recover(), replacement };
};
