import { createHash } from "node:crypto";
import { mkdtemp } from "node:fs/promises";
import { join } from "node:path";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  journalJsonDigest,
  normalizeJournalJson,
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingCompletionHandoff,
  type WorkflowFundingSubmissionHandoff,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import {
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import { unsafeOpenWatcherSqliteProverFundingReservationStoreForTest } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import { fundingTerminal } from "./funding-handoff-fixture.js";

export const temporaryDirectories: string[] = [];

const fundingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x55));

const fundingKeyHash = fundingKey.to_public().hash().to_hex();

export const walletAddress = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x60]), Buffer.from(fundingKeyHash, "hex")]),
).to_bech32();

export const signedTransition = ({
  inputHash = "11".repeat(32),
  outputLovelace = 99_000_000n,
  nonCanonicalBody = false,
  feeLovelace = 1_000_000n,
  collateralHash,
  validityUpperBound,
}: {
  readonly inputHash?: string;
  readonly outputLovelace?: bigint;
  readonly nonCanonicalBody?: boolean;
  readonly feeLovelace?: bigint;
  readonly collateralHash?: string;
  readonly validityUpperBound?: bigint;
} = {}) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(inputHash), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(outputLovelace),
    ),
  );
  const canonicalBody = CML.TransactionBody.new(inputs, outputs, feeLovelace);
  if (collateralHash !== undefined) {
    const collateral = CML.TransactionInputList.new();
    collateral.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(collateralHash),
        0n,
      ),
    );
    canonicalBody.set_collateral_inputs(collateral);
  }
  if (validityUpperBound !== undefined)
    canonicalBody.set_ttl(validityUpperBound);
  const body = nonCanonicalBody
    ? CML.TransactionBody.from_cbor_hex(
        "bf" + canonicalBody.to_cbor_hex().slice(2) + "ff",
      )
    : canonicalBody;
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      fundingKey.to_public(),
      fundingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const transaction = CML.Transaction.new(body, witnesses, true, undefined);
  const transactionHash = CML.hash_transaction(body).to_hex();
  return Object.freeze({
    signedTransactionCborHex: transaction.to_cbor_hex(),
    transactionHash,
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(body.to_cbor_hex(), "hex"))
      .digest("hex"),
    producedInputs: Object.freeze([
      Object.freeze({
        outRef: `${transactionHash}#0`,
        role: "funding" as const,
        lovelace: outputLovelace.toString(),
        assets: Object.freeze([]),
      }),
    ]),
  });
};

export const openStore = async () => {
  const directory = await mkdtemp(
    join(process.cwd(), ".watcher-funding-reservation-test-"),
  );
  temporaryDirectories.push(directory);
  const path = join(directory, "watcher.sqlite");
  return {
    path,
    runtime: await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
      { path },
      () => undefined,
    ),
  };
};

export const plan = (
  reservationByte: string,
  decisionByte: string,
  fundingOutRef = `${"11".repeat(32)}#0`,
): WatcherProverFundingReservationPlan =>
  Object.freeze({
    schemaVersion:
      "midgard-watcher-production-prover-funding-reservation-plan-v1",
    deploymentFingerprint: "22".repeat(32),
    decisionDigest: decisionByte.repeat(32),
    policyDigest: "33".repeat(32),
    reservationBasisDigest: "44".repeat(32),
    fundingPaymentKeyHash: fundingKeyHash,
    walletAddress,
    inputs: Object.freeze([
      Object.freeze({
        outRef: fundingOutRef,
        role: "funding" as const,
        lovelace: "100000000",
        assets: Object.freeze([]),
      }),
      Object.freeze({
        outRef: `${"12".repeat(32)}#0`,
        role: "collateral" as const,
        lovelace: "5000000",
        assets: Object.freeze([]),
      }),
    ]),
    fundingLovelace: "100000000",
    collateralLovelace: "5000000",
    assets: Object.freeze([]),
    reservationId: reservationByte.repeat(32),
  });

const handoffIdentity = (plan: WatcherProverFundingReservationPlan) => {
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: plan.deploymentFingerprint,
    decisionDigest: plan.decisionDigest,
    category: "doubleSpend" as const,
    target: {
      kind: "state_queue_header" as const,
      headerHash: "55".repeat(28),
    },
  };
  return {
    identity,
    workflowId: computeFraudProofWorkflowId(identity),
    preparedArtifactDigest: "66".repeat(32),
    expectedJournalSequence: 2,
  };
};

export const submissionHandoff = (
  input: Omit<
    Parameters<WatcherProverFundingReservationStore["prepareTransition"]>[0],
    "handoff"
  >,
): WorkflowFundingSubmissionHandoff => ({
  ...handoffIdentity(input.plan),
  preflight: {
    kind: "preflight_passed",
    actionId: input.actionKind,
    txHash: input.transactionHash,
    localEvaluator: "test-uplc",
    referenceScripts: [
      {
        role: "init",
        outRef: `${"77".repeat(32)}#0`,
        scriptHash: "88".repeat(28),
      },
    ],
  },
  submissionIntent: {
    kind: "submission_intent",
    actionId: input.actionKind,
    actionInput: { actionKind: input.actionKind },
    txHash: input.transactionHash,
    attempt: 1,
  },
});

export const prepareTransition = (
  store: WatcherProverFundingReservationStore,
  input: Omit<
    Parameters<WatcherProverFundingReservationStore["prepareTransition"]>[0],
    "handoff"
  >,
) => store.prepareTransition({ ...input, handoff: submissionHandoff(input) });

export const abandonmentHandoff = (
  plan: WatcherProverFundingReservationPlan,
  transactionHash: string,
  actionKind: string,
): WorkflowFundingAbandonmentHandoff => ({
  ...handoffIdentity(plan),
  submissionIntent: {
    kind: "submission_intent",
    actionId: actionKind,
    actionInput: { actionKind },
    attempt: 1,
    txHash: transactionHash,
  },
  reconciliation: {
    kind: "reconciled",
    actionId: actionKind,
    outcome: "not_found",
    txHash: transactionHash,
  },
});

export const completionHandoff = (
  plan: WatcherProverFundingReservationPlan,
): WorkflowFundingCompletionHandoff => {
  const terminal = fundingTerminal(
    "55".repeat(28),
    "aa".repeat(32),
    "bb".repeat(32),
  );
  return {
    ...handoffIdentity(plan),
    completion: {
      kind: "completed",
      terminal,
      terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
    },
  };
};
