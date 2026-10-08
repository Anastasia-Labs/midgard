import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { isDeadStatus } from "@al-ft/midgard-l1-follower";
import { depth as depthOf, isSafe } from "@al-ft/midgard-l1-follower/heads";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  type LucidEvolution,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { EventSettlementProofError } from "../commands/event-settlement-proof.js";
import * as Journal from "../database/settlement.js";
import { synchronizePublicationIndexerPoint } from "../transactions/reference-publication-provider.js";
import { NodeConfig, type NodeConfigDep } from "./config.js";
import { readIntentStatus } from "./intent-journal.js";
import { ContractDeploymentIdentity } from "./midgard-contracts.js";
import {
  describeSettlementError,
  isSettlementInputsSpentRejection,
  settlementCall,
  settlementCheck,
} from "./settlement-call.js";
import { readSettlementOutputEvidence } from "./settlement-output.js";

export type SettlementHealth = {
  observedAt: number;
  state: "starting" | "running" | "waiting" | "error";
  detail: string;
  /** Set by the node while worker runs keep dying: how many in a row, since
   * when, and the latest failure. */
  workerFailures?: { count: number; since: number; last: string };
  /** Set by the worker on a report made at the end of a tick that ran
   * without failing. Only such a report proves a worker run healthy, so only
   * it clears the node's failure streak; the node strips it before
   * publishing the health. */
  tickCompleted?: boolean;
};

export const settlementWalletAddress = (config: NodeConfigDep): string => {
  const seed = config.L1_SETTLEMENT_SEED_PHRASE;
  if (!seed)
    throw new Error(
      "Automatic settlement requires a funded L1_SETTLEMENT_SEED_PHRASE (a distinct fee/collateral wallet)",
    );
  const address = walletFromSeed(seed, { network: config.NETWORK }).address;
  for (const other of [
    config.L1_OPERATOR_SEED_PHRASE,
    config.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
    config.L1_REFERENCE_SCRIPT_SEED_PHRASE,
  ]) {
    if (walletFromSeed(other, { network: config.NETWORK }).address === address)
      throw new Error(
        "Settlement fee wallet must be distinct from commitment, merge and reference-script wallets",
      );
  }
  return address;
};

export const inspectSettlementAttempt = (
  attempt: Journal.SettlementAttempt,
) => {
  const tx = CML.Transaction.from_cbor_hex(attempt.signed_cbor);
  const body = tx.body();
  if (CML.hash_transaction(body).to_hex() !== attempt.tx_hash)
    throw new Error("Settlement journal transaction hash mismatch");
  const ttl = body.ttl();
  if (ttl === undefined || ttl > BigInt(Number.MAX_SAFE_INTEGER))
    throw new Error(
      "Settlement transaction needs a finite safe validity upper bound",
    );
  for (const index of attempt.required_outputs)
    if (
      !Number.isSafeInteger(index) ||
      index < 0 ||
      index >= body.outputs().len()
    )
      throw new Error("Settlement journal output index is invalid");
  const inputs = spendingInputs(attempt).map(
    (out) => `${out.txHash}#${out.outputIndex}`,
  );
  if (
    attempt.fee_inputs.length === 0 ||
    !attempt.fee_inputs.every((out) => inputs.includes(out))
  )
    throw new Error(
      "Settlement fee reservation does not match its transaction inputs",
    );
  return { validToSlot: Number(ttl), outputCount: body.outputs().len() };
};

export const settlementNextPhase = (
  phase: Journal.SettlementPhase,
): Journal.SettlementJob["phase"] => {
  switch (phase) {
    case "absorb":
    case "conclude":
      return "complete";
    case "initialize":
    case "fund":
      return "fund";
  }
};

export const settlementWaitUntil = (
  error: unknown,
  now: number,
): number | undefined => {
  let current = error;
  for (
    let depth = 0;
    depth < 12 && current !== null && typeof current === "object";
    depth++
  ) {
    if (current instanceof SDK.HistoryRetirementProtectedError)
      return Math.max(now + 5_000, Number(current.protectedUntilMs) + 1_000);
    if (
      current instanceof EventSettlementProofError &&
      current.message === "Expected exactly one settlement UTxO for header hash"
    )
      return now + 10_000;
    current = "cause" in current ? current.cause : undefined;
  }
  return undefined;
};

/** No timeout, not_found, or rejected submission by itself authorizes rebuilding. */
export const canExpireSettlementAttempt = (input: {
  status: string;
  validToSlot: number;
  beforeSlot: number;
  afterSlot: number;
  beforeHash: string;
  afterHash: string;
  allInputsUnspent: boolean;
}) =>
  input.status === "not_found" &&
  input.beforeSlot === input.afterSlot &&
  input.beforeHash === input.afterHash &&
  input.beforeSlot >= input.validToSlot &&
  input.allInputsUnspent;

export const exactStatus = (
  lucid: Pick<LucidEvolution, "transactionStatus">,
  hash: string,
) =>
  settlementCall(`transactionStatus ${hash}`, async () => {
    const status = await lucid.transactionStatus(hash);
    if (
      status.txHash !== hash ||
      (status.status === "confirmed" && status.confirmation.txHash !== hash)
    )
      throw new Error("Settlement provider returned a different transaction");
    return status;
  });

const spendingInputs = (attempt: Journal.SettlementAttempt) => {
  const inputs = CML.Transaction.from_cbor_hex(attempt.signed_cbor)
    .body()
    .inputs();
  return Array.from({ length: inputs.len() }, (_, i) => ({
    txHash: inputs.get(i).transaction_id().to_hex(),
    outputIndex: Number(inputs.get(i).index()),
  }));
};

export const reconcileAttempt = (
  owner: Journal.SettlementOwner,
  attempt: Journal.SettlementAttempt,
  lucid: LucidEvolution,
  generation: string,
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const identity = yield* ContractDeploymentIdentity;
    const { validToSlot } = yield* settlementCheck(
      "inspect settlement attempt",
      () => inspectSettlementAttempt(attempt),
    );
    const status = yield* exactStatus(lucid, attempt.tx_hash);
    if (status.status === "confirmed") {
      const depth =
        identity.l1Finality?.confirmationDepth ??
        identity.manifest?.l1Finality.confirmationDepth;
      if (depth === undefined)
        return yield* Effect.fail(
          new Error("Settlement requires manifest-bound L1 finality"),
        );
      const heights =
        status.confirmation.blockHash === undefined
          ? null
          : yield* Journal.confirmationHeights(
              owner,
              status.confirmation.blockHash,
            );
      if (
        heights === null ||
        !isSafe(depthOf(heights.tipHeight, heights.blockHeight), {
          confirmationDepth: depth,
        })
      )
        return "waiting for authenticated confirmation depth";
      if (attempt.required_outputs.length > 0) {
        const blockHash = status.confirmation.blockHash;
        if (blockHash === undefined)
          return "waiting for confirmation block identity";
        const exact = yield* settlementCall("Kupo output evidence", () =>
          readSettlementOutputEvidence(config.L1_KUPO_KEY, attempt, blockHash),
        );
        if (!exact)
          return yield* Effect.fail(
            new Error(
              `Confirmed settlement ${attempt.tx_hash} lacks its exact expected outputs`,
            ),
          );
      }
      yield* Journal.finishAttempt(
        owner,
        attempt,
        "confirmed",
        settlementNextPhase(attempt.phase),
        generation,
      );
      return "confirmed settlement transaction";
    }
    const before = yield* settlementCall("indexer sync", () =>
      synchronizePublicationIndexerPoint(
        config.L1_OGMIOS_KEY,
        config.L1_KUPO_KEY,
      ),
    );
    if (before.slot >= validToSlot) {
      const observed = yield* exactStatus(lucid, attempt.tx_hash);
      const inputs = attempt.fee_inputs.map(parseOutRefLabel);
      const visible = yield* settlementCall("fee input utxosByOutRef", () =>
        lucid.utxosByOutRef(inputs),
      );
      const expiredParents = yield* Journal.expiredParents(
        owner,
        inputs.map((out) => out.txHash),
      );
      const after = yield* settlementCall("indexer sync", () =>
        synchronizePublicationIndexerPoint(
          config.L1_OGMIOS_KEY,
          config.L1_KUPO_KEY,
        ),
      );
      if (
        canExpireSettlementAttempt({
          status: observed.status,
          validToSlot,
          beforeSlot: before.slot,
          afterSlot: after.slot,
          beforeHash: before.id,
          afterHash: after.id,
          allInputsUnspent:
            inputs.length > 0 &&
            (expiredParents.size > 0 ||
              inputs.every((out) =>
                visible.some(
                  (v) =>
                    v.txHash === out.txHash &&
                    v.outputIndex === out.outputIndex,
                ),
              )),
        })
      ) {
        yield* Journal.finishAttempt(
          owner,
          attempt,
          "expired",
          attempt.phase,
          generation,
        );
        return "expired unsubmitted body; rebuilding from current state";
      }
      return yield* Effect.fail(
        new Error(
          `Settlement transaction ${attempt.tx_hash} is ambiguous after expiry; retaining its journal and inputs`,
        ),
      );
    }
    yield* Journal.assertOwner(owner);
    // A dead intent (§8.2: an input spent by another tx, expired, failed) is
    // never resubmitted; its attempt waits for the expiry decision above.
    const intentStatus = yield* readIntentStatus(attempt.tx_hash);
    if (intentStatus !== null && isDeadStatus(intentStatus))
      return `settlement transaction ${attempt.tx_hash} is dead (${intentStatus.kind}); not resubmitted`;
    const provider = lucid.config().provider;
    if (provider === undefined)
      return yield* Effect.fail(new Error("Settlement provider unavailable"));
    // The exact body is resubmitted every tick until its status reads
    // confirmed. While it waits in a mempool (or in a block the indexer has
    // not reached) the node refuses the copy because its inputs are spent;
    // that is progress, not a failure, and the next tick reads its status. A
    // body whose inputs another transaction spent never confirms; past its
    // validity bound the expiry check above refuses it as ambiguous.
    const submitted = yield* settlementCall(
      "submit settlement transaction",
      () => provider.submitTx(attempt.signed_cbor),
    ).pipe(
      Effect.map((hash) => ({ hash })),
      Effect.catchIf(
        (error) => isSettlementInputsSpentRejection(error.cause),
        (error) =>
          Effect.succeed({
            waiting: `settlement transaction ${attempt.tx_hash} not confirmed yet; its resubmission is refused because its inputs are already spent (by it in a mempool, or by a block): ${describeSettlementError(error.cause).slice(0, 500)}`,
          }),
      ),
    );
    if ("waiting" in submitted) return submitted.waiting;
    const hash = submitted.hash;
    if (hash !== attempt.tx_hash)
      return yield* Effect.fail(
        new Error("Settlement submit returned a different transaction hash"),
      );
    return "submitted exact journaled settlement transaction";
  });

/** Synchronize before both positive and negative receipt observations: a
 * lagging indexer may still report a confirmation from the rolled-back fork. */
export const reconcileSettlementReceipts = (
  owner: Journal.SettlementOwner,
  job: Journal.SettlementJob,
  lucid: Pick<LucidEvolution, "transactionStatus">,
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const receipts = yield* Journal.attempts(job);
    if (receipts.length > 0)
      yield* settlementCall("indexer sync", () =>
        synchronizePublicationIndexerPoint(
          config.L1_OGMIOS_KEY,
          config.L1_KUPO_KEY,
        ),
      );
    for (const receipt of receipts) {
      const status = yield* exactStatus(lucid, receipt.tx_hash);
      if (status.status !== "confirmed") {
        yield* Journal.resumeReceipt(owner, receipt);
        return false;
      }
    }
    return true;
  });
