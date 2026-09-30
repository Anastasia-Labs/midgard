import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import type { DaBondPoolJourneyTimeoutResult } from "./da-bond-pool-journey.js";
import { type DaBondPoolJourneyOutput } from "./da-bond-pool-live-port.da-bond-pool-apply-refusal.js";
import { errorChainLinks, errorChainTexts } from "./error-chain.js";

/**
 * The Timeout's slash evidence from its landed body: the fee, the one pool
 * output (its lovelace, and whether it kept the input's datum and carries only
 * the pool NFT), and the outputs paid to the challenger's key address.
 */
export const summarizeDaBondPoolTimeout = (input: {
  readonly txId: string;
  readonly fee: bigint;
  readonly inputs: readonly string[];
  readonly outputs: readonly DaBondPoolJourneyOutput[];
  readonly pool: Readonly<{
    outRef: string;
    address: string;
    unit: string;
    lovelace: bigint;
    datum: string;
  }>;
  readonly challengerAddress: string;
  readonly challengerRemainingLovelace?: bigint;
}): DaBondPoolJourneyTimeoutResult => {
  if (!input.inputs.includes(input.pool.outRef))
    throw new Error(
      `Timeout ${input.txId} does not spend the observed pool ${input.pool.outRef}`,
    );
  const pools = input.outputs.filter(
    (output) =>
      output.address === input.pool.address &&
      (output.assets[input.pool.unit] ?? 0n) !== 0n,
  );
  if (pools.length !== 1)
    throw new Error(
      `Timeout ${input.txId} has ${pools.length.toString()} pool outputs; it must continue the pool exactly once`,
    );
  const pool = pools[0]!;
  const challenger = input.outputs.filter(
    (output) => output.address === input.challengerAddress,
  );
  return {
    txId: input.txId,
    fee: input.fee,
    challengerOutputLovelace: challenger.reduce(
      (sum, output) => sum + (output.assets.lovelace ?? 0n),
      0n,
    ),
    challengerOutputCount: challenger.length,
    poolBefore: input.pool.lovelace,
    poolAfter: pool.assets.lovelace ?? 0n,
    poolDatumAndNftKept:
      pool.datum === input.pool.datum &&
      pool.assets[input.pool.unit] === 1n &&
      Object.keys(pool.assets).every(
        (unit) => unit === "lovelace" || unit === input.pool.unit,
      ),
    ...(input.challengerRemainingLovelace === undefined
      ? {}
      : { challengerRemainingLovelace: input.challengerRemainingLovelace }),
  };
};

/** Fee, spent inputs and outputs of a transaction's CBOR. */
export const decodeJourneyTransaction = (
  txCbor: string,
): Readonly<{
  fee: bigint;
  inputs: readonly string[];
  outputs: readonly DaBondPoolJourneyOutput[];
}> => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  const body = tx.body();
  const inputList = body.inputs();
  const outputList = body.outputs();
  const inputs: string[] = [];
  for (let index = 0; index < inputList.len(); index += 1) {
    const input = inputList.get(index);
    inputs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  const outputs: DaBondPoolJourneyOutput[] = [];
  for (let index = 0; index < outputList.len(); index += 1) {
    const output = coreToTxOutput(outputList.get(index));
    outputs.push({
      address: output.address,
      assets: output.assets,
      datum: output.datum ?? null,
    });
  }
  return { fee: body.fee(), inputs, outputs };
};

/**
 * Canonical-source errors that clear once Kupo catches up with Ogmios, or once
 * the block that landed during an inclusion or foreign-spend read is indexed.
 */
export const isTransientCanonicalError = (error: unknown): boolean => {
  const message = error instanceof Error ? error.message : String(error);
  return /aligned at the same canonical tip|changed during canonical discovery|next stable point|could not read the canonical Kupo checkpoint|(?:inclusion|input spend) changed during its canonical read/u.test(
    message,
  );
};

/**
 * The ledger refused a rebroadcast because every input is already spent.
 * Reconciliation rebroadcasts a journaled intent while the canonical view
 * still shows its inputs unspent, so a transaction that is waiting in the
 * mempool, or sits in a block the canonical view has not reached, is refused
 * this way. The next reconciliation settles it: included, or expired when
 * another transaction spent the inputs. The watcher and the committee node
 * retry on their next tick in the same way.
 */
export const isSpentInputsRebroadcastRefusal = (error: unknown): boolean => {
  const message = error instanceof Error ? error.message : String(error);
  const data =
    typeof error === "object" && error !== null && "data" in error
      ? JSON.stringify((error as { data: unknown }).data)
      : "";
  return /All inputs are spent|BadInputsUTxO/u.test(`${message} ${data}`);
};

/** Ogmios's `submitTransaction` error for a slot outside the validity interval. */
const OGMIOS_OUTSIDE_VALIDITY_INTERVAL = 3118;

/**
 * The ledger's refusal of a submission whose validity interval does not hold
 * its current slot: `lower` when the ledger tip has not reached the interval's
 * start, `upper` when it has passed its end. It is read from the structured
 * Ogmios refusal (`data.validityInterval` and `data.currentSlot`) wherever it
 * sits in the error chain; undefined for any other error. This is a phase-1
 * time check that runs before any script, so it never stands for a script,
 * value or state failure.
 */
export type LedgerValidityRefusal = Readonly<{
  bound: "lower" | "upper";
  /** The refusing link's message and data, for the log. */
  text: string;
}>;

export const ledgerValidityRefusal = (
  error: unknown,
): LedgerValidityRefusal | undefined => {
  for (const link of errorChainLinks(error)) {
    if (typeof link !== "object" || link === null) continue;
    const { code, data } = link as { code?: unknown; data?: unknown };
    if (typeof code === "number" && code !== OGMIOS_OUTSIDE_VALIDITY_INTERVAL)
      continue;
    if (typeof data !== "object" || data === null) continue;
    const { validityInterval, currentSlot } = data as {
      validityInterval?: unknown;
      currentSlot?: unknown;
    };
    if (
      typeof currentSlot !== "number" ||
      typeof validityInterval !== "object" ||
      validityInterval === null
    )
      continue;
    const { invalidBefore } = validityInterval as { invalidBefore?: unknown };
    return {
      bound:
        typeof invalidBefore === "number" && currentSlot < invalidBefore
          ? "lower"
          : "upper",
      text: errorChainTexts(link)[0] ?? "",
    };
  }
  return undefined;
};

/**
 * The text of the ledger's validity-interval refusal (see
 * `ledgerValidityRefusal`); undefined for any other error.
 */
export const validityIntervalRefusal = (error: unknown): string | undefined =>
  ledgerValidityRefusal(error)?.text;

/**
 * A reconciliation error that means only "not settled yet": the rebroadcast of
 * a journaled intent was refused with spent inputs (see
 * `isSpentInputsRebroadcastRefusal`) or outside its validity interval (the tip
 * has not reached its start, or has passed its end and the canonical view will
 * soon record the expiry), or the canonical view is catching up. Its kind is
 * returned for the log; undefined for every other error, which stays fatal.
 */
export const unsettledReconciliationError = (
  error: unknown,
): string | undefined => {
  if (isSpentInputsRebroadcastRefusal(error))
    return "rebroadcast refused with spent inputs";
  const validity = ledgerValidityRefusal(error);
  if (validity !== undefined)
    return validity.bound === "lower"
      ? "rebroadcast refused before its validity interval's start"
      : "rebroadcast refused past its validity interval's end";
  if (isTransientCanonicalError(error))
    return "the canonical view is catching up";
  return undefined;
};

/**
 * The journal's detail for an intent that reached no block before its
 * validity ended while every normal input stayed canonically unspent
 * (`reconcileDaAvailabilityOperations`): it never landed and nothing else
 * spent its inputs, so a fresh plan is safe.
 */
const LAPSED_DETAIL = "Expired with every normal input canonically unspent";

/**
 * An availability transaction's validity ended before any block took it, and
 * every input it spends is still canonically unspent. The CLI builder bounds
 * each action about a minute past the wall clock, and the devnet can go that
 * long without a block. Only this ending is re-planned (`landAvailability`).
 */
export class AvailabilityIntentLapsedError extends Error {
  constructor(readonly txId: string) {
    super(
      `Availability transaction ${txId} ended expired: its validity ended before any block took it, with every input still unspent`,
    );
    this.name = "AvailabilityIntentLapsedError";
  }
}

/** A journal record as the inclusion wait reads it. */
export type AvailabilityJournalView = Readonly<{
  state?: string;
  detail?: string | null;
}>;

/**
 * The error for an availability transaction that ended `expired` or
 * `conflict`: `AvailabilityIntentLapsedError` only for an expiry the journal
 * records with every normal input canonically unspent; a plain error, naming
 * the journal's detail, for every other ending.
 */
export const availabilityEndingError = (
  txId: string,
  status: "expired" | "conflict",
  record: AvailabilityJournalView | undefined,
): Error =>
  status === "expired" && record?.detail === LAPSED_DETAIL
    ? new AvailabilityIntentLapsedError(txId)
    : new Error(
        `Availability transaction ${txId} ended ${status}${record?.detail ? ` (${record.detail})` : ""}`,
      );
