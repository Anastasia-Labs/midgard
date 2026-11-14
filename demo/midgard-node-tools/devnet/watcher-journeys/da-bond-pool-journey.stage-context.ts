import {
  type DaBondPoolJourneyAlerts,
  type DaBondPoolJourneyAssertion,
  type DaBondPoolJourneyBlockStatus,
  type DaBondPoolJourneyCommittedBlock,
  type DaBondPoolJourneyParams,
  type DaBondPoolJourneyResume,
  type DaBondPoolJourneySnapshot,
  type DaBondPoolJourneyStep,
  type DaBondPoolJourneyTimeoutResult,
} from "./da-bond-pool-journey.da-bond-pool-journey-port.js";
import { type DaBondPoolProcessRun } from "./da-bond-pool-process-evidence.js";
import { describeErrorChain } from "./error-chain.js";

export type DaBondPoolJourneyObservation =
  | { label: string; kind: "pool"; value: DaBondPoolJourneySnapshot }
  | { label: string; kind: "alerts"; value: DaBondPoolJourneyAlerts }
  | {
      label: string;
      kind: "block";
      headerHash: string;
      value: DaBondPoolJourneyBlockStatus;
    }
  | { label: string; kind: "timeout"; value: DaBondPoolJourneyTimeoutResult }
  | { label: string; kind: "process"; value: DaBondPoolProcessRun }
  | { label: string; kind: "value"; value: string | number | bigint | boolean };

/** One stage of the ledger: one spec step. */
export type DaBondPoolJourneyStepOutcome = {
  step: DaBondPoolJourneyStep;
  name: string;
  /** 1-based position in the run order. */
  order: number;
  status: "running" | "passed" | "failed";
  /** ISO wall-clock time. */
  startedAt: string;
  finishedAt?: string;
  /** Label to transaction id, in landing order. */
  txIds: Record<string, string>;
  observations: DaBondPoolJourneyObservation[];
  assertions: DaBondPoolJourneyAssertion[];
  error?: string;
};

/** The stage ledger. Stages are in run order. */
export type DaBondPoolJourneyRecord = {
  status: "running" | "passed" | "failed";
  startedAt: string;
  finishedAt?: string;
  chronology: readonly DaBondPoolJourneyStep[];
  params?: DaBondPoolJourneyParams;
  /** Set when the run resumed after this step: a smoke, not journey evidence. */
  resumedAfterStep?: DaBondPoolJourneyResume["afterStep"];
  stages: DaBondPoolJourneyStepOutcome[];
  failure?: { step: DaBondPoolJourneyStep; message: string };
};

/** A journey that stopped; `record` is the ledger up to and including the failed stage. */
export class DaBondPoolJourneyFailure extends Error {
  readonly record: DaBondPoolJourneyRecord;
  constructor(record: DaBondPoolJourneyRecord, cause: unknown) {
    super(
      `DA bond pool journey failed at step ${record.failure?.step ?? "?"}: ${describeErrorChain(cause)}`,
      { cause },
    );
    this.name = "DaBondPoolJourneyFailure";
    this.record = record;
  }
}

class DaBondPoolJourneyAssertionError extends Error {
  constructor(step: DaBondPoolJourneyStep, failed: readonly string[]) {
    super(`step ${step}: assertion failed: ${failed.join("; ")}`);
    this.name = "DaBondPoolJourneyAssertionError";
  }
}

export const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

export const maxBigInt = (a: bigint, b: bigint): bigint => (a > b ? a : b);

export const minBigInt = (a: bigint, b: bigint): bigint => (a < b ? a : b);

/** Spec #685 §5 / D2: what one Timeout takes from a pool backing `backing`. */
export const planDaBondPoolSlash = (input: {
  readonly daBond: bigint;
  readonly penalty: bigint;
  readonly backing: bigint;
}): Readonly<{ taken: bigint; feePart: bigint; payout: bigint }> => {
  const taken = minBigInt(input.daBond, maxBigInt(input.backing, 0n));
  const feePart = minBigInt(input.penalty, taken);
  return { taken, feePart, payout: taken - feePart };
};

export class StageContext {
  readonly #outcome: DaBondPoolJourneyStepOutcome;

  constructor(outcome: DaBondPoolJourneyStepOutcome) {
    this.#outcome = outcome;
  }

  tx(label: string, txId: string): void {
    let key = label;
    for (let n = 2; key in this.#outcome.txIds; n += 1) key = `${label} (${n})`;
    this.#outcome.txIds[key] = txId;
  }

  txs(label: string, txIds: readonly string[]): void {
    txIds.forEach((txId, index) => this.tx(`${label} [${index}]`, txId));
  }

  observe(observation: DaBondPoolJourneyObservation): void {
    this.#outcome.observations.push(observation);
  }

  /** Records an assertion; the stage fails at the next gate if it is false. */
  check(name: string, ok: boolean, detail: string): boolean {
    this.#outcome.assertions.push({ name, ok, detail });
    return ok;
  }

  notObservable(name: string, detail: string): void {
    this.#outcome.assertions.push({ name, ok: "not-observable", detail });
  }

  /** Records an assertion and stops the stage at once when it is false. */
  require(name: string, ok: boolean, detail: string): void {
    this.check(name, ok, detail);
    this.gate();
  }

  /** Stops the stage when any assertion so far failed. */
  gate(): void {
    const failed = this.#outcome.assertions
      .filter((assertion) => assertion.ok === false)
      .map((assertion) => `${assertion.name} (${assertion.detail})`);
    if (failed.length > 0)
      throw new DaBondPoolJourneyAssertionError(this.#outcome.step, failed);
  }
}

export type CommittedBlock = DaBondPoolJourneyCommittedBlock;

export const describePool = (pool: DaBondPoolJourneySnapshot): string =>
  `state=${pool.state}, lovelace=${pool.lovelace}, backing=${pool.backing}` +
  (pool.unlockAt === undefined ? "" : `, unlockAt=${pool.unlockAt}`) +
  (pool.utxoRef === undefined ? "" : `, utxo=${pool.utxoRef}`);

export const hasPrefixed = (
  values: readonly string[],
  prefix: string,
): boolean => values.some((value) => value.startsWith(prefix));
