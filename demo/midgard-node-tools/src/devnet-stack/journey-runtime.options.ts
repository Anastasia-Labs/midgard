import type { CliRunner } from "./journey-cli.js";

/**
 * How long each wait may take. Each must span a node SIGKILL and restart, a
 * DA member restart, a ~60 s Kupo/Ogmios outage and a ~30 s Postgres pause,
 * with those drills running while the journey runs.
 */
export type JourneyDeadlines = {
  /** One user submission, retried under its submission id. */
  readonly submitMs: number;
  /** L1 confirmation, then the 300 s event_wait window, then projection. */
  readonly depositCreditedMs: number;
  /** Mempool admission of an L2 transaction. */
  readonly txAcceptedMs: number;
  /** Block production, DA attestation and the state queue append on L1. */
  readonly txCommittedMs: number;
  /**
   * Block maturity (900 s) and merge, then the settlement worker's absorb,
   * initialize, fund and conclude phases.
   */
  readonly payoutMs: number;
  /** Kupo showing the concluded payout at the payout address. */
  readonly payoutVisibleMs: number;
  /**
   * Every remaining merge (900 s maturity each), local finalization and the
   * settlement worker's remaining jobs (e.g. the absorb of a late deposit).
   */
  readonly drainMs: number;
  /** The node answering /readyz again. */
  readonly readyMs: number;
  /**
   * How long the awaited settlement job may keep failing, or the worker keep
   * crashing, without a break before a payout wait gives up: such a job never
   * pays out, and the whole payout deadline would only hide it.
   */
  readonly settlementErrorMs: number;
  /** A read of the local L2 ledger view (the `utxos` command). */
  readonly readMs: number;
};

export const DEFAULT_DEADLINES: JourneyDeadlines = {
  submitMs: 30 * 60_000,
  depositCreditedMs: 45 * 60_000,
  txAcceptedMs: 30 * 60_000,
  txCommittedMs: 45 * 60_000,
  payoutMs: 180 * 60_000,
  payoutVisibleMs: 15 * 60_000,
  drainMs: 150 * 60_000,
  readyMs: 30 * 60_000,
  settlementErrorMs: 15 * 60_000,
  readMs: 30 * 60_000,
};

/**
 * When a payout wait first saw its own settlement job failing, and when it
 * first saw the settlement worker failing, each without a break since.
 */
export type SettlementWatch = {
  jobFailingSince?: number;
  workerFailingSince?: number;
};

export type JourneyOptions = {
  readonly runCli?: CliRunner;
  readonly fetch?: typeof fetch;
  readonly deadlines?: Partial<JourneyDeadlines>;
  /** Interval between the polls of a wait. */
  readonly pollMs?: number;
  /** Capped exponential backoff between the attempts of a submission. */
  readonly retryInitialMs?: number;
  readonly retryMaxMs?: number;
  /** One CLI attempt; a hung attempt is killed and retried. */
  readonly cliTimeoutMs?: number;
  readonly httpTimeoutMs?: number;
  /** How long the node may not know a journaled transfer before it is resubmitted. */
  readonly resubmitAfterMs?: number;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly log?: (message: string) => void;
};
