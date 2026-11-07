import type { Journal } from "./journal.js";

/**
 * Operator liquidity provisioning for the reserve, not a repair. It works
 * around a protocol-level fragmentation gap that is queued for an owner
 * decision:
 *
 * each absorbed deposit becomes its own NoDatum reserve UTxO holding the
 * deposit's original assets, and one AddFunds step takes min(held,
 * still-needed) of every unit from one reserve UTxO, lovelace included. A
 * reserve UTxO whose change would keep tokens but fall below the minimum-UTxO
 * lovelace cannot be taken (midgard-sdk reserve-payout/reserve-inputs.ts), and
 * the payout validator makes that greedy take mandatory
 * (`no_change_for_still_needed_assets`), so no off-chain selector avoids it.
 * When nothing qualifies, the payout's fund step fails and settlement retries
 * it forever.
 *
 * One large pure-ADA reserve UTxO removes the lovelace dimension: it covers a
 * payout's whole lovelace need in one step, leaving pure-ADA change, and the
 * token-bearing UTxOs are then taken only for tokens, with all their lovelace
 * kept as change. The reserve validator accepts any NoDatum, script-free UTxO
 * at its address and anyone may pay one in.
 */

const ADA = 1_000_000n;

/**
 * A float below this is topped up. The scripted journey deposits 5,650 ADA in
 * all, so no L2 UTxO it withdraws, and no sum of its payouts, can need more
 * lovelace than that. A float of at least 6,000 ADA at the journey's start
 * therefore covers every payout of the journey from what is left of it, with
 * at least 350 ADA of change: far above the minimum-UTxO lovelace a pure-ADA
 * change output needs, so the float is never itself rejected.
 */
export const FLOAT_MINIMUM_LOVELACE = 6_000n * ADA;
/** What a top-up pays: a fresh float lasts well past one journey. */
export const FLOAT_TARGET_LOVELACE = 10_000n * ADA;

/** One unspent L1 output as the chain index reports it. */
export type ChainOutput = {
  readonly outRef: string;
  readonly lovelace: bigint;
  /** Native-asset units with a non-zero quantity. */
  readonly assetUnits: number;
  readonly datumHash: string | null;
  readonly scriptHash: string | null;
};

/** A payment built and signed, never yet submitted. */
export type SignedPayment = {
  readonly txId: string;
  /** Path of the signed transaction bytes. */
  readonly signedTx: string;
  /** The one wallet input it spends, `txId#index`. */
  readonly input: string;
};

export const assertFloatActive = (signal?: AbortSignal) => {
  if (signal?.aborted) throw new Error("reserve-float cancelled");
};

export type FloatDeps = {
  readonly signal?: AbortSignal;
  readonly reserveAddress: string;
  /** Unspent outputs at `address`. */
  readonly unspentAt: (address: string) => Promise<readonly ChainOutput[]>;
  /** Whether any output of `txId` is on chain, spent or not. */
  readonly landed: (txId: string) => Promise<boolean>;
  /** An out-ref's state; `unknown` while the index has not seen it. */
  readonly outputState: (
    outRef: string,
  ) => Promise<"unspent" | "spent" | "unknown">;
  /** Builds and signs a payment of `lovelace` to `address`; never submits. */
  readonly buildPayment: (
    address: string,
    lovelace: bigint,
    sequence: number,
  ) => Promise<SignedPayment>;
  /** Submits signed bytes; a refusal (already applied, in the mempool) is not an error. */
  readonly submit: (signedTx: string) => Promise<void>;
  readonly sleep: (ms: number) => Promise<void>;
  readonly now: () => number;
  readonly log: (line: string) => void;
};

export type FloatRecord = {
  readonly sequence: number;
  readonly reserveAddress: string;
  readonly lovelace: string;
  readonly txId: string;
  readonly signedTx: string;
  readonly input: string;
  readonly status: "pending" | "confirmed" | "abandoned";
};

export type FloatOutcome =
  | { readonly action: "sufficient"; readonly float: ChainOutput }
  | {
      readonly action: "topped-up";
      readonly txId: string;
      readonly lovelace: bigint;
    };

const RECORD_PREFIX = "reserve-float:";
const CONFIRM_TIMEOUT_MS = 300_000;
const POLL_MS = 2_000;
/**
 * How long one float step keeps retrying failures: well past the longest L1
 * outage a drill injects (a 60 s Kupo or Ogmios stop, a cardano-node
 * restart), since the journey runs this step while drills run.
 */
export const FLOAT_STEP_TIMEOUT_MS = 10 * 60_000;
const RETRY_MS = 10_000;

/** Only a pure-ADA, NoDatum, script-free output can serve as the float. */
export const isFloatCandidate = (output: ChainOutput) =>
  output.assetUnits === 0 &&
  output.datumHash === null &&
  output.scriptHash === null;

/** The largest float candidate among `outputs`, if any. */
export const largestFloat = (
  outputs: readonly ChainOutput[],
): ChainOutput | undefined =>
  outputs
    .filter(isFloatCandidate)
    .reduce<
      ChainOutput | undefined
    >((best, output) => (best === undefined || output.lovelace > best.lovelace ? output : best), undefined);

const record = (journal: Journal, value: FloatRecord) =>
  journal.set(`${RECORD_PREFIX}${value.sequence}`, value);

const waitLanded = async (deps: FloatDeps, txId: string) => {
  const deadline = deps.now() + CONFIRM_TIMEOUT_MS;
  for (;;) {
    assertFloatActive(deps.signal);
    const landed = await deps.landed(txId);
    assertFloatActive(deps.signal);
    if (landed) return;
    if (deps.now() > deadline)
      throw new Error(
        `reserve float transaction ${txId} did not confirm; the next run resumes it`,
      );
    await deps.sleep(POLL_MS);
  }
};

/**
 * Settles a journaled float payment an earlier run left pending. The input's
 * state is read before the transaction's, so a block landing between the two
 * reads is seen as landed, never as a conflicting spend: the index applies a
 * block's spends and outputs together.
 */
const reconcile = async (
  deps: FloatDeps,
  journal: Journal,
  pending: FloatRecord,
) => {
  assertFloatActive(deps.signal);
  const input = await deps.outputState(pending.input);
  assertFloatActive(deps.signal);
  const landed = await deps.landed(pending.txId);
  assertFloatActive(deps.signal);
  if (landed) {
    deps.log(`reserve-float: journaled payment ${pending.txId} landed`);
    record(journal, { ...pending, status: "confirmed" });
    return;
  }
  if (input === "spent") {
    // Something else spent its input, so these bytes can never land.
    deps.log(
      `reserve-float: journaled payment ${pending.txId} lost its input; building another`,
    );
    record(journal, { ...pending, status: "abandoned" });
    return;
  }
  deps.log(`reserve-float: resubmitting journaled payment ${pending.txId}`);
  assertFloatActive(deps.signal);
  await deps.submit(pending.signedTx);
  await waitLanded(deps, pending.txId);
  assertFloatActive(deps.signal);
  record(journal, { ...pending, status: "confirmed" });
};

/**
 * Makes the reserve address hold a pure-ADA float of at least
 * FLOAT_MINIMUM_LOVELACE, paying FLOAT_TARGET_LOVELACE when it does not.
 * Idempotent and resumable: the signed payment is journaled before it is
 * submitted, and a rerun settles that exact payment (landed, resubmitted, or
 * abandoned once its input is spent elsewhere) before deciding anything, so a
 * crash between submission and its record never pays twice.
 */
export const ensureReserveFloat = async (
  deps: FloatDeps,
  journal: Journal,
): Promise<FloatOutcome> => {
  assertFloatActive(deps.signal);
  const records = journal.withPrefix<FloatRecord>(RECORD_PREFIX);
  for (const pending of records.filter((r) => r.status === "pending"))
    await reconcile(deps, journal, pending);

  const float = largestFloat(await deps.unspentAt(deps.reserveAddress));
  assertFloatActive(deps.signal);
  if (float !== undefined && float.lovelace >= FLOAT_MINIMUM_LOVELACE)
    return { action: "sufficient", float };

  const sequence = records.length + 1;
  assertFloatActive(deps.signal);
  const payment = await deps.buildPayment(
    deps.reserveAddress,
    FLOAT_TARGET_LOVELACE,
    sequence,
  );
  const intent: FloatRecord = {
    sequence,
    reserveAddress: deps.reserveAddress,
    lovelace: FLOAT_TARGET_LOVELACE.toString(),
    ...payment,
    status: "pending",
  };
  // Keep returned signed bytes durable even if signing completed during abort.
  record(journal, intent);
  assertFloatActive(deps.signal);
  await deps.submit(payment.signedTx);
  await waitLanded(deps, payment.txId);
  assertFloatActive(deps.signal);
  record(journal, { ...intent, status: "confirmed" });
  return {
    action: "topped-up",
    txId: payment.txId,
    lovelace: FLOAT_TARGET_LOVELACE,
  };
};

/**
 * ensureReserveFloat, retried until FLOAT_STEP_TIMEOUT_MS. A retry is safe:
 * each attempt first settles the payment the one before journaled, so an
 * outage between submission and confirmation never pays twice.
 */
export const ensureReserveFloatRetrying = async (
  deps: FloatDeps,
  journal: Journal,
): Promise<FloatOutcome> => {
  const deadline = deps.now() + FLOAT_STEP_TIMEOUT_MS;
  for (;;) {
    try {
      return await ensureReserveFloat(deps, journal);
    } catch (error) {
      assertFloatActive(deps.signal);
      if (deps.now() >= deadline) throw error;
      deps.log(
        `reserve-float: ${error instanceof Error ? error.message : String(error)}; retrying`,
      );
      await deps.sleep(RETRY_MS);
    }
  }
};

/**
 * The float's readiness: one reason while the reserve holds no pure-ADA
 * float of the minimum, none otherwise.
 */
export const reserveFloatReasons = (
  outputs: readonly ChainOutput[],
): readonly string[] => {
  const float = largestFloat(outputs);
  return float !== undefined && float.lovelace >= FLOAT_MINIMUM_LOVELACE
    ? []
    : [
        `reserve_float_below_minimum: floatLovelace=${float?.lovelace ?? 0n}, minimumLovelace=${FLOAT_MINIMUM_LOVELACE}`,
      ];
};

/** How a long-running maintainer shares the float step with up and the journey. */
export type FloatMaintainer = {
  /** The journal as it is on disk now: another process may have written it. */
  readonly openJournal: () => Journal;
  /** Runs `step` holding the run's float lock, waiting while another holds it. */
  readonly withLock: <T>(step: () => Promise<T>) => Promise<T>;
};

/**
 * One maintenance round. A sufficient float with no payment pending is read
 * without the lock; anything else is the float step, run under the lock on a
 * journal read after taking it, so this and the up or journey step never
 * journal the same sequence and never both pay.
 */
export const maintainReserveFloatOnce = async (
  deps: FloatDeps,
  maintainer: FloatMaintainer,
): Promise<FloatOutcome> => {
  const pending = maintainer
    .openJournal()
    .withPrefix<FloatRecord>(RECORD_PREFIX)
    .some((r) => r.status === "pending");
  if (!pending) {
    const float = largestFloat(await deps.unspentAt(deps.reserveAddress));
    if (float !== undefined && float.lovelace >= FLOAT_MINIMUM_LOVELACE)
      return { action: "sufficient", float };
  }
  return maintainer.withLock(() =>
    ensureReserveFloat(deps, maintainer.openJournal()),
  );
};

/**
 * Keeps the float for the life of the run: one round, then `intervalMs` of
 * sleep, until `signal` aborts. A failed round (an L1 outage, a payer that
 * cannot pay) is logged once per distinct error and tried again next round;
 * each round first settles what an earlier one journaled, so a retry never
 * pays twice. `alongside` runs after every round, its failures logged alike.
 */
export const runReserveFloatMaintainer = async (
  deps: FloatDeps,
  maintainer: FloatMaintainer,
  options: {
    readonly intervalMs: number;
    readonly signal: AbortSignal;
    readonly alongside?: () => Promise<void>;
  },
): Promise<void> => {
  // Each logs a failure only when it differs from the one it logged last.
  const reporter = (what: string) => {
    let last: string | undefined;
    return {
      failed: (error: unknown) => {
        const message = `${what}: ${error instanceof Error ? error.message : String(error)}`;
        if (message !== last)
          deps.log(`reserve-float: ${message}; retrying next round`);
        last = message;
      },
      succeeded: () => (last = undefined),
    };
  };
  const round = reporter("round failed");
  const watch = reporter("watch failed");
  while (!options.signal.aborted) {
    try {
      const outcome = await maintainReserveFloatOnce(deps, maintainer);
      round.succeeded();
      if (outcome.action === "topped-up")
        deps.log(
          `reserve-float: paid ${outcome.lovelace} lovelace in ${outcome.txId}`,
        );
    } catch (error) {
      if (options.signal.aborted) break;
      round.failed(error);
    }
    if (options.alongside !== undefined)
      await options.alongside().then(watch.succeeded, watch.failed);
    if (options.signal.aborted) break;
    await deps.sleep(options.intervalMs).catch(() => undefined);
  }
};
