import "./da-bond-pool-live-port.await-availability-inclusion.js";

import { existsSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import * as SDK from "@al-ft/midgard-sdk";
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import {
  PublishedTransactionExpiredError,
  PublishedTransactionSubmissionError,
} from "midgard-watcher/tests/support/published-block-actor";

import { deriveL1Origin } from "../../src/l1-origin.js";
import type {
  DaBondPoolJourneyPort,
  DaBondPoolJourneyResume,
} from "./da-bond-pool-journey.js";
import { type JourneyProcess } from "./da-bond-pool-live-port.da-bond-pool-apply-refusal.js";
import { validityIntervalRefusal } from "./da-bond-pool-live-port.summarize-da-bond-pool-timeout.js";
import { describeErrorChain } from "./error-chain.js";

/**
 * Commits a header through `submit`, and rebuilds it when the ledger refused
 * the submission outside its validity interval, or when a submission the
 * mempool took expired unminted. The block actor opens a commit's interval
 * sixty seconds before the wall clock and closes it about a minute after, and
 * the ledger checks it against its tip. So a block gap longer than that
 * refuses the commit or lets it lapse, and the devnet makes a block only every
 * twenty seconds on average. A refused submission never entered the mempool,
 * so it cannot land, and a rebuild from the current queue is safe.
 * `awaitFreshTip` runs before every attempt and `submit` gets the attempt
 * number.
 *
 * An expired submission (`PublishedTransactionExpiredError`) goes to
 * `settleExpired`, which returns the commit when it did land after all,
 * undefined only when it provably never can (the tip is past its upper bound,
 * the header is absent and its anchor unspent), and throws otherwise. An
 * adopted commit runs `refreshWallet` first: the actor pins the wallet to its
 * pre-commit view and refreshes it only when it sees the commit land, so the
 * pinned view still lists the inputs the commit spent. Any other error fails
 * the commit, and so do both conditions once `maxAttempts` is reached. A
 * refused submission's reason is logged, since the actor's error names only
 * its transaction.
 */
export const commitWithinLedgerValidity = async <T>({
  label,
  submit,
  awaitFreshTip,
  settleExpired,
  refreshWallet,
  maxAttempts,
  log,
}: Readonly<{
  label: string;
  submit: (attempt: number) => Promise<T>;
  awaitFreshTip: () => Promise<void>;
  settleExpired: (
    error: PublishedTransactionExpiredError,
  ) => Promise<T | undefined>;
  refreshWallet: () => Promise<void>;
  maxAttempts: number;
  log: (line: string) => void;
}>): Promise<T> => {
  for (let attempt = 1; ; attempt += 1) {
    await awaitFreshTip();
    try {
      return await submit(attempt);
    } catch (error) {
      if (error instanceof PublishedTransactionExpiredError) {
        const landed = await settleExpired(error);
        if (landed !== undefined) {
          log(
            `${label}: ${error.txHash} landed after its local expiry wait; adopting it`,
          );
          await refreshWallet();
          return landed;
        }
        if (attempt >= maxAttempts) {
          log(`${label}: ${error.txHash} expired unminted on the last attempt`);
          throw error;
        }
        log(
          `${label}: ${error.txHash} expired unminted past its validity bound; rebuilding on a fresh ledger tip`,
        );
        continue;
      }
      if (!(error instanceof PublishedTransactionSubmissionError)) throw error;
      const refusal = validityIntervalRefusal(error.cause);
      if (refusal === undefined || attempt >= maxAttempts) {
        log(
          `${label}: submission ${error.txHash} refused: ${describeErrorChain(error.cause)}`,
        );
        throw error;
      }
      log(
        `${label}: submission ${error.txHash} refused outside its validity interval (${refusal}); rebuilding on a fresh ledger tip`,
      );
    }
  }
};

/**
 * Attests a block through `attest`, and runs it again when the ledger refused
 * one of its transactions outside the validity interval. The Apply's interval
 * opens sixty seconds before the wall clock, and the ledger checks it against
 * its tip, so a long block gap refuses it. The actor resumes an attestation
 * from its on-chain progress, and a refused transaction never entered the
 * mempool. `awaitFreshTip` runs before every attempt, and `refreshWallet`
 * before every attempt after the first. An error `refusal` maps (a pool
 * refusal of the Apply) is returned as the result at once; every other error
 * fails the attestation, and so does the validity refusal once `maxAttempts`
 * is reached.
 */
export const attestWithinLedgerValidity = async <T, R>({
  label,
  attest,
  refusal,
  awaitFreshTip,
  refreshWallet,
  maxAttempts,
  log,
}: Readonly<{
  label: string;
  attest: () => Promise<T>;
  refusal: (error: unknown) => R | undefined;
  awaitFreshTip: () => Promise<void>;
  refreshWallet: () => Promise<void>;
  maxAttempts: number;
  log: (line: string) => void;
}>): Promise<T | R> => {
  for (let attempt = 1; ; attempt += 1) {
    await awaitFreshTip();
    if (attempt > 1) await refreshWallet();
    try {
      return await attest();
    } catch (error) {
      const refused = refusal(error);
      if (refused !== undefined) return refused;
      const validity = validityIntervalRefusal(error);
      if (validity === undefined || attempt >= maxAttempts) {
        log(`${label}: failed: ${describeErrorChain(error)}`);
        throw error;
      }
      log(
        `${label}: refused outside its validity interval (${validity}); attesting again on a fresh ledger tip`,
      );
    }
  }
};

// ---------------------------------------------------------------------------
// The live port
// ---------------------------------------------------------------------------

export type LiveDaBondPoolJourneyPortOptions = Readonly<{
  /** Default `daBondPoolJourneyDirectory(context.runDirectory)`. */
  artifactDirectory?: string;
  /** Default `DA_BOND_POOL_CHALLENGER_OPERATING_LOVELACE`. */
  operatingLovelace?: bigint;
  /** Progress lines; default `console.info`. */
  log?: (line: string) => void;
  /** Default `readLinuxProcesses`. */
  listProcesses?: () => readonly JourneyProcess[];
  /**
   * Resume a run that passed steps 1, 3, 4 and 5 in `artifactDirectory`: a
   * smoke of steps 2 and 6 on a kept devnet, never journey evidence. The
   * port reuses that run's committee runtime, keys, database and journals,
   * and writes its own evidence under a fresh `resume-<time>` directory.
   */
  resume?: Readonly<{ afterStep: 5 }>;
}>;

/** The committee node's two submitter mnemonics, relative to the run directory. */
export const DA_BOND_POOL_COMMITTEE_SUBMITTER_SECRETS = Object.freeze({
  l1: "secrets/da-bond-pool-committee-l1-submitter.seed",
  availability: "secrets/da-bond-pool-committee-availability-submitter.seed",
});

/** The finalized manifest the deployment writes and the CLI reads. */
export const JOURNEY_DEPLOYMENT_MANIFEST = "deploymentInfo/manifest.json";

/** The repository root of this worktree. */
export const REPOSITORY_ROOT = fileURLToPath(
  new URL("../../../../", import.meta.url),
);

/**
 * The committee node cannot be started against this run (P27(8), P31): a file
 * it needs is missing, its DA libp2p runtime could not be produced, or its
 * runtime manifest's peer set does not admit its libp2p identity. A missing
 * file or a refused runtime stops the journey before any transaction; a
 * refused configuration stops it after the challenger funding transaction may
 * have landed, but before any pool or availability transaction.
 */
export class DaBondPoolCommitteeUnavailableError extends Error {
  constructor(problem: string, options?: ErrorOptions) {
    super(
      `The DA bond pool journey cannot start its committee node (ruling P27): ${problem}`,
      options,
    );
    this.name = "DaBondPoolCommitteeUnavailableError";
  }
}

/**
 * The run's own node, which the committee's L1 follower reads, and the
 * deployment's origin on it: the point before its hub-oracle nonce's block.
 * Refuses when the run has no node to follow.
 */
export const daBondPoolCommitteeL1 = async (input: {
  readonly runDirectory: string;
  readonly networkMagic: number;
  readonly nonceTxHash: string;
}): Promise<
  Readonly<{
    nativeLedger: Readonly<{ socket: string; config: string; binary: string }>;
    l1Origin: L1Origin;
  }>
> => {
  const nativeLedger = {
    socket: join(input.runDirectory, "cardano/ipc/node.socket"),
    config: join(input.runDirectory, "config/config.json"),
    binary: join(input.runDirectory, "work/midgard-l1-node-transport"),
  };
  const missing = Object.values(nativeLedger).filter((p) => !existsSync(p));
  if (missing.length > 0)
    throw new DaBondPoolCommitteeUnavailableError(
      `its L1 follower needs the run's node: ${missing.join(", ")}`,
    );
  const { origin } = await deriveL1Origin({
    node: {
      socketPath: nativeLedger.socket,
      binaryPath: nativeLedger.binary,
      networkMagic: input.networkMagic,
    },
    nonceTxHash: input.nonceTxHash,
  });
  return { nativeLedger, l1Origin: origin };
};

export type LiveDaBondPoolJourneyPort = DaBondPoolJourneyPort &
  Readonly<{
    /** Where this run's evidence goes; a resumed run's own directory. */
    artifactDirectory: string;
    manifestId: string;
    networkMagic: number;
    challengerAddress: string;
    /**
     * Whether the committee node still runs. False after a passed journey
     * only when the checked stop at the end of step 6 ran (P27(3)); `dispose`
     * tears a running node down without those checks.
     */
    committeeRunning: () => boolean;
    /** Closes the availability journal; the port is unusable afterwards. */
    dispose: () => Promise<void>;
    /** Where the driver resumes, when the port was created with `resume`. */
    resume?: DaBondPoolJourneyResume;
  }>;

/** The queue is not root-only: the journey needs its head to be B1. */
export class DaBondPoolJourneyQueueNotEmptyError extends Error {
  constructor(headers: number) {
    super(
      `The DA bond pool journey needs an empty state queue (root only), but it holds ${headers.toString()} header(s). The journey appends B1 as the head, and only the head can be removed after its availability Timeout; run it on a freshly deployed journey run directory.`,
    );
    this.name = "DaBondPoolJourneyQueueNotEmptyError";
  }
}

/** A resumed run's chain or files are not what its earlier run left. */
export class DaBondPoolJourneyResumeMismatchError extends Error {
  constructor(problem: string) {
    super(
      `The DA bond pool journey cannot resume after step 5: ${problem}. Resume only a run directory whose journey passed steps 1, 3, 4 and 5 and stopped before step 2 landed anything.`,
    );
    this.name = "DaBondPoolJourneyResumeMismatchError";
  }
}

/**
 * A resumed run's queue precondition: the queue behind its root (`headers`,
 * in order) is exactly the B2 the earlier run recorded.
 */
export const requireResumableQueue = (
  headers: readonly string[],
  b2HeaderHash: string,
): void => {
  if (headers.length !== 1 || headers[0] !== b2HeaderHash)
    throw new DaBondPoolJourneyResumeMismatchError(
      `the state queue behind its root holds [${headers.join(", ")}], not only the recorded B2 ${b2HeaderHash}`,
    );
};

export const loadOrCreateSeed = (path: string): string => {
  if (existsSync(path)) {
    const mode = statSync(path).mode & 0o777;
    if ((mode & 0o077) !== 0)
      throw new Error(
        `${path} is readable by others (mode ${mode.toString(8)}); it must be 0600`,
      );
    const seed = readFileSync(path, "utf8").trim();
    if (seed.split(/\s+/u).length < 12)
      throw new Error(`${path} does not hold a mnemonic`);
    return seed;
  }
  const seed = generateSeedPhrase();
  writeFileSync(path, `${seed}\n`, { mode: 0o600, flag: "wx" });
  return seed;
};

export type JourneyBlock = Readonly<{
  label: string;
  header: SDK.Header;
  headerHash: string;
  payloadEnvelopeCbor: Buffer;
}>;

export type AvailabilityRequest =
  | "open"
  | "respond"
  | "settle"
  | "close"
  | "timeout";
