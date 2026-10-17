/**
 * Resumable L2 transfer submission.
 *
 * A transfer submitted under a submission ID is signed once. Its exact signed
 * bytes and the result fields are published to a local journal file before
 * the first `/submit`, so a rerun with the same ID returns or resubmits that
 * transaction instead of selecting inputs and signing a second one. The
 * journal is the caller's own file rather than a node table: the transfer CLI
 * is a client of the node's public HTTP API and holds no database connection.
 */

import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

import { normalizeAssets } from "@al-ft/midgard-core/assets";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { type Assets } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { writeTextFileAtomicNoReplace } from "../files/atomic-write.js";
import {
  ContractDeploymentIdentity,
  NodeConfig as NodeConfigService,
} from "../services/index.js";
import {
  fetchNodeTxStatus,
  type ResolvedWalletSeedPhrase,
} from "./command-utils.js";
import {
  buildRequestedAssets,
  FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
  type NativeTransferSubmitRetryPolicy,
  type PreparedL2Transfer,
  ResumableNativeTransferSubmitError,
  type SubmitL2TransferConfig,
  type SubmitL2TransferResult,
  toError,
} from "./submit-l2-transfer.compare-assets-by-coverage.js";
import { submitL2TransferProgram } from "./submit-l2-transfer.prepare-l2-terminal-drain-program.js";
import {
  buildL2TransferForSenderProgram,
  resolveL2TransferSenderProgram,
  submitNativeTransferTx,
} from "./submit-l2-transfer.submit-native-transfer-tx.js";

export const TRANSFER_SUBMISSION_JOURNAL_DIR_ENV =
  "MIDGARD_L2_TRANSFER_JOURNAL_DIR";

/** The journal directory used when `--submission-journal-dir` is omitted. */
export const defaultTransferSubmissionJournalDir = (
  env: NodeJS.ProcessEnv = process.env,
): string => {
  const configured = env[TRANSFER_SUBMISSION_JOURNAL_DIR_ENV]?.trim() ?? "";
  return configured.length > 0
    ? configured
    : join(homedir(), ".midgard", "l2-transfer-submissions");
};

/** Same bound as the deposit and withdrawal submission IDs. */
export const parseTransferSubmissionId = (value: string): string => {
  if (value.length < 1 || value.length > 128) {
    throw new Error("--submission-id must be 1 to 128 characters long.");
  }
  return value;
};

/** File names are hashed so any submission ID maps to one safe path. */
export const transferSubmissionJournalPath = (
  journalDir: string,
  submissionId: string,
): string =>
  join(
    journalDir,
    `${createHash("sha256").update(submissionId, "utf8").digest("hex")}.json`,
  );

export type SubmitL2TransferSubmission = {
  readonly submissionId: string;
  readonly journalDir: string;
};

export type JournaledL2TransferResult = SubmitL2TransferResult & {
  readonly submissionId: string;
  readonly signedTxCbor: string;
};

type StoredAssets = Readonly<Record<string, string>>;

/** One journal file; written once and never replaced. */
type StoredTransferSubmission = {
  readonly version: 1;
  readonly submissionId: string;
  /** What the caller asked for; a rerun must ask for the same. */
  readonly intent: {
    readonly senderAddress: string;
    readonly destinationAddress: string;
    readonly requestedAssets: StoredAssets;
    /** Absent in journals written before `--exclude-out-ref`: none. */
    readonly excludedOutRefs: readonly string[];
  };
  readonly transfer: {
    readonly txId: string;
    readonly signedTxCbor: string;
    readonly selectedInputs: readonly string[];
    readonly requestedAssets: StoredAssets;
    readonly changeAssets: StoredAssets;
  };
};

const encodeAssets = (assets: Readonly<Assets>): StoredAssets =>
  Object.fromEntries(
    Object.entries(normalizeAssets({ ...assets }))
      .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
      .map(([unit, amount]) => [unit, BigInt(amount).toString()]),
  );

const decodeAssets = (value: unknown, field: string): Readonly<Assets> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an asset map.`);
  }
  const assets: Record<string, bigint> = {};
  for (const [unit, amount] of Object.entries(value)) {
    if (typeof amount !== "string" || !/^\d+$/.test(amount)) {
      throw new Error(`${field}.${unit} must be a non-negative integer.`);
    }
    assets[unit] = BigInt(amount);
  }
  return assets;
};

const sameAssets = (left: StoredAssets, right: StoredAssets): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

const requireStringArray = (
  value: unknown,
  field: string,
): readonly string[] => {
  if (
    !Array.isArray(value) ||
    !value.every((item) => typeof item === "string")
  ) {
    throw new Error(`${field} must be a string array.`);
  }
  return value;
};

const requireString = (value: unknown, field: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`${field} must be a non-empty string.`);
  }
  return value;
};

/**
 * Validates a journal file, including that its signed bytes hash to its
 * recorded transaction ID, so a damaged file is never resubmitted or
 * reported under another transaction's ID.
 */
const parseStoredTransferSubmission = (
  text: string,
): StoredTransferSubmission => {
  const parsed = JSON.parse(text) as Partial<StoredTransferSubmission>;
  if (parsed.version !== 1) {
    throw new Error("unsupported journal version.");
  }
  const intent = parsed.intent;
  const transfer = parsed.transfer;
  if (intent === undefined || transfer === undefined) {
    throw new Error("intent and transfer are required.");
  }
  const stored: StoredTransferSubmission = {
    version: 1,
    submissionId: requireString(parsed.submissionId, "submissionId"),
    intent: {
      senderAddress: requireString(
        intent.senderAddress,
        "intent.senderAddress",
      ),
      destinationAddress: requireString(
        intent.destinationAddress,
        "intent.destinationAddress",
      ),
      requestedAssets: encodeAssets(
        decodeAssets(intent.requestedAssets, "intent.requestedAssets"),
      ),
      excludedOutRefs: requireStringArray(
        (intent as { readonly excludedOutRefs?: unknown }).excludedOutRefs ??
          [],
        "intent.excludedOutRefs",
      ),
    },
    transfer: {
      txId: requireString(transfer.txId, "transfer.txId"),
      signedTxCbor: requireString(
        transfer.signedTxCbor,
        "transfer.signedTxCbor",
      ),
      selectedInputs: requireStringArray(
        transfer.selectedInputs,
        "transfer.selectedInputs",
      ),
      requestedAssets: encodeAssets(
        decodeAssets(transfer.requestedAssets, "transfer.requestedAssets"),
      ),
      changeAssets: encodeAssets(
        decodeAssets(transfer.changeAssets, "transfer.changeAssets"),
      ),
    },
  };
  const computedTxId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(stored.transfer.signedTxCbor, "hex"),
    ),
  ).toString("hex");
  if (computedTxId !== stored.transfer.txId) {
    throw new Error(
      `signed transaction hashes to ${computedTxId}, not the recorded ${stored.transfer.txId}.`,
    );
  }
  return stored;
};

const readStoredTransferSubmission = (
  path: string,
): Effect.Effect<Option.Option<StoredTransferSubmission>, Error> =>
  Effect.tryPromise({
    try: async () => {
      let text: string;
      try {
        text = await readFile(path, "utf8");
      } catch (cause) {
        if ((cause as NodeJS.ErrnoException).code === "ENOENT") {
          return Option.none();
        }
        throw cause;
      }
      return Option.some(parseStoredTransferSubmission(text));
    },
    catch: (cause) =>
      toError(cause, `Invalid L2 transfer submission journal ${path}`),
  });

/** Returns false when another run already published this submission. */
const publishStoredTransferSubmission = (
  path: string,
  stored: StoredTransferSubmission,
): Effect.Effect<boolean, Error> =>
  Effect.tryPromise({
    try: async () => {
      try {
        await writeTextFileAtomicNoReplace(
          path,
          `${JSON.stringify(stored, null, 2)}\n`,
          { mode: 0o600 },
        );
        return true;
      } catch (cause) {
        if ((cause as NodeJS.ErrnoException).code === "EEXIST") return false;
        throw cause;
      }
    },
    catch: (cause) =>
      toError(
        cause,
        `Failed to journal the signed L2 transfer before submitting it to ${path}`,
      ),
  });

const storedFromPrepared = (
  submissionId: string,
  intent: StoredTransferSubmission["intent"],
  prepared: PreparedL2Transfer,
): StoredTransferSubmission => ({
  version: 1,
  submissionId,
  intent,
  transfer: {
    txId: prepared.txId,
    signedTxCbor: prepared.signedTxCbor,
    selectedInputs: prepared.selectedInputs,
    requestedAssets: encodeAssets(prepared.requestedAssets),
    changeAssets: encodeAssets(prepared.changeAssets),
  },
});

const resumeHint = (submissionId: string, txId: string): string =>
  `The signed transfer ${txId} is journaled; rerun submit-l2-transfer with --submission-id ${submissionId} to resume it without signing a new transaction.`;

/**
 * Submits an L2 transfer under a stable submission ID.
 *
 * The first run signs the transfer and publishes it to the journal before any
 * `/submit`. A rerun with the same ID never selects inputs or signs again: if
 * the node already knows the journaled transaction (any `/tx-status` other
 * than `not_found`) it returns the saved result, otherwise it resubmits the
 * exact journaled bytes, which the node answers as a duplicate when it had
 * admitted them after all. A rerun that asks for a different signer,
 * destination, value or set of excluded outputs is refused. A failure that leaves the node's view
 * unknown fails as a `ResumableNativeTransferSubmitError` naming the ID to
 * rerun with. The bytes are fixed once journaled, so the in-process submit
 * retry defaults to the bounded fanout policy.
 */
export const submitJournaledL2TransferProgram = ({
  config,
  submission,
  resolvedWalletSeedPhrase,
  assertWalletAddress,
  apiSubmitRetryPolicy,
}: {
  readonly config: SubmitL2TransferConfig;
  readonly submission: SubmitL2TransferSubmission;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
  readonly apiSubmitRetryPolicy?: NativeTransferSubmitRetryPolicy;
}): Effect.Effect<
  JournaledL2TransferResult,
  Error,
  NodeConfigService | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const { submissionId } = submission;
    const sender = yield* resolveL2TransferSenderProgram({
      config,
      resolvedWalletSeedPhrase,
      assertWalletAddress,
    });
    const intent: StoredTransferSubmission["intent"] = {
      senderAddress: sender.senderAddress,
      destinationAddress: config.l2Address,
      requestedAssets: encodeAssets(buildRequestedAssets(config)),
      excludedOutRefs: config.excludedOutRefs,
    };
    const path = transferSubmissionJournalPath(
      submission.journalDir,
      submissionId,
    );

    let stored = yield* readStoredTransferSubmission(path);
    let resumed = Option.isSome(stored);
    if (Option.isNone(stored)) {
      const prepared = yield* buildL2TransferForSenderProgram({
        config,
        sender,
        walletSeedSource: resolvedWalletSeedPhrase.resolvedFrom,
      });
      const fresh = storedFromPrepared(submissionId, intent, prepared);
      if (yield* publishStoredTransferSubmission(path, fresh)) {
        stored = Option.some(fresh);
      } else {
        // A concurrent run with this ID published first; its transaction is
        // the one this ID stands for.
        stored = yield* readStoredTransferSubmission(path);
        resumed = true;
      }
    }
    if (Option.isNone(stored)) {
      return yield* Effect.fail(
        new Error(`L2 transfer submission journal ${path} disappeared.`),
      );
    }
    const saved = stored.value;
    if (
      saved.submissionId !== submissionId ||
      saved.intent.senderAddress !== intent.senderAddress ||
      saved.intent.destinationAddress !== intent.destinationAddress ||
      !sameAssets(saved.intent.requestedAssets, intent.requestedAssets) ||
      saved.intent.excludedOutRefs.join(",") !==
        intent.excludedOutRefs.join(",")
    ) {
      return yield* Effect.fail(
        new Error(
          `Submission ID ${submissionId} belongs to a different transfer (signer, destination, value or excluded outputs); use a new --submission-id for a new transfer.`,
        ),
      );
    }

    const { txId, signedTxCbor } = saved.transfer;
    const result = (status: string): JournaledL2TransferResult => ({
      submissionId,
      txId,
      status,
      senderAddress: saved.intent.senderAddress,
      destinationAddress: saved.intent.destinationAddress,
      selectedInputs: saved.transfer.selectedInputs,
      requestedAssets: decodeAssets(
        saved.transfer.requestedAssets,
        "transfer.requestedAssets",
      ),
      changeAssets: decodeAssets(
        saved.transfer.changeAssets,
        "transfer.changeAssets",
      ),
      walletSeedSource: resolvedWalletSeedPhrase.resolvedFrom,
      nodeEndpoint: config.nodeEndpoint,
      signedTxCbor,
    });

    if (resumed) {
      const knownStatus = yield* Effect.tryPromise({
        try: () =>
          fetchNodeTxStatus(
            config.nodeEndpoint,
            txId,
            config.submitRequestTimeoutMs,
          ),
        catch: (cause) =>
          new ResumableNativeTransferSubmitError(
            `Failed to read /tx-status for journaled L2 transfer ${txId}: ${String(cause)}. ${resumeHint(submissionId, txId)}`,
          ),
      });
      if (knownStatus !== "not_found") {
        return result(knownStatus);
      }
    }

    const submitted = yield* submitNativeTransferTx(
      config.nodeEndpoint,
      signedTxCbor,
      txId,
      config.submitRequestTimeoutMs,
      apiSubmitRetryPolicy ?? FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
    ).pipe(
      Effect.mapError((error) =>
        error instanceof ResumableNativeTransferSubmitError
          ? new ResumableNativeTransferSubmitError(
              `${error.message}. ${resumeHint(submissionId, txId)}`,
            )
          : error,
      ),
    );
    return result(submitted.status);
  });

/**
 * The `submit-l2-transfer` command: journaled when a submission ID is given,
 * otherwise a one-shot build, sign and submit.
 */
export const submitL2TransferCommandProgram = ({
  config,
  submission,
  resolvedWalletSeedPhrase,
  assertWalletAddress,
  apiSubmitRetryPolicy,
}: {
  readonly config: SubmitL2TransferConfig;
  readonly submission?: SubmitL2TransferSubmission;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
  readonly apiSubmitRetryPolicy?: NativeTransferSubmitRetryPolicy;
}): Effect.Effect<
  SubmitL2TransferResult | JournaledL2TransferResult,
  Error,
  NodeConfigService | ContractDeploymentIdentity
> =>
  submission === undefined
    ? submitL2TransferProgram({
        config,
        resolvedWalletSeedPhrase,
        assertWalletAddress,
        apiSubmitRetryPolicy,
      })
    : submitJournaledL2TransferProgram({
        config,
        submission,
        resolvedWalletSeedPhrase,
        assertWalletAddress,
        apiSubmitRetryPolicy,
      });
