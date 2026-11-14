/**
 * The local crash record of one submission id: a create-once `intent.json`
 * and immutable `attempt-NNNN.json` files, each written with the core
 * no-replace write (temp file, fsync, link). Nothing here is ever overwritten
 * or deleted; the latest attempt is the highest-numbered file. The seed is
 * never written.
 *
 *   <root>/<manifestId>/<sha256(kind ":" submissionId)>/   (0700)
 *     intent.json        0600
 *     attempt-0001.json  0600
 */
import { createHash } from "node:crypto";
import { mkdir, readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { writeTextFileAtomicNoReplace } from "@al-ft/midgard-core/atomic-write";
import { outRefLabel, parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  assertUserSubmissionAttempt,
  outputReferenceToPlutusDataCbor,
  type UserSubmissionAttempt,
  type UserSubmissionStore,
} from "@al-ft/midgard-sdk";

import { refuse } from "./options.js";

export type SubmissionKind = "Deposit" | "Withdrawal";

const FILE_MODE = 0o600;
const DIRECTORY_MODE = 0o700;
const ATTEMPT_FILE = /^attempt-(\d{4,})\.json$/u;

export const recordDirectory = (
  root: string,
  manifestId: string,
  kind: SubmissionKind,
  submissionId: string,
): string =>
  join(
    root,
    manifestId,
    createHash("sha256").update(`${kind}:${submissionId}`).digest("hex"),
  );

const attemptFileName = (attempt: number): string =>
  `attempt-${attempt.toString().padStart(4, "0")}.json`;

const isAlreadyExists = (cause: unknown): boolean =>
  (cause as { code?: unknown } | null)?.code === "EEXIST";

const isMissing = (cause: unknown): boolean =>
  (cause as { code?: unknown } | null)?.code === "ENOENT";

const invalid = (path: string, reason: string): never =>
  refuse(`Record file ${path} is invalid: ${reason}`);

const readJson = async (path: string): Promise<unknown> => {
  const text = await readFile(path, "utf8");
  try {
    return JSON.parse(text);
  } catch (cause) {
    return invalid(path, `not JSON (${String(cause)})`);
  }
};

/** Create-once, 0600, in a directory chain created 0700. EEXIST: another
 * run (or an earlier one) already wrote it. */
const createOnce = async (
  directory: string,
  name: string,
  value: unknown,
): Promise<"created" | "exists"> => {
  await mkdir(directory, { recursive: true, mode: DIRECTORY_MODE });
  try {
    await writeTextFileAtomicNoReplace(
      join(directory, name),
      `${JSON.stringify(value, null, 2)}\n`,
      { mode: FILE_MODE },
    );
    return "created";
  } catch (cause) {
    if (isAlreadyExists(cause)) return "exists";
    throw cause;
  }
};

export type IntentRecord = {
  readonly version: 1;
  readonly kind: SubmissionKind;
  readonly submissionId: string;
  readonly manifestId: string;
  readonly hubPolicyId: string;
  readonly walletAddress: string;
  readonly intentHash: string;
  readonly intent: Readonly<Record<string, unknown>>;
};

const INTENT_FILE = "intent.json";

const parseIntent = (path: string, value: unknown): IntentRecord => {
  const record = value as Partial<IntentRecord> | null;
  if (
    typeof record !== "object" ||
    record === null ||
    record.version !== 1 ||
    (record.kind !== "Deposit" && record.kind !== "Withdrawal") ||
    typeof record.submissionId !== "string" ||
    typeof record.manifestId !== "string" ||
    typeof record.hubPolicyId !== "string" ||
    typeof record.walletAddress !== "string" ||
    typeof record.intentHash !== "string" ||
    typeof record.intent !== "object" ||
    record.intent === null
  )
    return invalid(path, "not a version 1 intent record");
  return record as IntentRecord;
};

export const readIntent = async (
  directory: string,
): Promise<IntentRecord | undefined> => {
  const path = join(directory, INTENT_FILE);
  let value: unknown;
  try {
    value = await readJson(path);
  } catch (cause) {
    if (isMissing(cause)) return undefined;
    throw cause;
  }
  return parseIntent(path, value);
};

/** Writes the intent unless one exists; returns the one on disk either way. */
export const createIntent = async (
  directory: string,
  record: IntentRecord,
): Promise<IntentRecord> => {
  if ((await createOnce(directory, INTENT_FILE, record)) === "created")
    return record;
  const existing = await readIntent(directory);
  if (existing === undefined)
    throw new Error(`${join(directory, INTENT_FILE)} vanished after EEXIST`);
  return existing;
};

/** The same request: same kind, id, deployment, wallet and intent hash. */
export const requireSameIntent = (
  existing: IntentRecord,
  wanted: Pick<
    IntentRecord,
    "kind" | "submissionId" | "manifestId" | "walletAddress" | "intentHash"
  >,
): void => {
  if (
    existing.kind !== wanted.kind ||
    existing.submissionId !== wanted.submissionId ||
    existing.manifestId !== wanted.manifestId ||
    existing.walletAddress !== wanted.walletAddress ||
    existing.intentHash !== wanted.intentHash
  )
    refuse(
      `--submission-id ${JSON.stringify(wanted.submissionId)} already names a different request.`,
    );
};

type AttemptFile = {
  readonly version: 1;
  readonly attempt: number;
  readonly phase: UserSubmissionAttempt["phase"];
  readonly nonce: string;
  readonly eventId: string;
  readonly publication: string | null;
  readonly signedTxCbor: string;
  readonly txHash: string;
  readonly ttlSlot: number;
};

const attemptFileOf = (attempt: UserSubmissionAttempt): AttemptFile => ({
  version: 1,
  attempt: attempt.attempt,
  phase: attempt.phase,
  nonce: outRefLabel(attempt.nonce),
  eventId: outputReferenceToPlutusDataCbor(attempt.nonce),
  publication:
    attempt.publication === null ? null : outRefLabel(attempt.publication),
  signedTxCbor: attempt.signedTxCbor,
  txHash: attempt.txHash,
  ttlSlot: attempt.ttlSlot,
});

const parseAttempt = (
  path: string,
  number: number,
  value: unknown,
): UserSubmissionAttempt => {
  const file = value as Partial<AttemptFile> | null;
  if (typeof file !== "object" || file === null || file.version !== 1)
    return invalid(path, "not a version 1 attempt record");
  if (file.attempt !== number)
    return invalid(path, "its attempt number is not the file's number");
  let attempt: UserSubmissionAttempt;
  try {
    attempt = {
      attempt: file.attempt,
      phase: file.phase as UserSubmissionAttempt["phase"],
      nonce: parseOutRefLabel(String(file.nonce)),
      publication:
        file.publication === null
          ? null
          : parseOutRefLabel(String(file.publication)),
      signedTxCbor: String(file.signedTxCbor),
      txHash: String(file.txHash),
      ttlSlot: file.ttlSlot as number,
    };
    assertUserSubmissionAttempt(attempt);
  } catch (cause) {
    return invalid(
      path,
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  if (file.eventId !== outputReferenceToPlutusDataCbor(attempt.nonce))
    return invalid(path, "eventId is not the nonce's OutputReference CBOR");
  return attempt;
};

/** Every recorded attempt, in order, each checked on read. */
export const readAttempts = async (
  directory: string,
): Promise<readonly UserSubmissionAttempt[]> => {
  let names: string[];
  try {
    names = await readdir(directory);
  } catch (cause) {
    if (isMissing(cause)) return [];
    throw cause;
  }
  const numbered = names
    .map((name) => ({ name, match: ATTEMPT_FILE.exec(name) }))
    .filter(({ match }) => match !== null)
    .map(({ name, match }) => ({ name, number: Number(match![1]) }))
    .sort((left, right) => left.number - right.number);
  const attempts: UserSubmissionAttempt[] = [];
  for (const { name, number } of numbered) {
    const path = join(directory, name);
    attempts.push(parseAttempt(path, number, await readJson(path)));
  }
  return attempts;
};

/** The SDK's store over one record directory. */
export const fileSubmissionStore = (
  directory: string,
): UserSubmissionStore => ({
  latest: async () => (await readAttempts(directory)).at(-1),
  append: async (attempt) =>
    (await createOnce(
      directory,
      attemptFileName(attempt.attempt),
      attemptFileOf(attempt),
    )) === "created"
      ? "created"
      : "lost",
});
