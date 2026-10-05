import {
  lstat,
  mkdir,
  readdir,
  readFile,
  realpath,
  unlink,
} from "node:fs/promises";
import { dirname, join } from "node:path";

import type { WatcherFinalityPolicy } from "../l1/finality-engine.js";
import {
  publishExclusiveFile,
  stagedRecordPath,
  syncDirectory,
} from "../storage/exclusive-record-file.js";
import {
  assertAuthorityGeometry,
  AUTHORITY_DATABASE_FILE,
  AUTHORITY_ENVELOPE_MAX_BYTES,
  AUTHORITY_MAX_LIVE_RECORDS,
  AUTHORITY_SELECTOR_FILE,
  authorityEnvelopeCodec,
} from "./trusted-head-authority.envelope-codec.js";
import {
  canonicalDirectory,
  exactRecord,
  parseJson,
  sameHead,
  sha256,
} from "./trusted-head-authority.exact-record.js";
import { auditLegacyAuthority } from "./trusted-head-authority.legacy-audit.js";
import { authorityRecordCodec } from "./trusted-head-authority.record-codec.js";
import { createSqliteAuthority } from "./trusted-head-authority.sqlite-initialization.js";
import {
  isUncommittedAuthorityDatabase,
  openSqliteAuthority,
  type SqliteAuthorityStore,
} from "./trusted-head-authority.sqlite-store.js";

export type AuthorityStorageInput = Readonly<{
  directory: string;
  policy: WatcherFinalityPolicy;
  recordAuthenticationKey: Uint8Array;
  liveRecordLimit: number;
}>;
const regularFile = async (path: string, maximumBytes?: number) => {
  const info = await lstat(path);
  if (
    !info.isFile() ||
    (maximumBytes !== undefined &&
      (info.size < 1 || info.size > maximumBytes)) ||
    (await realpath(path)) !== path
  )
    throw new Error("trusted-head authority selected file identity invalid");
};
const existingDirectory = async (path: string) => {
  const info = await lstat(path);
  if (!info.isDirectory() || (await realpath(path)) !== path)
    throw new Error("trusted-head authority directory traverses a symlink");
};
const selected = async (input: AuthorityStorageInput) => {
  const directory = canonicalDirectory(input.directory);
  await existingDirectory(directory);
  const selectorPath = join(directory, AUTHORITY_SELECTOR_FILE);
  await regularFile(selectorPath, AUTHORITY_ENVELOPE_MAX_BYTES);
  const selectorBytes = await readFile(selectorPath);
  const raw = exactRecord(parseJson(selectorBytes), [
    "schemaVersion",
    "policyDigest",
    "deploymentMarker",
    "recordAuthenticationKeyId",
    "generation",
    "liveRecordLimit",
    "payload",
    "envelopeMac",
  ]);
  if (raw === null || typeof raw.generation !== "string")
    throw new Error("trusted-head authority selector structure invalid");
  const records = authorityRecordCodec(input),
    envelopes = authorityEnvelopeCodec({
      records,
      generation: raw.generation,
      liveRecordLimit: input.liveRecordLimit,
    });
  const payload = envelopes.decode("selector", selectorBytes, [
    "initializationSha256",
  ]);
  if (
    typeof payload.initializationSha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(payload.initializationSha256)
  )
    throw new Error("trusted-head authority selector receipt invalid");
  const generationDirectory = join(directory, envelopes.generation);
  await existingDirectory(generationDirectory);
  const databasePath = join(generationDirectory, AUTHORITY_DATABASE_FILE);
  await regularFile(databasePath);
  for (const suffix of ["-wal", "-shm"]) {
    try {
      await regularFile(databasePath + suffix);
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
    }
  }
  return {
    records,
    envelopes,
    databasePath,
    initializationSha256: payload.initializationSha256,
  };
};

/** Open an already selected backend. Missing state never initializes or falls back. */
export const openSelectedAuthorityStore = async (
  input: AuthorityStorageInput,
): Promise<SqliteAuthorityStore> => openSqliteAuthority(await selected(input));

export type AuthorityInitializationInput = AuthorityStorageInput &
  Readonly<{
    /** Stable attempt identity, persisted by the offline provisioning caller. */
    generation: string;
  }>;

/** Existing names may be left by a process that never received directory-fsync
 * acknowledgement. Explicit preparation retries must discharge that debt. */
const syncSelectedNamespace = async (directory: string, generation: string) => {
  await syncDirectory(join(directory, generation));
  await syncDirectory(directory);
  await syncDirectory(dirname(directory));
};

const prepare = async (
  input: AuthorityInitializationInput,
  source: Awaited<ReturnType<typeof auditLegacyAuthority>> | null,
  beforeSelection: () => Promise<void>,
) => {
  const directory = canonicalDirectory(input.directory);
  const records = authorityRecordCodec(input),
    envelopes = authorityEnvelopeCodec({
      records,
      generation: input.generation,
      liveRecordLimit: input.liveRecordLimit,
    });
  assertAuthorityGeometry(input.generation, input.liveRecordLimit);
  const initialization = {
    sourceKind: source === null ? "new" : "legacy",
    initialHead: source?.head ?? null,
    initialRecordSha256: source?.recordSha256 ?? null,
    sourceChainSha256: source?.sourceChainSha256 ?? null,
  };
  const initializationBytes = envelopes.encode(
    "initialization",
    initialization,
  );
  const initializationSha256 = sha256(initializationBytes);

  await existingDirectory(dirname(directory));
  try {
    await mkdir(directory, { mode: 0o700 });
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
  }
  await existingDirectory(directory);
  await syncDirectory(dirname(directory));
  // Existing selection is resolved as the same attempt only; never replace it.
  try {
    const already = await selected(input);
    if (
      already.envelopes.generation !== input.generation ||
      already.initializationSha256 !== initializationSha256
    )
      throw new Error(
        "trusted-head authority backend already selected by a different initialization",
      );
    const store = openSqliteAuthority(already);
    store.close();
    await syncSelectedNamespace(directory, input.generation);
    return;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  // A present selector naming a missing DB is corruption, not an unselected attempt.
  try {
    await lstat(join(directory, AUTHORITY_SELECTOR_FILE));
    throw new Error("trusted-head authority selected state is incomplete");
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  const generationDirectory = join(directory, input.generation);
  try {
    await mkdir(generationDirectory, { mode: 0o700 });
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
  }
  await existingDirectory(generationDirectory);
  await syncDirectory(directory);
  const databasePath = join(generationDirectory, AUTHORITY_DATABASE_FILE);
  const intentPath = join(generationDirectory, "initialization-intent.json");
  try {
    await regularFile(intentPath, AUTHORITY_ENVELOPE_MAX_BYTES);
    const retainedIntent = await readFile(intentPath);
    envelopes.decode("initialization", retainedIntent, [
      "sourceKind",
      "initialHead",
      "initialRecordSha256",
      "sourceChainSha256",
    ]);
    if (sha256(retainedIntent) !== initializationSha256)
      throw new Error(
        "trusted-head authority retained initialization intent differs",
      );
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
    await publishExclusiveFile({
      stagingPath: stagedRecordPath(generationDirectory),
      path: intentPath,
      bytes: initializationBytes,
    });
    await syncDirectory(generationDirectory);
  }
  // This offline caller owns this generation exclusively. The signed intent
  // proves what may be rebuilt; no selected state or nonempty schema is deleted.
  try {
    await regularFile(databasePath);
    if (isUncommittedAuthorityDatabase(databasePath)) {
      for (const suffix of ["", "-wal", "-shm"]) {
        try {
          await regularFile(databasePath + suffix);
          await unlink(databasePath + suffix);
        } catch (error) {
          if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
        }
      }
      await syncDirectory(generationDirectory);
    }
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }

  try {
    await regularFile(databasePath);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
    createSqliteAuthority({
      databasePath,
      envelopes,
      initialRecords: source?.initialRecords ?? [],
      boundaryRecord: source?.boundaryRecord ?? null,
      initialization,
      head: source?.head ?? null,
      recordSha256: source?.recordSha256 ?? null,
    });
  }
  // Flush/close and reopen before selecting. A nonempty invalid staging schema
  // remains held; it is never rewritten or promoted as an initialization.
  const verified = openSqliteAuthority({
    databasePath,
    records,
    envelopes,
    initializationSha256,
  });
  try {
    if (!sameHead(await verified.readCurrent(), source?.head ?? null))
      throw new Error("trusted-head authority import readback differs");
  } finally {
    verified.close();
  }
  await beforeSelection();
  await syncDirectory(generationDirectory);
  const selectorBytes = envelopes.encode("selector", { initializationSha256 });
  try {
    await publishExclusiveFile({
      stagingPath: stagedRecordPath(directory),
      path: join(directory, AUTHORITY_SELECTOR_FILE),
      bytes: selectorBytes,
    });
    await syncSelectedNamespace(directory, input.generation);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
    const winner = await selected(input);
    if (
      winner.envelopes.generation !== input.generation ||
      winner.initializationSha256 !== initializationSha256
    )
      throw new Error(
        "trusted-head authority selection lost to another generation",
      );
    const store = openSqliteAuthority(winner);
    store.close();
    await syncSelectedNamespace(directory, input.generation);
  }
};

/** Explicit provisioning only. Caller must own the independent new volume. */
export const initializeSelectedAuthorityStore = async (
  input: AuthorityInitializationInput,
): Promise<void> => {
  canonicalDirectory(input.directory);
  assertAuthorityGeometry(input.generation, input.liveRecordLimit);
  // Reject legacy files/other generations. Same generation is the only resume path.
  try {
    const names = await readdir(input.directory);
    if (
      names.some(
        (name) =>
          name !== input.generation &&
          name !== AUTHORITY_SELECTOR_FILE &&
          !/^\.staged-[0-9a-f-]+\.tmp$/u.test(name),
      )
    )
      throw new Error(
        "trusted-head authority initialization requires a genuinely new deployment",
      );
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  await prepare(input, null, async () => {});
};

/** Offline/quiescent import, not a live writer fence. All old writers must be
 * stopped and prevented from restarting across both audit and selection. */
export const importLegacyAuthorityStore = async (
  input: AuthorityInitializationInput & Readonly<{ legacyDirectory: string }>,
): Promise<void> => {
  canonicalDirectory(input.directory);
  assertAuthorityGeometry(input.generation, input.liveRecordLimit);
  const legacyDirectory = canonicalDirectory(input.legacyDirectory);
  if (legacyDirectory === input.directory)
    throw new Error(
      "trusted-head authority legacy archive must be separate from selected backend",
    );
  const records = authorityRecordCodec(input),
    source = await auditLegacyAuthority({
      directory: legacyDirectory,
      records,
      liveRecordLimit: input.liveRecordLimit,
    });
  await prepare(input, source, async () => {
    const latest = await auditLegacyAuthority({
      directory: legacyDirectory,
      records,
      liveRecordLimit: input.liveRecordLimit,
    });
    if (
      latest.sourceChainSha256 !== source.sourceChainSha256 ||
      !sameHead(latest.head, source.head)
    )
      throw new Error(
        "trusted-head authority legacy source changed before selection",
      );
  });
};

export const auditLegacyWatcherTrustedHeadAuthority = async (
  input: AuthorityStorageInput,
) => {
  canonicalDirectory(input.directory);
  if (
    !Number.isSafeInteger(input.liveRecordLimit) ||
    input.liveRecordLimit < 1 ||
    input.liveRecordLimit > AUTHORITY_MAX_LIVE_RECORDS
  )
    throw new Error(
      "trusted-head legacy audit requires supported liveRecordLimit",
    );
  return await auditLegacyAuthority({
    directory: input.directory,
    records: authorityRecordCodec(input),
    liveRecordLimit: input.liveRecordLimit,
  });
};
