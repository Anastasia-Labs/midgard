import { FrontierPeakSchema, hashHexWithBlake2b } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const MISSING_SCRIPT_SOURCE_SCAN_BUDGET = 24 as const;

export type MissingScriptSourceUniverseSource = Readonly<{
  sourceIndex: number;
  locationKind: 0 | 1;
  scriptHashHex: string;
  sourceKeyHex: string;
  itemCommitmentHex: string;
}>;

/** Output shape consumed from the frozen transaction-derived script universe. */
export type MissingScriptSourceUniverseResult = Readonly<{
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  requiredScriptHashHex: string;
  sources: readonly MissingScriptSourceUniverseSource[];
}>;

export type BoundMissingScriptSourceUniverse =
  MissingScriptSourceUniverseResult & Readonly<{ universeIdentityHex: string }>;

export type MissingScriptSourceScanCheckpoint = Readonly<{
  universeIdentityHex: string;
  cursor: number;
  totalCount: number;
  found: boolean;
  nextExpectedScriptHashHex: string;
  checkpointHashHex: string;
}>;

const fail = (message: string): never => {
  throw new Error(`missingScriptSource: ${message}`);
};
const natural = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};
const hex = (value: string, bytes: number, label: string): string => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value))
    return fail(`${label} must be canonical ${bytes.toString()}-byte hex`);
  return value;
};
const u32 = (value: number, label: string): Buffer => {
  natural(value, label);
  if (value > 0xffff_ffff) return fail(`${label} exceeds uint32`);
  const encoded = Buffer.alloc(4);
  encoded.writeUInt32BE(value);
  return encoded;
};
const sized = (value: Buffer): Buffer => {
  if (value.length > 0xffff) return fail("source key exceeds uint16");
  const size = Buffer.alloc(2);
  size.writeUInt16BE(value.length);
  return Buffer.concat([size, value]);
};
const digest = (bytes: Buffer): string =>
  Effect.runSync(hashHexWithBlake2b(bytes.toString("hex"), 32));

const aikenSerialise = <T>(value: T, schema: Parameters<typeof Data.to>[1]) =>
  Buffer.from(Data.to(value as never, schema as never), "hex");
const ONCHAIN_CHECKPOINT_DOMAIN = Buffer.from(
  "midgard/fraud-proofs/missing-script-source/checkpoint-v1",
  "ascii",
);

/** Byte-for-byte mirror of `source_identity_hash_v1` in the validator. */
export const missingScriptSourceOnchainSourceIdentity = ({
  priorLedgerRootHex,
  sourceCount,
  scanLimit,
  sourcePeaks,
  transactionSourceCount,
  resolvedReferenceSourceCount,
}: {
  priorLedgerRootHex: string;
  sourceCount: bigint;
  scanLimit: bigint;
  sourcePeaks: readonly { height: bigint; hash: string }[];
  transactionSourceCount: bigint;
  resolvedReferenceSourceCount: bigint;
}): string =>
  digest(
    Buffer.concat([
      ONCHAIN_CHECKPOINT_DOMAIN,
      Buffer.from(hex(priorLedgerRootHex, 32, "prior ledger root"), "hex"),
      aikenSerialise(sourceCount, Data.Integer()),
      aikenSerialise(scanLimit, Data.Integer()),
      aikenSerialise(sourcePeaks, Data.Array(FrontierPeakSchema)),
      aikenSerialise(transactionSourceCount, Data.Integer()),
      aikenSerialise(resolvedReferenceSourceCount, Data.Integer()),
    ]),
  );

/** Byte-for-byte mirror of `checkpoint_v1` in the validator. */
export const missingScriptSourceOnchainCheckpoint = ({
  sourceIdentityHex,
  cursor,
  found,
  nextExpectedScriptHashHex,
}: {
  sourceIdentityHex: string;
  cursor: bigint;
  found: boolean;
  nextExpectedScriptHashHex: string;
}): string =>
  digest(
    Buffer.concat([
      ONCHAIN_CHECKPOINT_DOMAIN,
      Buffer.from(hex(sourceIdentityHex, 32, "source identity"), "hex"),
      aikenSerialise(cursor, Data.Integer()),
      aikenSerialise(found, Data.Boolean()),
      aikenSerialise(
        hex(nextExpectedScriptHashHex, 28, "next script hash"),
        Data.Bytes(),
      ),
    ]),
  );

export const bindMissingScriptSourceUniverse = (
  result: MissingScriptSourceUniverseResult,
): BoundMissingScriptSourceUniverse => {
  if (![0, 1, 2, 3].includes(result.purposeKind))
    return fail("purpose kind is outside consensus order");
  natural(result.purposeIndex, "purpose index");
  hex(result.requiredScriptHashHex, 28, "required script hash");
  const sourceBytes = result.sources.map((source, sourceIndex) => {
    if (source.sourceIndex !== sourceIndex)
      return fail("script universe source frontier is incomplete or reordered");
    if (![0, 1].includes(source.locationKind))
      return fail("source location is outside witness/resolved frontiers");
    return Buffer.concat([
      u32(sourceIndex, "source index"),
      Buffer.from([source.locationKind]),
      Buffer.from(hex(source.scriptHashHex, 28, "source script hash"), "hex"),
      sized(Buffer.from(source.sourceKeyHex, "hex")),
      Buffer.from(hex(source.itemCommitmentHex, 32, "item commitment"), "hex"),
    ]);
  });
  const universeIdentityHex = digest(
    Buffer.concat([
      Buffer.from("midgard/missing-script-source/universe-v1", "ascii"),
      Buffer.from([result.purposeKind]),
      u32(result.purposeIndex, "purpose index"),
      Buffer.from(result.requiredScriptHashHex, "hex"),
      u32(result.sources.length, "source count"),
      ...sourceBytes,
    ]),
  );
  return Object.freeze({
    ...result,
    sources: Object.freeze(
      result.sources.map((source) => Object.freeze({ ...source })),
    ),
    universeIdentityHex,
  });
};

export const missingScriptSourceScanCheckpointHash = ({
  universeIdentityHex,
  cursor,
  totalCount,
  found,
  nextExpectedScriptHashHex,
}: Omit<MissingScriptSourceScanCheckpoint, "checkpointHashHex">): string =>
  digest(
    Buffer.concat([
      Buffer.from("midgard/missing-script-source/scan-checkpoint-v1", "ascii"),
      Buffer.from(hex(universeIdentityHex, 32, "universe identity"), "hex"),
      u32(cursor, "scan cursor"),
      u32(totalCount, "scan total"),
      Buffer.from([found ? 1 : 0]),
      Buffer.from(
        hex(nextExpectedScriptHashHex, 28, "next script hash"),
        "hex",
      ),
    ]),
  );

const checkpoint = (
  value: Omit<MissingScriptSourceScanCheckpoint, "checkpointHashHex">,
): MissingScriptSourceScanCheckpoint =>
  Object.freeze({
    ...value,
    checkpointHashHex: missingScriptSourceScanCheckpointHash(value),
  });

export const initialMissingScriptSourceScan = (
  universe: BoundMissingScriptSourceUniverse,
  nextExpectedScriptHashHex: string,
): MissingScriptSourceScanCheckpoint =>
  checkpoint({
    universeIdentityHex: universe.universeIdentityHex,
    cursor: 0,
    totalCount: universe.sources.length,
    found: false,
    nextExpectedScriptHashHex,
  });

export const advanceMissingScriptSourceScan = ({
  universe,
  prior,
  itemBudget = MISSING_SCRIPT_SOURCE_SCAN_BUDGET,
  scanScriptHashHex,
  finalScriptHashHex,
}: {
  universe: BoundMissingScriptSourceUniverse;
  prior: MissingScriptSourceScanCheckpoint;
  itemBudget?: number;
  scanScriptHashHex: string;
  finalScriptHashHex: string;
}): MissingScriptSourceScanCheckpoint => {
  natural(itemBudget, "item budget");
  if (itemBudget === 0 || itemBudget > MISSING_SCRIPT_SOURCE_SCAN_BUDGET)
    return fail("item budget is outside the frozen scan bound");
  if (
    prior.universeIdentityHex !== universe.universeIdentityHex ||
    prior.totalCount !== universe.sources.length ||
    prior.checkpointHashHex !== missingScriptSourceScanCheckpointHash(prior)
  )
    return fail("source scan checkpoint or universe identity was substituted");
  let cursor = prior.cursor;
  let found = prior.found;
  const stop = Math.min(universe.sources.length, cursor + itemBudget);
  while (cursor < stop) {
    const source = universe.sources[cursor];
    if (source === undefined || source.sourceIndex !== cursor)
      return fail("source scan cursor no longer names the complete frontier");
    found ||= source.scriptHashHex === universe.requiredScriptHashHex;
    cursor += 1;
  }
  return checkpoint({
    universeIdentityHex: universe.universeIdentityHex,
    cursor,
    totalCount: universe.sources.length,
    found,
    nextExpectedScriptHashHex:
      cursor === universe.sources.length
        ? finalScriptHashHex
        : scanScriptHashHex,
  });
};

export const missingScriptSourceScanIsComplete = (
  state: MissingScriptSourceScanCheckpoint,
): boolean => state.cursor === state.totalCount;
