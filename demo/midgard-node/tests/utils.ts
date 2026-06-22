import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { type Address, Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import * as LedgerUtils from "@/database/utils/ledger.js";
import { NodeConfig } from "@/services/config.js";
import { Database } from "@/services/database.js";

const explicitPostgresDb =
  process.env.POSTGRES_DB !== undefined && process.env.POSTGRES_DB !== "";

const TEST_ENV_DEFAULTS: Record<string, string> = {
  L1_PROVIDER: "Kupmios",
  L1_OGMIOS_KEY: "http://127.0.0.1:1337",
  L1_KUPO_KEY: "http://127.0.0.1:1442",
  L1_OPERATOR_SEED_PHRASE:
    "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX:
    "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
  L1_REFERENCE_SCRIPT_SEED_PHRASE:
    "cactus chalk grit reopen true slight whale sand law sibling silver fringe cement twist process bracket history leopard churn federal coral three hockey fossil",
  L1_REFERENCE_SCRIPT_ADDRESS:
    "addr_test1qpdjresrrk294hy9ndtqly955ldlhy688507shkfxpwtgf39vzk9uwp87k96zkd5yal83h9x0qheeu0lrqp9lldvsqjs5s4ggd",
  NETWORK: "Preprod",
  POSTGRES_HOST: "127.0.0.1",
  POSTGRES_PORT: "5433",
  POSTGRES_USER: "postgres",
  POSTGRES_PASSWORD: "postgres",
  POSTGRES_DB: "midgard_test",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_A:
    "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_B:
    "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_C:
    "cactus chalk grit reopen true slight whale sand law sibling silver fringe cement twist process bracket history leopard churn federal coral three hockey fossil",
};

for (const [key, value] of Object.entries(TEST_ENV_DEFAULTS)) {
  if (process.env[key] === undefined || process.env[key] === "") {
    process.env[key] = value;
  }
}

if (
  process.env.POSTGRES_DB === "midgard" &&
  process.env.MIDGARD_ALLOW_TEST_DATABASE_MIDGARD !== "1" &&
  process.env.CI !== "true"
) {
  throw new Error(
    explicitPostgresDb
      ? "Refusing to run Midgard tests against POSTGRES_DB=midgard. Use an isolated test database, or set MIDGARD_ALLOW_TEST_DATABASE_MIDGARD=1 only for an intentionally disposable local environment."
      : "Refusing to default Midgard tests to POSTGRES_DB=midgard. Use an isolated test database.",
  );
}

export const provideDatabaseLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(Effect.provide(Database.layer), Effect.provide(NodeConfig.layer));

export const deterministicFixtureBytes = (
  label: string,
  length: number,
): Buffer => {
  const chunks: Buffer[] = [];
  let bytesGenerated = 0;
  for (let counter = 0; bytesGenerated < length; counter += 1) {
    const chunk = createHash("sha256")
      .update("midgard-node-test-fixture")
      .update("\0")
      .update(label)
      .update("\0")
      .update(counter.toString())
      .digest();
    chunks.push(chunk);
    bytesGenerated += chunk.length;
  }
  return Buffer.concat(chunks).subarray(0, length);
};

export const deterministicFixtureTxHash = (label: string): Buffer =>
  deterministicFixtureBytes(`tx-hash:${label}`, 32);

export const deterministicFixtureOutputReference = (
  label: string,
  outputIndex: number | bigint = 0n,
): SDK.OutputReference => ({
  transactionId: deterministicFixtureTxHash(
    `output-reference:${label}`,
  ).toString("hex"),
  outputIndex: BigInt(outputIndex),
});

export const deterministicFixtureOutputReferenceId = (
  label: string,
  outputIndex: number | bigint = 0n,
): Buffer =>
  Buffer.from(
    LucidData.to(
      deterministicFixtureOutputReference(label, outputIndex),
      SDK.OutputReference,
    ),
    "hex",
  );

type LedgerLikeEntry = {
  readonly [LedgerUtils.Columns.TX_ID]: Buffer;
  readonly [LedgerUtils.Columns.OUTREF]: Buffer;
  readonly [LedgerUtils.Columns.OUTPUT]: Buffer;
  readonly [LedgerUtils.Columns.ADDRESS]: Address;
};

const withoutLedgerTimestamp = (
  entry: LedgerLikeEntry,
): LedgerUtils.EntryNoTimeStamp => ({
  [LedgerUtils.Columns.TX_ID]: entry[LedgerUtils.Columns.TX_ID],
  [LedgerUtils.Columns.OUTREF]: entry[LedgerUtils.Columns.OUTREF],
  [LedgerUtils.Columns.OUTPUT]: entry[LedgerUtils.Columns.OUTPUT],
  [LedgerUtils.Columns.ADDRESS]: entry[LedgerUtils.Columns.ADDRESS],
});

const sortLedgerEntries = (
  entries: readonly LedgerUtils.EntryNoTimeStamp[],
): LedgerUtils.EntryNoTimeStamp[] =>
  [...entries].sort((left, right) =>
    Buffer.compare(
      left[LedgerUtils.Columns.OUTREF],
      right[LedgerUtils.Columns.OUTREF],
    ),
  );

export const expectLedgerUtxos = (
  actual: readonly LedgerLikeEntry[],
  expected: readonly LedgerLikeEntry[],
): void => {
  expect(
    sortLedgerEntries(actual.map((entry) => withoutLedgerTimestamp(entry))),
  ).toStrictEqual(
    sortLedgerEntries(expected.map((entry) => withoutLedgerTimestamp(entry))),
  );
};
