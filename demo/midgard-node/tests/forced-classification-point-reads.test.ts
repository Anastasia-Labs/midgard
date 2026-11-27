/**
 * Forced classification reads its pre-state by key from the block's base
 * ledger and derives the ledger mutation steps from a trie over that
 * ledger's nodes. On the forced fixtures in
 * `fixtures/forced-mutation-steps.json` (accepted transactions, a chain of
 * two in one block, Phase B rejections, an absent input, a malformed native
 * witness, an undecodable script) every output is byte for byte what
 * classification over a whole-ledger trie of the same state gives: the
 * fixture holds the SHA-256 of each output field of that classification,
 * all under the canonical consensus profile.
 */
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Effect } from "effect";
import { expect, it } from "vitest";

import type * as ForcedTransactionsDB from "../src/database/forcedTransactions.js";
import { classifyForcedTransactions } from "../src/mpf/event-window.classify-forced-transactions.js";
import { ledgerOfOutputs } from "./helpers/ledger-of.js";

type Encoded =
  | null
  | string
  | number
  | boolean
  | readonly Encoded[]
  | { readonly [key: string]: Encoded };

/** The fixture's JSON form: bytes, integers, dates and maps tagged. */
const encode = (value: unknown): Encoded => {
  if (value === null || value === undefined) return null;
  if (value instanceof Uint8Array)
    return { $b: Buffer.from(value).toString("hex") };
  if (typeof value === "bigint") return { $n: value.toString() };
  if (value instanceof Date) return { $d: value.toISOString() };
  if (value instanceof Map) return { $m: [...value.entries()].map(encode) };
  if (Array.isArray(value)) return value.map(encode);
  if (typeof value === "object")
    return Object.fromEntries(
      Object.entries(value).map(([key, item]) => [key, encode(item)]),
    );
  return value as Encoded;
};

const decode = (value: Encoded): unknown => {
  if (value === null || typeof value !== "object") return value;
  if (Array.isArray(value)) return value.map(decode);
  const tagged = value as Record<string, Encoded>;
  if ("$b" in tagged) return Buffer.from(tagged.$b as string, "hex");
  if ("$n" in tagged) return BigInt(tagged.$n as string);
  if ("$d" in tagged) return new Date(tagged.$d as string);
  if ("$m" in tagged)
    return new Map(
      (tagged.$m as Encoded[]).map(decode) as [unknown, unknown][],
    );
  return Object.fromEntries(
    Object.entries(tagged).map(([key, item]) => [key, decode(item)]),
  );
};

const digest = (value: unknown) =>
  createHash("sha256")
    .update(JSON.stringify(encode(value)))
    .digest("hex");

type Case = {
  readonly source: string;
  readonly entries: Encoded;
  readonly initialState: Encoded;
  readonly blockTime: Encoded;
  readonly slot: Encoded;
  readonly validation: Encoded;
  readonly expected: readonly Record<string, unknown>[];
};

const fixture = JSON.parse(
  readFileSync(
    new URL("./fixtures/forced-mutation-steps.json", import.meta.url),
    "utf8",
  ),
) as { readonly cases: readonly Case[] };

it.each(
  fixture.cases.map((item, index) => [index, item.source, item] as const),
)(
  "case %i (%s): every output matches the whole-ledger trie classification",
  async (_, __, item) => {
    const ledger = await ledgerOfOutputs(
      decode(item.initialState) as Map<string, Buffer>,
    );
    const slot = decode(item.slot) as bigint;
    const classified = await Effect.runPromise(
      classifyForcedTransactions({
        entries: decode(item.entries) as ForcedTransactionsDB.Entry[],
        ledger,
        blockTime: decode(item.blockTime) as Date,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation: {
          ...(decode(item.validation) as {
            expectedNetworkId: bigint;
            minFeeA: bigint;
            minFeeB: bigint;
            bucketConcurrency: number;
            l1SlotConfig: Parameters<
              typeof classifyForcedTransactions
            >[0]["validation"]["l1SlotConfig"];
          }),
          slotForUnixTime: () => slot,
        },
      }),
    );
    expect(
      classified.map((result) => ({
        ledgerMutationSteps: digest(result.ledgerMutationSteps),
        ledgerWitnessEntries: digest(result.ledgerWitnessEntries),
        ledgerOps: digest(result.ledgerOps),
        rawLedgerOps: digest(result.rawLedgerOps),
        transitionEffect: digest(result.transitionEffect),
        entry: digest(result.entry),
        rejectionCode: result.rejectionCode,
        rejectionReason: digest(result.rejectionReason),
        stepCount: result.ledgerMutationSteps.length,
      })),
    ).toEqual(item.expected);
  },
);
