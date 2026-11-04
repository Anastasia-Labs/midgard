/**
 * The hub-oracle nonce command over a local node's ledger alone: it reads
 * its own wallet once, on a connection of its own with no request deadline,
 * builds and submits from that view, and reads the nonce back by exact
 * outref. The node's ledger is a double over an emulator's.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  Data,
  Emulator,
  generateEmulatorAccount,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  parseNonceLovelaceOption,
  prepareHubOracleOneShotNonceProgram,
  reconcileHubOracleOneShotNonceAttemptProgram,
  type SignedHubOracleNonceAttempt,
} from "../src/commands/prepare-hub-oracle-nonce.js";
import { Lucid as LucidService } from "../src/services/lucid.js";
import {
  SETTLED_ORIGIN,
  settledOriginSource,
} from "./helpers/hub-oracle-nonce-origin.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { lucidServiceStub } from "./helpers/lucid-service-stub.js";
import { nodeLedgerDouble } from "./helpers/node-ledger-double.js";

const NONCE_LOVELACE = 5_000_000n;

afterEach(() => {
  vi.useRealTimers();
});

/** An operator holding one funded wallet, and the node's ledger over it. */
const operatorChain = () => {
  const operator = generateEmulatorAccount({ lovelace: 100_000_000n });
  const node = nodeLedgerDouble(new Emulator([operator]));
  /** The tool's Lucid: a fresh provider over the node, as one command opens it. */
  const tool = async () => {
    const lucid = await node.lucid([operator.address]);
    lucid.selectWallet.fromSeed(operator.seedPhrase);
    return lucidServiceStub({ api: lucid });
  };
  return { operator, node, tool };
};

const markerOf = (inlineDatum: string): string => {
  const decoded = Data.from(inlineDatum) as Constr<string>;
  expect(decoded).toBeInstanceOf(Constr);
  return decoded.fields[0]!;
};

describe("prepare hub-oracle one-shot nonce over the node's ledger", () => {
  it("parses positive integer lovelace options and rejects invalid values", () => {
    expect(parseNonceLovelaceOption("5000000")).toBe(5_000_000n);
    expect(parseNonceLovelaceOption(" 42 ")).toBe(42n);
    expect(() => parseNonceLovelaceOption("0")).toThrow(
      "--amount-lovelace must be greater than zero",
    );
    for (const value of ["-1", "   ", "1.5"])
      expect(() => parseNonceLovelaceOption(value)).toThrow(
        "--amount-lovelace must be a positive integer",
      );
  });

  it("reads its own wallet once, on its own connection with no deadline, and reads the nonce back by exact outref", async () => {
    const { operator, node, tool } = operatorChain();
    const signed: SignedHubOracleNonceAttempt[] = [];

    const result = await runWithoutFollower(
      prepareHubOracleOneShotNonceProgram(NONCE_LOVELACE, {
        l1Origin: settledOriginSource(),
        beforeSubmission: (attempt) =>
          Effect.sync(() => {
            signed.push(attempt);
          }),
      }).pipe(Effect.provideService(LucidService, await tool())),
    );

    expect(node.submitted).toHaveLength(1);
    const nonce = node
      .live()
      .find(
        ({ txHash, outputIndex }) =>
          `${txHash}#${outputIndex.toString()}` === result.outRef,
      );
    expect(nonce).toMatchObject({
      address: operator.address,
      assets: { lovelace: NONCE_LOVELACE },
      datum: result.inlineDatum,
    });
    expect(result).toMatchObject({
      txHash: signed[0]!.txHash,
      address: operator.address,
      lovelace: NONCE_LOVELACE.toString(10),
      confirmationStatus: "confirmed",
      l1Origin: SETTLED_ORIGIN,
    });
    expect(
      Buffer.from(markerOf(result.inlineDatum), "hex")
        .toString("utf8")
        .startsWith(SDK.HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN),
    ).toBe(true);
    // One wallet read, on the wallet's own connection, which it then closed.
    expect(
      node.queries.filter((query) => query.includes("utxo_by_address")),
    ).toEqual(["wallet:utxo_by_address:none"]);
    expect(node.queries).toContain("main:utxo_by_txin:bounded");
    expect(node.closed).toEqual(["wallet"]);
  });

  it("reconciles a recorded attempt by exact outref, never building, submitting or reading a wallet", async () => {
    const { node, tool } = operatorChain();
    const signed: SignedHubOracleNonceAttempt[] = [];
    const prepared = await runWithoutFollower(
      prepareHubOracleOneShotNonceProgram(NONCE_LOVELACE, {
        l1Origin: settledOriginSource(),
        beforeSubmission: (attempt) =>
          Effect.sync(() => {
            signed.push(attempt);
          }),
      }).pipe(Effect.provideService(LucidService, await tool())),
    );
    node.queries.length = 0;
    node.closed.length = 0;

    const reconciled = await Effect.runPromise(
      reconcileHubOracleOneShotNonceAttemptProgram(signed[0]!).pipe(
        Effect.provideService(LucidService, await tool()),
      ),
    );

    expect(reconciled).toEqual({
      ...prepared,
      confirmationStatus: "reconciled_after_timeout",
    });
    expect(node.submitted).toHaveLength(1);
    expect(
      node.queries.some((query) => query.includes("utxo_by_address")),
    ).toBe(false);
    expect(node.closed).toEqual([]);
  });

  it("refuses an attempt recorded without its signed transaction, reading nothing", async () => {
    const { operator, node, tool } = operatorChain();
    const txHash = "ab".repeat(32);

    await expect(
      Effect.runPromise(
        reconcileHubOracleOneShotNonceAttemptProgram({
          txHash,
          address: operator.address,
          lovelace: NONCE_LOVELACE.toString(10),
          inlineDatum: "d87980",
        }).pipe(Effect.provideService(LucidService, await tool())),
      ),
    ).rejects.toThrow(
      `Hub-oracle nonce attempt ${txHash} was recorded without its signed transaction`,
    );
    expect(node.queries).toEqual([]);
  });
});
