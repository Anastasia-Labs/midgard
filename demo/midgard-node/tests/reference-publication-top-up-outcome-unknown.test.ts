/**
 * A reference-script wallet top-up sent with an unknown outcome (the follower
 * provider's `L1SubmitOutcomeUnknownError`, on the send and on its same-bytes
 * resend) ends the publication pass. The pass resumes only once the top-up's
 * exact id is settled: a top-up that landed is not built again, and one that
 * never landed is built again and publication completes.
 */
import { L1SubmitOutcomeUnknownError } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import type { ProviderRetryOptions } from "../src/provider-retry.js";
import { ensureReferenceScriptTargetsProgram } from "../src/transactions/reference-scripts.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";

const IMMEDIATE_RETRY: ProviderRetryOptions = {
  maxAttempts: 3,
  baseDelayMs: 0,
  maxDelayMs: 0,
};

const txIdOf = (cbor: string): string =>
  CML.hash_transaction(CML.Transaction.from_cbor_hex(cbor).body()).to_hex();

const inputsOf = (cbor: string): string[] => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from(
    { length: inputs.len() },
    (_, index) =>
      `${inputs.get(index).transaction_id().to_hex()}#${inputs.get(index).index().toString()}`,
  );
};

/**
 * A reference-script wallet below its working-capital floor, funded from a
 * separate wallet holding two coins. The emulator's first two top-up sends
 * end with an unknown outcome; `taken` decides whether the node took the
 * first. The status reads of the top-up are answered by the emulator, which
 * produces a block before the third: the submit recovery's own read and the
 * pass's first read see a taken top-up pending, and it lands while the pass
 * waits.
 */
const fixture = async (taken: boolean) => {
  const publisher = generateEmulatorAccount({ lovelace: 20_000_000n });
  const funder = generateEmulatorAccount({ lovelace: 5_000_000_000n });
  const provider = new Emulator([publisher, funder, funder]);
  const lucid = await Lucid(provider, "Custom");
  lucid.selectWallet.fromSeed(publisher.seedPhrase);
  const fundingLucid = await Lucid(provider, "Custom");
  fundingLucid.selectWallet.fromSeed(funder.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(lucid);
  const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
    .slice(0, 2)
    .map((name) => ({ name, script: authPolicy.mintingScript }));
  const options = {
    mode: "chained" as const,
    synchronize: async () => provider.slot,
    wait: async () => {
      provider.awaitBlock(1);
    },
  };
  provider.awaitTx = async () => {
    await options.wait();
    return true;
  };

  const funderCoins = new Set(
    (await provider.getUtxos(funder.address)).map(
      (utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`,
    ),
  );
  const topUpsSent: string[] = [];
  const topUpsTaken: string[] = [];
  const submit = provider.submitTx.bind(provider);
  provider.submitTx = async (cbor) => {
    if (!inputsOf(cbor).some((input) => funderCoins.has(input)))
      return submit(cbor);
    const txHash = txIdOf(cbor);
    topUpsSent.push(txHash);
    if (topUpsSent.length <= 2) {
      if (taken && topUpsSent.length === 1) {
        await submit(cbor);
        topUpsTaken.push(txHash);
      }
      throw new L1SubmitOutcomeUnknownError(txHash, "request_timeout");
    }
    topUpsTaken.push(txHash);
    return submit(cbor);
  };
  const statusReads: string[] = [];
  const status = provider.getTransactionStatus.bind(provider);
  provider.getTransactionStatus = async (txHash, statusOptions) => {
    if (topUpsSent.includes(txHash)) {
      statusReads.push(txHash);
      if (statusReads.length === 3) provider.awaitBlock(1);
    }
    return status(txHash, statusOptions);
  };

  const run = () =>
    runWithoutFollower(
      ensureReferenceScriptTargetsProgram(
        lucid,
        "test",
        targets,
        authPolicy,
        fundingLucid,
        undefined,
        1,
        new Set(),
        options,
        IMMEDIATE_RETRY,
      ),
    );
  const landedTopUps = () =>
    topUpsTaken.filter(
      (txHash) => provider.transactionHistory[txHash]?.status === "confirmed",
    );
  return { run, targets, topUpsSent, statusReads, landedTopUps };
};

it("does not build a top-up again once the one sent with an unknown outcome landed", async () => {
  const f = await fixture(true);

  const result = await f.run();

  expect(result.map(({ name }) => name)).toEqual(
    f.targets.map(({ name }) => name),
  );
  expect(new Set(f.topUpsSent).size).toBe(1);
  expect(f.landedTopUps()).toEqual([f.topUpsSent[0]]);
  // The submit recovery's read, then the pass's: pending, then landed.
  expect(f.statusReads).toHaveLength(3);
});

it("builds the top-up again once the one sent with an unknown outcome never landed", async () => {
  const f = await fixture(false);

  const result = await f.run();

  expect(result.map(({ name }) => name)).toEqual(
    f.targets.map(({ name }) => name),
  );
  // Sent, resent with the same bytes, then built again and sent once more.
  expect(f.topUpsSent).toHaveLength(3);
  expect(f.topUpsSent[1]).toBe(f.topUpsSent[0]);
  expect(f.landedTopUps()).toEqual([f.topUpsSent[2]]);
});
