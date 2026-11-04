/**
 * Reference publication over a local node's ledger alone (`--l1 node`): a
 * node answers an address read by scanning its whole UTxO set, so the
 * publication reads its own addresses once, on a connection of their own
 * with no request deadline, however many transactions it chains, and reads
 * its references back by outref.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  Lucid as makeLucid,
} from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import { IntentJournal } from "../src/services/intent-journal.js";
import { publishReferenceScripts } from "../src/transactions/reference-publication.js";
import { selectNodeWallet } from "../src/transactions/utils.wallet-view.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { nodeLedgerDouble } from "./helpers/node-ledger-double.js";

afterEach(() => {
  vi.useRealTimers();
});

const publish = async (
  count: number,
  /** Runs once, at the publication's first wait. */
  duringWait?: (emulator: Emulator, account: EmulatorAccount) => Promise<void>,
) => {
  const account = generateEmulatorAccount({ lovelace: 10_000_000_000n });
  const emulator = new Emulator([account]);
  const node = nodeLedgerDouble(emulator);
  const lucid = await node.lucid([account.address]);
  // The deploy command's wallet selection.
  selectNodeWallet(lucid, account.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(lucid);
  const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
    .slice(0, count)
    .map((name) => ({ name, script: authPolicy.mintingScript }));
  const result = await publishReferenceScripts({
    lucid,
    address: account.address,
    targets,
    authPolicy,
    reserved: new Set(),
    journal: await runWithoutFollower(IntentJournal),
    options: {
      mode: "chained",
      synchronize: async () => emulator.slot,
      wait: async () => {
        const once = duringWait;
        duringWait = undefined;
        await once?.(emulator, account);
        node.nextBlock();
      },
    },
  });
  return { result, node };
};

it("reads its own address once, on its own connection with no deadline, however many transactions it chains", async () => {
  for (const count of [4, 28]) {
    const { result, node } = await publish(count);
    expect(result).toHaveLength(count);
    expect(node.submitted.length).toBeGreaterThan(0);
    for (const { utxo } of result)
      expect(
        node
          .live()
          .some(
            ({ txHash, outputIndex }) =>
              txHash === utxo.txHash && outputIndex === utxo.outputIndex,
          ),
      ).toBe(true);
    expect(
      node.queries.filter((query) => query.includes("utxo_by_address")),
    ).toEqual(["wallet:utxo_by_address:none"]);
    expect(node.queries).toContain("main:utxo_by_txin:bounded");
  }
});

it("refuses, naming the fix, when its wallet changed after its one read", async () => {
  // Another publication from the same wallet lands during the restart wait.
  const landsDuringWait = async (
    emulator: Emulator,
    account: EmulatorAccount,
  ) => {
    const other = await makeLucid(emulator, "Custom");
    other.selectWallet.fromSeed(account.seedPhrase);
    const tx = await other
      .newTx()
      .pay.ToAddress(account.address, { lovelace: 5_000_000n })
      .complete();
    await (await tx.sign.withWallet().complete()).submit();
  };
  await expect(publish(4, landsDuringWait)).rejects.toThrow(
    "An earlier reference publication landed after this command read its wallet; run the command again",
  );
});
