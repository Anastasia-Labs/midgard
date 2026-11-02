import "./helpers/follower-emulator-installed.js";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { IntentJournal } from "../src/services/intent-journal.js";
import { publishReferenceScripts } from "../src/transactions/reference-publication.js";
import { selectNodeWallet } from "../src/transactions/utils.wallet-view.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";

it("reads the publication address a fixed number of times, however many transactions it chains", async () => {
  // A local node answers an address read by scanning its whole UTxO set,
  // seconds on Preprod; an out-ref read is a lookup.
  const addressReads = async (count: number) => {
    const account = generateEmulatorAccount({ lovelace: 10_000_000_000n });
    const provider = new Emulator([account]);
    const lucid = await Lucid(provider, "Custom");
    // The deploy command's wallet selection.
    selectNodeWallet(lucid, account.seedPhrase);
    const authPolicy = await SDK.createReferenceScriptAuthPolicy(lucid);
    const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
      .slice(0, count)
      .map((name) => ({ name, script: authPolicy.mintingScript }));
    let reads = 0;
    const getUtxos = provider.getUtxos.bind(provider);
    provider.getUtxos = async (address) => {
      reads += 1;
      return getUtxos(address);
    };
    provider.awaitTx = async () => {
      provider.awaitBlock(1);
      return true;
    };
    const result = await publishReferenceScripts({
      lucid,
      address: await lucid.wallet().address(),
      targets,
      authPolicy,
      reserved: new Set(),
      journal: await runWithoutFollower(IntentJournal),
      options: {
        mode: "chained",
        synchronize: async () => provider.slot,
        wait: async () => {
          provider.awaitBlock(1);
        },
      },
    });
    expect(result).toHaveLength(count);
    return reads;
  };
  expect(await addressReads(28)).toBe(await addressReads(4));
});
