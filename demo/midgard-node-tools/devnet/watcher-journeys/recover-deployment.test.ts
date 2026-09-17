import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  CML,
  coreToUtxo,
  Lucid,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import {
  admitWatcherNativeRollForwardBlock,
  parseWatcherNativeChainSyncEvent,
  WatcherLocalKupmios,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { journeyNativeNodeQuery } from "./native-node.js";

// Recovery consumes captured raw Cardano blocks; no transaction or receipt is
// inferred from logs. The regular deployment helper revalidates all references.
it.skipIf(process.env.MIDGARD_WATCHER_RECOVER_DEPLOYMENT !== "1")(
  "recovers the confirmed deployment from native chain bytes",
  async () => {
    const directory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR!;
    const native = await journeyNativeNodeQuery(directory);
    const env = Object.fromEntries(
      (await readFile(join(directory, "run.env"), "utf8"))
        .trim()
        .split("\n")
        .map((line) => {
          const at = line.indexOf("=");
          return [line.slice(0, at), line.slice(at + 1)];
        }),
    );
    const provider = new WatcherLocalKupmios(
      `http://127.0.0.1:${env.MIDGARD_PHASE4_KUPO_PORT}`,
      `http://127.0.0.1:${env.MIDGARD_PHASE4_OGMIOS_PORT}`,
      native,
    );
    const lucid = await Lucid(provider, "Custom", {
      slotConfig: native.watcherConfig.customNetwork!.slotConfig,
    });
    const transactions = new Map<string, string>();
    for (const line of (
      await readFile(
        join(directory, "work/recovery-native-blocks.ndjson"),
        "utf8",
      )
    )
      .trim()
      .split("\n")) {
      const parsed = JSON.parse(line);
      if (parsed.kind === "ready") continue;
      const event = parseWatcherNativeChainSyncEvent(parsed);
      if (event.kind !== "roll_forward") continue;
      const block = admitWatcherNativeRollForwardBlock(event);
      block.transactionIds.forEach((id, index) =>
        transactions.set(id, block.transactionCbors[index]!),
      );
    }
    const accounts = JSON.parse(
      await readFile(join(directory, "secrets/journey-accounts.json"), "utf8"),
    );
    const matches = await (
      await fetch(
        `http://127.0.0.1:${env.MIDGARD_PHASE4_KUPO_PORT}/matches/${accounts.operator.address}`,
      )
    ).json();
    const candidates = matches.filter(
      (value: {
        output_index: number;
        value: { coins: number };
        spent_at: unknown;
      }) =>
        value.output_index === 0 &&
        value.value.coins === 10_000_000 &&
        value.spent_at !== null,
    );
    expect(candidates).toHaveLength(1);
    const candidate = candidates[0];
    const bootstrap = CML.Transaction.from_cbor_hex(
      transactions.get(candidate.transaction_id)!,
    );
    const nonce = coreToUtxo(
      CML.TransactionUnspentOutput.new(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(candidate.transaction_id),
          0n,
        ),
        bootstrap.body().outputs().get(0),
      ),
    );
    const initializationCbor = transactions.get(
      candidate.spent_at.transaction_id,
    );
    expect(initializationCbor).toBeDefined();
    const publications = (
      await readFile(
        join(directory, "work/reference-publications.ndjson"),
        "utf8",
      )
    )
      .trim()
      .split("\n")
      .map((line) => {
        const { role, outRef } = JSON.parse(line);
        const signedCbor = transactions.get(outRef.txHash);
        if (signedCbor === undefined)
          throw new Error(`Native capture omitted ${role}`);
        return { role, outRef, signedCbor };
      });
    const authPublication = publications.find(
      ({ role }) => role === "reference-script-auth minting",
    )!;
    const [authUtxo] = await provider.getUtxosByOutRef([
      authPublication.outRef,
    ]);
    if (authUtxo.scriptRef?.type !== "Native")
      throw new Error("Reference authority is not a native script");
    const expiresAtSlot = Number(
      CML.NativeScript.from_cbor_hex(authUtxo.scriptRef.script)
        .as_script_invalid_hereafter()!
        .after(),
    );
    const authPolicy = {
      mintingScriptCBOR: authUtxo.scriptRef.script,
      mintingScript: authUtxo.scriptRef,
      policyId: validatorToScriptHash(authUtxo.scriptRef),
      expiresAtSlot,
      expiresAtUnixTime: lucid.slotToUnixTime(expiresAtSlot),
      timelockDurationMs: 15 * 60_000,
    };
    await writeFile(
      join(directory, "work/deployment-resume.json"),
      JSON.stringify(
        { nonce, authPolicy, publications, initializationCbor },
        (_, value) =>
          typeof value === "bigint" ? { bigint: value.toString() } : value,
      ),
    );
    console.info(
      `Recovered ${publications.length} published transactions and initialization from ${transactions.size} native transactions`,
    );
  },
);
