#!/usr/bin/env node
/**
 * The devnet shadow-diff soak (plan §14, §16.1).
 *
 *   soak-cli run --dir <soak dir> [--max-events n]
 *   soak-cli report --dir <soak dir>
 *
 * `run` follows the node into `<dir>/follower.sqlite`, compares after every
 * event and appends to `<dir>/journal.jsonl`; restarted, it resumes from the
 * store's cursor. Exit status: 0 stopped on request or at the limit, 3 an
 * intervention (do not restart; an operator must act), 4 refused, 1 crashed
 * (safe to restart).
 */
import { writeFile } from "node:fs/promises";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

import { L1NodeTransport } from "@al-ft/l1-node-transport";

import { readArray, readBytes, readSmallUint } from "../cbor/reader.js";
import { openSqliteFactStore } from "../sqlite.js";
import type { FactStore } from "../store/fact-store.js";
import type { ShadowComparator } from "./comparator.js";
import {
  formatSummary,
  readJournal,
  type SoakSummary,
  summarise,
} from "./journal.js";
import { ledgerComparator } from "./ledger-comparator.js";
import { isShadowPlugin, type ShadowPlugin } from "./plugin.js";
import { projectionStoreOptions } from "./projection.js";
import { runSoak } from "./soak.js";
import { readSoakConfig } from "./soak-config.js";

const log = (line: string): void => {
  process.stderr.write(`${new Date().toISOString()} ${line}\n`);
};

const argument = (
  args: readonly string[],
  name: string,
): string | undefined => {
  const index = args.indexOf(name);
  return index === -1 ? undefined : args[index + 1];
};

const loadPlugin = async (module: string): Promise<ShadowPlugin> => {
  const loaded = (await import(pathToFileURL(module).href)) as {
    default?: unknown;
  };
  if (!isShadowPlugin(loaded.default))
    throw new Error(`${module} does not default-export a shadow plugin`);
  return loaded.default;
};

/** The node's tip as the soak's origin, from one acquired ledger state. */
const tipOrigin = async (
  transport: L1NodeTransport,
): Promise<{ point: { slot: number; hash: Buffer }; height: number }> =>
  await transport.withLedgerState("tip", async (session) => {
    const point = await session.query({ query: "chain_point" });
    const blockNo = await session.query({ query: "chain_block_no" });
    const [slot, hash] = readArray(point, 0).items;
    const [tag, height] = readArray(blockNo, 0).items;
    if (
      slot === undefined ||
      hash === undefined ||
      tag === undefined ||
      height === undefined ||
      readSmallUint(blockNo, tag) !== 1
    )
      throw new Error("the node's ledger is at the genesis; wait for a block");
    return {
      point: { slot: readSmallUint(point, slot), hash: readBytes(point, hash) },
      height: readSmallUint(blockNo, height),
    };
  });

const report = async (dir: string): Promise<SoakSummary> => {
  const { records, corrupt } = await readJournal(dir);
  const summary = summarise(records, corrupt);
  await writeFile(join(dir, "summary.json"), JSON.stringify(summary, null, 2));
  return summary;
};

const run = async (
  dir: string,
  maxEvents: number | undefined,
): Promise<number> => {
  const config = await readSoakConfig(dir);
  const plugins = await Promise.all(
    config.plugins.map((p) => loadPlugin(p.module)),
  );
  const transport = new L1NodeTransport({
    binaryPath: config.binaryPath,
    socketPath: config.socketPath,
    networkMagic: config.networkMagic,
    onDiagnostic: (line) => log(`transport: ${line}`),
  });
  let store: FactStore | undefined;
  const comparators: ShadowComparator[] = [];
  try {
    store = openSqliteFactStore({
      ...projectionStoreOptions(
        plugins.flatMap((p) => p.projections ?? []),
        {
          securityParameter: config.securityParameter,
          trackedSet: config.trackedSet,
        },
        "sqlite",
      ),
      path: join(dir, "follower.sqlite"),
    });
    const started = await store.start();
    if (started.kind !== "ready") {
      log(
        `store refused to start: ${started.kind === "intervention" ? started.reason : started.kind}: ${started.detail}`,
      );
      return 3;
    }
    if ((await store.cursor()) === null) {
      await transport.whenReady();
      const origin = await tipOrigin(transport);
      const init = await store.initialize(origin);
      if (init.kind !== "initialized")
        throw new Error(`initialize: ${init.kind}`);
      log(`initialized at height ${origin.height} slot ${origin.point.slot}`);
    }
    if (config.ledgerAddresses.length > 0)
      comparators.push(
        ledgerComparator({
          ledger: transport,
          addresses: config.ledgerAddresses,
          dir,
        }),
      );
    for (const [index, plugin] of plugins.entries())
      comparators.push(
        ...(await plugin.comparators({
          transport,
          store,
          dir,
          options: config.plugins[index]?.options,
        })),
      );
    const abort = new AbortController();
    const onSignal = (): void => abort.abort();
    process.once("SIGINT", onSignal);
    process.once("SIGTERM", onSignal);
    const stopped = await runSoak({
      dir,
      store,
      openChainSync: (options) => transport.openChainSync(options),
      comparators,
      signal: abort.signal,
      log,
      ...(maxEvents === undefined ? {} : { maxEvents }),
    });
    log(formatSummary(await report(dir)));
    return stopped.reason === "intervention"
      ? 3
      : stopped.reason === "refused"
        ? 4
        : 0;
  } finally {
    for (const comparator of comparators) await comparator.close?.();
    await store?.close();
    await transport.close();
  }
};

const main = async (): Promise<number> => {
  const [command, ...args] = process.argv.slice(2);
  const dir = argument(args, "--dir");
  if (dir === undefined || (command !== "run" && command !== "report")) {
    process.stderr.write(
      "usage: soak-cli run --dir <soak dir> [--max-events n] | report --dir <soak dir>\n",
    );
    return 2;
  }
  if (command === "report") {
    process.stdout.write(`${formatSummary(await report(dir))}\n`);
    return 0;
  }
  const max = argument(args, "--max-events");
  return await run(dir, max === undefined ? undefined : Number(max));
};

main().then(
  (status) => {
    process.exitCode = status;
  },
  (error: unknown) => {
    log(
      `soak crashed: ${error instanceof Error ? (error.stack ?? error.message) : String(error)}`,
    );
    process.exitCode = 1;
  },
);
