/**
 * A local node for a process under test: a Custom network's config files
 * and a fake transport sidecar (`fixtures/chain-parameters-handler.mjs`)
 * that answers the chain parameters. The test reads what the process asked
 * the node (`journal`) and sets how the node answers (`answer`): until the
 * first `answer`, every ledger query waits.
 */
import {
  appendFile,
  mkdtemp,
  readFile,
  realpath,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";

import type { NativeLedgerSettings } from "../../src/services/native-ledger.js";
import { ledgerProtocolParametersOf } from "./follower-emulator.ledger.js";

const HANDLER_MODULE = fileURLToPath(
  new URL("../fixtures/chain-parameters-handler.mjs", import.meta.url),
);

export type FakeNodeMode = "answer" | "unavailable" | "slot-length-2000";

export type FakeLocalNode = Readonly<{
  nativeLedger: NativeLedgerSettings;
  /** Each handshake ("hello") and ledger query the node received, in order. */
  journal: () => Promise<readonly string[]>;
  /** How every ledger query from now on is answered. */
  answer: (mode: FakeNodeMode) => Promise<void>;
  remove: () => Promise<void>;
}>;

export const fakeLocalNode = async (): Promise<FakeLocalNode> => {
  const directory = await realpath(
    await mkdtemp(join(tmpdir(), "midgard-fake-local-node-")),
  );
  await writeFile(
    join(directory, "shelley-genesis.json"),
    JSON.stringify({ networkMagic: 42 }),
  );
  await writeFile(
    join(directory, "config.json"),
    JSON.stringify({ ShelleyGenesisFile: "shelley-genesis.json" }),
  );
  const journal = join(directory, "journal");
  const mode = join(directory, "mode");
  await appendFile(journal, "");
  return {
    nativeLedger: {
      socketPath: join(directory, "node.socket"),
      nodeConfigPath: join(directory, "config.json"),
      binaryPath: await writeFakeSidecar({
        path: join(directory, "sidecar"),
        handlerModule: HANDLER_MODULE,
        options: {
          journal,
          mode,
          protocolParameters: Buffer.from(
            ledgerProtocolParametersOf(PROTOCOL_PARAMETERS_DEFAULT),
          ).toString("hex"),
        },
      }),
    },
    journal: async () =>
      (await readFile(journal, "utf8"))
        .split("\n")
        .filter((line) => line !== ""),
    answer: (next) => writeFile(mode, next),
    remove: () => rm(directory, { recursive: true, force: true }),
  };
};
