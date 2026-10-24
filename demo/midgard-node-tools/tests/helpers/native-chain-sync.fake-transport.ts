// Fake node transports for the native chain-sync tests. Importing
// this module registers hooks that stop every sidecar after each test, so it
// is for vitest files only; native-chain-sync.config.ts stays importable from
// compiled probes.
import { mkdtempSync, readFileSync, realpathSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { closeSharedL1NodeTransports } from "@al-ft/l1-node-transport";
import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { afterAll, afterEach } from "vitest";

import { startWatcherNativeChainSync } from "../../src/devnet-stack/native-chain-sync.js";
import {
  config,
  INTERSECTION,
  readIdentityFixture,
} from "./native-chain-sync.config.js";

const handlerModule = fileURLToPath(
  new URL("./native-chain-sync-handler.mjs", import.meta.url),
);

// Every fake node transport of a test file lives in one directory, and each
// test ends with no sidecar left running.
const fakeDirectory = realpathSync(
  mkdtempSync(join(tmpdir(), "midgard-watcher-native-")),
);
let fakeCount = 0;
afterEach(async () => {
  await closeSharedL1NodeTransports();
});
afterAll(() => {
  rmSync(fakeDirectory, { recursive: true, force: true });
});

export type FakeNodeTransport = Readonly<{
  binaryPath: string;
  /** The steps the node took and the streams it saw closed, in order. */
  journal: () => readonly string[];
  /** The pid of the sidecar process last started. */
  pid: () => number;
  /** Fails every open stream, as a lost node connection would. */
  failStreams: () => void;
}>;

/**
 * A node transport executable whose node follows `mode`, or takes the
 * scripted `steps` one chain-sync open (or refused handshake) at a time.
 */
export const fakeNodeTransport = async (
  mode = "honest",
  {
    steps = [],
    magic = 1,
  }: Readonly<{ steps?: readonly string[]; magic?: number }> = {},
): Promise<FakeNodeTransport> => {
  fakeCount += 1;
  const base = join(fakeDirectory, `transport-${String(fakeCount)}`);
  const journalPath = `${base}.journal`;
  const pidFile = `${base}.pid`;
  const binaryPath = await writeFakeSidecar({
    path: base,
    handlerModule,
    options: { mode, steps, journal: journalPath, pidFile, magic },
  });
  const read = (path: string): string => {
    try {
      return readFileSync(path, "utf8");
    } catch {
      return "";
    }
  };
  return Object.freeze({
    binaryPath,
    journal: () => read(journalPath).split("\n").filter(Boolean),
    pid: () => Number(read(pidFile)),
    failStreams: () => process.kill(Number(read(pidFile)), "SIGUSR1"),
  });
};

export const start = async (
  mode: string,
  onEvent: Parameters<typeof startWatcherNativeChainSync>[0]["onEvent"],
  transport?: FakeNodeTransport,
) =>
  await startWatcherNativeChainSync({
    binaryPath: (transport ?? (await fakeNodeTransport(mode))).binaryPath,
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2_000,
    onEvent,
    unsafeReadIdentityFileForTest: readIdentityFixture,
  });
