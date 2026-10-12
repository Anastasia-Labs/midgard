import { L1NodeTransport } from "@al-ft/l1-node-transport";
import { formatL1Origin, parseL1Origin } from "@al-ft/midgard-core/l1-origin";

import { findOrigin, type FindOriginResult } from "../find-origin.js";
import { parseReset, RESET_HELP, RESET_USAGE, runReset } from "./reset.js";

export const USAGE = `midgard-l1-follower

Usage:
  midgard-l1-follower find-origin --tx <prepareHubOracleNonce tx id>
      --network-magic <n> [--socket <node socket>] [--sidecar <binary>]
      [--from <slot>.<block hash>]
${RESET_USAGE}
find-origin prints the deployment's l1Origin: the point immediately before
the block holding the tx, as JSON on stdout. --from is a known point before
that block (default: genesis). --socket defaults to CARDANO_NODE_SOCKET_PATH,
--sidecar to MIDGARD_L1_NODE_TRANSPORT_BINARY (the compiled
midgard-l1-node-transport).

${RESET_HELP}`;

/** Exit codes: 0 done, 1 failed, 2 usage, 3 not found, 4 store locked (reset). */
export const EXIT_NOT_FOUND = 3;
const EXIT_USAGE = 2;

export type CliIo = Readonly<{
  stdout: (text: string) => void;
  stderr: (text: string) => void;
}>;

type FindOriginArguments = Readonly<{
  txHash: string;
  networkMagic: number;
  socketPath: string;
  binaryPath: string;
  from: ReturnType<typeof parseL1Origin> | undefined;
}>;

class UsageError extends Error {}

const parseFlags = (args: readonly string[]): Map<string, string> => {
  const flags = new Map<string, string>();
  for (let index = 0; index < args.length; index += 2) {
    const flag = args[index] as string;
    const value = args[index + 1];
    if (!flag.startsWith("--") || value === undefined)
      throw new UsageError(`expected --flag value pairs, got ${flag}`);
    if (flags.has(flag)) throw new UsageError(`${flag} given twice`);
    flags.set(flag, value);
  }
  return flags;
};

const parseFindOrigin = (
  args: readonly string[],
  env: Readonly<Record<string, string | undefined>>,
): FindOriginArguments => {
  const flags = parseFlags(args);
  const take = (flag: string, fallback?: string): string => {
    const value = flags.get(flag) ?? fallback;
    flags.delete(flag);
    if (value === undefined || value === "")
      throw new UsageError(`${flag} is required`);
    return value;
  };
  const txHash = take("--tx");
  if (!/^[0-9a-f]{64}$/u.test(txHash))
    throw new UsageError("--tx must be 64 lowercase hex characters");
  const magicText = take("--network-magic");
  if (!/^(?:0|[1-9][0-9]*)$/u.test(magicText))
    throw new UsageError("--network-magic must be a natural number");
  const socketPath = take("--socket", env.CARDANO_NODE_SOCKET_PATH);
  const binaryPath = take("--sidecar", env.MIDGARD_L1_NODE_TRANSPORT_BINARY);
  const fromText = flags.get("--from");
  flags.delete("--from");
  let from;
  try {
    from =
      fromText === undefined ? undefined : parseL1Origin(fromText, "--from");
  } catch (error) {
    throw new UsageError((error as Error).message);
  }
  const unknown = [...flags.keys()];
  if (unknown.length > 0)
    throw new UsageError(`unknown flag ${unknown.join(", ")}`);
  return {
    txHash,
    networkMagic: Number(magicText),
    socketPath,
    binaryPath,
    from,
  };
};

const report = (result: FindOriginResult, io: CliIo): number => {
  switch (result.kind) {
    case "found":
      io.stdout(
        `${JSON.stringify({
          l1Origin: formatL1Origin(result.origin),
          origin: result.origin,
          prepareHubOracleNonceBlock: result.nonceBlock,
          txIndex: result.txIndex,
          depth: result.depth,
        })}\n`,
      );
      return 0;
    case "not_found":
      io.stderr(
        `the tx is not on the node's chain after the start point (scanned to ${
          result.scannedTo === "genesis"
            ? "genesis"
            : formatL1Origin(result.scannedTo)
        }); is --from before its block, and is the node synced?\n`,
      );
      return EXIT_NOT_FOUND;
    case "from_not_on_chain":
      io.stderr("--from is not on the node's chain\n");
      return EXIT_NOT_FOUND;
    case "no_preceding_point":
      io.stderr(
        "the tx is in the chain's first block, which has no preceding block point\n",
      );
      return EXIT_NOT_FOUND;
  }
};

/** Runs one CLI invocation and returns its exit code. */
export const runFollowerCli = async (
  argv: readonly string[],
  env: Readonly<Record<string, string | undefined>>,
  io: CliIo,
): Promise<number> => {
  const [command, ...args] = argv;
  if (command === "reset") {
    const parsed = parseReset(args);
    if (typeof parsed === "string") {
      io.stderr(`${parsed}\n\n${USAGE}`);
      return EXIT_USAGE;
    }
    return runReset(parsed, io);
  }
  if (command !== "find-origin") {
    io.stderr(USAGE);
    return EXIT_USAGE;
  }
  let parsed: FindOriginArguments;
  try {
    parsed = parseFindOrigin(args, env);
  } catch (error) {
    if (!(error instanceof UsageError)) throw error;
    io.stderr(`${error.message}\n\n${USAGE}`);
    return EXIT_USAGE;
  }
  const transport = new L1NodeTransport({
    binaryPath: parsed.binaryPath,
    socketPath: parsed.socketPath,
    networkMagic: parsed.networkMagic,
  });
  try {
    return report(
      await findOrigin({
        transport,
        txHash: parsed.txHash,
        ...(parsed.from === undefined ? {} : { from: parsed.from }),
      }),
      io,
    );
  } catch (error) {
    io.stderr(`find-origin failed: ${(error as Error).message}\n`);
    return 1;
  } finally {
    await transport.close();
  }
};
