import { L1NodeTransport } from "@al-ft/l1-node-transport";
import { formatL1Origin, parseL1Origin } from "@al-ft/midgard-core/l1-origin";

import { findOrigin, type FindOriginResult } from "../find-origin.js";
import { parseReset, RESET_HELP, RESET_USAGE, runReset } from "./reset.js";

export const USAGE = `midgard-l1-follower

Usage:
  midgard-l1-follower find-origin --tx <prepareHubOracleNonce tx id>
      --network-magic <n> [--socket <node socket>] [--sidecar <binary>]
      [--from <slot>.<block hash> | --recorded <slot>.<block hash>]
${RESET_USAGE}
find-origin prints the deployment's l1Origin: the point immediately before
the block holding the tx, as JSON on stdout. --from is a known point before
that block (default: genesis). --recorded is the l1Origin the deployment
recorded before it signed the tx: find-origin scans from it to the tx's block
and prints it as l1Origin, or exits 3 when it is not on the node's chain or
does not precede that block. --socket defaults to CARDANO_NODE_SOCKET_PATH,
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
  /** --recorded: the scan starts here and, when it finds the tx, prints it. */
  recorded: boolean;
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
  const recordedText = flags.get("--recorded");
  flags.delete("--from");
  flags.delete("--recorded");
  if (fromText !== undefined && recordedText !== undefined)
    throw new UsageError("--recorded and --from exclude each other");
  const [startFlag, startText] =
    recordedText === undefined
      ? ["--from", fromText]
      : ["--recorded", recordedText];
  let from;
  try {
    from =
      startText === undefined ? undefined : parseL1Origin(startText, startFlag);
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
    recorded: recordedText !== undefined,
  };
};

/**
 * Prints a found tx's l1Origin. With --recorded the scan started at the
 * recorded origin and found the tx after it, so the recorded origin is on the
 * node's chain and precedes the tx's block: that origin is the one printed.
 */
const report = (
  result: FindOriginResult,
  parsed: FindOriginArguments,
  io: CliIo,
): number => {
  const start = parsed.recorded ? "--recorded" : "--from";
  switch (result.kind) {
    case "found": {
      const origin =
        parsed.recorded && parsed.from !== undefined
          ? parsed.from
          : result.origin;
      io.stdout(
        `${JSON.stringify({
          l1Origin: formatL1Origin(origin),
          origin,
          prepareHubOracleNonceBlock: result.nonceBlock,
          txIndex: result.txIndex,
          depth: result.depth,
        })}\n`,
      );
      return 0;
    }
    case "not_found": {
      const scannedTo =
        result.scannedTo === "genesis"
          ? "genesis"
          : formatL1Origin(result.scannedTo);
      io.stderr(
        parsed.recorded
          ? `the tx is not on the node's chain after the recorded origin (scanned to ${scannedTo}): the recorded origin does not precede the tx's block, or the node is not synced\n`
          : `the tx is not on the node's chain after the start point (scanned to ${scannedTo}); is --from before its block, and is the node synced?\n`,
      );
      return EXIT_NOT_FOUND;
    }
    case "from_not_on_chain":
      io.stderr(`${start} is not on the node's chain\n`);
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
  if (command === "--help" && args.length === 0) {
    io.stdout(USAGE);
    return 0;
  }
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
      parsed,
      io,
    );
  } catch (error) {
    io.stderr(`find-origin failed: ${(error as Error).message}\n`);
    return 1;
  } finally {
    await transport.close();
  }
};
