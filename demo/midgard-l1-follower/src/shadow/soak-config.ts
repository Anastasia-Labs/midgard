import { readFile } from "node:fs/promises";
import { isAbsolute, join, resolve } from "node:path";

import type { TrackedSet } from "../types.js";

/** `<dir>/soak.json`: one soak over one devnet, for every role at once. */
export type SoakConfig = Readonly<{
  socketPath: string;
  networkMagic: number;
  /** The transport sidecar binary (`demo/l1-node-transport/dist/native/...`). */
  binaryPath: string;
  securityParameter: number;
  trackedSet: TrackedSet;
  /** Addresses the ledger stub comparator checks (default: the tracked addresses). */
  ledgerAddresses: readonly Buffer[];
  plugins: readonly Readonly<{ module: string; options: unknown }>[];
}>;

export class SoakConfigError extends Error {
  override readonly name = "SoakConfigError";
}

const fail = (message: string): never => {
  throw new SoakConfigError(message);
};

const field = (object: object, key: string): unknown =>
  key in object ? (object as Record<string, unknown>)[key] : undefined;

const text = (object: object, key: string): string => {
  const value = field(object, key);
  return typeof value === "string" && value !== ""
    ? value
    : fail(`soak.json: ${key} must be a non-empty string`);
};

const natural = (object: object, key: string): number => {
  const value = field(object, key);
  return typeof value === "number" && Number.isSafeInteger(value) && value > 0
    ? value
    : fail(`soak.json: ${key} must be a positive integer`);
};

const hexList = (value: unknown, key: string): string[] => {
  if (value === undefined) return [];
  if (!Array.isArray(value)) return fail(`soak.json: ${key} must be a list`);
  return (value as unknown[]).map((item) =>
    typeof item === "string" && /^([0-9a-f]{2})+$/u.test(item)
      ? item
      : fail(`soak.json: ${key} holds a value that is not lowercase hex`),
  );
};

const pathIn = (dir: string, path: string): string =>
  isAbsolute(path) ? path : resolve(dir, path);

/** Reads and checks `<dir>/soak.json`; relative paths resolve against `dir`. */
export const readSoakConfig = async (dir: string): Promise<SoakConfig> => {
  const raw = JSON.parse(
    await readFile(join(dir, "soak.json"), "utf8"),
  ) as unknown;
  if (typeof raw !== "object" || raw === null)
    return fail("soak.json must hold an object");
  const tracked = field(raw, "trackedSet");
  if (typeof tracked !== "object" || tracked === null)
    return fail("soak.json: trackedSet must be an object");
  const addresses = hexList(
    field(tracked, "addresses"),
    "trackedSet.addresses",
  );
  const ledger = field(raw, "ledgerAddresses");
  const plugins = field(raw, "plugins") ?? [];
  if (!Array.isArray(plugins)) return fail("soak.json: plugins must be a list");
  return {
    socketPath: pathIn(dir, text(raw, "socketPath")),
    networkMagic: natural(raw, "networkMagic"),
    binaryPath: pathIn(dir, text(raw, "binaryPath")),
    securityParameter: natural(raw, "securityParameter"),
    trackedSet: {
      addresses: new Set(addresses),
      paymentCredentials: new Set(
        hexList(
          field(tracked, "paymentCredentials"),
          "trackedSet.paymentCredentials",
        ),
      ),
      policies: new Set(
        hexList(field(tracked, "policies"), "trackedSet.policies"),
      ),
    },
    ledgerAddresses: (ledger === undefined
      ? addresses
      : hexList(ledger, "ledgerAddresses")
    ).map((hex) => Buffer.from(hex, "hex")),
    plugins: (plugins as unknown[]).map((plugin, index) => {
      if (typeof plugin !== "object" || plugin === null)
        return fail(`soak.json: plugins[${index}] must be an object`);
      return {
        module: pathIn(dir, text(plugin, "module")),
        options: field(plugin, "options") ?? {},
      };
    }),
  };
};
