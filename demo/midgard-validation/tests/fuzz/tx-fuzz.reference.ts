import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  Application,
  Builtin,
  Case,
  Constr,
  Delay,
  Force,
  Lambda,
  type UPLCTerm,
} from "../../src/uplc-term.js";
import { aikenBinary } from "./cek-step-fuzz.oracle.js";

/**
 * The reference leg of the transaction differential: a UPLC evaluator outside
 * this repository, `<bin> uplc eval`, given by `MIDGARD_TX_FUZZ_UPLC_BIN`.
 * The leg and its pins are taken against the PV 11 fork build of the Aiken
 * CLI the repository pins (`demo/flake.lock`, `aiken v1.1.24+28e4288`, the
 * binary `scripts/aiken` runs); another build may report other spends. It
 * reads the script's own flat bytes, applies them to the script context the
 * trace builder derived (`contextArgument`), runs the result under its default
 * budget (the Cardano per-transaction maximum), and reports the result and the
 * spent budget. Nothing in the leg passes through this repository's Flat or
 * Data decoders, so a fault in either shows as a divergence. Its verdict under the declared ex-units
 * is: the result is unit (PlutusV3) and the spend is within them.
 * The value `pinned` names that build, resolved as the step oracle resolves
 * it; CI runs the leg so (the `tx-fuzz-reference` preflight check). Unset,
 * the leg is off.
 */

const PINNED_REFERENCE = "pinned";

export const referenceBinary = (): string | undefined => {
  const configured = process.env.MIDGARD_TX_FUZZ_UPLC_BIN;
  return configured === PINNED_REFERENCE ? aikenBinary() : configured;
};

export type ReferenceRun =
  | {
      readonly kind: "result";
      readonly result: string;
      readonly cpu: bigint;
      readonly memory: bigint;
    }
  | { readonly kind: "overBudget" }
  | { readonly kind: "failed"; readonly output: string };

/** The evaluator's report of a run stopped by its budget. */
const OVER_BUDGET_REPORT = "execution went over budget";

/**
 * Reads one CBOR Plutus Data item into the evaluator's text syntax for a data
 * literal. The reader is the leg's own: it shares no code with the decoders
 * under test.
 */
const dataText = (cbor: Uint8Array): string => {
  let offset = 0;
  const take = (length: number): Uint8Array => {
    if (offset + length > cbor.length) {
      throw new Error("context CBOR ends early");
    }
    const bytes = cbor.subarray(offset, offset + length);
    offset += length;
    return bytes;
  };
  const unsigned = (bytes: Uint8Array): bigint =>
    bytes.reduce((value, byte) => (value << 8n) | BigInt(byte), 0n);
  /** A head's major type and argument; null for an indefinite length. */
  const head = (): {
    readonly major: number;
    readonly argument: bigint | null;
  } => {
    const initial = take(1)[0]!;
    const info = initial & 0x1f;
    const major = initial >> 5;
    if (info < 24) return { major, argument: BigInt(info) };
    if (info === 31) return { major, argument: null };
    const width = { 24: 1, 25: 2, 26: 4, 27: 8 }[info];
    if (width === undefined)
      throw new Error(`context CBOR head ${initial.toString(16)}`);
    return { major, argument: unsigned(take(width)) };
  };
  const atBreak = (): boolean => {
    if (cbor[offset] !== 0xff) return false;
    offset += 1;
    return true;
  };
  const items = <A>(count: bigint | null, item: () => A): A[] => {
    const read: A[] = [];
    if (count === null) while (!atBreak()) read.push(item());
    else for (let index = 0n; index < count; index += 1n) read.push(item());
    return read;
  };
  const bytes = (argument: bigint | null): Uint8Array => {
    if (argument !== null) return take(Number(argument));
    const chunks = items(null, () => {
      const chunk = head();
      if (chunk.major !== 2 || chunk.argument === null) {
        throw new Error("context CBOR byte-string chunk");
      }
      return take(Number(chunk.argument));
    });
    return Buffer.concat(chunks);
  };
  const constr = (tag: bigint, fields: string): string =>
    `Constr ${tag.toString()} ${fields}`;
  const list = (count: bigint | null): string =>
    `[${items(count, item).join(", ")}]`;
  const listOf = (): string => {
    const { major, argument } = head();
    if (major !== 4) throw new Error("context CBOR constructor fields");
    return list(argument);
  };
  const item = (): string => {
    const { major, argument } = head();
    switch (major) {
      case 0:
        return `I ${argument!.toString()}`;
      case 1:
        return `I ${(-1n - argument!).toString()}`;
      case 2:
        return `B #${Buffer.from(bytes(argument)).toString("hex")}`;
      case 4:
        return `List ${list(argument)}`;
      case 5:
        return `Map [${items(argument, () => `(${item()}, ${item()})`).join(", ")}]`;
      case 6: {
        const tag = argument!;
        if (tag === 2n || tag === 3n) {
          const magnitude = head();
          if (magnitude.major !== 2) throw new Error("context CBOR bignum");
          const value = unsigned(bytes(magnitude.argument));
          return `I ${(tag === 2n ? value : -1n - value).toString()}`;
        }
        if (tag >= 121n && tag <= 127n) return constr(tag - 121n, listOf());
        if (tag >= 1280n && tag <= 1400n) {
          return constr(tag - 1280n + 7n, listOf());
        }
        if (tag === 102n) {
          const pair = head();
          if (pair.major !== 4 || pair.argument !== 2n) {
            throw new Error("context CBOR tag-102 constructor");
          }
          const index = head();
          if (index.major !== 0)
            throw new Error("context CBOR constructor index");
          return constr(index.argument!, listOf());
        }
        throw new Error(`context CBOR tag ${tag.toString()}`);
      }
      default:
        throw new Error(`context CBOR major type ${major.toString()}`);
    }
  };
  const text = item();
  if (offset !== cbor.length)
    throw new Error("context CBOR has trailing bytes");
  return text;
};

/** The script context as an argument for `uplc eval`: a data constant. */
export const contextArgument = (contextCbor: Uint8Array): string =>
  `(con data (${dataText(contextCbor)}))`;

export const makeReference = (binary: string) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-tx-fuzz-"));
  let counter = 0;
  /** Runs the script, given as its own flat bytes, on the context. */
  const run = (
    scriptFlat: Uint8Array,
    contextCbor: Uint8Array,
  ): ReferenceRun => {
    counter += 1;
    const file = join(directory, `script-${counter.toString()}.flat`);
    writeFileSync(file, scriptFlat);
    try {
      const evaluated = spawnSync(
        binary,
        ["uplc", "eval", "--flat", file, contextArgument(contextCbor)],
        { encoding: "utf8", maxBuffer: 256 * 1024 * 1024 },
      );
      if (evaluated.error !== undefined) throw evaluated.error;
      const output = `${evaluated.stdout ?? ""}${evaluated.stderr ?? ""}`;
      if (evaluated.status !== 0) {
        return output.includes(OVER_BUDGET_REPORT)
          ? { kind: "overBudget" }
          : { kind: "failed", output };
      }
      const parsed = JSON.parse(evaluated.stdout) as {
        readonly result: string;
        readonly cpu: number;
        readonly mem: number;
      };
      return {
        kind: "result",
        result: parsed.result,
        cpu: BigInt(parsed.cpu),
        memory: BigInt(parsed.mem),
      };
    } finally {
      rmSync(file, { force: true });
    }
  };
  return {
    run,
    close: () => rmSync(directory, { recursive: true, force: true }),
  };
};

export type Reference = ReturnType<typeof makeReference>;

/** The flat builtin tags a term names. */
export const builtinTagsOf = (term: UPLCTerm): ReadonlySet<number> => {
  const tags = new Set<number>();
  const pending: UPLCTerm[] = [term];
  for (let next = pending.pop(); next !== undefined; next = pending.pop()) {
    if (next instanceof Builtin) tags.add(Number(next.builtinTag));
    else if (next instanceof Application) pending.push(next.func, next.arg);
    else if (next instanceof Lambda) pending.push(next.body);
    else if (next instanceof Delay) pending.push(next.delayedTerm);
    else if (next instanceof Force) pending.push(next.forced);
    else if (next instanceof Constr) pending.push(...next.terms);
    else if (next instanceof Case) {
      pending.push(next.constrTerm, ...next.continuations);
    }
  }
  return tags;
};
