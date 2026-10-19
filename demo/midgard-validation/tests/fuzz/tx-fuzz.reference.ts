import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { dataFromCbor } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  Case,
  Constr,
  Delay,
  Force,
  Lambda,
  parseUPLC,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";

import { encodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.cardano-flat.js";

/**
 * The reference leg of the transaction differential: a UPLC evaluator outside
 * this repository, `<bin> uplc eval`, given by `MIDGARD_TX_FUZZ_UPLC_BIN`.
 * The leg and its pins are taken against the PV 11 fork build of the Aiken
 * CLI the repository pins (`demo/flake.lock`, `aiken v1.1.24+28e4288`, the
 * binary `scripts/aiken` runs); another build may report other spends. It
 * runs the script applied to the script context the trace builder derived,
 * under its default budget (the Cardano per-transaction maximum), and reports
 * the result and the spent budget. Its verdict under the declared ex-units
 * is: the result is unit (PlutusV3) and the spend is within them.
 * Opt-in only: CI never sets the variable and never depends on the binary.
 */

export const referenceBinary = (): string | undefined =>
  process.env.MIDGARD_TX_FUZZ_UPLC_BIN;

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

/** The Cardano flat of `script` applied to the context Data. */
export const appliedProgramFlat = (
  scriptFlat: Uint8Array,
  contextCbor: Uint8Array,
): Buffer =>
  encodeMidgardCekCardanoFlatProgram(
    new UPLCProgram(
      [1, 1, 0],
      new Application(
        parseUPLC(scriptFlat, "flat").body,
        UPLCConst.data(dataFromCbor(contextCbor)),
      ),
    ),
  );

export const makeReference = (binary: string) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-tx-fuzz-"));
  let counter = 0;
  const run = (programFlat: Uint8Array): ReferenceRun => {
    counter += 1;
    const file = join(directory, `program-${counter.toString()}.flat`);
    writeFileSync(file, programFlat);
    try {
      const evaluated = spawnSync(binary, ["uplc", "eval", "--flat", file], {
        encoding: "utf8",
        maxBuffer: 256 * 1024 * 1024,
      });
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
