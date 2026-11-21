/**
 * The real `midgard-node da-bond` CLI behind the live pooled DA bond
 * journey's pool transactions (rulings P18 and P27(6)). Each transaction is a
 * chain of processes: `da-bond status`, then `da-bond top-up`, or
 * `da-bond withdraw <step> --build-unsigned`, one `da-bond witness` per key
 * and `da-bond assemble`; then `da-bond status` again. Every process's argv,
 * exit code, stdout, stderr and environment (secrets redacted) is kept as the
 * step's P18 evidence, and the submitted transaction is confirmed by the
 * adapter's own chain read, not by the CLI.
 *
 * A process that exits non-zero stops the chain and fails the step; nothing
 * falls back to the in-process commands.
 */
import { spawn } from "node:child_process";
import { join } from "node:path";

import { redactDaBondPoolEnv } from "./da-bond-pool-committee-process.js";
import type {
  DaBondCliSubmitEvidence,
  DaBondPoolProcessRun,
} from "./da-bond-pool-process-evidence.js";

/** The variable `da-bond top-up --wallet-seed-env` names. */
export const DA_BOND_JOURNEY_WALLET_ENV = "DA_BOND_JOURNEY_WALLET_SEED";
/** The variable `da-bond witness --key-env` names. */
export const DA_BOND_JOURNEY_KEY_ENV = "DA_BOND_JOURNEY_KEY";

const SECRETS = new Set([DA_BOND_JOURNEY_WALLET_ENV, DA_BOND_JOURNEY_KEY_ENV]);

/** Runs one process to completion and records it. */
export type DaBondCliProcessRunner = (
  argv: readonly [string, ...string[]],
  env: Readonly<Record<string, string>>,
) => Promise<DaBondPoolProcessRun>;

/**
 * Spawns `argv` with exactly `env` (plus nothing inherited), waits for it,
 * and records it with its secrets redacted. SIGKILLs it after `timeoutMs`.
 */
export const spawnDaBondCliProcess =
  (options: {
    readonly cwd: string;
    readonly timeoutMs: number;
    /** Inherited variables, left out of the record. */
    readonly inheritedNames: ReadonlySet<string>;
  }): DaBondCliProcessRunner =>
  (argv, env) =>
    new Promise((resolveRun, reject) => {
      const [program, ...args] = argv;
      const child = spawn(program, args, {
        cwd: options.cwd,
        env: { ...env },
        stdio: ["ignore", "pipe", "pipe"],
      });
      const stdout: Buffer[] = [];
      const stderr: Buffer[] = [];
      child.stdout.on("data", (chunk: Buffer) => stdout.push(chunk));
      child.stderr.on("data", (chunk: Buffer) => stderr.push(chunk));
      let timedOut = false;
      const timer = setTimeout(() => {
        timedOut = true;
        child.kill("SIGKILL");
      }, options.timeoutMs);
      child.once("error", (error) => {
        clearTimeout(timer);
        reject(error);
      });
      child.once("close", (code, signal) => {
        clearTimeout(timer);
        const recorded = Object.fromEntries(
          Object.entries(env).filter(
            ([name]) => !options.inheritedNames.has(name),
          ),
        );
        resolveRun({
          argv: [...argv],
          exitCode: code,
          ...(signal === null ? {} : { signal }),
          ...(timedOut ? { timedOutAfterMs: options.timeoutMs } : {}),
          stdout: Buffer.concat(stdout).toString("utf8"),
          stderr: Buffer.concat(stderr).toString("utf8"),
          env: redactDaBondPoolEnv(recorded, SECRETS),
        });
      });
    });

/** How a recorded run ended: its exit code, or the signal that killed it. */
const processEnding = (run: DaBondPoolProcessRun): string =>
  run.signal === undefined
    ? `exit ${String(run.exitCode)}`
    : `killed by ${run.signal}${run.timedOutAfterMs === undefined ? "" : ` after the ${run.timedOutAfterMs.toString()} ms timeout`}`;

export class DaBondCliProcessError extends Error {
  readonly runs: readonly DaBondPoolProcessRun[];
  constructor(message: string, runs: readonly DaBondPoolProcessRun[]) {
    const last = runs.at(-1);
    super(
      `${message}${last === undefined ? "" : `: ${processEnding(last)} from ${last.argv.join(" ")}${last.stderr ? `; stderr: ${last.stderr.trim().split("\n").slice(-3).join(" | ")}` : ""}`}`,
    );
    this.name = "DaBondCliProcessError";
    this.runs = runs;
  }
}

export type DaBondPoolCliTx = Readonly<{
  txId: string;
  cli: DaBondCliSubmitEvidence;
  /** The submitting process's stdout object. */
  output: Readonly<Record<string, unknown>>;
}>;

export type DaBondPoolWithdrawKey = Readonly<{ role: string; seed: string }>;

/**
 * The da-bond CLI over one deployment. `confirm` is the adapter's own chain
 * read of a submitted transaction; `record` keeps each chain as an artifact
 * (called on failure too).
 */
export const createDaBondPoolCli = (deps: {
  readonly run: DaBondCliProcessRunner;
  /** `[node, <repo>/demo/midgard-node/dist/index.js]`. */
  readonly command: readonly [string, string];
  readonly manifestPath: string;
  /**
   * The environment every process gets (the inherited allowlist and the
   * settings, with the node's L1 access: its local node and its database).
   */
  readonly env: Readonly<Record<string, string>>;
  /** A fresh directory for one withdraw step's files. */
  readonly workDirectory: (label: string) => string;
  readonly confirm: (txHash: string) => Promise<boolean>;
  readonly record?: (
    label: string,
    runs: readonly DaBondPoolProcessRun[],
  ) => Promise<void>;
}) => {
  const chainOptions = ["--manifest", deps.manifestPath];
  const daBond = (...args: string[]): [string, ...string[]] => [
    ...deps.command,
    "da-bond",
    ...args,
  ];
  const record = deps.record ?? (async () => {});

  /** Runs one process of the chain; stops the chain on a non-zero exit. */
  const step = async (
    label: string,
    runs: DaBondPoolProcessRun[],
    argv: [string, ...string[]],
    secrets: Readonly<Record<string, string>> = {},
  ): Promise<DaBondPoolProcessRun> => {
    const run = await deps.run(argv, { ...deps.env, ...secrets });
    runs.push(run);
    if (run.exitCode !== 0) {
      await record(label, runs);
      throw new DaBondCliProcessError(`da-bond ${label} failed`, runs);
    }
    return run;
  };

  const submitted = async (
    label: string,
    runs: DaBondPoolProcessRun[],
    statusBefore: DaBondPoolProcessRun,
    steps: readonly DaBondPoolProcessRun[],
    submit: DaBondPoolProcessRun,
  ): Promise<DaBondPoolCliTx> => {
    let output: Record<string, unknown>;
    try {
      output = JSON.parse(submit.stdout) as Record<string, unknown>;
    } catch {
      await record(label, runs);
      throw new DaBondCliProcessError(
        `da-bond ${label} printed no JSON object`,
        runs,
      );
    }
    const txId = output?.txHash;
    if (typeof txId !== "string") {
      await record(label, runs);
      throw new DaBondCliProcessError(
        `da-bond ${label} printed no txHash`,
        runs,
      );
    }
    const confirmedOnChain = await deps.confirm(txId);
    const statusAfter = await step(
      label,
      runs,
      daBond("status", ...chainOptions),
    );
    await record(label, runs);
    return {
      txId,
      output,
      cli: { statusBefore, steps, submit, statusAfter, confirmedOnChain },
    };
  };

  return {
    topUp: async (input: {
      readonly amount: bigint;
      readonly walletSeed: string;
    }): Promise<DaBondPoolCliTx> => {
      const label = `top-up ${input.amount.toString()}`;
      const runs: DaBondPoolProcessRun[] = [];
      const statusBefore = await step(
        label,
        runs,
        daBond("status", ...chainOptions),
      );
      const submit = await step(
        label,
        runs,
        daBond(
          "top-up",
          ...chainOptions,
          "--amount",
          input.amount.toString(),
          "--wallet-seed-env",
          DA_BOND_JOURNEY_WALLET_ENV,
        ),
        { [DA_BOND_JOURNEY_WALLET_ENV]: input.walletSeed },
      );
      return submitted(label, runs, statusBefore, [], submit);
    },

    withdraw: async (input: {
      readonly step: "begin" | "cancel" | "complete";
      readonly feeAddress: string;
      /** The owners who sign, as key hashes. */
      readonly signers: readonly string[];
      /** One witness per key: each signer, then the fee payer if distinct. */
      readonly witnesses: readonly DaBondPoolWithdrawKey[];
      readonly complete?: Readonly<{ amount: bigint; to: string }>;
    }): Promise<DaBondPoolCliTx> => {
      const label = `withdraw ${input.step}`;
      const directory = deps.workDirectory(label);
      const unsigned = join(directory, "unsigned.json");
      const runs: DaBondPoolProcessRun[] = [];
      const statusBefore = await step(
        label,
        runs,
        daBond("status", ...chainOptions),
      );
      const build = await step(
        label,
        runs,
        daBond(
          "withdraw",
          input.step,
          ...chainOptions,
          "--fee-address",
          input.feeAddress,
          "--signers",
          input.signers.join(","),
          "--build-unsigned",
          unsigned,
          ...(input.complete === undefined
            ? []
            : [
                "--amount",
                input.complete.amount.toString(),
                "--to",
                input.complete.to,
              ]),
        ),
      );
      const witnessRuns: DaBondPoolProcessRun[] = [];
      const witnessFiles: string[] = [];
      for (const key of input.witnesses) {
        const out = join(directory, `witness-${key.role}.json`);
        witnessRuns.push(
          await step(
            label,
            runs,
            daBond(
              "witness",
              unsigned,
              "--key-env",
              DA_BOND_JOURNEY_KEY_ENV,
              "--out",
              out,
            ),
            { [DA_BOND_JOURNEY_KEY_ENV]: key.seed },
          ),
        );
        witnessFiles.push(out);
      }
      const submit = await step(
        label,
        runs,
        daBond("assemble", ...chainOptions, unsigned, ...witnessFiles),
      );
      return submitted(
        label,
        runs,
        statusBefore,
        [build, ...witnessRuns],
        submit,
      );
    },
  };
};
