/**
 * Process-level evidence for the pooled DA bond journey (ticket #692,
 * program rulings P16 and P18): what a real `da-committee-node` process and
 * real `midgard-node da-bond` processes must show, and the pure checks the
 * journey driver applies to it.
 *
 * - P16: the committee node's pool readiness reasons come from its `GET
 *   /readyz` body and its pool transitions from the JSON event lines it
 *   writes to stderr (`createDaBondPoolWiring` in
 *   `da-committee-node/src/coordinator/pool-monitor.ts`).
 * - P18: every top-up and withdraw step is submitted by the real CLI
 *   (`da-bond top-up`; `da-bond withdraw <step> --build-unsigned`, one
 *   `da-bond witness` per signer, `da-bond assemble`), bracketed by real
 *   `da-bond status` processes. The status printed by the submitting process
 *   must already be the post-transaction pool; one that still shows the old
 *   pool means the installed submit did not wait for confirmation (P15).
 *
 * Nothing here spawns a process: an adapter records what it ran, and these
 * functions judge the record, so both polarities are testable without a chain.
 */

/** One finished process, as the adapter ran it. */
export type DaBondPoolProcessRun = Readonly<{
  /** The full command line, program first. */
  argv: readonly string[];
  /** `null` when the process was killed by a signal. */
  exitCode: number | null;
  /** The signal that killed the process, when one did. */
  signal?: string;
  /** Set when the adapter killed the process after this many milliseconds. */
  timedOutAfterMs?: number;
  stdout: string;
  stderr?: string;
  /**
   * The variables the adapter set for the process, beyond the inherited
   * environment; a secret's value is recorded as `<redacted>`.
   */
  env?: Readonly<Record<string, string>>;
}>;

/** The CLI chain behind one pool transaction. */
export type DaBondCliSubmitEvidence = Readonly<{
  /** A `da-bond status` process run before the transaction was built. */
  statusBefore: DaBondPoolProcessRun;
  /**
   * The steps before the submitting process: for a withdraw step, the
   * `da-bond withdraw <step> --build-unsigned` run, then one `da-bond witness`
   * run per witness; empty for a top-up.
   */
  steps: readonly DaBondPoolProcessRun[];
  /** The submitting process: `da-bond top-up` or `da-bond assemble`. */
  submit: DaBondPoolProcessRun;
  /** A `da-bond status` process run after the submitting process exited. */
  statusAfter: DaBondPoolProcessRun;
  /**
   * The adapter found the submitting process's txHash confirmed on chain,
   * read independently of the CLI.
   */
  confirmedOnChain: boolean;
}>;

/** The pool transaction the CLI chain was asked for. */
export type DaBondCliExpectation =
  | Readonly<{ action: "top-up"; amount: bigint }>
  | Readonly<{ action: "withdraw"; step: "begin" | "cancel" }>
  | Readonly<{ action: "withdraw"; step: "complete"; amount: bigint }>;

/** The adapter's own read of the pool after the transaction. */
export type DaBondCliChainRead = Readonly<{
  state: "bonded" | "withdrawing" | "missing";
  lovelace: bigint;
  utxoRef?: string;
}>;

export type DaBondProcessEvidenceCheck = Readonly<{
  name: string;
  ok: boolean;
  detail: string;
}>;

/** The `status` object every da-bond chain command prints. */
export type DaBondCliStatus = Readonly<{
  poolOutRef: string;
  state: "bonded" | "withdrawing";
  lovelace: bigint;
  unlockAt?: bigint;
}>;

export const TX_HASH = /^[0-9a-f]{64}$/u;

const OUT_REF = /^[0-9a-f]{64}#\d+$/u;

const DECIMAL = /^\d+$/u;

/** The name `assemble` prints for each withdraw step. */
export const WITHDRAW_OUTPUT_ACTION = Object.freeze({
  begin: "BeginWithdraw",
  cancel: "CancelWithdraw",
  complete: "CompleteWithdraw",
});

type Parsed<T> =
  | Readonly<{ ok: true; value: T }>
  | Readonly<{ ok: false; error: string }>;

export const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

/** The single JSON object a da-bond command prints on stdout. */
export const parseStdoutObject = (
  run: DaBondPoolProcessRun,
): Parsed<Record<string, unknown>> => {
  let value: unknown;
  try {
    value = JSON.parse(run.stdout);
  } catch (error) {
    return {
      ok: false,
      error: `stdout is not one JSON value: ${error instanceof Error ? error.message : String(error)}`,
    };
  }
  return isRecord(value)
    ? { ok: true, value }
    : { ok: false, error: "stdout is not a JSON object" };
};

/** Reads the `status` object da-bond prints; refuses any other shape. */
export const parseDaBondCliStatus = (value: unknown): DaBondCliStatus => {
  if (!isRecord(value)) throw new Error("status is not an object");
  const { poolOutRef, state, lovelace, unlockAt } = value;
  if (typeof poolOutRef !== "string" || !OUT_REF.test(poolOutRef))
    throw new Error(
      `status.poolOutRef is not an outref: ${String(poolOutRef)}`,
    );
  if (state !== "bonded" && state !== "withdrawing")
    throw new Error(
      `status.state is not bonded or withdrawing: ${String(state)}`,
    );
  if (typeof lovelace !== "string" || !DECIMAL.test(lovelace))
    throw new Error(
      `status.lovelace is not a decimal string: ${String(lovelace)}`,
    );
  if (
    unlockAt !== undefined &&
    (typeof unlockAt !== "string" || !DECIMAL.test(unlockAt))
  )
    throw new Error(
      `status.unlockAt is not a decimal string: ${JSON.stringify(unlockAt)}`,
    );
  if ((state === "withdrawing") !== (unlockAt !== undefined))
    throw new Error("status.unlockAt must be present exactly when Withdrawing");
  return {
    poolOutRef,
    state,
    lovelace: BigInt(lovelace),
    ...(unlockAt === undefined ? {} : { unlockAt: BigInt(unlockAt) }),
  };
};

export const parseStatusField = (
  object: Parsed<Record<string, unknown>>,
  field: "status" | null,
): Parsed<DaBondCliStatus> => {
  if (!object.ok) return object;
  try {
    return {
      ok: true,
      value: parseDaBondCliStatus(
        field === null ? object.value : object.value[field],
      ),
    };
  } catch (error) {
    return {
      ok: false,
      error: error instanceof Error ? error.message : String(error),
    };
  }
};

export const describeStatus = (status: Parsed<DaBondCliStatus>): string =>
  status.ok
    ? `${status.value.poolOutRef} ${status.value.state} ${status.value.lovelace}` +
      (status.value.unlockAt === undefined
        ? ""
        : ` unlockAt=${status.value.unlockAt}`)
    : `unreadable (${status.error})`;

/** The argv after `da-bond`, or undefined when there is no `da-bond`. */
export const daBondArgs = (
  run: DaBondPoolProcessRun,
): readonly string[] | undefined => {
  const at = run.argv.indexOf("da-bond");
  return at < 0 ? undefined : run.argv.slice(at + 1);
};

const optionValue = (
  args: readonly string[],
  option: string,
): string | undefined => {
  const at = args.indexOf(option);
  return at < 0 ? undefined : args[at + 1];
};

/** Why `run` is not `da-bond <words...>` with the given options; undefined when it is. */
const commandMismatch = (
  run: DaBondPoolProcessRun,
  words: readonly string[],
  options: Readonly<Record<string, string | true>>,
): string | undefined => {
  const args = daBondArgs(run);
  const shown = run.argv.join(" ");
  if (args === undefined) return `not a da-bond command: ${shown}`;
  if (words.some((word, index) => args[index] !== word))
    return `expected da-bond ${words.join(" ")}: ${shown}`;
  for (const [option, expected] of Object.entries(options)) {
    const value = optionValue(args, option);
    if (value === undefined || value.startsWith("--"))
      return `missing ${option}: ${shown}`;
    if (expected !== true && value !== expected)
      return `${option} is ${value}, expected ${expected}: ${shown}`;
  }
  return undefined;
};

/** The expected command line of every process in the chain, in order. */
export const commandMismatches = (
  expectation: DaBondCliExpectation,
  evidence: DaBondCliSubmitEvidence,
): string[] => {
  const status = { "--manifest": true } as const;
  const found = [
    commandMismatch(evidence.statusBefore, ["status"], status),
    commandMismatch(evidence.statusAfter, ["status"], status),
  ];
  if (expectation.action === "top-up") {
    if (evidence.steps.length > 0)
      found.push("a top-up is one da-bond top-up process, with no steps");
    found.push(
      commandMismatch(evidence.submit, ["top-up"], {
        "--manifest": true,
        "--amount": expectation.amount.toString(),
        "--wallet-seed-env": true,
      }),
    );
  } else {
    const [build, ...witnesses] = evidence.steps;
    if (build === undefined)
      found.push("no da-bond withdraw --build-unsigned run");
    else
      found.push(
        commandMismatch(build, ["withdraw", expectation.step], {
          "--manifest": true,
          "--build-unsigned": true,
          ...(expectation.step === "complete"
            ? { "--amount": expectation.amount.toString() }
            : {}),
        }),
      );
    if (witnesses.length === 0) found.push("no da-bond witness run");
    for (const witness of witnesses)
      found.push(commandMismatch(witness, ["witness"], { "--key-env": true }));
    found.push(
      commandMismatch(evidence.submit, ["assemble"], { "--manifest": true }),
    );
  }
  return found.filter((mismatch) => mismatch !== undefined);
};
