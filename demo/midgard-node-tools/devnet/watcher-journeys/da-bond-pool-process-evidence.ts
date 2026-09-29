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

const TX_HASH = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#\d+$/u;
const DECIMAL = /^\d+$/u;

/** The name `assemble` prints for each withdraw step. */
const WITHDRAW_OUTPUT_ACTION = Object.freeze({
  begin: "BeginWithdraw",
  cancel: "CancelWithdraw",
  complete: "CompleteWithdraw",
});

type Parsed<T> =
  | Readonly<{ ok: true; value: T }>
  | Readonly<{ ok: false; error: string }>;

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

/** The single JSON object a da-bond command prints on stdout. */
const parseStdoutObject = (
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

const parseStatusField = (
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

const describeStatus = (status: Parsed<DaBondCliStatus>): string =>
  status.ok
    ? `${status.value.poolOutRef} ${status.value.state} ${status.value.lovelace}` +
      (status.value.unlockAt === undefined
        ? ""
        : ` unlockAt=${status.value.unlockAt}`)
    : `unreadable (${status.error})`;

/** The argv after `da-bond`, or undefined when there is no `da-bond`. */
const daBondArgs = (
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
const commandMismatches = (
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

/**
 * Judges one CLI chain (P18). Every check is returned, passing or not, so the
 * journey ledger records each piece of evidence.
 */
export const checkDaBondCliSubmitEvidence = (input: {
  readonly expectation: DaBondCliExpectation;
  readonly evidence: DaBondCliSubmitEvidence;
  /** The transaction id the adapter reported for this step. */
  readonly txId: string;
  readonly chainAfter?: DaBondCliChainRead;
}): readonly DaBondProcessEvidenceCheck[] => {
  const { expectation, evidence, txId, chainAfter } = input;
  const checks: DaBondProcessEvidenceCheck[] = [];
  const check = (name: string, ok: boolean, detail: string) =>
    checks.push({ name, ok, detail });

  const mismatches = commandMismatches(expectation, evidence);
  check(
    "command lines are the da-bond CLI",
    mismatches.length === 0,
    mismatches.length === 0
      ? [
          evidence.statusBefore,
          ...evidence.steps,
          evidence.submit,
          evidence.statusAfter,
        ]
          .map((run) => (daBondArgs(run) ?? []).slice(0, 2).join(" "))
          .join(" -> ")
      : mismatches.join("; "),
  );

  const runs = [
    evidence.statusBefore,
    ...evidence.steps,
    evidence.submit,
    evidence.statusAfter,
  ];
  const nonZero = runs.filter((run) => run.exitCode !== 0);
  check(
    "every process exits 0",
    nonZero.length === 0,
    nonZero.length === 0
      ? `${runs.length} processes, all exit 0`
      : nonZero
          .map(
            (run) =>
              `exit ${String(run.exitCode)}: ${run.argv.join(" ")}${run.stderr === undefined || run.stderr === "" ? "" : ` (${run.stderr.trim().split("\n").at(-1)})`}`,
          )
          .join("; "),
  );

  const output = parseStdoutObject(evidence.submit);
  const txHash =
    output.ok && typeof output.value.txHash === "string"
      ? output.value.txHash
      : undefined;
  check(
    "stdout names the landed txHash",
    txHash !== undefined && TX_HASH.test(txHash) && txHash === txId,
    output.ok
      ? `stdout txHash=${String(output.value.txHash)}, adapter txId=${txId}`
      : output.error,
  );
  check(
    "txHash confirmed on chain",
    evidence.confirmedOnChain,
    `txHash=${txHash ?? "none"}, confirmedOnChain=${String(evidence.confirmedOnChain)}`,
  );

  const before = parseStatusField(
    parseStdoutObject(evidence.statusBefore),
    null,
  );
  const same = parseStatusField(output, "status");
  const after = parseStatusField(parseStdoutObject(evidence.statusAfter), null);

  // The status in the submitting process's own output is already the pool
  // this transaction produced.
  const previous =
    output.ok && typeof output.value.previousPoolOutRef === "string"
      ? output.value.previousPoolOutRef
      : undefined;
  check(
    "same-output status is the post-transaction pool",
    before.ok &&
      same.ok &&
      txHash !== undefined &&
      same.value.poolOutRef.startsWith(`${txHash}#`) &&
      same.value.poolOutRef !== before.value.poolOutRef &&
      (expectation.action !== "top-up" || previous === before.value.poolOutRef),
    `before: ${describeStatus(before)}; same output: ${describeStatus(same)}` +
      (expectation.action === "top-up"
        ? `; previousPoolOutRef=${previous ?? "none"}`
        : ""),
  );

  const outputAction =
    output.ok && typeof output.value.action === "string"
      ? output.value.action
      : undefined;
  let matches = false;
  let expected = "";
  if (before.ok && same.ok) {
    const b = before.value;
    const s = same.value;
    switch (expectation.action === "top-up" ? "top-up" : expectation.step) {
      case "top-up": {
        const amount = (expectation as { amount: bigint }).amount;
        expected = `top-up of ${amount}: bonded ${b.lovelace + amount}, state ${b.state}`;
        matches =
          outputAction === "top-up" &&
          output.ok &&
          output.value.amount === amount.toString() &&
          s.state === b.state &&
          s.lovelace === b.lovelace + amount;
        break;
      }
      case "begin":
        expected = `BeginWithdraw: withdrawing with unlockAt, lovelace ${b.lovelace}`;
        matches =
          outputAction === WITHDRAW_OUTPUT_ACTION.begin &&
          b.state === "bonded" &&
          s.state === "withdrawing" &&
          s.unlockAt !== undefined &&
          s.lovelace === b.lovelace;
        break;
      case "cancel":
        expected = `CancelWithdraw: bonded, lovelace ${b.lovelace}`;
        matches =
          outputAction === WITHDRAW_OUTPUT_ACTION.cancel &&
          b.state === "withdrawing" &&
          s.state === "bonded" &&
          s.lovelace === b.lovelace;
        break;
      case "complete": {
        const amount = (expectation as { amount: bigint }).amount;
        expected = `CompleteWithdraw of ${amount}: bonded ${b.lovelace - amount}`;
        matches =
          outputAction === WITHDRAW_OUTPUT_ACTION.complete &&
          b.state === "withdrawing" &&
          s.state === "bonded" &&
          s.lovelace === b.lovelace - amount;
        break;
      }
    }
  }
  check(
    "same-output status matches the transaction",
    matches,
    `expected ${expected || "a readable status before and after"}; action=${outputAction ?? "none"}, same output: ${describeStatus(same)}`,
  );

  check(
    "da-bond status after reads the same pool",
    same.ok &&
      after.ok &&
      after.value.poolOutRef === same.value.poolOutRef &&
      after.value.state === same.value.state &&
      after.value.lovelace === same.value.lovelace &&
      after.value.unlockAt === same.value.unlockAt,
    `same output: ${describeStatus(same)}; status after: ${describeStatus(after)}`,
  );

  if (chainAfter !== undefined)
    check(
      "CLI status agrees with the adapter's chain read",
      same.ok &&
        same.value.state === chainAfter.state &&
        same.value.lovelace === chainAfter.lovelace &&
        (chainAfter.utxoRef === undefined ||
          chainAfter.utxoRef === same.value.poolOutRef),
      `same output: ${describeStatus(same)}; chain: ${chainAfter.state} ${chainAfter.lovelace} ${chainAfter.utxoRef ?? "(no outref)"}`,
    );
  return checks;
};

/** One `GET /readyz` answer of the committee node. */
export type DaBondPoolReadyz = Readonly<{
  httpStatus: number;
  ready: boolean;
  /** Every reason, in the body's order. */
  reasons: readonly string[];
  /** The reasons beginning `da_bond_pool_`, in the body's order. */
  poolReasons: readonly string[];
  /** `scanner.lastStartedAt`: when the node's last tick started, if any. */
  scannerLastStartedAt?: string;
}>;

/**
 * Reads the committee node's `/readyz` body (`{ ready, reasons, scanner }`,
 * served 200 when ready and 503 otherwise); refuses any other shape.
 */
export const parseDaBondPoolReadyz = (
  httpStatus: number,
  body: string,
): DaBondPoolReadyz => {
  const value: unknown = JSON.parse(body);
  if (
    !isRecord(value) ||
    typeof value.ready !== "boolean" ||
    !Array.isArray(value.reasons) ||
    !value.reasons.every((reason) => typeof reason === "string")
  )
    throw new Error(`/readyz body is not { ready, reasons[] }: ${body}`);
  if (
    (httpStatus === 200) !== value.ready ||
    (httpStatus !== 200 && httpStatus !== 503)
  )
    throw new Error(
      `/readyz answered ${httpStatus} with ready=${String(value.ready)}`,
    );
  const reasons = value.reasons as string[];
  const lastStartedAt = isRecord(value.scanner)
    ? value.scanner.lastStartedAt
    : undefined;
  return {
    httpStatus,
    ready: value.ready,
    reasons,
    poolReasons: reasons.filter((reason) => reason.startsWith("da_bond_pool_")),
    ...(typeof lastStartedAt === "string"
      ? { scannerLastStartedAt: lastStartedAt }
      : {}),
  };
};

/** One pool-monitor event line, with the pid of the process that wrote it. */
export type DaBondPoolStderrEvent = Readonly<{
  pid: number;
  event: string;
  line: string;
}>;

const NEWLINE = 0x0a;

/** A pool-monitor event name: `da_bond_pool_*`. */
export const isDaBondPoolEvent = (event: string): boolean =>
  event.startsWith("da_bond_pool_");

/**
 * Collects the pool-monitor events one node process writes to stderr (P27).
 * The cursor belongs to one pid: each `take` gets that process's whole stderr
 * capture so far, and returns the JSON event lines whose `event` `matches`
 * (by default the `da_bond_pool_*` ones) between the byte offset it stopped
 * at last time and the capture's last newline, each tied to the pid. A
 * partial last line waits for its newline; every other line (logs, other
 * events) is skipped. A restarted node gets a new cursor, so an event can
 * never be carried across the restart.
 */
export const createDaBondPoolStderrCursor = (
  pid: number,
  matches: (event: string) => boolean = isDaBondPoolEvent,
) => {
  let consumed = 0;
  return {
    pid,
    /** The byte offset read up to. */
    offset: () => consumed,
    take: (captured: Uint8Array): readonly DaBondPoolStderrEvent[] => {
      if (captured.length < consumed)
        throw new Error(`the stderr capture of pid ${pid} shrank`);
      const end = captured.lastIndexOf(NEWLINE) + 1;
      if (end <= consumed) return [];
      const text = Buffer.from(captured.subarray(consumed, end)).toString(
        "utf8",
      );
      consumed = end;
      const events: DaBondPoolStderrEvent[] = [];
      for (const line of text.split("\n")) {
        const trimmed = line.trim();
        if (!trimmed.startsWith("{")) continue;
        let value: unknown;
        try {
          value = JSON.parse(trimmed);
        } catch {
          continue;
        }
        if (
          isRecord(value) &&
          typeof value.event === "string" &&
          matches(value.event)
        )
          events.push({ pid, event: value.event, line: trimmed });
      }
      return events;
    },
  };
};
