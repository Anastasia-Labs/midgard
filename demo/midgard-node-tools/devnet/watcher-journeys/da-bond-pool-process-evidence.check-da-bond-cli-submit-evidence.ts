import {
  commandMismatches,
  daBondArgs,
  type DaBondCliChainRead,
  type DaBondCliExpectation,
  type DaBondCliSubmitEvidence,
  type DaBondProcessEvidenceCheck,
  describeStatus,
  isRecord,
  parseStatusField,
  parseStdoutObject,
  TX_HASH,
  WITHDRAW_OUTPUT_ACTION,
} from "./da-bond-pool-process-evidence.command-mismatches.js";

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
  /**
   * `l1Source.status` and, when it is `intervention`, the follower's reason
   * no wait clears (`l1Source.intervention`), when the body has them.
   */
  l1Source?: Readonly<{ status: string; intervention?: string }>;
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
  const l1Source = isRecord(value.l1Source) ? value.l1Source : undefined;
  return {
    httpStatus,
    ready: value.ready,
    reasons,
    poolReasons: reasons.filter((reason) => reason.startsWith("da_bond_pool_")),
    ...(typeof lastStartedAt === "string"
      ? { scannerLastStartedAt: lastStartedAt }
      : {}),
    ...(typeof l1Source?.status === "string"
      ? {
          l1Source: {
            status: l1Source.status,
            ...(typeof l1Source.intervention === "string"
              ? { intervention: l1Source.intervention }
              : {}),
          },
        }
      : {}),
  };
};

/** One pool-monitor event line, with the pid of the process that wrote it. */
export type DaBondPoolStderrEvent = Readonly<{
  pid: number;
  event: string;
  line: string;
}>;

export const NEWLINE = 0x0a;

/**
 * The pool-monitor transition events (pool-monitor.ts): the backing falling
 * below or returning to one DA bond, and the pool entering or leaving
 * `Withdrawing`. `da_bond_pool_read_failed` is not a transition: a failed read
 * keeps the last good check and adds no readiness reason. Nor are the
 * submitter's `*_backoff` lines.
 */
const DA_BOND_POOL_TRANSITION_EVENTS: ReadonlySet<string> = new Set([
  "da_bond_pool_backing_short",
  "da_bond_pool_backing_restored",
  "da_bond_pool_withdrawing",
  "da_bond_pool_bonded",
]);

/** A pool-monitor transition event. */
export const isDaBondPoolEvent = (event: string): boolean =>
  DA_BOND_POOL_TRANSITION_EVENTS.has(event);
