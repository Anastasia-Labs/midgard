import { describe, expect, it } from "vitest";

import {
  checkDaBondCliSubmitEvidence,
  createDaBondPoolStderrCursor,
  type DaBondCliExpectation,
  type DaBondCliSubmitEvidence,
  type DaBondPoolProcessRun,
  parseDaBondCliStatus,
  parseDaBondPoolReadyz,
} from "./da-bond-pool-process-evidence.js";

const OLD_TX = "11".repeat(32);
const NEW_TX = "22".repeat(32);
const MANIFEST = ["--manifest", "/run/deployment-manifest.json"];

const run = (
  args: readonly string[],
  stdout: unknown,
  exitCode: number | null = 0,
): DaBondPoolProcessRun => ({
  argv: ["node", "dist/index.js", "da-bond", ...args],
  exitCode,
  stdout: JSON.stringify(stdout, null, 2),
});

const status = (
  txHash: string,
  lovelace: bigint,
  unlockAt?: bigint,
): Record<string, unknown> => ({
  poolOutRef: `${txHash}#0`,
  state: unlockAt === undefined ? "bonded" : "withdrawing",
  lovelace: lovelace.toString(),
  backing: (lovelace - 5_000_000n).toString(),
  requiredBacking: "500000000",
  belowBond: lovelace - 5_000_000n < 500_000_000n,
  ...(unlockAt === undefined
    ? {}
    : { unlockAt: unlockAt.toString(), unlockable: false }),
});

const topUpEvidence = (
  amount = 400_000_000n,
  overrides: Partial<DaBondCliSubmitEvidence> = {},
): DaBondCliSubmitEvidence => ({
  statusBefore: run(["status", ...MANIFEST], status(OLD_TX, 125_000_000n)),
  steps: [],
  submit: run(
    [
      "top-up",
      ...MANIFEST,
      "--amount",
      amount.toString(),
      "--wallet-seed-env",
      "FUNDING_SEED",
    ],
    {
      action: "top-up",
      txHash: NEW_TX,
      amount: amount.toString(),
      previousPoolOutRef: `${OLD_TX}#0`,
      status: status(NEW_TX, 125_000_000n + amount),
    },
  ),
  statusAfter: run(
    ["status", ...MANIFEST],
    status(NEW_TX, 125_000_000n + amount),
  ),
  confirmedOnChain: true,
  ...overrides,
});

const withdrawEvidence = (
  step: "begin" | "cancel" | "complete",
  overrides: Partial<DaBondCliSubmitEvidence> = {},
): DaBondCliSubmitEvidence => {
  const lovelace = 700_000_000n;
  const unlockAt = 1_800_000_000_000n;
  const before =
    step === "begin"
      ? status(OLD_TX, lovelace)
      : status(OLD_TX, lovelace, unlockAt);
  const after =
    step === "begin"
      ? status(NEW_TX, lovelace, unlockAt)
      : status(NEW_TX, step === "complete" ? lovelace - 50_000_000n : lovelace);
  return {
    statusBefore: run(["status", ...MANIFEST], before),
    steps: [
      run(
        [
          "withdraw",
          step,
          ...MANIFEST,
          "--fee-address",
          "addr_test1fee",
          "--signers",
          "aa,bb",
          "--build-unsigned",
          "/w/unsigned.json",
          ...(step === "complete"
            ? ["--amount", "50000000", "--to", "addr_test1to"]
            : []),
        ],
        {},
      ),
      run(["witness", "/w/unsigned.json", "--key-env", "OWNER_A"], {}),
      run(["witness", "/w/unsigned.json", "--key-env", "OWNER_B"], {}),
    ],
    submit: run(
      ["assemble", ...MANIFEST, "/w/unsigned.json", "/w/a.json", "/w/b.json"],
      {
        action: {
          begin: "BeginWithdraw",
          cancel: "CancelWithdraw",
          complete: "CompleteWithdraw",
        }[step],
        txHash: NEW_TX,
        ownerWitnesses: ["aa", "bb"],
        updateThreshold: "2",
        status: after,
      },
    ),
    statusAfter: run(["status", ...MANIFEST], after),
    confirmedOnChain: true,
    ...overrides,
  };
};

const failing = (
  expectation: DaBondCliExpectation,
  evidence: DaBondCliSubmitEvidence,
  chainAfter?: Parameters<typeof checkDaBondCliSubmitEvidence>[0]["chainAfter"],
): string[] =>
  checkDaBondCliSubmitEvidence({
    expectation,
    evidence,
    txId: NEW_TX,
    ...(chainAfter === undefined ? {} : { chainAfter }),
  })
    .filter((check) => !check.ok)
    .map((check) => check.name);

const TOP_UP = { action: "top-up", amount: 400_000_000n } as const;

describe("da-bond CLI submit evidence (P18)", () => {
  it("accepts an honest top-up chain and names every check", () => {
    const checks = checkDaBondCliSubmitEvidence({
      expectation: TOP_UP,
      evidence: topUpEvidence(),
      txId: NEW_TX,
      chainAfter: {
        state: "bonded",
        lovelace: 525_000_000n,
        utxoRef: `${NEW_TX}#0`,
      },
    });
    expect(checks.map(({ name, ok }) => [name, ok])).toEqual([
      ["command lines are the da-bond CLI", true],
      ["every process exits 0", true],
      ["stdout names the landed txHash", true],
      ["txHash confirmed on chain", true],
      ["same-output status is the post-transaction pool", true],
      ["same-output status matches the transaction", true],
      ["da-bond status after reads the same pool", true],
      ["CLI status agrees with the adapter's chain read", true],
    ]);
  });

  it("accepts honest begin, cancel and complete chains", () => {
    expect(
      failing({ action: "withdraw", step: "begin" }, withdrawEvidence("begin")),
    ).toEqual([]);
    expect(
      failing(
        { action: "withdraw", step: "cancel" },
        withdrawEvidence("cancel"),
      ),
    ).toEqual([]);
    expect(
      failing(
        { action: "withdraw", step: "complete", amount: 50_000_000n },
        withdrawEvidence("complete"),
      ),
    ).toEqual([]);
  });

  it("refuses a same-output status that still shows the old pool (the submit did not wait)", () => {
    const honest = topUpEvidence();
    const stale = {
      ...honest,
      submit: run(honest.submit.argv.slice(3), {
        action: "top-up",
        txHash: NEW_TX,
        amount: "400000000",
        previousPoolOutRef: `${OLD_TX}#0`,
        status: status(OLD_TX, 125_000_000n),
      }),
    };
    expect(failing(TOP_UP, stale)).toEqual([
      "same-output status is the post-transaction pool",
      "same-output status matches the transaction",
      "da-bond status after reads the same pool",
    ]);
  });

  it("refuses a pool outref the transaction did not produce", () => {
    const honest = topUpEvidence();
    const foreign = "33".repeat(32);
    const moved = {
      ...honest,
      submit: run(honest.submit.argv.slice(3), {
        action: "top-up",
        txHash: NEW_TX,
        amount: "400000000",
        previousPoolOutRef: `${OLD_TX}#0`,
        status: status(foreign, 525_000_000n),
      }),
      statusAfter: run(["status", ...MANIFEST], status(foreign, 525_000_000n)),
    };
    expect(failing(TOP_UP, moved)).toEqual([
      "same-output status is the post-transaction pool",
    ]);
  });

  it("refuses a previousPoolOutRef that is not the pool status read before", () => {
    const honest = topUpEvidence();
    const other = {
      ...honest,
      statusBefore: run(
        ["status", ...MANIFEST],
        status("44".repeat(32), 125_000_000n),
      ),
    };
    expect(failing(TOP_UP, other)).toEqual([
      "same-output status is the post-transaction pool",
    ]);
  });

  it("refuses a status amount that is not the top-up's", () => {
    expect(
      failing({ action: "top-up", amount: 400_000_001n }, topUpEvidence()),
    ).toEqual([
      "command lines are the da-bond CLI",
      "same-output status matches the transaction",
    ]);
  });

  it("refuses an unconfirmed txHash, a different txHash and a failed process", () => {
    expect(
      failing(TOP_UP, topUpEvidence(undefined, { confirmedOnChain: false })),
    ).toEqual(["txHash confirmed on chain"]);
    expect(
      checkDaBondCliSubmitEvidence({
        expectation: TOP_UP,
        evidence: topUpEvidence(),
        txId: "55".repeat(32),
      })
        .filter((check) => !check.ok)
        .map((check) => check.name),
    ).toEqual(["stdout names the landed txHash"]);
    const honest = topUpEvidence();
    expect(
      failing(TOP_UP, {
        ...honest,
        statusAfter: { ...honest.statusAfter, exitCode: 1 },
      }),
    ).toEqual(["every process exits 0"]);
    expect(
      failing(TOP_UP, {
        ...honest,
        submit: { ...honest.submit, exitCode: null, stdout: "" },
      }),
    ).toEqual([
      "every process exits 0",
      "stdout names the landed txHash",
      "same-output status is the post-transaction pool",
      "same-output status matches the transaction",
      "da-bond status after reads the same pool",
    ]);
  });

  it("refuses a submit that is not the da-bond CLI (an SDK-driven transaction)", () => {
    const honest = topUpEvidence();
    expect(
      failing(TOP_UP, {
        ...honest,
        submit: {
          ...honest.submit,
          argv: ["node", "journey.js", "top-up", "--amount", "400000000"],
        },
      }),
    ).toEqual(["command lines are the da-bond CLI"]);
    expect(
      failing(TOP_UP, {
        ...honest,
        statusBefore: {
          ...honest.statusBefore,
          argv: ["node", "dist/index.js", "da-bond", "status"],
        },
      }),
    ).toEqual(["command lines are the da-bond CLI"]);
  });

  it("refuses a withdraw step whose submitting process is not da-bond assemble --manifest", () => {
    for (const step of ["begin", "cancel", "complete"] as const) {
      const expectation =
        step === "complete"
          ? ({ action: "withdraw", step, amount: 50_000_000n } as const)
          : ({ action: "withdraw", step } as const);
      const honest = withdrawEvidence(step);
      const [, , , ...assembleArgs] = honest.submit.argv;
      for (const [what, argv] of [
        [
          "the build command",
          ["node", "dist/index.js", "da-bond", "withdraw", step, ...MANIFEST],
        ],
        ["another program", ["node", "journey.js", ...assembleArgs]],
        [
          "assemble without --manifest",
          [
            "node",
            "dist/index.js",
            "da-bond",
            "assemble",
            "/w/unsigned.json",
            "/w/a.json",
            "/w/b.json",
          ],
        ],
      ] as const)
        expect(
          failing(expectation, {
            ...honest,
            submit: { ...honest.submit, argv: [...argv] },
          }),
          `${step}: ${what}`,
        ).toEqual(["command lines are the da-bond CLI"]);
    }
  });

  it("refuses an option whose value is another option", () => {
    const topUp = topUpEvidence();
    expect(
      failing(TOP_UP, {
        ...topUp,
        submit: {
          ...topUp.submit,
          argv: [
            "node",
            "dist/index.js",
            "da-bond",
            "top-up",
            ...MANIFEST,
            "--wallet-seed-env",
            "--amount",
            "400000000",
          ],
        },
      }),
    ).toEqual(["command lines are the da-bond CLI"]);
  });

  it("refuses a withdraw chain without its build or witness processes, or with the wrong step", () => {
    const honest = withdrawEvidence("cancel");
    const cancel = { action: "withdraw", step: "cancel" } as const;
    expect(
      failing(cancel, { ...honest, steps: honest.steps.slice(0, 1) }),
    ).toEqual(["command lines are the da-bond CLI"]);
    expect(failing(cancel, { ...honest, steps: [] })).toEqual([
      "command lines are the da-bond CLI",
    ]);
    expect(
      failing(
        { action: "withdraw", step: "begin" },
        withdrawEvidence("cancel"),
      ),
    ).toEqual([
      "command lines are the da-bond CLI",
      "same-output status matches the transaction",
    ]);
  });

  it("refuses a complete whose status lovelace is not the withdrawal", () => {
    expect(
      failing(
        { action: "withdraw", step: "complete", amount: 50_000_001n },
        withdrawEvidence("complete"),
      ),
    ).toEqual([
      "command lines are the da-bond CLI",
      "same-output status matches the transaction",
    ]);
  });

  it("refuses a CLI status the adapter's chain read disagrees with", () => {
    expect(
      failing(TOP_UP, topUpEvidence(), {
        state: "withdrawing",
        lovelace: 525_000_000n,
        utxoRef: `${NEW_TX}#0`,
      }),
    ).toEqual(["CLI status agrees with the adapter's chain read"]);
    expect(
      failing(TOP_UP, topUpEvidence(), {
        state: "bonded",
        lovelace: 525_000_000n,
        utxoRef: `${OLD_TX}#0`,
      }),
    ).toEqual(["CLI status agrees with the adapter's chain read"]);
    expect(
      failing(TOP_UP, topUpEvidence(), {
        state: "bonded",
        lovelace: 525_000_001n,
      }),
    ).toEqual(["CLI status agrees with the adapter's chain read"]);
  });

  it("refuses a chain whose command lines lack a required option or run the wrong command", () => {
    const withoutOption = (
      evidenceRun: DaBondPoolProcessRun,
      option: string,
    ): DaBondPoolProcessRun => {
      const at = evidenceRun.argv.indexOf(option);
      return {
        ...evidenceRun,
        argv: [
          ...evidenceRun.argv.slice(0, at),
          ...evidenceRun.argv.slice(at + 2),
        ],
      };
    };
    const topUp = topUpEvidence();
    expect(
      failing(TOP_UP, {
        ...topUp,
        submit: withoutOption(topUp.submit, "--wallet-seed-env"),
      }),
    ).toEqual(["command lines are the da-bond CLI"]);
    expect(
      failing(TOP_UP, {
        ...topUp,
        statusAfter: {
          ...topUp.statusAfter,
          argv: [
            "node",
            "dist/index.js",
            "da-bond",
            "top-up",
            ...topUp.statusAfter.argv.slice(4),
          ],
        },
      }),
    ).toEqual(["command lines are the da-bond CLI"]);
    // A top-up is one process: build, witness or assemble runs do not belong.
    expect(
      failing(TOP_UP, {
        ...topUp,
        steps: withdrawEvidence("cancel").steps,
      }),
    ).toEqual(["command lines are the da-bond CLI"]);

    const cancel = { action: "withdraw", step: "cancel" } as const;
    const honest = withdrawEvidence("cancel");
    const [build, witnessA, witnessB] = honest.steps;
    expect(
      failing(cancel, {
        ...honest,
        steps: [withoutOption(build, "--build-unsigned"), witnessA, witnessB],
      }),
    ).toEqual(["command lines are the da-bond CLI"]);
    expect(
      failing(cancel, {
        ...honest,
        steps: [build, withoutOption(witnessA, "--key-env"), witnessB],
      }),
    ).toEqual(["command lines are the da-bond CLI"]);
  });

  it("refuses a submit output whose action or amount is not the transaction's", () => {
    const topUp = topUpEvidence();
    const topUpOutput = JSON.parse(topUp.submit.stdout) as Record<
      string,
      unknown
    >;
    expect(
      failing(TOP_UP, {
        ...topUp,
        submit: {
          ...topUp.submit,
          stdout: JSON.stringify({ ...topUpOutput, amount: "400000001" }),
        },
      }),
    ).toEqual(["same-output status matches the transaction"]);
    expect(
      failing(TOP_UP, {
        ...topUp,
        submit: {
          ...topUp.submit,
          stdout: JSON.stringify({ ...topUpOutput, action: "withdraw" }),
        },
      }),
    ).toEqual(["same-output status matches the transaction"]);
    for (const [step, wrong] of [
      ["begin", "CancelWithdraw"],
      ["cancel", "BeginWithdraw"],
      ["complete", "CancelWithdraw"],
    ] as const) {
      const honest = withdrawEvidence(step);
      const output = JSON.parse(honest.submit.stdout) as Record<
        string,
        unknown
      >;
      expect(
        failing(
          step === "complete"
            ? { action: "withdraw", step, amount: 50_000_000n }
            : { action: "withdraw", step },
          {
            ...honest,
            submit: {
              ...honest.submit,
              stdout: JSON.stringify({ ...output, action: wrong }),
            },
          },
        ),
        step,
      ).toEqual(["same-output status matches the transaction"]);
    }
  });

  it("refuses a withdraw step from the wrong pool state or that moves lovelace", () => {
    const begin = { action: "withdraw", step: "begin" } as const;
    const cancel = { action: "withdraw", step: "cancel" } as const;
    const replaceStatus = (
      evidence: DaBondCliSubmitEvidence,
      field: "statusBefore" | "same" | "statusAfter",
      value: Record<string, unknown>,
    ): DaBondCliSubmitEvidence => {
      if (field !== "same")
        return {
          ...evidence,
          [field]: { ...evidence[field], stdout: JSON.stringify(value) },
        };
      const output = JSON.parse(evidence.submit.stdout) as Record<
        string,
        unknown
      >;
      return {
        ...evidence,
        submit: {
          ...evidence.submit,
          stdout: JSON.stringify({ ...output, status: value }),
        },
      };
    };
    // A BeginWithdraw of a pool that was already Withdrawing.
    expect(
      failing(
        begin,
        replaceStatus(
          withdrawEvidence("begin"),
          "statusBefore",
          status(OLD_TX, 700_000_000n, 1_700_000_000_000n),
        ),
      ),
    ).toEqual(["same-output status matches the transaction"]);
    // A BeginWithdraw that also moves lovelace.
    const moved = status(NEW_TX, 700_000_001n, 1_800_000_000_000n);
    expect(
      failing(
        begin,
        replaceStatus(
          replaceStatus(withdrawEvidence("begin"), "same", moved),
          "statusAfter",
          moved,
        ),
      ),
    ).toEqual(["same-output status matches the transaction"]);
    // A CancelWithdraw of a pool that was Bonded.
    expect(
      failing(
        cancel,
        replaceStatus(
          withdrawEvidence("cancel"),
          "statusBefore",
          status(OLD_TX, 700_000_000n),
        ),
      ),
    ).toEqual(["same-output status matches the transaction"]);
    // A CancelWithdraw that also moves lovelace.
    const cancelled = status(NEW_TX, 699_999_999n);
    expect(
      failing(
        cancel,
        replaceStatus(
          replaceStatus(withdrawEvidence("cancel"), "same", cancelled),
          "statusAfter",
          cancelled,
        ),
      ),
    ).toEqual(["same-output status matches the transaction"]);
    // The post-transaction state of each action: a status whose outref moved
    // but whose state is not the one the action produces.
    const complete = {
      action: "withdraw",
      step: "complete",
      amount: 50_000_000n,
    } as const;
    const unlockAt = 1_800_000_000_000n;
    const sameAndAfter = (
      evidence: DaBondCliSubmitEvidence,
      value: Record<string, unknown>,
    ) =>
      replaceStatus(
        replaceStatus(evidence, "same", value),
        "statusAfter",
        value,
      );
    for (const [what, expectation, evidence] of [
      [
        "a CompleteWithdraw of a pool that was Bonded",
        complete,
        replaceStatus(
          withdrawEvidence("complete"),
          "statusBefore",
          status(OLD_TX, 700_000_000n),
        ),
      ],
      [
        "a CompleteWithdraw that leaves the pool Withdrawing",
        complete,
        sameAndAfter(
          withdrawEvidence("complete"),
          status(NEW_TX, 650_000_000n, unlockAt),
        ),
      ],
      [
        "a CancelWithdraw that leaves the pool Withdrawing",
        cancel,
        sameAndAfter(
          withdrawEvidence("cancel"),
          status(NEW_TX, 700_000_000n, unlockAt),
        ),
      ],
      [
        "a BeginWithdraw that leaves the pool Bonded",
        begin,
        sameAndAfter(withdrawEvidence("begin"), status(NEW_TX, 700_000_000n)),
      ],
      [
        "a top-up that makes the pool Withdrawing",
        TOP_UP,
        sameAndAfter(topUpEvidence(), status(NEW_TX, 525_000_000n, unlockAt)),
      ],
    ] as const)
      expect(failing(expectation, evidence), what).toEqual([
        "same-output status matches the transaction",
      ]);
  });

  it("refuses a status-after process that reads another pool than the same output", () => {
    const begin = { action: "withdraw", step: "begin" } as const;
    const honest = withdrawEvidence("begin");
    const differentAfter = (value: Record<string, unknown>) => ({
      ...honest,
      statusAfter: { ...honest.statusAfter, stdout: JSON.stringify(value) },
    });
    for (const [what, value] of [
      ["outref", status("66".repeat(32), 700_000_000n, 1_800_000_000_000n)],
      ["lovelace", status(NEW_TX, 700_000_001n, 1_800_000_000_000n)],
      ["unlockAt", status(NEW_TX, 700_000_000n, 1_800_000_000_001n)],
      ["state", status(NEW_TX, 700_000_000n)],
    ] as const)
      expect(failing(begin, differentAfter(value)), what).toEqual([
        "da-bond status after reads the same pool",
      ]);
  });

  it("parses only the status shape da-bond prints", () => {
    expect(parseDaBondCliStatus(status(NEW_TX, 9n, 7n))).toEqual({
      poolOutRef: `${NEW_TX}#0`,
      state: "withdrawing",
      lovelace: 9n,
      unlockAt: 7n,
    });
    expect(() =>
      parseDaBondCliStatus({ ...status(NEW_TX, 9n), poolOutRef: "pool#1" }),
    ).toThrow(/poolOutRef/u);
    expect(() =>
      parseDaBondCliStatus({ ...status(NEW_TX, 9n), lovelace: 9 }),
    ).toThrow(/lovelace/u);
    expect(() =>
      parseDaBondCliStatus({ ...status(NEW_TX, 9n), state: "Bonded" }),
    ).toThrow(/state/u);
    expect(() =>
      parseDaBondCliStatus({ ...status(NEW_TX, 9n), unlockAt: "7" }),
    ).toThrow(/exactly when Withdrawing/u);
  });
});

describe("committee node process view (P16)", () => {
  it("reads the pool reasons from /readyz", () => {
    const short =
      "da_bond_pool_backing_short: backing=20000000, required=500000000, checkedAt=2026-09-28T00:00:00.000Z";
    const other = "last committee node tick completed with errors";
    expect(
      parseDaBondPoolReadyz(
        503,
        JSON.stringify({
          ready: false,
          reasons: [other, short],
          scanner: { lastStartedAt: "2026-09-28T00:00:01.000Z" },
        }),
      ),
    ).toEqual({
      httpStatus: 503,
      ready: false,
      reasons: [other, short],
      poolReasons: [short],
      scannerLastStartedAt: "2026-09-28T00:00:01.000Z",
    });
    expect(
      parseDaBondPoolReadyz(200, JSON.stringify({ ready: true, reasons: [] })),
    ).toEqual({ httpStatus: 200, ready: true, reasons: [], poolReasons: [] });
  });

  it("reads the L1 source status and its quarantine reason from /readyz", () => {
    const reason = "committee replay cannot advance its durable queue";
    expect(
      parseDaBondPoolReadyz(
        503,
        JSON.stringify({
          ready: false,
          reasons: [`L1 source is quarantined: ${reason}`],
          l1Source: {
            sourceMode: "local_node",
            status: "quarantined",
            observedAt: "2026-09-29T05:52:05.000Z",
            quarantineReason: reason,
          },
        }),
      ).l1Source,
    ).toEqual({ status: "quarantined", quarantineReason: reason });
    expect(
      parseDaBondPoolReadyz(
        200,
        JSON.stringify({
          ready: true,
          reasons: [],
          l1Source: { status: "healthy" },
        }),
      ).l1Source,
    ).toEqual({ status: "healthy" });
  });

  it("refuses a /readyz answer whose status and body disagree, or another shape", () => {
    expect(() =>
      parseDaBondPoolReadyz(200, JSON.stringify({ ready: false, reasons: [] })),
    ).toThrow(/answered 200/u);
    expect(() =>
      parseDaBondPoolReadyz(500, JSON.stringify({ ready: false, reasons: [] })),
    ).toThrow(/answered 500/u);
    expect(() =>
      parseDaBondPoolReadyz(503, JSON.stringify({ ready: false })),
    ).toThrow(/not \{ ready, reasons\[\] \}/u);
    expect(() =>
      parseDaBondPoolReadyz(
        503,
        JSON.stringify({ ready: "false", reasons: [] }),
      ),
    ).toThrow(/not \{ ready, reasons\[\] \}/u);
    expect(() => parseDaBondPoolReadyz(503, "not json")).toThrow();
  });

  it("collects the pool-monitor events from stderr by byte offset, once each, whole lines only, tied to the pid", () => {
    const cursor = createDaBondPoolStderrCursor(4242);
    const names = (events: readonly { pid: number; event: string }[]) =>
      events.map(({ pid, event }) => `${pid}:${event}`);
    // A multi-byte character before the events: offsets are bytes.
    let stderr =
      'DA committee node listening on 127.0.0.1:7001 \u2713\n{"event":"da_bond_pool_backing_short","backing":"20000000"}\n{"event":"l1_submitter_funding","ok":"true"}\n{"event":"da_bond_pool_withd';
    const bytes = () => new Uint8Array(Buffer.from(stderr, "utf8"));
    expect(names(cursor.take(bytes()))).toEqual([
      "4242:da_bond_pool_backing_short",
    ]);
    expect(cursor.offset()).toBe(
      Buffer.byteLength(stderr.slice(0, stderr.lastIndexOf("\n") + 1)),
    );
    expect(cursor.take(bytes())).toEqual([]);
    stderr += 'rawing","unlockAt":"1"}\nnot json {\n';
    expect(names(cursor.take(bytes()))).toEqual([
      "4242:da_bond_pool_withdrawing",
    ]);
    stderr +=
      '{"event":"da_bond_pool_backing_restored"}\n{"event":"da_bond_pool_bonded"}\n';
    const taken = cursor.take(bytes());
    expect(names(taken)).toEqual([
      "4242:da_bond_pool_backing_restored",
      "4242:da_bond_pool_bonded",
    ]);
    expect(taken[1]!.line).toBe('{"event":"da_bond_pool_bonded"}');
    expect(() => cursor.take(new Uint8Array(0))).toThrow(/pid 4242 shrank/u);
  });

  it("collects only the events its predicate admits", () => {
    const stderr = new Uint8Array(
      Buffer.from(
        '{"event":"availability_responder","headerHash":"b1","status":"unavailable"}\n{"event":"da_bond_pool_bonded"}\n{"event":"availability_responder_extra"}\n',
        "utf8",
      ),
    );
    const responder = createDaBondPoolStderrCursor(
      7,
      (event) => event === "availability_responder",
    );
    expect(responder.take(stderr)).toEqual([
      {
        pid: 7,
        event: "availability_responder",
        line: '{"event":"availability_responder","headerHash":"b1","status":"unavailable"}',
      },
    ]);
    expect(
      createDaBondPoolStderrCursor(7)
        .take(stderr)
        .map(({ event }) => event),
    ).toEqual(["da_bond_pool_bonded"]);
  });

  it("collects only pool transitions by default, never read failures or backoffs", () => {
    const stderr = new Uint8Array(
      Buffer.from(
        '{"event":"da_bond_pool_read_failed","error":"fetch failed","failedAt":"t"}\n' +
          '{"event":"da_bond_pool_apply_backoff","headerHash":"b2","reason":"pool-under-backed"}\n' +
          '{"event":"da_bond_pool_init_backoff","headerHash":"b2","reason":"pool-unavailable"}\n' +
          '{"event":"da_bond_pool_backing_short","backing":"1"}\n' +
          '{"event":"da_bond_pool_backing_restored"}\n' +
          '{"event":"da_bond_pool_withdrawing","unlockAt":"1"}\n' +
          '{"event":"da_bond_pool_bonded"}\n',
        "utf8",
      ),
    );
    expect(
      createDaBondPoolStderrCursor(9)
        .take(stderr)
        .map(({ event }) => event),
    ).toEqual([
      "da_bond_pool_backing_short",
      "da_bond_pool_backing_restored",
      "da_bond_pool_withdrawing",
      "da_bond_pool_bonded",
    ]);
  });
});
