import {
  checkDaBondCliSubmitEvidence,
  type DaBondCliExpectation,
  type DaBondCliSubmitEvidence,
  type DaBondPoolProcessRun,
} from "./da-bond-pool-process-evidence.js";

export const OLD_TX = "11".repeat(32);

export const NEW_TX = "22".repeat(32);

export const MANIFEST = ["--manifest", "/run/deployment-manifest.json"];

export const run = (
  args: readonly string[],
  stdout: unknown,
  exitCode: number | null = 0,
): DaBondPoolProcessRun => ({
  argv: ["node", "dist/index.js", "da-bond", ...args],
  exitCode,
  stdout: JSON.stringify(stdout, null, 2),
});

export const status = (
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

export const topUpEvidence = (
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

export const withdrawEvidence = (
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

export const failing = (
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

export const TOP_UP = { action: "top-up", amount: 400_000_000n } as const;
