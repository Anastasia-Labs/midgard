import { readFileSync } from "node:fs";
import { join } from "node:path";

import {
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { cardanoCli, readL1Liveness } from "./chain.js";
import { requireSuccess } from "./exec.js";
import {
  containerPath,
  GENESIS_SKEY,
  GENESIS_VKEY,
  lowBalanceReasons,
} from "./funding.js";
import type { WalletInfo, WalletRole } from "./identities.js";
import { Journal } from "./journal.js";
import type { Layout, RunEnv } from "./layout.js";
import { acquireLock, ControllerLockBusy } from "./lock.js";
import {
  type ChainOutput,
  ensureReserveFloatRetrying,
  FLOAT_STEP_TIMEOUT_MS,
  type FloatDeps,
  type FloatMaintainer,
  type FloatOutcome,
  largestFloat,
  reserveFloatReasons,
  runReserveFloatMaintainer,
} from "./reserve-float.js";

type ManifestContract = {
  readonly scriptHash?: string;
  readonly contract?: { readonly type?: string; readonly cborHex?: string };
};

/**
 * The reserve address, derived from the deployment manifest the way the node
 * derives its reserve contract (midgard-node
 * services/midgard-contracts.assert-deployment-manifest-matches-config.ts,
 * spendingValidatorFromManifest): the script's own hash must match the one the
 * manifest records, and the address is that script's enterprise address.
 */
export const reserveAddressFromManifest = (manifestPath: string): string => {
  const manifest = JSON.parse(readFileSync(manifestPath, "utf8")) as {
    contracts?: Record<string, ManifestContract>;
  };
  const entry = manifest.contracts?.reserveSpend;
  const type = entry?.contract?.type;
  const cborHex = entry?.contract?.cborHex;
  if (
    type === undefined ||
    cborHex === undefined ||
    entry?.scriptHash === undefined
  )
    throw new Error(`${manifestPath} records no contracts.reserveSpend script`);
  const script = { type, script: cborHex } as Script;
  const hash = validatorToScriptHash(script);
  if (hash !== entry.scriptHash.toLowerCase())
    throw new Error(
      `${manifestPath} contracts.reserveSpend hashes to ${hash}, not its recorded ${entry.scriptHash}`,
    );
  return validatorToAddress("Custom", script);
};

type KupoMatch = {
  readonly transaction_id: string;
  readonly output_index: number;
  readonly value: {
    readonly coins: number | string;
    readonly assets?: Record<string, number | string>;
  };
  readonly datum_hash: string | null;
  readonly script_hash: string | null;
  readonly spent_at: unknown;
};

const kupo = async (
  run: RunEnv,
  pattern: string,
): Promise<readonly KupoMatch[]> => {
  const response = await fetch(
    `http://127.0.0.1:${run.kupoPort}/matches/${pattern}`,
    {
      signal: AbortSignal.timeout(10_000),
    },
  );
  if (!response.ok)
    throw new Error(`Kupo answered ${response.status} for ${pattern}`);
  return (await response.json()) as KupoMatch[];
};

const chainOutput = (match: KupoMatch): ChainOutput => ({
  outRef: `${match.transaction_id}#${match.output_index}`,
  lovelace: BigInt(String(match.value.coins)),
  assetUnits: Object.values(match.value.assets ?? {}).filter(
    (q) => BigInt(String(q)) !== 0n,
  ).length,
  datumHash: match.datum_hash ?? null,
  scriptHash: match.script_hash ?? null,
});

/** Fee headroom a payer input keeps above the payment. */
const FEE_HEADROOM = 5_000_000n;

/**
 * The genesis UTxO key pays: after funding, only this controller spends it
 * (every service and the journey spend their own role wallets, and the
 * operator wallet is the node's once it runs), so paying from it is safe
 * while the stack runs. Reads and confirmation go through Kupo alone, which
 * applies a block's spends and outputs together.
 */
export const productionFloatDeps = (layout: Layout, run: RunEnv): FloatDeps => {
  const work = join(layout.state, "work");
  const magic = ["--testnet-magic", String(run.networkMagic)];
  const socket = ["--socket-path", "/run/cardano/ipc/node.socket"];
  const cli = async (args: readonly string[], label: string) =>
    requireSuccess(
      await cardanoCli(layout, run, args, label),
      `cardano-cli ${label}`,
    ).stdout.trim();
  const unspentAt = async (address: string) =>
    (await kupo(run, `${address}?unspent`)).map(chainOutput);
  return {
    reserveAddress: reserveAddressFromManifest(layout.contractManifest),
    unspentAt,
    landed: async (txId) => (await kupo(run, `*@${txId}`)).length > 0,
    outputState: async (outRef) => {
      const [txId, index] = outRef.split("#");
      const matches = await kupo(run, `${index}@${txId}`);
      if (matches.length === 0) return "unknown";
      return matches.some((match) => match.spent_at === null)
        ? "unspent"
        : "spent";
    },
    buildPayment: async (address, lovelace, sequence) => {
      const payer = await cli(
        [
          "latest",
          "genesis",
          "initial-addr",
          "--verification-key-file",
          GENESIS_VKEY,
          ...magic,
        ],
        "reserve-float-payer",
      );
      const input = largestFloat(await unspentAt(payer));
      if (input === undefined || input.lovelace < lovelace + FEE_HEADROOM)
        throw new Error(
          `the genesis wallet ${payer} holds no output that can pay ${lovelace} lovelace`,
        );
      const body = join(work, `reserve-float-${sequence}.txbody`);
      const signed = join(work, `reserve-float-${sequence}.signed`);
      await cli(
        [
          "latest",
          "transaction",
          "build",
          ...socket,
          ...magic,
          "--tx-in",
          input.outRef,
          "--tx-out",
          `${address}+${lovelace}`,
          "--change-address",
          payer,
          "--out-file",
          containerPath(layout, body),
        ],
        "reserve-float-build",
      );
      await cli(
        [
          "latest",
          "transaction",
          "sign",
          "--tx-body-file",
          containerPath(layout, body),
          "--signing-key-file",
          GENESIS_SKEY,
          ...magic,
          "--out-file",
          containerPath(layout, signed),
        ],
        "reserve-float-sign",
      );
      const txId = await cli(
        [
          "latest",
          "transaction",
          "txid",
          "--tx-file",
          containerPath(layout, signed),
          "--output-text",
        ],
        "reserve-float-txid",
      );
      return { txId, signedTx: signed, input: input.outRef };
    },
    submit: async (signedTx) => {
      const submitted = await cardanoCli(
        layout,
        run,
        [
          "latest",
          "transaction",
          "submit",
          ...socket,
          ...magic,
          "--tx-file",
          containerPath(layout, signedTx),
        ],
        "reserve-float-submit",
      );
      // A resubmission of bytes already applied is refused; landing is the
      // only success criterion.
      if (submitted.code !== 0)
        console.log(`reserve-float submit: ${submitted.stderr.trim()}`);
    },
    sleep: (ms) => new Promise((resolve) => setTimeout(resolve, ms)),
    now: Date.now,
    log: (line) => console.log(line),
  };
};

const floatLock = (layout: Layout) => join(layout.state, "reserve-float.lock");
const LOCK_POLL_MS = 2_000;

/** Only explicit kernel/legacy-owner contention is retried; storage and other
 * failures keep their original classification and refuse immediately. */
const lockRetryable = (error: unknown) => error instanceof ControllerLockBusy;

/**
 * Runs `step` holding the run's float lock. A live holder (the maintainer
 * service, or up or the journey) is waited for, up to FLOAT_STEP_TIMEOUT_MS
 * on `clock`: its step ends on its own. A lock its holder frees while this
 * acquires is taken on the next poll; any other failure to write the lock is
 * refused at once.
 */
export const withFloatLock = async <T>(
  layout: Layout,
  clock: Pick<FloatDeps, "sleep" | "now">,
  step: () => Promise<T>,
): Promise<T> => {
  const lock = floatLock(layout);
  const deadline = clock.now() + FLOAT_STEP_TIMEOUT_MS;
  let release: () => void;
  for (;;) {
    try {
      release = acquireLock(lock);
      break;
    } catch (error) {
      if (!lockRetryable(error) || clock.now() > deadline) throw error;
      await clock.sleep(LOCK_POLL_MS);
    }
  }
  try {
    return await step();
  } finally {
    release();
  }
};

/**
 * The controller step: one float step at a time per run (up, the journey and
 * the maintainer all take the lock), retried through L1 outages, with what it
 * did printed. The run's journal is read again under the lock, not taken
 * from the caller's instance, so a record the maintainer wrote since the
 * caller opened it is settled, never overwritten. `deps` is the devnet's
 * chain unless a test supplies its own.
 */
export const provisionReserveFloat = async (
  layout: Layout,
  run: RunEnv,
  _journal: Journal,
  deps: FloatDeps = productionFloatDeps(layout, run),
): Promise<FloatOutcome> => {
  const outcome = await withFloatLock(layout, deps, () =>
    ensureReserveFloatRetrying(deps, new Journal(layout.journal)),
  );
  console.log(
    outcome.action === "sufficient"
      ? `reserve-float: ${deps.reserveAddress} holds ${outcome.float.lovelace} lovelace pure-ADA float ${outcome.float.outRef}; no top-up`
      : `reserve-float: paid ${outcome.lovelace} lovelace to ${deps.reserveAddress} in ${outcome.txId}`,
  );
  return outcome;
};

/** Every endurance reason of a running stack, for status and readiness. */
export type EnduranceInputs = {
  readonly layout: Layout;
  readonly run: RunEnv;
  readonly wallets: Record<WalletRole, WalletInfo>;
};

const walletBalances = async (input: EnduranceInputs) => {
  const balances: Partial<Record<WalletRole, bigint>> = {};
  for (const [role, wallet] of Object.entries(input.wallets) as [
    WalletRole,
    WalletInfo,
  ][])
    balances[role] = (await kupo(input.run, `${wallet.address}?unspent`))
      .map(chainOutput)
      .reduce((sum, output) => sum + output.lovelace, 0n);
  return balances;
};

/**
 * What will stop the run unless someone acts: a reserve float below its
 * minimum, a role wallet below its floor, an L1 that stopped producing blocks
 * or whose pool KES key is near its end. Reported, never refused on.
 */
export const enduranceReasons = async (
  input: EnduranceInputs,
): Promise<readonly string[]> => {
  const float = productionFloatDeps(input.layout, input.run);
  return [
    ...reserveFloatReasons(await float.unspentAt(float.reserveAddress)),
    ...lowBalanceReasons(await walletBalances(input)),
    ...(await readL1Liveness(input.layout, input.run)).reasons,
  ];
};

/** A reason's identity across rounds: its name, and the role it names. */
const reasonKey = (reason: string) =>
  `${reason.split(":")[0]}/${/role=(\w+)/.exec(reason)?.[1] ?? ""}`;

/**
 * Logs each endurance reason when it appears and when it clears, so a
 * standing condition is one line, not one per round.
 */
export const enduranceReporter = (log: (line: string) => void) => {
  let standing = new Map<string, string>();
  return (reasons: readonly string[]) => {
    const now = new Map(reasons.map((reason) => [reasonKey(reason), reason]));
    for (const [key, reason] of now)
      if (!standing.has(key)) log(`endurance: WARNING ${reason}`);
    for (const key of standing.keys())
      if (!now.has(key)) log(`endurance: cleared ${key}`);
    standing = now;
  };
};

/** A sleep that rejects as soon as `signal` aborts, leaving no listener behind. */
export const abortableSleep =
  (signal: AbortSignal) =>
  (ms: number): Promise<void> =>
    new Promise((resolve, reject) => {
      if (signal.aborted) return reject(new Error("stopped"));
      const stop = () => {
        clearTimeout(timer);
        reject(new Error("stopped"));
      };
      const timer = setTimeout(() => {
        signal.removeEventListener("abort", stop);
        resolve();
      }, ms);
      signal.addEventListener("abort", stop, { once: true });
    });

/** How often the maintainer looks: a payout drains the float in one block. */
export const MAINTAINER_INTERVAL_MS = 60_000;

/**
 * The long-running maintainer for a supervised service: keeps the reserve
 * float for the life of the run and reports every endurance reason as it
 * appears and clears. It pays only the reserve, from the genesis wallet, and
 * only through the journaled float step.
 */
export const runEnduranceMaintainer = async (
  input: EnduranceInputs & { readonly signal: AbortSignal },
): Promise<void> => {
  const deps = {
    ...productionFloatDeps(input.layout, input.run),
    sleep: abortableSleep(input.signal),
  };
  const maintainer: FloatMaintainer = {
    openJournal: () => new Journal(input.layout.journal),
    withLock: (step) => withFloatLock(input.layout, deps, step),
  };
  const report = enduranceReporter(deps.log);
  await runReserveFloatMaintainer(deps, maintainer, {
    intervalMs: MAINTAINER_INTERVAL_MS,
    signal: input.signal,
    alongside: async () => report(await enduranceReasons(input)),
  });
};
