import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

import { cardanoCli } from "./chain.js";
import { writeDurableJson } from "./durable.js";
import { requireSuccess } from "./exec.js";
import {
  ROLE_COLLATERAL_LOVELACE,
  roleBudgetLovelace,
} from "./funding-budget.js";
import {
  USER_ROLES,
  WALLET_ROLES,
  type WalletInfo,
  type WalletRole,
} from "./identities.js";
import type { Journal } from "./journal.js";
import type { Layout, RunEnv } from "./layout.js";

/**
 * A role whose wallet holds less than this share of its budget is reported:
 * nothing refills a wallet, and every L1 fee it pays is gone for good.
 */
const LOW_BALANCE_DIVISOR = 5n;

/** The balance below which a role's wallet is reported low. */
export const lowBalanceFloorLovelace = (role: WalletRole): bigint =>
  roleBudgetLovelace(role) / LOW_BALANCE_DIVISOR;

/** One reason per role whose wallet is below its floor. */
export const lowBalanceReasons = (
  balances: Partial<Record<WalletRole, bigint>>,
): readonly string[] =>
  WALLET_ROLES.flatMap((role) => {
    const lovelace = balances[role];
    const floor = lowBalanceFloorLovelace(role);
    return lovelace === undefined || lovelace >= floor
      ? []
      : [
          `wallet_balance_low: role=${role}, lovelace=${lovelace}, floorLovelace=${floor}`,
        ];
  });

/** Users get several outputs so concurrent submissions do not share one. */
const USER_OUTPUTS = 5n;
const USER_TOKEN_AMOUNT = 1_000_000n;

/** Exact bootstrap outputs; keeping this pure lets the role coverage be
 * checked without genesis keys, a running chain or a submission. */
export const initialFundingOutputs = (
  wallets: Record<WalletRole, Pick<WalletInfo, "address">>,
  perUser: string,
): string[] => {
  const outputs: string[] = [];
  for (const role of WALLET_ROLES) {
    const address = wallets[role].address;
    const budget = roleBudgetLovelace(role);
    if ((USER_ROLES as readonly string[]).includes(role)) {
      const share = budget / USER_OUTPUTS;
      outputs.push("--tx-out", `${address}+${share}+${perUser}`);
      for (let index = 1n; index < USER_OUTPUTS; index += 1n)
        outputs.push("--tx-out", `${address}+${share}`);
    } else {
      outputs.push("--tx-out", `${address}+${budget}`);
    }
    outputs.push("--tx-out", `${address}+${ROLE_COLLATERAL_LOVELACE}`);
  }
  return outputs;
};

export type TestAsset = {
  readonly policyId: string;
  readonly assetNameHex: string;
  readonly label: string;
};

export type FundingRecord = {
  readonly txId: string;
  readonly signedTx: string;
  readonly assets: readonly TestAsset[];
};

/** A path under the run directory as cardano-cli's container sees it. */
export const containerPath = (layout: Layout, hostPath: string) =>
  `/run/${hostPath.slice(layout.runDir.length + 1)}`;

/** The generated chain's genesis UTxO key: only this controller spends it. */
export const GENESIS_VKEY = "/run/genesis/utxo-keys/utxo1/utxo.vkey";
export const GENESIS_SKEY = "/run/genesis/utxo-keys/utxo1/utxo.skey";

const hexName = (name: string) => Buffer.from(name, "utf8").toString("hex");

/**
 * Funds every wallet role and mints the test assets in one genesis
 * transaction. The signed transaction is journaled before submission; a rerun
 * resubmits those exact bytes (idempotent on the ledger) and waits for them.
 */
export const ensureFunded = async (
  layout: Layout,
  run: RunEnv,
  journal: Journal,
  wallets: Record<WalletRole, WalletInfo>,
): Promise<FundingRecord> => {
  const work = join(layout.state, "work");
  mkdirSync(work, { recursive: true, mode: 0o700 });
  const magic = ["--testnet-magic", String(run.networkMagic)];
  const socket = ["--socket-path", "/run/cardano/ipc/node.socket"];
  const cli = async (args: readonly string[], label: string) =>
    requireSuccess(
      await cardanoCli(layout, run, args, label),
      `cardano-cli ${label}`,
    ).stdout.trim();

  let record = journal.get<FundingRecord>("funding");
  if (record === undefined) {
    const vkey = GENESIS_VKEY;
    const genesisAddress = await cli(
      [
        "latest",
        "genesis",
        "initial-addr",
        "--verification-key-file",
        vkey,
        ...magic,
      ],
      "genesis-address",
    );
    const keyHash = await cli(
      [
        "latest",
        "address",
        "key-hash",
        "--payment-verification-key-file",
        vkey,
      ],
      "genesis-key-hash",
    );
    const policies = {
      a: { type: "sig", keyHash },
      b: { type: "any", scripts: [{ type: "sig", keyHash }] },
    };
    const policyIds: Record<string, string> = {};
    for (const [name, script] of Object.entries(policies)) {
      const file = join(work, `policy-${name}.json`);
      writeDurableJson(file, script, 0o644);
      policyIds[name] = await cli(
        ["hash", "script", "--script-file", containerPath(layout, file)],
        `policy-${name}-id`,
      );
    }
    const assets: TestAsset[] = [
      {
        policyId: policyIds.a!,
        assetNameHex: hexName("tALPHA"),
        label: "tALPHA",
      },
      {
        policyId: policyIds.a!,
        assetNameHex: hexName("tBETA"),
        label: "tBETA",
      },
      {
        policyId: policyIds.b!,
        assetNameHex: hexName("tGAMMA"),
        label: "tGAMMA",
      },
    ];
    const unit = (asset: TestAsset) =>
      `${asset.policyId}.${asset.assetNameHex}`;
    const perUser = assets
      .map((asset) => `${USER_TOKEN_AMOUNT} ${unit(asset)}`)
      .join("+");
    const minted = assets
      .map(
        (asset) =>
          `${USER_TOKEN_AMOUNT * BigInt(USER_ROLES.length)} ${unit(asset)}`,
      )
      .join("+");

    const utxoFile = join(work, "genesis-utxos.json");
    await cli(
      [
        "latest",
        "query",
        "utxo",
        ...socket,
        ...magic,
        "--address",
        genesisAddress,
        "--out-file",
        containerPath(layout, utxoFile),
      ],
      "genesis-utxos",
    );
    const utxos = JSON.parse(readFileSync(utxoFile, "utf8")) as Record<
      string,
      { value: { lovelace: number } }
    >;
    const input = Object.entries(utxos).sort(
      (left, right) => right[1].value.lovelace - left[1].value.lovelace,
    )[0]?.[0];
    if (input === undefined)
      throw new Error("the genesis UTxO is not available");

    const outputs = initialFundingOutputs(wallets, perUser);
    const body = join(work, "funding.txbody");
    const signed = join(work, "funding.signed");
    await cli(
      [
        "latest",
        "transaction",
        "build",
        ...socket,
        ...magic,
        "--tx-in",
        input,
        ...outputs,
        "--mint",
        minted,
        "--mint-script-file",
        containerPath(layout, join(work, "policy-a.json")),
        "--mint-script-file",
        containerPath(layout, join(work, "policy-b.json")),
        "--change-address",
        genesisAddress,
        "--out-file",
        containerPath(layout, body),
      ],
      "funding-build",
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
      "funding-sign",
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
      "funding-txid",
    );
    record = { txId, signedTx: signed, assets };
    journal.set("funding", record);
  }

  if (!(await fundingConfirmed(layout, run, record.txId))) {
    if (!existsSync(record.signedTx))
      throw new Error(
        `journaled funding transaction ${record.signedTx} is missing`,
      );
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
        containerPath(layout, record.signedTx),
      ],
      "funding-submit",
    );
    // A resubmission after a lost response is refused as already spent;
    // confirmation below is the only success criterion.
    if (submitted.code !== 0)
      console.log(`funding submit: ${submitted.stderr.trim()}`);
    const deadline = Date.now() + 300_000;
    while (!(await fundingConfirmed(layout, run, record.txId))) {
      if (Date.now() > deadline)
        throw new Error(`funding transaction ${record.txId} did not confirm`);
      await new Promise((resolve) => setTimeout(resolve, 2_000));
    }
  }
  return record;
};

/** Confirmed once Kupo indexes the transaction's first output. */
const fundingConfirmed = async (
  _layout: Layout,
  run: RunEnv,
  txId: string,
): Promise<boolean> => {
  const response = await fetch(
    `http://127.0.0.1:${run.kupoPort}/matches/0@${txId}`,
    { signal: AbortSignal.timeout(5_000) },
  );
  if (!response.ok) return false;
  return ((await response.json()) as unknown[]).length > 0;
};
