import { execFile } from "node:child_process";
import { constants } from "node:fs";
import { access, realpath } from "node:fs/promises";
import { isAbsolute, normalize } from "node:path";

import {
  CML,
  Kupmios,
  type RewardAccountState,
  stakeCredentialOf,
} from "@lucid-evolution/lucid";

import { parseWatcherStrictJsonValue } from "../runtime/config.js";
import {
  watcherCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";
import {
  deriveWatcherNativeGenesisIdentity,
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeNodeConfig,
} from "./native-chain-sync.js";

export type WatcherNativeRewardAccountQuery = Readonly<{
  watcherConfig: WatcherNativeNodeConfig;
  binaryPath: string;
  timeoutMs: number;
}>;

const exact = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.keys(value).sort().join(",") !== [...keys].sort().join(",")
  ) {
    throw new Error("Native reward-account result has an invalid shape");
  }
  return value as Record<string, unknown>;
};
const natural = (value: unknown): value is string =>
  typeof value === "string" && /^(?:0|[1-9][0-9]*)$/u.test(value);
const hash = (value: unknown, size: number): value is string =>
  typeof value === "string" &&
  value.length === size * 2 &&
  /^[0-9a-f]+$/u.test(value);

export const parseWatcherNativeRewardAccountResult = (
  value: unknown,
  expected: {
    credential: { type: "Key" | "Script"; hash: string };
    startupDigest: string;
  },
): RewardAccountState => {
  const result = exact(value, [
    "credential",
    "depositLovelace",
    "kind",
    "point",
    "poolIdHash",
    "registered",
    "rewardsLovelace",
    "schemaVersion",
    "startupDigest",
  ]);
  const credential = exact(result.credential, ["hash", "type"]);
  const point = exact(result.point, ["blockHash", "blockNo", "slot"]);
  if (
    result.schemaVersion !== WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION ||
    result.kind !== "reward_account" ||
    result.startupDigest !== expected.startupDigest ||
    credential.hash !== expected.credential.hash ||
    credential.type !== expected.credential.type ||
    !hash(point.blockHash, 32) ||
    !natural(point.blockNo) ||
    !natural(point.slot) ||
    typeof result.registered !== "boolean" ||
    !natural(result.rewardsLovelace) ||
    (result.poolIdHash !== null && !hash(result.poolIdHash, 28)) ||
    (result.registered
      ? !natural(result.depositLovelace)
      : result.depositLovelace !== null) ||
    (!result.registered &&
      (result.rewardsLovelace !== "0" || result.poolIdHash !== null))
  ) {
    throw new Error(
      "Native reward-account result differs from the requested credential or ledger state",
    );
  }
  return {
    registered: result.registered,
    rewards: BigInt(result.rewardsLovelace),
    poolId:
      result.poolIdHash === null
        ? null
        : CML.Ed25519KeyHash.from_hex(result.poolIdHash as string).to_bech32(
            "pool",
          ),
  };
};

/** Query registration, rewards and pool delegation at one acquired node snapshot. */
export const queryWatcherNativeRewardAccount = async (
  input: WatcherNativeRewardAccountQuery,
  rewardAddress: string,
): Promise<RewardAccountState> => {
  const source = input.watcherConfig.l1.source;
  if (source.sourceMode !== "local_node")
    throw new Error(
      "Native reward-account query requires local-node authority",
    );
  if (
    !Number.isSafeInteger(input.timeoutMs) ||
    input.timeoutMs < 100 ||
    input.timeoutMs > 120_000
  )
    throw new Error("Native reward-account query timeout is invalid");
  if (
    !isAbsolute(input.binaryPath) ||
    normalize(input.binaryPath) !== input.binaryPath ||
    (await realpath(input.binaryPath)) !== input.binaryPath
  )
    throw new Error(
      "Native reward-account binary path must be absolute without symlinks",
    );
  await access(input.binaryPath, constants.X_OK);
  const address = CML.Address.from_bech32(rewardAddress);
  if (
    CML.RewardAddress.from_address(address) === undefined ||
    address.network_id() !==
      (input.watcherConfig.targetNetwork === "Mainnet" ? 1 : 0)
  )
    throw new Error("Reward address differs from the native node network");
  const credential = stakeCredentialOf(rewardAddress);
  const identity = await deriveWatcherNativeGenesisIdentity(input);
  const startup = {
    authorityNodeId: source.authorityNodeId,
    ...identity,
    intersection: { kind: "origin" },
    network: input.watcherConfig.targetNetwork,
    operation: {
      kind: "reward_account",
      credential,
      timeoutMs: input.timeoutMs,
    },
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    socketPath: source.chainSync.socketPath,
  };
  const output = await new Promise<string>((resolve, reject) => {
    const child = execFile(
      input.binaryPath,
      [],
      {
        timeout: input.timeoutMs + 1000,
        killSignal: "SIGKILL",
        maxBuffer: 64 * 1024,
        encoding: "utf8",
        env: { PATH: process.env.PATH ?? "/usr/bin:/bin" },
      },
      (error, stdout) =>
        error === null
          ? resolve(stdout)
          : reject(
              new Error(
                `native reward-account helper failed: ${error.message}`,
                { cause: error },
              ),
            ),
    );
    child.stdin!.on("error", reject);
    child.stdin!.end(`${watcherCanonicalJson(startup)}\n`, "utf8");
  });
  return parseWatcherNativeRewardAccountResult(
    parseWatcherStrictJsonValue(output),
    {
      credential,
      startupDigest: watcherSha256CanonicalJson(startup),
    },
  );
};

/** Kupo/Ogmios transport with reward-account state obtained from the local ledger. */
export class WatcherLocalKupmios extends Kupmios {
  readonly #native: WatcherNativeRewardAccountQuery;

  constructor(
    kupoUrl: string,
    ogmiosUrl: string,
    native: WatcherNativeRewardAccountQuery,
  ) {
    super(kupoUrl, ogmiosUrl);
    this.#native = native;
  }

  override async getRewardAccount(
    rewardAddress: string,
  ): Promise<RewardAccountState> {
    return await queryWatcherNativeRewardAccount(this.#native, rewardAddress);
  }
}
