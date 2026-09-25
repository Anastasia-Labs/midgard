import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { constants } from "node:fs";
import { access, readFile, realpath } from "node:fs/promises";
import { dirname, isAbsolute, normalize, resolve } from "node:path";

import {
  CML,
  Kupmios,
  type KupmiosOptions,
  type RewardAccountState,
  stakeCredentialOf,
} from "@lucid-evolution/lucid";

/**
 * Reward-account registration read from the local node's ledger.
 *
 * Ogmios `queryLedgerState/rewardAccountSummaries` (through v7.0.0) joins the
 * node's delegation and reward maps and drops every account without a stake
 * pool delegation. Midgard's observer and role scripts are registered and
 * never delegated, so through Ogmios they are indistinguishable from
 * unregistered accounts. The native chain-sync helper queries the ledger's
 * deposit map over the node socket instead; registration is membership in
 * that map.
 */
export const NATIVE_CHAIN_SYNC_SCHEMA_VERSION =
  "midgard-watcher-native-chain-sync-v1" as const;

export type NativeLedgerNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

export const NATIVE_LEDGER_PUBLIC_NETWORK_MAGIC = Object.freeze({
  Mainnet: 764_824_073,
  Preprod: 1,
  Preview: 2,
} as const);

/** One local node, identified by its socket and its Shelley genesis. */
export type NativeLedgerAuthority = Readonly<{
  authorityNodeId: string;
  binaryPath: string;
  genesisIdentitySha256: string;
  network: NativeLedgerNetwork;
  networkMagic: number;
  socketPath: string;
  timeoutMs: number;
}>;

export type NativeRewardCredential = Readonly<{
  type: "Key" | "Script";
  hash: string;
}>;

const MAX_IDENTITY_FILE_BYTES = 4 * 1024 * 1024;
const AUTHORITY_ID = /^[a-z0-9](?:[a-z0-9._-]{0,62}[a-z0-9])?$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const hexOf = (value: unknown, size: number): value is string =>
  typeof value === "string" &&
  value.length === size * 2 &&
  /^[0-9a-f]+$/u.test(value);

const record = (value: unknown, subject: string): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error(`${subject} is not a JSON object`);
  return value as Record<string, unknown>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> => {
  const result = record(value, "Native reward-account result");
  if (Object.keys(result).sort().join(",") !== [...keys].sort().join(","))
    throw new Error("Native reward-account result has an invalid shape");
  return result;
};

const readIdentityFile = async (path: string, subject: string) => {
  const bytes = await readFile(path);
  if (bytes.byteLength === 0 || bytes.byteLength > MAX_IDENTITY_FILE_BYTES)
    throw new Error(`${subject} size is invalid`);
  return bytes;
};

const assertCanonicalAbsolutePath = async (path: string, subject: string) => {
  if (
    !isAbsolute(path) ||
    normalize(path) !== path ||
    (await realpath(path)) !== path
  )
    throw new Error(`${subject} must be an absolute path without symlinks`);
};

/**
 * Derives the authority's network magic and genesis identity from the node
 * configuration the local node runs with, so the query is bound to the
 * genesis the operator actually deployed rather than to a declared label.
 */
export const resolveNativeLedgerAuthority = async (input: {
  readonly authorityNodeId: string;
  readonly binaryPath: string;
  readonly network: NativeLedgerNetwork;
  readonly nodeConfigPath: string;
  readonly socketPath: string;
  readonly timeoutMs: number;
}): Promise<NativeLedgerAuthority> => {
  if (!AUTHORITY_ID.test(input.authorityNodeId))
    throw new Error("Native ledger authority id is invalid");
  if (
    !Number.isSafeInteger(input.timeoutMs) ||
    input.timeoutMs < 100 ||
    input.timeoutMs > 120_000
  )
    throw new Error("Native reward-account query timeout is invalid");
  await assertCanonicalAbsolutePath(input.nodeConfigPath, "Node config path");
  await assertCanonicalAbsolutePath(input.socketPath, "Node socket path");
  const decoder = new TextDecoder("utf-8", { fatal: true });
  const nodeConfig = record(
    JSON.parse(
      decoder.decode(
        await readIdentityFile(input.nodeConfigPath, "Node config"),
      ),
    ),
    "Node config",
  );
  const declaredGenesis = nodeConfig.ShelleyGenesisFile;
  if (typeof declaredGenesis !== "string" || declaredGenesis.length === 0)
    throw new Error("Node config does not declare ShelleyGenesisFile");
  const genesisPath = normalize(
    isAbsolute(declaredGenesis)
      ? declaredGenesis
      : resolve(dirname(input.nodeConfigPath), declaredGenesis),
  );
  const genesisBytes = await readIdentityFile(genesisPath, "Shelley genesis");
  const genesis = record(
    JSON.parse(decoder.decode(genesisBytes)),
    "Shelley genesis",
  );
  const magic = genesis.networkMagic;
  if (typeof magic !== "number" || !Number.isSafeInteger(magic) || magic < 0)
    throw new Error("Shelley genesis network magic is invalid");
  const publicMagics: readonly number[] = Object.values(
    NATIVE_LEDGER_PUBLIC_NETWORK_MAGIC,
  );
  if (
    input.network === "Custom"
      ? publicMagics.includes(magic)
      : NATIVE_LEDGER_PUBLIC_NETWORK_MAGIC[input.network] !== magic
  )
    throw new Error(
      `Shelley genesis network magic ${magic} differs from network ${input.network}`,
    );
  return {
    authorityNodeId: input.authorityNodeId,
    binaryPath: input.binaryPath,
    genesisIdentitySha256: createHash("sha256")
      .update(genesisBytes)
      .digest("hex"),
    network: input.network,
    networkMagic: magic,
    socketPath: input.socketPath,
    timeoutMs: input.timeoutMs,
  };
};

export type NativeLedgerAuthorityInput = Parameters<
  typeof resolveNativeLedgerAuthority
>[0];

/**
 * Resolves the authority once, on first use, so processes that never read
 * reward-account state do not require the node files. A failed resolution is
 * not cached and is retried by the next read.
 */
export const nativeLedgerAuthoritySource = (
  input: NativeLedgerAuthorityInput | undefined,
): (() => Promise<NativeLedgerAuthority | undefined>) => {
  let pending: Promise<NativeLedgerAuthority> | undefined;
  return async () => {
    if (input === undefined) return undefined;
    pending ??= resolveNativeLedgerAuthority(input).catch((cause: unknown) => {
      pending = undefined;
      throw cause;
    });
    return await pending;
  };
};

/**
 * The helper admits only canonical startup JSON: keys in lexicographic order
 * at every level, which is the insertion order used here.
 */
const startupLine = (
  authority: NativeLedgerAuthority,
  credential: NativeRewardCredential,
): string =>
  JSON.stringify({
    authorityNodeId: authority.authorityNodeId,
    genesisIdentitySha256: authority.genesisIdentitySha256,
    intersection: { kind: "origin" },
    network: authority.network,
    networkMagic: authority.networkMagic,
    operation: {
      credential: { hash: credential.hash, type: credential.type },
      kind: "reward_account",
      timeoutMs: authority.timeoutMs,
    },
    schemaVersion: NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    socketPath: authority.socketPath,
  });

/** Admits one helper answer only for the credential and startup it was asked. */
export const parseNativeRewardAccountResult = (
  value: unknown,
  expected: {
    readonly credential: NativeRewardCredential;
    readonly startupDigest: string;
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
  const natural = (field: unknown): field is string =>
    typeof field === "string" && NATURAL.test(field);
  if (
    result.schemaVersion !== NATIVE_CHAIN_SYNC_SCHEMA_VERSION ||
    result.kind !== "reward_account" ||
    result.startupDigest !== expected.startupDigest ||
    credential.hash !== expected.credential.hash ||
    credential.type !== expected.credential.type ||
    !hexOf(point.blockHash, 32) ||
    !natural(point.blockNo) ||
    !natural(point.slot) ||
    typeof result.registered !== "boolean" ||
    !natural(result.rewardsLovelace) ||
    (result.poolIdHash !== null && !hexOf(result.poolIdHash, 28)) ||
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
export const queryNativeRewardAccount = async (
  authority: NativeLedgerAuthority,
  rewardAddress: string,
): Promise<RewardAccountState> => {
  if (
    !Number.isSafeInteger(authority.timeoutMs) ||
    authority.timeoutMs < 100 ||
    authority.timeoutMs > 120_000
  )
    throw new Error("Native reward-account query timeout is invalid");
  await assertCanonicalAbsolutePath(
    authority.binaryPath,
    "Native reward-account binary path",
  );
  await access(authority.binaryPath, constants.X_OK);
  const address = CML.Address.from_bech32(rewardAddress);
  if (
    CML.RewardAddress.from_address(address) === undefined ||
    address.network_id() !== (authority.network === "Mainnet" ? 1 : 0)
  )
    throw new Error("Reward address differs from the native node network");
  const credential = stakeCredentialOf(rewardAddress);
  const startup = startupLine(authority, credential);
  const output = await new Promise<string>((resolveOutput, reject) => {
    const child = execFile(
      authority.binaryPath,
      [],
      {
        timeout: authority.timeoutMs + 1000,
        killSignal: "SIGKILL",
        maxBuffer: 64 * 1024,
        encoding: "utf8",
        env: { PATH: process.env.PATH ?? "/usr/bin:/bin" },
      },
      (error, stdout, stderr) =>
        error === null
          ? resolveOutput(stdout)
          : reject(
              new Error(
                `native reward-account helper failed: ${error.message}${
                  stderr.length === 0 ? "" : `: ${stderr.trim().slice(0, 512)}`
                }`,
              ),
            ),
    );
    child.stdin!.on("error", reject);
    child.stdin!.end(`${startup}\n`, "utf8");
  });
  const lines = output.split("\n").filter((line) => line.length > 0);
  if (lines.length !== 1)
    throw new Error("Native reward-account helper emitted an invalid answer");
  return parseNativeRewardAccountResult(JSON.parse(lines[0]!), {
    credential,
    startupDigest: createHash("sha256").update(startup, "utf8").digest("hex"),
  });
};

/**
 * Kupo/Ogmios transport whose reward-account state comes from the local
 * ledger. Without a configured authority it refuses reward-account reads:
 * Ogmios would report every undelegated registered account as absent.
 */
export class NativeLedgerKupmios extends Kupmios {
  readonly #authority: () => Promise<NativeLedgerAuthority | undefined>;

  constructor(
    kupoUrl: string,
    ogmiosUrl: string,
    authority: () => Promise<NativeLedgerAuthority | undefined>,
    options?: KupmiosOptions,
  ) {
    super(kupoUrl, ogmiosUrl, options);
    this.#authority = authority;
  }

  override async getRewardAccount(
    rewardAddress: string,
  ): Promise<RewardAccountState> {
    const authority = await this.#authority();
    if (authority === undefined)
      throw new Error(
        "Reward-account state requires a local node ledger: Ogmios omits registered accounts that have no stake-pool delegation. Configure the node socket, node config and native chain-sync helper.",
      );
    return await queryNativeRewardAccount(authority, rewardAddress);
  }
}
