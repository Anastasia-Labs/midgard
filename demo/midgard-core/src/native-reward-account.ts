import { createHash } from "node:crypto";
import { constants } from "node:fs";
import { access, readFile, realpath } from "node:fs/promises";
import { dirname, isAbsolute, normalize, resolve } from "node:path";

import {
  queryRewardAccount,
  type RewardAccountSnapshot,
  sharedL1NodeTransport,
} from "@al-ft/l1-node-transport";
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
 * unregistered accounts. The node transport queries the ledger's deposit map
 * over the node socket instead; registration is membership in that map.
 */
export const NATIVE_CHAIN_SYNC_SCHEMA_VERSION =
  "midgard-watcher-native-chain-sync-v1" as const;

export type NativeLedgerNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

export const NATIVE_LEDGER_PUBLIC_NETWORK_MAGIC = Object.freeze({
  Mainnet: 764_824_073,
  Preprod: 1,
  Preview: 2,
} as const);

/**
 * One local node, identified by its socket and its Shelley genesis, read
 * through the `midgard-l1-node-transport` sidecar at `binaryPath`.
 */
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
const hexOf = (value: unknown, size: number): value is string =>
  typeof value === "string" &&
  value.length === size * 2 &&
  /^[0-9a-f]+$/u.test(value);

const record = (value: unknown, subject: string): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error(`${subject} is not a JSON object`);
  return value as Record<string, unknown>;
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
 * The network magic and genesis identity of the node configuration at
 * `nodeConfigPath`, read from that file and the Shelley genesis it declares
 * and checked against `network`. Static files only: the node's socket need
 * not exist yet.
 */
export const readNativeLedgerGenesis = async (input: {
  readonly network: NativeLedgerNetwork;
  readonly nodeConfigPath: string;
}): Promise<
  Readonly<{ genesisIdentitySha256: string; networkMagic: number }>
> => {
  await assertCanonicalAbsolutePath(input.nodeConfigPath, "Node config path");
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
    genesisIdentitySha256: createHash("sha256")
      .update(genesisBytes)
      .digest("hex"),
    networkMagic: magic,
  };
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
  await assertCanonicalAbsolutePath(input.socketPath, "Node socket path");
  const genesis = await readNativeLedgerGenesis(input);
  return {
    authorityNodeId: input.authorityNodeId,
    binaryPath: input.binaryPath,
    genesisIdentitySha256: genesis.genesisIdentitySha256,
    network: input.network,
    networkMagic: genesis.networkMagic,
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
 * Admits one ledger read: an unregistered account has no deposit, no rewards
 * and no delegation, and a registered one has a deposit.
 */
export const admitNativeRewardAccount = (
  snapshot: RewardAccountSnapshot,
): RewardAccountState => {
  if (
    snapshot.registered
      ? snapshot.depositLovelace === null
      : snapshot.depositLovelace !== null ||
        snapshot.rewardsLovelace !== 0n ||
        snapshot.poolIdHash !== null
  )
    throw new Error(
      "Native reward-account ledger state is inconsistent for the requested credential",
    );
  if (snapshot.poolIdHash !== null && !hexOf(snapshot.poolIdHash, 28))
    throw new Error("Native reward-account pool id is not a key hash");
  return {
    registered: snapshot.registered,
    rewards: snapshot.rewardsLovelace,
    poolId:
      snapshot.poolIdHash === null
        ? null
        : CML.Ed25519KeyHash.from_hex(snapshot.poolIdHash).to_bech32("pool"),
  };
};

/**
 * Query registration, rewards and pool delegation at one acquired node
 * snapshot, over the process's shared node transport.
 */
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
    "Native node transport binary path",
  );
  await access(authority.binaryPath, constants.X_OK);
  const address = CML.Address.from_bech32(rewardAddress);
  if (
    CML.RewardAddress.from_address(address) === undefined ||
    address.network_id() !== (authority.network === "Mainnet" ? 1 : 0)
  )
    throw new Error("Reward address differs from the native node network");
  const credential = stakeCredentialOf(rewardAddress);
  const transport = sharedL1NodeTransport({
    binaryPath: authority.binaryPath,
    socketPath: authority.socketPath,
    networkMagic: authority.networkMagic,
  });
  let timer: NodeJS.Timeout | undefined;
  try {
    return admitNativeRewardAccount(
      await Promise.race([
        queryRewardAccount(transport, credential),
        new Promise<never>((_, reject) => {
          timer = setTimeout(
            () =>
              reject(
                new Error(
                  `Native reward-account query had no answer within ${authority.timeoutMs} ms`,
                ),
              ),
            authority.timeoutMs,
          );
        }),
      ]),
    );
  } finally {
    clearTimeout(timer);
  }
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
        "Reward-account state requires a local node ledger: Ogmios omits registered accounts that have no stake-pool delegation. Configure the node socket, node config and node transport binary.",
      );
    return await queryNativeRewardAccount(authority, rewardAddress);
  }
}
