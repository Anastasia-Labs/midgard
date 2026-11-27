import { CborMap, type CborValue, decodeCbor } from "./cbor.js";
import {
  type BlockPoint,
  bytesOf,
  bytesToHex,
  decodePoint,
  natural,
  type StakeCredential,
  TransportProtocolError,
} from "./protocol.js";
import type { L1NodeTransport } from "./transport.js";

/** One reward account read from a single acquired ledger state. */
export type RewardAccountSnapshot = Readonly<{
  /** Membership in the ledger's deposit map. */
  registered: boolean;
  /** The deposit, when registered. */
  depositLovelace: bigint | null;
  rewardsLovelace: bigint;
  /** The pool key hash the account delegates to, if any. */
  poolIdHash: string | null;
  point: BlockPoint;
  blockNo: bigint;
}>;

const credentialKey = (value: CborValue, subject: string): string => {
  if (!Array.isArray(value) || value.length !== 2)
    throw new TransportProtocolError(`${subject} is not a stake credential`);
  const tag = natural(value[0], `${subject} tag`);
  if (tag > 1n)
    throw new TransportProtocolError(`${subject} has an unknown tag`);
  return `${tag}:${bytesToHex(bytesOf(value[1], 28, `${subject} hash`))}`;
};

const mapOf = (value: CborValue, subject: string): CborMap => {
  if (!(value instanceof CborMap))
    throw new TransportProtocolError(`${subject} is not a map`);
  return value;
};

const lookup = (
  map: CborMap,
  key: string,
  subject: string,
): CborValue | undefined => {
  let found: CborValue | undefined;
  for (const [entryKey, value] of map.entries)
    if (credentialKey(entryKey, `${subject} key`) === key) {
      if (found !== undefined)
        throw new TransportProtocolError(`${subject} repeats a credential`);
      found = value;
    }
  return found;
};

/**
 * Registration, rewards and delegation of one stake credential, all read from
 * one acquired ledger state at the node's tip. Registration is membership in
 * the ledger's deposit map: the reward-account summary omits registered
 * accounts that never delegated.
 */
export const queryRewardAccount = async (
  transport: L1NodeTransport,
  credential: StakeCredential,
): Promise<RewardAccountSnapshot> =>
  await transport.withLedgerState("tip", async (state) => {
    const point = decodePoint(
      decodeCbor(await state.query({ query: "chain_point" })),
      "ledger point",
    );
    const blockNoAnswer = decodeCbor(
      await state.query({ query: "chain_block_no" }),
    );
    if (point.kind === "origin")
      throw new TransportProtocolError(
        "a reward-account read needs a ledger past the origin",
      );
    if (
      !Array.isArray(blockNoAnswer) ||
      blockNoAnswer.length !== 2 ||
      natural(blockNoAnswer[0], "block number tag") !== 1n
    )
      throw new TransportProtocolError("ledger block number is not At n");
    const blockNo = natural(blockNoAnswer[1], "ledger block number");
    const credentials = [credential];
    const deposits = mapOf(
      decodeCbor(
        await state.query({ query: "stake_deleg_deposits", credentials }),
      ),
      "deposit map",
    );
    const filtered = decodeCbor(
      await state.query({
        query: "filtered_delegations_and_rewards",
        credentials,
      }),
    );
    if (!Array.isArray(filtered) || filtered.length !== 2)
      throw new TransportProtocolError(
        "delegations and rewards answer is not a pair",
      );
    const key = `${credential.type === "Key" ? 0 : 1}:${credential.hash}`;
    const deposit = lookup(deposits, key, "deposit map");
    const pool = lookup(mapOf(filtered[0], "delegations"), key, "delegations");
    const rewards = lookup(mapOf(filtered[1], "rewards"), key, "rewards");
    return Object.freeze({
      registered: deposit !== undefined,
      depositLovelace:
        deposit === undefined ? null : natural(deposit, "deposit"),
      rewardsLovelace: rewards === undefined ? 0n : natural(rewards, "rewards"),
      poolIdHash:
        pool === undefined ? null : bytesToHex(bytesOf(pool, 28, "pool id")),
      point,
      blockNo,
    });
  });
