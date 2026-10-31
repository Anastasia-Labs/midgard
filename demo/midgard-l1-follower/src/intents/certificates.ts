/**
 * Stake-credential registration reads over transaction bodies (§8.4 read
 * helpers): which credentials a body's certificates (key 4) register or
 * deregister, and the latest such change among the canonical landed
 * transactions the store holds.
 *
 * Conway certificate tags: 0 `stake_registration`, 7 `reg_cert`,
 * 11 `stake_reg_deleg_cert`, 12 `vote_reg_deleg_cert` and
 * 13 `stake_vote_reg_deleg_cert` register their credential; 1
 * `stake_deregistration` and 8 `unreg_cert` deregister it. Every other
 * certificate leaves registration unchanged. A credential is
 * `[0, keyhash]` or `[1, scripthash]`, named here `<0|1>:<hash hex>`.
 */
import {
  CborReadError,
  readArray,
  readBytes,
  readMap,
  readSmallUint,
  untag,
} from "../cbor/reader.js";
import { decodeTransaction } from "../decode/tx.js";
import { asBuffer, type Dialect, type SqlTx } from "../sql/backend.js";

const SET_TAG = 258n;
const REGISTERS = new Set([0, 7, 11, 12, 13]);
const DEREGISTERS = new Set([1, 8]);

/** One certificate's effect on a stake credential's registration. */
export type StakeRegistrationChange = Readonly<{
  /** `<0|1>:<hash hex>`: a key hash (0) or a script hash (1). */
  credential: string;
  registered: boolean;
}>;

const credentialAt = (bytes: Uint8Array, offset: number): string => {
  const [kind, hash] = readArray(bytes, offset).items;
  if (kind === undefined || hash === undefined)
    throw new CborReadError("a credential is [kind, hash]", offset);
  const tag = readSmallUint(bytes, kind);
  if (tag !== 0 && tag !== 1)
    throw new CborReadError(`unknown credential kind ${tag.toString()}`, kind);
  return `${tag.toString()}:${readBytes(bytes, hash).toString("hex")}`;
};

/**
 * The registration changes a transaction body's certificates make, in body
 * order. A body without certificates makes none. Takes the body map's exact
 * bytes (`DecodedTransaction.bodyCbor`, `l1_txs.body_cbor`).
 */
export const stakeRegistrationChanges = (
  body: Uint8Array,
): StakeRegistrationChange[] => {
  const field = readMap(body, 0).entries.find(
    (entry) => readSmallUint(body, entry.key) === 4,
  );
  if (field === undefined) return [];
  return readArray(body, untag(body, field.value, SET_TAG)).items.flatMap(
    (item): StakeRegistrationChange[] => {
      const [tagAt, credentialOffset] = readArray(body, item).items;
      if (tagAt === undefined)
        throw new CborReadError("a certificate is [tag, ...]", item);
      const tag = readSmallUint(body, tagAt);
      const registered = REGISTERS.has(tag);
      if (!registered && !DEREGISTERS.has(tag)) return [];
      if (credentialOffset === undefined)
        throw new CborReadError("a stake certificate names a credential", item);
      return [{ credential: credentialAt(body, credentialOffset), registered }];
    },
  );
};

/** The credentials a whole transaction's certificates register. */
export const credentialsRegisteredBy = (txCbor: Uint8Array): string[] =>
  stakeRegistrationChanges(decodeTransaction(txCbor).bodyCbor)
    .filter((change) => change.registered)
    .map((change) => change.credential);

/**
 * Each of `credentials`' registration as the latest certificate on it among
 * the canonical phase-2-valid landed transactions the store holds says;
 * absent when no such transaction carries one (the credential's state
 * predates the facts, or no tracked transaction changed it). Reads only the
 * bodies that carry one of the credentials' hashes.
 */
export const landedStakeRegistrationsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  credentials: readonly string[],
): Promise<Map<string, boolean>> => {
  const wanted = new Set(credentials);
  const hashes = [
    ...new Set(credentials.map((credential) => credential.slice(2))),
  ].map((hex) => Buffer.from(hex, "hex"));
  const found = new Map<string, boolean>();
  if (hashes.length === 0) return found;
  const contains =
    dialect.name === "postgres"
      ? "position(? IN t.body_cbor) > 0"
      : "instr(t.body_cbor, ?) > 0";
  const rows = await tx.query(
    `SELECT t.body_cbor FROM l1_txs t
      WHERE ${dialect.name === "postgres" ? "t.is_valid" : "t.is_valid = 1"}
        AND (${hashes.map(() => contains).join(" OR ")})
      ORDER BY t.block_slot, t.block_tx_index`,
    hashes,
  );
  for (const row of rows)
    for (const change of stakeRegistrationChanges(asBuffer(row.body_cbor)))
      if (wanted.has(change.credential))
        found.set(change.credential, change.registered);
  return found;
};
