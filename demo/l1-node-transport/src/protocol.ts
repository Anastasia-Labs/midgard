import type { CborInput, CborValue } from "./cbor.js";

/** A chain point. Hashes are lowercase hex. */
export type ChainPoint =
  | Readonly<{ kind: "origin" }>
  | Readonly<{ kind: "point"; slot: bigint; hash: string }>;

export type BlockPoint = Extract<ChainPoint, { kind: "point" }>;

export type ChainTip = Readonly<{ point: ChainPoint; blockNo: bigint }>;

export const ORIGIN: ChainPoint = Object.freeze({ kind: "origin" });

export const chainPoint = (slot: bigint, hash: string): BlockPoint => {
  if (slot < 0n || !/^[0-9a-f]{64}$/u.test(hash))
    throw new TypeError("chain point needs a natural slot and a 32-byte hash");
  return Object.freeze({ kind: "point", slot, hash });
};

export const samePoint = (a: ChainPoint, b: ChainPoint): boolean =>
  a.kind === "origin" || b.kind === "origin"
    ? a.kind === b.kind
    : a.slot === b.slot && a.hash === b.hash;

export const pointKey = (point: ChainPoint): string =>
  point.kind === "origin" ? "origin" : `${point.slot}:${point.hash}`;

export const hexToBytes = (hex: string): Uint8Array => {
  if (hex.length % 2 !== 0 || !/^[0-9a-f]*$/u.test(hex))
    throw new TypeError("not lowercase hex");
  return Uint8Array.from(Buffer.from(hex, "hex"));
};

export const bytesToHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes.buffer, bytes.byteOffset, bytes.byteLength).toString("hex");

export class TransportProtocolError extends Error {
  override readonly name = "TransportProtocolError";
}

const fail = (message: string): never => {
  throw new TransportProtocolError(message);
};

/** A header field as text for a diagnostic; a non-text value is "unknown". */
export const headerText = (value: CborValue | CborInput): string =>
  typeof value === "string" ? value : "unknown";

export const natural = (value: CborValue, subject: string): bigint =>
  (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) ||
  (typeof value === "bigint" && value >= 0n)
    ? BigInt(value)
    : fail(`${subject} is not a natural number`);

export const bytesOf = (
  value: CborValue,
  size: number | undefined,
  subject: string,
): Uint8Array =>
  value instanceof Uint8Array && (size === undefined || value.length === size)
    ? value
    : fail(`${subject} is not ${size ?? "a"} byte${size === 1 ? "" : "s"}`);

export const encodePoint = (point: ChainPoint): CborInput =>
  point.kind === "origin" ? [] : [point.slot, hexToBytes(point.hash)];

export const decodePoint = (value: CborValue, subject: string): ChainPoint => {
  if (!Array.isArray(value)) return fail(`${subject} is not a point`);
  if (value.length === 0) return ORIGIN;
  if (value.length !== 2) return fail(`${subject} has an invalid arity`);
  return chainPoint(
    natural(value[0], `${subject} slot`),
    bytesToHex(bytesOf(value[1], 32, `${subject} hash`)),
  );
};

export const decodeTip = (value: CborValue, subject: string): ChainTip => {
  if (!Array.isArray(value) || value.length !== 2)
    return fail(`${subject} is not a tip`);
  return Object.freeze({
    point: decodePoint(value[0], `${subject} point`),
    blockNo: natural(value[1], `${subject} block number`),
  });
};

/** One chain-sync event; seq orders a stream's events without gaps. */
export type RollForward = Readonly<{
  kind: "roll_forward";
  seq: bigint;
  point: BlockPoint;
  blockNo: bigint;
  /** The N2C block type (era tag) the node reported. */
  blockType: number;
  /** The parent block hash; null for a block without a parent hash. */
  prevHash: string | null;
  tip: ChainTip;
  /** The block exactly as the node sent it. */
  block: Uint8Array;
}>;

export type RollBackward = Readonly<{
  kind: "roll_backward";
  seq: bigint;
  point: ChainPoint;
  tip: ChainTip;
}>;

export type ChainSyncEvent = RollForward | RollBackward;

/**
 * Transient unready reasons of the transport: the supervisor restarts the
 * sidecar with backoff until it is ready (`stopped`: closed by its owner).
 */
export type TransportUnreadyReason =
  | "sidecar_starting"
  | "sidecar_restarting"
  | "sidecar_unavailable"
  | "node_unreachable"
  | "node_connection_lost"
  | "stopped";

/**
 * The sidecar's `fatal` codes no restart repairs: the node refused the N2C
 * handshake (`node_handshake_failed`: another network magic, or no common
 * protocol version), or the client and the sidecar disagree on the frame
 * protocol (a binary of another version, a client fault). The transport
 * reports one as `failed` and does not restart the sidecar.
 */
export const TRANSPORT_FAILED_REASONS = [
  "node_handshake_failed",
  "version_unsupported",
  "malformed_frame",
  "client_protocol_violation",
] as const;

export type TransportFailedReason = (typeof TRANSPORT_FAILED_REASONS)[number];

export const isTransportFailedReason = (
  code: string,
): code is TransportFailedReason =>
  (TRANSPORT_FAILED_REASONS as readonly string[]).includes(code);

/**
 * `ready: false` with `failed: true` is terminal: the sidecar ended on a
 * fault no restart repairs and stays down until the process restarts.
 */
export type TransportReadiness =
  | Readonly<{ ready: true; nodeToClientVersion: number }>
  | Readonly<{ ready: false; reason: TransportUnreadyReason; detail: string }>
  | Readonly<{
      ready: false;
      failed: true;
      reason: TransportFailedReason;
      detail: string;
    }>;

/** Credentials as the ledger names them: [0, keyHash] or [1, scriptHash]. */
export type StakeCredential = Readonly<{
  type: "Key" | "Script";
  hash: string;
}>;

export type TxIn = Readonly<{ txId: string; index: number }>;

export type LedgerQuery =
  | Readonly<{ query: "system_start" }>
  | Readonly<{ query: "chain_block_no" }>
  | Readonly<{ query: "chain_point" }>
  | Readonly<{ query: "current_era" }>
  | Readonly<{ query: "era_history" }>
  | Readonly<{ query: "protocol_params" }>
  | Readonly<{ query: "utxo_by_address"; addresses: readonly Uint8Array[] }>
  | Readonly<{ query: "utxo_by_txin"; txIns: readonly TxIn[] }>
  | Readonly<{
      query: "stake_deleg_deposits" | "filtered_delegations_and_rewards";
      credentials: readonly StakeCredential[];
    }>;

export const encodeCredential = (credential: StakeCredential): CborInput => [
  credential.type === "Key" ? 0 : 1,
  hexToBytes(credential.hash),
];

export const encodeQuery = (
  id: number,
  query: LedgerQuery,
): { readonly [key: string]: CborInput } => {
  switch (query.query) {
    case "utxo_by_address":
      return {
        type: "lsq_query",
        id,
        query: query.query,
        addresses: [...query.addresses],
      };
    case "utxo_by_txin":
      return {
        type: "lsq_query",
        id,
        query: query.query,
        txIns: query.txIns.map((input) => [
          hexToBytes(input.txId),
          input.index,
        ]),
      };
    case "stake_deleg_deposits":
    case "filtered_delegations_and_rewards":
      return {
        type: "lsq_query",
        id,
        query: query.query,
        credentials: query.credentials.map(encodeCredential),
      };
    case "system_start":
    case "chain_block_no":
    case "chain_point":
    case "current_era":
    case "era_history":
    case "protocol_params":
      return { type: "lsq_query", id, query: query.query };
  }
};
