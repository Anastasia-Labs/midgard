/**
 * The `ledger-snapshot` request-response messages. A peer that has no
 * confirmed ledger yet asks a node for the ledger with a given `utxos_root`,
 * one page at a time, each page the entries in the ledger's trie path order
 * strictly after the out-ref `after`. The asker checks the whole stream
 * against the root it read from L1, so nothing a page carries is trusted.
 *
 *   request  = [deployment_fingerprint, utxos_root, after / null]
 *   response = [status, [entry, ...] / null, done / null]
 *   entry    = [out_ref, output]
 *
 * `entries` and `done` are present exactly when `status` is `ok`.
 *
 * The server side reads its pages with `daLedgerSnapshotPage`; the asking
 * side reads a peer's stream with `fetchDaLedgerSnapshot`.
 */
import { asArray, encodeCbor, encodeCborUnsigned } from "./codec/cbor.js";
import { MidgardTxCodecErrorCodes } from "./codec/errors.js";
import { decodeTupleCbor } from "./da-transport.decode-payload-announcement-value.js";
import { bytesValue, fail, fixedArray } from "./da-transport.enum-label.js";
import {
  ensureDaDeploymentFingerprint,
  ensureDaHash32,
  optionalBytesValue,
} from "./da-transport.parse-da-libp2p-runtime-manifest-deployment.js";
import {
  cborEnum,
  decodedEnum,
  deploymentFingerprintValue,
  hashValue,
} from "./da-transport.parse-public-retained-da-profile.js";

export const DaLedgerSnapshotStatus = {
  ok: 0,
  not_held: 1,
  rejected: 2,
} as const;

export type DaLedgerSnapshotStatus = keyof typeof DaLedgerSnapshotStatus;

export type DaLedgerSnapshotRequest = {
  readonly deploymentFingerprint: Buffer;
  /** The root of the ledger asked for. */
  readonly utxosRoot: Buffer;
  /** The out-ref bytes the page starts after; `null` for the first page. */
  readonly after: Buffer | null;
};

/** One ledger entry: the out-ref bytes and the output CBOR. */
export type DaLedgerSnapshotEntry = {
  readonly outRef: Uint8Array;
  readonly output: Uint8Array;
};

export type DaLedgerSnapshotPage = {
  readonly entries: readonly DaLedgerSnapshotEntry[];
  /** The page reaches the end of the ledger. */
  readonly done: boolean;
};

/**
 * `ok`: a page. `not_held`: the node does not hold that root. `rejected`:
 * the fingerprint is not the node's deployment.
 */
export type DaLedgerSnapshotResponse =
  | ({ readonly status: "ok" } & DaLedgerSnapshotPage)
  | { readonly status: "not_held" }
  | { readonly status: "rejected" };

const encodeRequestValue = (message: DaLedgerSnapshotRequest): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHash32(message.utxosRoot, "utxos_root"),
  message.after === null ? null : bytesValue(message.after, "after"),
];

const decodeRequestValue = (
  value: unknown,
  fieldName: string,
): DaLedgerSnapshotRequest => {
  const v = fixedArray(value, 3, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    utxosRoot: hashValue(v[1], `${fieldName}.utxos_root`),
    after: optionalBytesValue(v[2], `${fieldName}.after`),
  };
};

export const encodeDaLedgerSnapshotRequestCbor = (
  message: DaLedgerSnapshotRequest,
): Buffer => encodeCbor(encodeRequestValue(message));

export const decodeDaLedgerSnapshotRequestCbor = (
  bytes: Uint8Array,
): DaLedgerSnapshotRequest =>
  decodeTupleCbor(bytes, "LedgerSnapshotRequestV1", decodeRequestValue);

const encodeEntryValue = (
  entry: DaLedgerSnapshotEntry,
  fieldName: string,
): unknown[] => [
  bytesValue(entry.outRef, `${fieldName}.out_ref`),
  bytesValue(entry.output, `${fieldName}.output`),
];

const decodeEntryValue = (
  value: unknown,
  fieldName: string,
): DaLedgerSnapshotEntry => {
  const v = fixedArray(value, 2, fieldName);
  return {
    outRef: bytesValue(v[0], `${fieldName}.out_ref`),
    output: bytesValue(v[1], `${fieldName}.output`),
  };
};

const booleanValue = (value: unknown, fieldName: string): boolean =>
  typeof value === "boolean"
    ? value
    : fail(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${fieldName} must be a boolean`,
      );

const encodeResponseValue = (message: DaLedgerSnapshotResponse): unknown[] =>
  message.status === "ok"
    ? [
        cborEnum(DaLedgerSnapshotStatus, message.status, "status"),
        message.entries.map((entry, index) =>
          encodeEntryValue(entry, `entries[${index}]`),
        ),
        booleanValue(message.done, "done"),
      ]
    : [cborEnum(DaLedgerSnapshotStatus, message.status, "status"), null, null];

const decodeResponseValue = (
  value: unknown,
  fieldName: string,
): DaLedgerSnapshotResponse => {
  const v = fixedArray(value, 3, fieldName);
  const status = decodedEnum(
    DaLedgerSnapshotStatus,
    v[0],
    `${fieldName}.status`,
  );
  if (status === "ok")
    return {
      status,
      entries: asArray(v[1], `${fieldName}.entries`).map((item, index) =>
        decodeEntryValue(item, `${fieldName}.entries[${index}]`),
      ),
      done: booleanValue(v[2], `${fieldName}.done`),
    };
  if (v[1] !== null || v[2] !== null)
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} carries entries only when status is ok`,
      `status=${status}`,
    );
  return { status };
};

export const encodeDaLedgerSnapshotResponseCbor = (
  message: DaLedgerSnapshotResponse,
): Buffer => encodeCbor(encodeResponseValue(message));

export const decodeDaLedgerSnapshotResponseCbor = (
  bytes: Uint8Array,
): DaLedgerSnapshotResponse =>
  decodeTupleCbor(bytes, "LedgerSnapshotResponseV1", decodeResponseValue);

/** Bytes of the CBOR head of an array of `n` items or a byte string of `n`
 * bytes. */
const headBytes = (n: number): number => encodeCborUnsigned(BigInt(n)).length;

const entryBytes = ({ outRef, output }: DaLedgerSnapshotEntry): number =>
  headBytes(2) +
  headBytes(outRef.length) +
  outRef.length +
  headBytes(output.length) +
  output.length;

/** Bytes of the encoded `ok` response holding `count` entries of
 * `entriesBytes` bytes in all: the 3-item head, the status, the entries'
 * array head, the entries and the one-byte `done`. */
const okResponseBytes = (count: number, entriesBytes: number): number =>
  headBytes(3) +
  headBytes(DaLedgerSnapshotStatus.ok) +
  headBytes(count) +
  entriesBytes +
  1;

/**
 * One page read from `source`: the entries, in order, up to the last one
 * whose encoded `ok` response stays within `maxResponseBytes`, and whether
 * `source` ended with them. The first entry is always taken, so an entry
 * larger than the bound is sent alone. Pulls at most one entry past the
 * page, to learn whether the source has ended, and then closes `source`.
 */
export const daLedgerSnapshotPage = async (
  source: AsyncIterable<DaLedgerSnapshotEntry>,
  maxResponseBytes: number,
): Promise<DaLedgerSnapshotPage> => {
  const entries: DaLedgerSnapshotEntry[] = [];
  let entriesBytes = 0;
  for await (const entry of source) {
    const bytes = entryBytes(entry);
    if (
      entries.length > 0 &&
      okResponseBytes(entries.length + 1, entriesBytes + bytes) >
        maxResponseBytes
    )
      return { entries, done: false };
    entries.push(entry);
    entriesBytes += bytes;
  }
  return { entries, done: true };
};

/**
 * The peer did not serve the snapshot this time: the request failed (it
 * does not speak the protocol, the stream broke or timed out), or it
 * answered `not_held` or `rejected`. It may be asked again later.
 */
export class DaLedgerSnapshotUnserved extends Error {
  constructor(
    readonly peerIdentity: string,
    detail: string,
    options?: ErrorOptions,
  ) {
    super(
      `DA peer ${peerIdentity} did not serve the ledger snapshot: ${detail}`,
      options,
    );
    this.name = "DaLedgerSnapshotUnserved";
  }
}

/**
 * The peer answered what no honest peer sends: a response that does not
 * decode, or an `ok` page with no entry that does not reach the end.
 */
export class DaLedgerSnapshotMalformed extends Error {
  constructor(
    readonly peerIdentity: string,
    detail: string,
    options?: ErrorOptions,
  ) {
    super(
      `DA peer ${peerIdentity} served a malformed ledger snapshot: ${detail}`,
      options,
    );
    this.name = "DaLedgerSnapshotMalformed";
  }
}

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * The entries of the ledger with root `utxosRoot` (hex), as one peer serves
 * them. The peer is asked one page per request, in the ledger's trie path
 * order; the next page is asked for after the last out-ref of the page
 * before, once the consumer has taken every entry of it, until a page says
 * it reaches the end of the ledger. One page is held at a time, and no
 * request is repeated. Nothing a page carries is checked here: the consumer
 * checks the whole stream against the root it asked for.
 *
 * Rejects with `DaLedgerSnapshotUnserved` when `request` rejects or the
 * peer answers `not_held` or `rejected`, with `DaLedgerSnapshotMalformed`
 * when it answers what no honest peer sends, and with `signal`'s reason
 * when `request` rejects after `signal` is aborted.
 */
export async function* fetchDaLedgerSnapshot(
  input: Readonly<{
    /** The peer's name in errors. */
    peerIdentity: string;
    /** The deployment fingerprint, hex. */
    deploymentFingerprint: string;
    utxosRoot: string;
    /** Sends one encoded request to the peer; its encoded response. */
    request: (requestCbor: Buffer) => Promise<Uint8Array>;
    /** The transfer's lifetime. */
    signal: AbortSignal;
  }>,
): AsyncGenerator<{ outRef: Buffer; output: Buffer }> {
  const { peerIdentity } = input;
  const deploymentFingerprint = Buffer.from(input.deploymentFingerprint, "hex");
  const utxosRoot = Buffer.from(input.utxosRoot, "hex");
  const ask = async (
    request: DaLedgerSnapshotRequest,
  ): Promise<DaLedgerSnapshotResponse> => {
    let response: Uint8Array;
    try {
      response = await input.request(
        encodeDaLedgerSnapshotRequestCbor(request),
      );
    } catch (error) {
      // The caller closing ends the transfer; any other failure is this
      // peer's, for this transfer.
      input.signal.throwIfAborted();
      throw new DaLedgerSnapshotUnserved(
        peerIdentity,
        `the request failed: ${message(error)}`,
        { cause: error },
      );
    }
    try {
      return decodeDaLedgerSnapshotResponseCbor(response);
    } catch (error) {
      throw new DaLedgerSnapshotMalformed(
        peerIdentity,
        `the response does not decode: ${message(error)}`,
        { cause: error },
      );
    }
  };
  let after: Buffer | null = null;
  for (;;) {
    const page = await ask({ deploymentFingerprint, utxosRoot, after });
    if (page.status !== "ok")
      throw new DaLedgerSnapshotUnserved(
        peerIdentity,
        `it answered ${page.status}`,
      );
    const last = page.entries.at(-1);
    // Asking after the same out-ref again would get the same page.
    if (last === undefined && !page.done)
      throw new DaLedgerSnapshotMalformed(
        peerIdentity,
        "a page with no entry does not reach the end of the ledger",
      );
    for (const entry of page.entries)
      yield {
        outRef: Buffer.from(entry.outRef),
        output: Buffer.from(entry.output),
      };
    if (page.done || last === undefined) return;
    after = Buffer.from(last.outRef);
  }
}
