import blake2b from "blake2b";
import { Level } from "level";

import * as Ledger from "../src/database/utils/ledger.js";
import * as Tx from "../src/database/utils/tx.js";
import { DecodedMempoolTxForCommit } from "../src/mpf/index.js";

export const TEST_DB = "test-mpf-db";

export const EMPTY_DELETE_DB = "test-mpf-empty-delete-db";

export const BATCH_PERSIST_DB = "test-mpf-batch-persist-db";

export const CORRUPT_DB = "test-mpf-corrupt-db";

export const OVERLAY_DB = "test-mpf-overlay-db";

export const OVERLAY_RESET_DB = "test-mpf-overlay-reset-db";

export const OVERLAY_SPILL_DB = "test-mpf-overlay-spill-db";

export const OVERLAY_FAILURE_DB = "test-mpf-overlay-failure-db";

export const PATH_HYDRATION_DB = "test-mpf-path-hydration-db";

export const PATH_HYDRATION_FAILURE_DB = "test-mpf-path-hydration-failure-db";

export const key1 = Buffer.from("01", "hex");

export const key2 = Buffer.from("02", "hex");

export const key3 = Buffer.from("03", "hex");

export const value1 = Buffer.from("aa", "hex");

export const value2 = Buffer.from("bb", "hex");

export const value3 = Buffer.from("cc", "hex");

export const mpfDigest = (value: Buffer): Buffer =>
  Buffer.from(blake2b(32).update(value).digest());

const mpfMerkleRoot = (children: readonly (Buffer | undefined)[]): Buffer => {
  let nodes = children.map((child) => child ?? Buffer.alloc(32));
  while (nodes.length > 1) {
    const next: Buffer[] = [];
    for (let index = 0; index < nodes.length; index += 2) {
      next.push(mpfDigest(Buffer.concat([nodes[index]!, nodes[index + 1]!])));
    }
    nodes = next;
  }
  return nodes[0]!;
};

const mpfLeafHash = (prefix: string, value: Buffer): Buffer => {
  const odd = prefix.length % 2 > 0;
  const head = odd
    ? Buffer.from([0, Number.parseInt(prefix[0]!, 16)])
    : Buffer.from([255]);
  const tail = Buffer.from(odd ? prefix.slice(1) : prefix, "hex");
  return mpfDigest(Buffer.concat([head, tail, mpfDigest(value)]));
};

export const buildDeepSharedMpfDag = (
  key: Buffer,
  value: Buffer,
  depth = 10,
  sharedPrefix = "f",
) => {
  const path = mpfDigest(key).toString("hex");
  const records = new Map<string, Record<string, unknown>>();
  const chosenLeaf = {
    __kind: "Leaf",
    prefix: path.slice(depth),
    key: key.toString("hex"),
    value: value.toString("hex"),
  };
  let currentHash = mpfLeafHash(chosenLeaf.prefix, value);
  records.set(currentHash.toString("hex"), chosenLeaf);

  const sharedValue = Buffer.from("5a", "hex");
  const sharedLeaf = {
    __kind: "Leaf",
    prefix: sharedPrefix,
    key: Buffer.alloc(32, 0x5a).toString("hex"),
    value: sharedValue.toString("hex"),
  };
  const sharedHash = mpfLeafHash(sharedPrefix, sharedValue);
  records.set(sharedHash.toString("hex"), sharedLeaf);
  const chainHashes: string[] = [];

  for (let index = depth - 1; index >= 0; index -= 1) {
    const selected = Number.parseInt(path[index]!, 16);
    const sibling = (selected + 1) % 16;
    const childHashes = Array<Buffer | undefined>(16).fill(undefined);
    childHashes[selected] = currentHash;
    childHashes[sibling] = sharedHash;
    const children = childHashes.map((child) => child?.toString("hex"));
    currentHash = mpfDigest(mpfMerkleRoot(childHashes));
    records.set(currentHash.toString("hex"), {
      __kind: "Branch",
      prefix: "",
      children,
      size: depth - index + 1,
    });
    chainHashes[index] = currentHash.toString("hex");
  }
  return {
    root: currentHash.toString("hex"),
    path,
    records,
    sharedHash: sharedHash.toString("hex"),
    chainHashes,
  };
};

export const seedSerializedMpfDag = async (
  path: string,
  dag: ReturnType<typeof buildDeepSharedMpfDag>,
): Promise<void> => {
  const level = new Level<string, string | Record<string, unknown>>(path, {
    valueEncoding: "json",
  });
  await level.open();
  await level.batch([
    ...[...dag.records].map(([key, value]) => ({
      type: "put" as const,
      key,
      value,
    })),
    { type: "put" as const, key: "__root__", value: dag.root },
  ]);
  await level.close();
};

export const makeTxHash = (byte: number) => Buffer.alloc(32, byte);

export const makeOutRef = (byte: number) => Buffer.from([byte, 0]);

export const makeDecodedMempoolTx = ({
  txHash,
  spent,
  produced,
}: {
  readonly txHash: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly Buffer[];
}): DecodedMempoolTxForCommit => ({
  entry: {
    [Tx.Columns.TX_ID]: txHash,
    [Tx.Columns.TX]: txHash,
    [Tx.Columns.TIMESTAMPTZ]: new Date(0),
  },
  txHash,
  txCbor: txHash,
  spent,
  produced: produced.map((outRef) => ({
    [Ledger.Columns.OUTREF]: outRef,
    [Ledger.Columns.OUTPUT]: Buffer.from("01", "hex"),
  })),
});
