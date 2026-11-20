import { createReadStream } from "node:fs";
import { appendFile, mkdir, readdir, rm, stat } from "node:fs/promises";
import { resolve } from "node:path";
import { createInterface } from "node:readline";

const BUCKET_BUFFER_BYTES = 64 * 1024;
const TOTAL_BUFFER_BYTES = 8 * 1024 * 1024;
const VERIFY_BUCKET_BYTES = 8 * 1024 * 1024;
const SHA256_PATTERN = /^[0-9a-f]{64}$/u;

const parseRecord = (line, bucketPath) => {
  const separator = line.indexOf("\t");
  const rowIndex = Number(line.slice(0, separator));
  const txHash = line.slice(separator + 1);
  if (
    separator <= 0 ||
    !Number.isSafeInteger(rowIndex) ||
    rowIndex < 0 ||
    !SHA256_PATTERN.test(txHash)
  ) {
    throw new Error(`invalid transaction-ID bucket record in ${bucketPath}`);
  }
  return { rowIndex, txHash };
};

const createBucketWriter = async (directory, prefixHexLength) => {
  await mkdir(directory, { recursive: true, mode: 0o700 });
  const buffers = new Map();
  let totalBufferedBytes = 0;

  const appendBuffers = async (pending) => {
    await Promise.all(
      pending.map(([bucket, contents]) =>
        appendFile(resolve(directory, `${bucket}.tsv`), contents, {
          encoding: "utf8",
          mode: 0o600,
        }),
      ),
    );
  };
  const flushBucket = async (bucket) => {
    const contents = buffers.get(bucket);
    if (contents === undefined) return;
    buffers.delete(bucket);
    totalBufferedBytes -= Buffer.byteLength(contents);
    await appendBuffers([[bucket, contents]]);
  };
  const flushAll = async () => {
    if (buffers.size === 0) return;
    const pending = [...buffers.entries()];
    buffers.clear();
    totalBufferedBytes = 0;
    await appendBuffers(pending);
  };

  return {
    write: async ({ rowIndex, txHash }) => {
      if (!Number.isSafeInteger(rowIndex) || rowIndex < 0) {
        throw new Error(
          "transaction-ID row index must be a non-negative integer",
        );
      }
      if (!SHA256_PATTERN.test(txHash)) {
        throw new Error("transaction ID must be 32-byte lowercase hex");
      }
      const bucket = txHash.slice(prefixHexLength, prefixHexLength + 2);
      if (!/^[0-9a-f]{2}$/u.test(bucket)) {
        throw new Error("transaction-ID bucket prefix is exhausted");
      }
      const record = `${rowIndex.toString()}\t${txHash}\n`;
      const next = `${buffers.get(bucket) ?? ""}${record}`;
      buffers.set(bucket, next);
      totalBufferedBytes += Buffer.byteLength(record);
      if (Buffer.byteLength(next) >= BUCKET_BUFFER_BYTES) {
        await flushBucket(bucket);
      } else if (totalBufferedBytes >= TOTAL_BUFFER_BYTES) {
        await flushAll();
      }
    },
    close: flushAll,
  };
};

const verifyBucket = async (bucketPath, prefixHexLength) => {
  const bucketStat = await stat(bucketPath);
  if (bucketStat.size <= VERIFY_BUCKET_BYTES) {
    const seen = new Map();
    const input = createInterface({
      input: createReadStream(bucketPath),
      crlfDelay: Infinity,
    });
    for await (const line of input) {
      const record = parseRecord(line, bucketPath);
      const firstRow = seen.get(record.txHash);
      if (firstRow !== undefined) {
        throw new Error(
          `source corpus transaction ${record.txHash} is duplicated globally at rows ${firstRow.toString()} and ${record.rowIndex.toString()}`,
        );
      }
      seen.set(record.txHash, record.rowIndex);
    }
    return;
  }

  if (prefixHexLength >= 64) {
    let firstRecord;
    const input = createInterface({
      input: createReadStream(bucketPath),
      crlfDelay: Infinity,
    });
    for await (const line of input) {
      const record = parseRecord(line, bucketPath);
      if (firstRecord === undefined) {
        firstRecord = record;
      } else if (record.txHash === firstRecord.txHash) {
        throw new Error(
          `source corpus transaction ${record.txHash} is duplicated globally at rows ${firstRecord.rowIndex.toString()} and ${record.rowIndex.toString()}`,
        );
      } else {
        throw new Error("transaction-ID bucket partitioning is inconsistent");
      }
    }
    return;
  }

  const splitDirectory = `${bucketPath}.parts-${prefixHexLength.toString()}`;
  const writer = await createBucketWriter(splitDirectory, prefixHexLength);
  try {
    const input = createInterface({
      input: createReadStream(bucketPath),
      crlfDelay: Infinity,
    });
    for await (const line of input) {
      await writer.write(parseRecord(line, bucketPath));
    }
  } finally {
    await writer.close();
  }
  try {
    const children = (await readdir(splitDirectory, { withFileTypes: true }))
      .filter((entry) => entry.isFile() && entry.name.endsWith(".tsv"))
      .sort((left, right) => left.name.localeCompare(right.name));
    for (const child of children) {
      await verifyBucket(
        resolve(splitDirectory, child.name),
        prefixHexLength + 2,
      );
    }
  } finally {
    await rm(splitDirectory, { recursive: true, force: true });
  }
};

export const createPhase5TransactionIdDisjointnessTracker = async (
  directory,
) => {
  const absoluteDirectory = resolve(directory);
  const writer = await createBucketWriter(absoluteDirectory, 0);
  let closed = false;
  const closeWriter = async () => {
    if (closed) return;
    closed = true;
    await writer.close();
  };
  return {
    add: writer.write,
    verify: async () => {
      await closeWriter();
      const buckets = (
        await readdir(absoluteDirectory, { withFileTypes: true })
      )
        .filter((entry) => entry.isFile() && entry.name.endsWith(".tsv"))
        .sort((left, right) => left.name.localeCompare(right.name));
      for (const bucket of buckets) {
        await verifyBucket(resolve(absoluteDirectory, bucket.name), 2);
      }
    },
    cleanup: async () => {
      await closeWriter().catch(() => undefined);
      await rm(absoluteDirectory, { recursive: true, force: true });
    },
  };
};
