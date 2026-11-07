import { summarizeMidgardCekLucidData } from "../cek-context.js";
import { txOutRefData } from "../tx-out-ref.js";

export const sameSummary = (
  left: {
    readonly root: Uint8Array;
    readonly cborLength: bigint;
    readonly memory: bigint;
  },
  right: {
    readonly root: Uint8Array;
    readonly cborLength: bigint;
    readonly memory: bigint;
  },
): boolean =>
  Buffer.from(left.root).equals(Buffer.from(right.root)) &&
  left.cborLength === right.cborLength &&
  left.memory === right.memory;

export const sameSequence = (
  left: {
    readonly root: Uint8Array;
    readonly length: bigint;
    readonly payloadCborLength: bigint;
    readonly memory: bigint;
  },
  right: {
    readonly root: Uint8Array;
    readonly length: bigint;
    readonly payloadCborLength: bigint;
    readonly memory: bigint;
  },
): boolean =>
  Buffer.from(left.root).equals(Buffer.from(right.root)) &&
  left.length === right.length &&
  left.payloadCborLength === right.payloadCborLength &&
  left.memory === right.memory;

export const exactDescriptorSummary = (summary: {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
}) => ({
  root: Buffer.from(summary.root),
  cborLength: summary.cborLength,
  memory: summary.memory,
});

export const outRefSummary = (key: Buffer) =>
  summarizeMidgardCekLucidData(txOutRefData(key.toString("hex")) as never);
