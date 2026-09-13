import { Worker } from "node:worker_threads";

import * as SDK from "@al-ft/midgard-sdk";

import { resolveWorkerEntry } from "../../fibers/resolve-worker-entry.js";
import type {
  MpfRootBuilderRequest,
  MpfRootBuilderResponse,
} from "../mpf-root-builder.js";

type Pending = {
  readonly request: Omit<MpfRootBuilderRequest, "id">;
  readonly transfer: readonly ArrayBuffer[];
  readonly resolve: (value: MpfRootWorkerResult) => void;
  readonly reject: (reason: Error) => void;
  timeout?: ReturnType<typeof setTimeout>;
};

export type MpfRootWorkerMetrics = {
  readonly requests: number;
  readonly serializationMs: number;
  readonly serializedBytes: number;
  readonly failures: number;
  readonly activeWorkers: number;
  readonly maxActiveWorkers: number;
  readonly unpackMs: number;
  readonly canonicalizeMs: number;
  readonly trieFromListMs: number;
  readonly domainCommitMs: number;
};

export type MpfRootWorkerResult = {
  readonly rootHex: string;
  readonly phasRoot: string;
  readonly count: bigint;
  readonly timings: {
    readonly unpackMs: number;
    readonly canonicalizeMs: number;
    readonly trieFromListMs: number;
    readonly domainCommitMs: number;
  };
};

class RootWorkerPool {
  private readonly workers = new Set<Worker>();
  private readonly idle: Worker[] = [];
  private readonly queue: Pending[] = [];
  private readonly pendingById = new Map<
    number,
    Pending & { worker: Worker }
  >();
  private nextId = 1;
  private closing = false;

  constructor(
    private readonly size: number,
    private readonly timeoutMs: number,
  ) {
    for (let index = 0; index < size; index += 1) this.spawn();
  }

  run(
    request: Omit<MpfRootBuilderRequest, "id">,
    transfer: readonly ArrayBuffer[],
  ): Promise<MpfRootWorkerResult> {
    if (this.closing) return Promise.reject(new Error("MPF root pool closed"));
    return new Promise((resolve, reject) => {
      this.queue.push({ request, transfer, resolve, reject });
      this.drain();
    });
  }

  close(): void {
    this.closing = true;
    for (const queued of this.queue.splice(0)) {
      queued.reject(new Error("MPF root pool closed before dispatch"));
    }
    for (const [id, pending] of this.pendingById) {
      if (pending.timeout !== undefined) clearTimeout(pending.timeout);
      pending.reject(new Error("MPF root pool closed during request"));
      this.pendingById.delete(id);
    }
    for (const worker of this.workers) void worker.terminate();
    this.workers.clear();
    this.idle.length = 0;
    metrics = { ...metrics, activeWorkers: 0 };
  }

  private spawn() {
    if (this.closing) return;
    const worker = new Worker(
      resolveWorkerEntry(import.meta.url, "mpf-root-builder.js"),
    );
    worker.unref();
    this.workers.add(worker);
    worker.on("message", (response: MpfRootBuilderResponse) =>
      this.complete(worker, response),
    );
    worker.on("error", (error) => this.failWorker(worker, error));
    worker.on("exit", (code) => {
      if (this.workers.has(worker)) {
        this.failWorker(
          worker,
          new Error(`MPF root worker exited unexpectedly with code ${code}`),
        );
      }
    });
    this.idle.push(worker);
  }

  private drain() {
    while (this.idle.length > 0 && this.queue.length > 0) {
      const worker = this.idle.pop()!;
      const pending = this.queue.shift()!;
      const id = this.nextId++;
      worker.ref();
      pending.timeout = setTimeout(
        () => this.timeout(worker, id),
        this.timeoutMs,
      );
      this.pendingById.set(id, { ...pending, worker });
      metrics = {
        ...metrics,
        activeWorkers: this.pendingById.size,
        maxActiveWorkers: Math.max(
          metrics.maxActiveWorkers,
          this.pendingById.size,
        ),
      };
      worker.postMessage(
        { id, ...pending.request } satisfies MpfRootBuilderRequest,
        [...pending.transfer],
      );
    }
  }

  private complete(worker: Worker, response: MpfRootBuilderResponse) {
    const pending = this.pendingById.get(response.id);
    if (pending === undefined || pending.worker !== worker) {
      this.failWorker(worker, new Error("MPF root worker response mismatch"));
      return;
    }
    this.pendingById.delete(response.id);
    metrics = { ...metrics, activeWorkers: this.pendingById.size };
    if (pending.timeout !== undefined) clearTimeout(pending.timeout);
    worker.unref();
    this.idle.push(worker);
    if ("error" in response) pending.reject(new Error(response.error));
    else {
      metrics = {
        ...metrics,
        unpackMs: metrics.unpackMs + response.timings.unpackMs,
        canonicalizeMs:
          metrics.canonicalizeMs + response.timings.canonicalizeMs,
        trieFromListMs:
          metrics.trieFromListMs + response.timings.trieFromListMs,
        domainCommitMs:
          metrics.domainCommitMs + response.timings.domainCommitMs,
      };
      pending.resolve(response);
    }
    this.drain();
  }

  private timeout(worker: Worker, id: number) {
    const pending = this.pendingById.get(id);
    if (pending === undefined || pending.worker !== worker) return;
    this.pendingById.delete(id);
    metrics = { ...metrics, activeWorkers: this.pendingById.size };
    pending.reject(
      new Error(`MPF root worker timed out after ${this.timeoutMs} ms`),
    );
    this.replace(worker);
  }

  private failWorker(worker: Worker, error: Error) {
    for (const [id, pending] of this.pendingById) {
      if (pending.worker === worker) {
        if (pending.timeout !== undefined) clearTimeout(pending.timeout);
        this.pendingById.delete(id);
        pending.reject(error);
      }
    }
    metrics = { ...metrics, activeWorkers: this.pendingById.size };
    this.replace(worker);
  }

  private replace(worker: Worker) {
    this.workers.delete(worker);
    const idleIndex = this.idle.indexOf(worker);
    if (idleIndex >= 0) this.idle.splice(idleIndex, 1);
    void worker.terminate();
    if (!this.closing && this.workers.size < this.size) this.spawn();
    this.drain();
  }
}

let configured:
  | {
      readonly enabled: boolean;
      readonly workers: number;
      readonly minEntries: number;
      readonly timeoutMs: number;
    }
  | undefined;
let pool: RootWorkerPool | undefined;
const emptyMetrics = (): MpfRootWorkerMetrics => ({
  requests: 0,
  serializationMs: 0,
  serializedBytes: 0,
  failures: 0,
  activeWorkers: 0,
  maxActiveWorkers: 0,
  unpackMs: 0,
  canonicalizeMs: 0,
  trieFromListMs: 0,
  domainCommitMs: 0,
});
let metrics: MpfRootWorkerMetrics = emptyMetrics();

export const configureMpfRootWorkers = (options: {
  readonly enabled: boolean;
  readonly workers: number;
  readonly minEntries: number;
  readonly timeoutMs?: number;
}): void => {
  const next = { ...options, timeoutMs: options.timeoutMs ?? 120_000 };
  if (
    configured !== undefined &&
    (configured.workers !== next.workers ||
      configured.timeoutMs !== next.timeoutMs ||
      !next.enabled)
  ) {
    pool?.close();
    pool = undefined;
  }
  configured = next;
};

export const closeMpfRootWorkers = (): void => {
  pool?.close();
  pool = undefined;
};

export const prewarmMpfRootWorkers = async (): Promise<void> => {
  if (configured?.enabled !== true || pool !== undefined) return;
  pool = new RootWorkerPool(configured.workers, configured.timeoutMs);
  await Promise.all(
    Array.from({ length: configured.workers }, () => {
      const keys = new ArrayBuffer(0);
      const values = new ArrayBuffer(0);
      const offsets = new ArrayBuffer(0);
      return pool!.run(
        {
          domain: SDK.ROOT_DOMAINS.transactionsV1,
          counted: true,
          keys,
          values,
          offsets,
        },
        [keys, values, offsets],
      );
    }),
  );
  metrics = emptyMetrics();
};

export const mpfRootWorkerMetrics = (): MpfRootWorkerMetrics => ({
  ...metrics,
});

export const shouldBuildMpfRootInWorker = (entryCount: number): boolean =>
  configured?.enabled === true && entryCount >= configured.minEntries;

const packEntries = (
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
) => {
  const startedAt = performance.now();
  const keyBytes = entries.reduce((sum, entry) => sum + entry.key.length, 0);
  const valueBytes = entries.reduce(
    (sum, entry) => sum + entry.value.length,
    0,
  );
  const keys = new ArrayBuffer(keyBytes);
  const values = new ArrayBuffer(valueBytes);
  const offsets = new ArrayBuffer(
    entries.length * 4 * Uint32Array.BYTES_PER_ELEMENT,
  );
  const keyArena = Buffer.from(keys);
  const valueArena = Buffer.from(values);
  const offsetArena = new Uint32Array(offsets);
  let keyOffset = 0;
  let valueOffset = 0;
  for (const [index, entry] of entries.entries()) {
    entry.key.copy(keyArena, keyOffset);
    entry.value.copy(valueArena, valueOffset);
    offsetArena.set(
      [keyOffset, entry.key.length, valueOffset, entry.value.length],
      index * 4,
    );
    keyOffset += entry.key.length;
    valueOffset += entry.value.length;
  }
  metrics = {
    ...metrics,
    requests: metrics.requests + 1,
    serializationMs: metrics.serializationMs + (performance.now() - startedAt),
    serializedBytes:
      metrics.serializedBytes + keyBytes + valueBytes + offsets.byteLength,
  };
  return { keys, values, offsets };
};

export const buildCountedMpfRootInWorker = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Promise<string> => {
  const workerCount = configured?.workers ?? 1;
  pool ??= new RootWorkerPool(workerCount, configured?.timeoutMs ?? 120_000);
  const packed = packEntries(entries);
  const { keys, values, offsets } = packed;
  return pool
    .run({ domain, counted: true, keys, values, offsets }, [
      keys,
      values,
      offsets,
    ])
    .then((result) => result.rootHex)
    .catch((error) => {
      metrics = { ...metrics, failures: metrics.failures + 1 };
      throw error;
    });
};

export const buildPhasMpfRootInWorker = (
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Promise<string> => {
  const workerCount = configured?.workers ?? 1;
  pool ??= new RootWorkerPool(workerCount, configured?.timeoutMs ?? 120_000);
  const packed = packEntries(entries);
  const { keys, values, offsets } = packed;
  return pool
    .run(
      {
        domain: SDK.ROOT_DOMAINS.transactionsV1,
        counted: false,
        keys,
        values,
        offsets,
      },
      [keys, values, offsets],
    )
    .then((result) => result.rootHex);
};

export const buildAuthenticatedMpfRootInWorker = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Promise<MpfRootWorkerResult> => {
  const workerCount = configured?.workers ?? 1;
  pool ??= new RootWorkerPool(workerCount, configured?.timeoutMs ?? 120_000);
  const packed = packEntries(entries);
  const { keys, values, offsets } = packed;
  return pool.run(
    {
      domain,
      counted: true,
      keys,
      values,
      offsets,
    },
    [keys, values, offsets],
  );
};
