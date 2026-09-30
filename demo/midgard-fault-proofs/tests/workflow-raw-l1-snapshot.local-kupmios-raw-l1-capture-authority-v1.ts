import {
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createLocalKupmiosFraudProofRawL1SnapshotAuthority,
  LocalKupmiosCheckpointChangedError,
} from "../src/workflow/index.js";
import {
  admit,
  chainPoint,
  fixture,
  releaseFinality,
  UNIT,
} from "./workflow-raw-l1-snapshot.fixture.js";
import { localSource } from "./workflow-raw-l1-snapshot.raw-l1-snapshot-v1-admission.js";

describe("local Kupmios raw L1 capture authority V1", () => {
  it("drains failed capture siblings before a queued authority repins the source", async () => {
    const value = fixture();
    const original = localSource(value);
    let releaseRead!: () => void;
    let readStarted!: () => void;
    const pending = new Promise<void>((resolve) => {
      releaseRead = resolve;
    });
    const started = new Promise<void>((resolve) => {
      readStarted = resolve;
    });
    const providerError = new Error("provider address read failed");
    let boundaries = 0;
    let pendingReadFinished = false;
    const source = localSource(value, {
      readBoundary: async () => {
        boundaries += 1;
        if (boundaries > 1) expect(pendingReadFinished).toBe(true);
        return await original.readBoundary();
      },
      scanAddressPage: async (input) => {
        if (boundaries === 1) {
          if (input.address === "failed-address") throw providerError;
          readStarted();
          await pending;
          pendingReadFinished = true;
        }
        return await original.scanAddressPage(input);
      },
    });
    const first = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source,
      releaseFinality,
    })
      .capture({
        ...value.request,
        scopes: [
          ...value.request.scopes,
          { role: "hub_oracle", address: "failed-address" },
        ],
      })
      .catch((error: unknown) => error);
    await started;
    const second = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source,
      releaseFinality,
    }).capture(value.request);
    await new Promise<void>((resolve) => setImmediate(resolve));
    const boundariesWhilePending = boundaries;
    releaseRead();
    expect(await first).toBe(providerError);
    await expect(second).resolves.toEqual(value.snapshot);
    expect(boundariesWhilePending).toBe(1);
    expect(boundaries).toBe(2);
  });

  it("restarts the whole unpublished snapshot and discards prior scope/history/transaction data", async () => {
    const value = fixture();
    const original = localSource(value);
    const freshPoint = chainPoint({
      slot: "1072",
      blockNo: "72",
      blockHash: "51".repeat(32),
    });
    const freshTip = chainPoint({
      slot: "1101",
      blockNo: "101",
      blockHash: "52".repeat(32),
    });
    let attempts = 0;
    let transactionReads = 0;
    const source = localSource(value, {
      readBoundary: async () => {
        attempts += 1;
        return attempts === 1
          ? original.readBoundary()
          : { kupoCheckpoint: freshPoint, ogmiosTip: freshTip };
      },
      scanAddressPage: async (input) =>
        attempts === 1
          ? original.scanAddressPage(input)
          : {
              checkpoint: freshPoint,
              utxos: [],
              nextCursor: null,
              complete: true,
            },
      scanUnitHistoryPage: async (input) =>
        attempts === 1
          ? original.scanUnitHistoryPage(input)
          : {
              checkpoint: freshPoint,
              transactions: [],
              nextCursor: null,
              complete: true,
            },
      readTransaction: async (input) => {
        transactionReads += 1;
        return original.readTransaction(input);
      },
      confirmCanonicalPoint: async ({ point }) => {
        if (attempts === 1)
          throw new LocalKupmiosCheckpointChangedError(
            "head advanced after complete first read",
          );
        return { canonical: true, point };
      },
    });
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source,
      releaseFinality,
    });
    const result = admit(await authority.capture(value.request), value.request);
    expect(attempts).toBe(2);
    expect(transactionReads).toBe(1);
    expect(result.cursor.point).toEqual(freshPoint);
    expect(result.cursor.tip).toEqual(freshTip);
    expect(result.scopes).toEqual([{ ...value.request.scopes[0], utxos: [] }]);
    expect(result.history).toEqual([
      {
        unit: UNIT,
        fromGenesis: true,
        completeThroughPointId: freshPoint.pointId,
        transactionHashes: [],
      },
    ]);
    expect(result.transactions).toEqual([]);
  });

  it("exhausts exactly three typed head-change attempts and preserves the last error", async () => {
    const value = fixture();
    const failures = Array.from(
      { length: 3 },
      (_, attempt) =>
        new LocalKupmiosCheckpointChangedError(`head change ${attempt}`),
    );
    let attempts = 0;
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source: localSource(value, {
        readBoundary: async () => {
          throw failures[attempts++]!;
        },
      }),
      releaseFinality,
    });
    await expect(authority.capture(value.request)).rejects.toBe(failures[2]);
    expect(attempts).toBe(3);
  });

  it.each(["rollback", "malformed", "ordinary", "abort", "lookalike"] as const)(
    "does not retry %s confirmation failures",
    async (kind) => {
      const value = fixture();
      const original = localSource(value);
      let attempts = 0;
      const failure =
        kind === "abort"
          ? new DOMException("cancelled", "AbortError")
          : new Error("source capture head changed");
      if (kind === "lookalike")
        Object.defineProperty(failure, "name", {
          value: "LocalKupmiosCheckpointChangedError",
        });
      const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
        source: localSource(value, {
          readBoundary: async () => {
            attempts += 1;
            return original.readBoundary();
          },
          confirmCanonicalPoint: async ({ point }) => {
            if (kind === "rollback") return { canonical: false, point };
            if (kind === "malformed") return { canonical: "false", point };
            throw failure;
          },
        }),
        releaseFinality,
      });
      if (kind === "rollback")
        await expect(authority.capture(value.request)).rejects.toThrow(
          "rolled back during snapshot capture",
        );
      else if (kind === "malformed")
        await expect(authority.capture(value.request)).rejects.toThrow(
          "invalid verdict",
        );
      else await expect(authority.capture(value.request)).rejects.toBe(failure);
      expect(attempts).toBe(1);
    },
  );

  it("retains source ownership and drains siblings before retrying or admitting a competing capture", async () => {
    const value = fixture();
    const original = localSource(value);
    const siblingAddress = credentialToAddress(
      "Preview",
      scriptHashToCredential("22".repeat(28)),
    );
    let finishSibling!: () => void;
    let siblingStarted!: () => void;
    const pending = new Promise<void>((resolve) => {
      finishSibling = resolve;
    });
    const started = new Promise<void>((resolve) => {
      siblingStarted = resolve;
    });
    let boundaries = 0;
    let siblingFinished = false;
    const request = {
      ...value.request,
      scopes: [
        ...value.request.scopes,
        { role: "hub_oracle" as const, address: siblingAddress },
      ],
    };
    const source = localSource(value, {
      readBoundary: async () => {
        boundaries += 1;
        if (boundaries > 1) expect(siblingFinished).toBe(true);
        return original.readBoundary();
      },
      scanAddressPage: async (input) => {
        if (boundaries === 1) {
          if (input.address === siblingAddress)
            throw new LocalKupmiosCheckpointChangedError(
              "head advanced during sibling read",
            );
          siblingStarted();
          await pending;
          siblingFinished = true;
        }
        return input.address === siblingAddress
          ? {
              checkpoint: value.snapshot.cursor.point,
              utxos: [],
              nextCursor: null,
              complete: true,
            }
          : original.scanAddressPage(input);
      },
    });
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source,
      releaseFinality,
    });
    const first = authority.capture(request);
    await started;
    const competing = authority.capture(value.request);
    await new Promise<void>((resolve) => setImmediate(resolve));
    expect(boundaries).toBe(1);
    finishSibling();
    await expect(first).resolves.toMatchObject({
      scopes: [
        { ...value.request.scopes[0], utxos: value.snapshot.scopes[0].utxos },
        { role: "hub_oracle", address: siblingAddress, utxos: [] },
      ],
    });
    await expect(competing).resolves.toEqual(value.snapshot);
    expect(boundaries).toBe(3);
  });

  it("paginates address/unit scans from origin and cross-checks Ogmios bytes", async () => {
    const value = fixture();
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source: localSource(value),
      releaseFinality,
    });
    await expect(authority.capture(value.request)).resolves.toEqual(
      value.snapshot,
    );
  });

  it("rejects a truncated page without a continuation cursor", async () => {
    const value = fixture();
    const source = localSource(value, {
      scanUnitHistoryPage: async () => ({
        checkpoint: value.snapshot.cursor.point,
        transactions: [],
        nextCursor: null,
        complete: false,
      }),
    });
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source,
      releaseFinality,
    });
    await expect(authority.capture(value.request)).rejects.toThrow(
      /truncated or contradictory continuation/u,
    );
  });

  it("rejects rollback of the pinned point during a paginated scan", async () => {
    const value = fixture();
    const rolledBackPoint = chainPoint({
      slot: value.snapshot.cursor.point.slot,
      blockNo: value.snapshot.cursor.point.blockNo,
      blockHash: "91".repeat(32),
    });
    const source = localSource(value, {
      confirmCanonicalPoint: async () => ({
        canonical: false,
        point: rolledBackPoint,
      }),
    });
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source,
      releaseFinality,
    });
    await expect(authority.capture(value.request)).rejects.toThrow(
      /rolled back during snapshot capture/u,
    );
  });

  it("propagates a failed canonical read without reclassifying it as rollback", async () => {
    const value = fixture();
    const originalError = new Error("source capture head changed");
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source: localSource(value, {
        confirmCanonicalPoint: async () => {
          throw originalError;
        },
      }),
      releaseFinality,
    });
    await expect(authority.capture(value.request)).rejects.toBe(originalError);
  });

  it("distinguishes a substituted successful point from an explicit rollback", async () => {
    const value = fixture();
    const substituted = chainPoint({
      slot: value.snapshot.cursor.point.slot,
      blockNo: value.snapshot.cursor.point.blockNo,
      blockHash: "91".repeat(32),
    });
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source: localSource(value, {
        confirmCanonicalPoint: async () => ({
          canonical: true,
          point: substituted,
        }),
      }),
      releaseFinality,
    });
    await expect(authority.capture(value.request)).rejects.toThrow(
      "substituted the pinned point",
    );
  });

  it("rejects a malformed canonical verdict without inventing a rollback", async () => {
    const value = fixture();
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source: localSource(value, {
        confirmCanonicalPoint: async () => ({
          canonical: "false",
          point: value.snapshot.cursor.point,
        }),
      }),
      releaseFinality,
    });
    await expect(authority.capture(value.request)).rejects.toThrow(
      "invalid verdict",
    );
  });

  it("rejects Kupo/Ogmios transaction inclusion disagreement", async () => {
    const value = fixture();
    const substitutedPoint = chainPoint({
      slot: "1069",
      blockNo: "69",
      blockHash: "92".repeat(32),
    });
    const source = localSource(value, {
      readTransaction: async ({ txHash }) => ({
        kupo: {
          txHash,
          inclusionPoint: value.snapshot.transactions[0]!.inclusionPoint,
        },
        ogmios: {
          ...value.snapshot.transactions[0],
          inclusionPoint: substitutedPoint,
        },
      }),
    });
    const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source,
      releaseFinality,
    });
    await expect(authority.capture(value.request)).rejects.toThrow(
      /Kupo and Ogmios disagree/u,
    );
  });

  it("refuses non-loopback provider endpoints", () => {
    const value = fixture();
    expect(() =>
      createLocalKupmiosFraudProofRawL1SnapshotAuthority({
        source: localSource(value, {
          kupoHttpUrl: "https://operator.example.invalid/kupo",
        }),
        releaseFinality,
      }),
    ).toThrow(/loopback local provider/u);
  });
});
