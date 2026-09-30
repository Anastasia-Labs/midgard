import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitFraudProofRawL1Snapshot,
  admitFraudProofRawL1Transaction,
  type FraudProofRawL1SnapshotRequest,
  LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
  type LocalKupmiosFraudProofRawSource,
} from "../src/workflow/index.js";
import {
  admit,
  chainPoint,
  fixture,
  mutable,
  OTHER_UNIT,
  releaseFinality,
  SOURCE,
} from "./workflow-raw-l1-snapshot.fixture.js";

describe("raw L1 snapshot V1 admission", () => {
  it("admits canonical address-scoped bytes and complete unit history", () => {
    const value = fixture();
    expect(admit(value.snapshot, value.request)).toEqual(value.snapshot);
  });

  it("preserves legal noncanonical transaction encoding under its exact signed body hash", () => {
    const row = mutable(fixture().snapshot.transactions[0]!);
    row.bodyCbor = `bf${row.bodyCbor.slice(2)}ff`;
    row.witnessSetCbor = "bfff";
    row.txHash = CML.hash_transaction(
      CML.TransactionBody.from_cbor_hex(row.bodyCbor),
    ).to_hex();
    expect(
      CML.TransactionBody.from_cbor_hex(row.bodyCbor).to_canonical_cbor_hex(),
    ).not.toBe(row.bodyCbor);
    expect(admitFraudProofRawL1Transaction(row, "recorded", 1)).toEqual(row);
    const normalized = {
      ...row,
      bodyCbor: CML.TransactionBody.from_cbor_hex(
        row.bodyCbor,
      ).to_canonical_cbor_hex(),
    };
    expect(() =>
      admitFraudProofRawL1Transaction(normalized, "substituted", 1),
    ).toThrow(/hash-mismatched/u);
    for (const key of ["bodyCbor", "witnessSetCbor"] as const) {
      expect(() =>
        admitFraudProofRawL1Transaction(
          { ...row, [key]: row[key] + "00" },
          "trailing",
          1,
        ),
      ).toThrow();
      expect(() =>
        admitFraudProofRawL1Transaction(
          { ...row, [key]: "ff" },
          "malformed",
          1,
        ),
      ).toThrow();
    }
  });

  it("rejects an output from an address outside its requested scope", () => {
    const value = fixture();
    const forged = mutable(value.snapshot);
    forged.scopes[0]!.utxos[0]!.outputCbor = value.alternateAddressOutput;
    expect(() => admit(forged, value.request)).toThrow(/different address/u);
  });

  it("rejects omitted or substituted reference-input resolutions", () => {
    const value = fixture();
    const omitted = mutable(value.snapshot);
    omitted.transactions[0]!.resolvedReferenceInputs = [];
    expect(() => admit(omitted, value.request)).toThrow(
      /resolvedReferenceInputs do not exactly resolve/u,
    );

    const substituted = mutable(value.snapshot);
    substituted.transactions[0]!.resolvedReferenceInputs[0]!.outRef = `${"33".repeat(32)}#1`;
    expect(() => admit(substituted, value.request)).toThrow(
      /resolvedReferenceInputs do not exactly resolve/u,
    );
  });

  it("admits authenticated inclusion without changing the release policy", () => {
    const value = fixture();
    const included = mutable(value.snapshot);
    // Move the observed tip back to the cursor while retaining all exact history.
    included.cursor.tip = included.cursor.point;
    included.provenance.ogmiosTip = included.cursor.point;
    included.cursor.confirmationDepth = 1;
    included.transactions[0]!.confirmationDepth = 2;
    expect(() => admit(included, value.request)).toThrow(/observation depth/u);
    const admitted = admitFraudProofRawL1Snapshot({
      value: included,
      request: value.request,
      releaseFinality,
      observationDepth: "inclusion",
    });
    expect(admitted.cursor.confirmationDepth).toBe(1);
    expect(admitted.finalityPolicyDigest).toBe(releaseFinality.policyDigest);
    expect(releaseFinality.policy.confirmationDepth).toBe(30);
    included.transactions[0]!.bodyCbor = "00";
    expect(() =>
      admitFraudProofRawL1Snapshot({
        value: included,
        request: value.request,
        releaseFinality,
        observationDepth: "inclusion",
      }),
    ).toThrow();
  });

  it("derives confirmation depth from inclusion, cursor, and tip points", () => {
    const value = fixture();
    const forgedTransaction = mutable(value.snapshot);
    forgedTransaction.transactions[0]!.confirmationDepth = 30;
    expect(() => admit(forgedTransaction, value.request)).toThrow(
      /inconsistent inclusion finality/u,
    );

    const forgedCursor = mutable(value.snapshot);
    forgedCursor.cursor.confirmationDepth = 31;
    expect(() => admit(forgedCursor, value.request)).toThrow(
      /confirmation depth disagrees/u,
    );
  });

  it("rejects rollback and provider-checkpoint substitution", () => {
    const value = fixture();
    const forgedCursor = mutable(value.snapshot);
    forgedCursor.cursor.rollbackCursor = "f0".repeat(32);
    expect(() => admit(forgedCursor, value.request)).toThrow(
      /rollback cursor does not bind/u,
    );

    const forgedCheckpoint = mutable(value.snapshot);
    forgedCheckpoint.provenance.kupoCheckpoint = chainPoint({
      slot: "1071",
      blockNo: "71",
      blockHash: "99".repeat(32),
    });
    expect(() => admit(forgedCheckpoint, value.request)).toThrow(
      /provider checkpoints disagree/u,
    );
  });

  it("rejects echoed or incomplete history without matching admitted transactions", () => {
    const value = fixture();
    const absent = mutable(value.snapshot);
    absent.history = [];
    expect(() => admit(absent, value.request)).toThrow(
      /omitted or duplicated unit history/u,
    );

    const unrelatedRequest = {
      ...value.request,
      historyUnits: [OTHER_UNIT],
    };
    const unrelated = mutable(value.snapshot);
    unrelated.historyUnits = [OTHER_UNIT];
    unrelated.history[0]!.unit = OTHER_UNIT;
    expect(() => admit(unrelated, unrelatedRequest)).toThrow(
      /does not touch its unit/u,
    );
  });

  it("rejects unknown scope roles explicitly", () => {
    const value = fixture();
    const request = {
      ...value.request,
      scopes: [
        {
          role: "operator_private_database",
          address: value.request.scopes[0]!.address,
        },
      ],
    } as unknown as FraudProofRawL1SnapshotRequest;
    expect(() => admit(value.snapshot, request)).toThrow(
      /role is unsupported/u,
    );
  });
});

export const localSource = (
  value: ReturnType<typeof fixture>,
  overrides: Partial<LocalKupmiosFraudProofRawSource> = {},
): LocalKupmiosFraudProofRawSource => {
  const snapshot = value.snapshot;
  const source: LocalKupmiosFraudProofRawSource = {
    sourceVersion: LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
    sourceId: SOURCE,
    kupoHttpUrl: "http://127.0.0.1:1442",
    ogmiosWebSocketUrl: "ws://127.0.0.1:1337",
    readBoundary: async () => ({
      kupoCheckpoint: snapshot.cursor.point,
      ogmiosTip: snapshot.cursor.tip,
    }),
    readBlockAtPoint: async ({ point }) => ({
      point,
      transactions: [],
    }),
    scanAddressPage: async ({ after }) =>
      after === null
        ? {
            checkpoint: snapshot.cursor.point,
            utxos: [],
            nextCursor: "address-page-2",
            complete: false,
          }
        : {
            checkpoint: snapshot.cursor.point,
            utxos: snapshot.scopes[0]!.utxos,
            nextCursor: null,
            complete: true,
          },
    scanUnitHistoryPage: async ({ after }) =>
      after === null
        ? {
            checkpoint: snapshot.cursor.point,
            transactions: [],
            nextCursor: "history-page-2",
            complete: false,
          }
        : {
            checkpoint: snapshot.cursor.point,
            transactions: snapshot.transactions.map((transaction) => ({
              txHash: transaction.txHash,
              inclusionPoint: transaction.inclusionPoint,
            })),
            nextCursor: null,
            complete: true,
          },
    readTransaction: async ({ txHash }) => {
      const transaction = snapshot.transactions.find(
        (candidate) => candidate.txHash === txHash,
      )!;
      return {
        kupo: { txHash, inclusionPoint: transaction.inclusionPoint },
        ogmios: transaction,
      };
    },
    confirmCanonicalPoint: async ({ point }) => ({
      canonical: true,
      point,
    }),
  };
  return { ...source, ...overrides };
};
