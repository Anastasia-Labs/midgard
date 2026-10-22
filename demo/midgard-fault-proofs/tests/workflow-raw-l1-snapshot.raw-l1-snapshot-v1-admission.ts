import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitFraudProofRawL1Snapshot,
  admitFraudProofRawL1Transaction,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotRequest,
} from "../src/workflow/index.js";
import { createFraudProofAuthenticatedPublicationObserver } from "../src/workflow/raw-l1-publication-observation.js";
import {
  admit,
  chainPoint,
  fixture,
  mutable,
  OTHER_UNIT,
  releaseFinality,
  UNIT,
} from "./workflow-raw-l1-snapshot.fixture.js";

describe("raw L1 snapshot V1 admission", () => {
  it("admits canonical address-scoped bytes and complete unit history", () => {
    const value = fixture();
    expect(admit(value.snapshot, value.request)).toEqual(value.snapshot);
  });

  it("requires exact admitted certificate references before authenticating a spent publication", async () => {
    const value = fixture();
    const snapshot = mutable(value.snapshot);
    const row = snapshot.transactions[0]!;
    const originalBody = CML.TransactionBody.from_cbor_hex(row.bodyCbor);
    const originalOutput = originalBody.outputs().get(0);
    const datumCbor = "00";
    const output = CML.TransactionOutput.new(
      originalOutput.address(),
      originalOutput.amount(),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCbor)),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(output);
    const body = CML.TransactionBody.new(
      originalBody.inputs(),
      outputs,
      originalBody.fee(),
    );
    body.set_reference_inputs(originalBody.reference_inputs()!);
    body.set_mint(originalBody.mint()!);
    const txHash = CML.hash_transaction(body).to_hex();
    row.txHash = txHash;
    row.bodyCbor = body.to_canonical_cbor_hex();
    snapshot.history[0]!.transactionHashes = [txHash];
    const scope = snapshot.scopes[0]!;
    scope.role = "field_certificate";
    scope.utxos[0] = {
      ...scope.utxos[0]!,
      outRef: `${txHash}#0`,
      outputCbor: output.to_canonical_cbor_hex(),
      datumCbor,
    };
    const observer = createFraudProofAuthenticatedPublicationObserver({
      authority: {
        authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
        // The source answers each scope as requested.
        capture: async (asked) => ({
          ...snapshot,
          scopes: asked.scopes.map((entry) => ({
            ...entry,
            utxos: scope.utxos,
          })),
        }),
      },
      releaseFinality,
    });
    const request = {
      headerHash: value.request.headerHash,
      kind: "field_certificate" as const,
      address: scope.address,
      expectedOutRef: `${txHash}#0`,
      expectedDatumCbor: datumCbor,
      expectedUnit: UNIT,
      expectedReferenceOutRef: `${"32".repeat(32)}#1`,
    };
    await expect(observer.observeExact(request)).resolves.toEqual({
      kind: "confirmed",
      outRef: request.expectedOutRef,
    });
    await expect(
      observer.observeExact({
        ...request,
        expectedReferenceOutRef: `${"33".repeat(32)}#1`,
      }),
    ).resolves.toEqual({ kind: "not_found" });
    row.resolvedReferenceInputs = [];
    await expect(observer.observeExact(request)).rejects.toThrow(
      /resolvedReferenceInputs do not exactly resolve/u,
    );
  });

  it("observes a publication by its out-ref alone, at an address holding more outputs than one snapshot carries", async () => {
    const value = fixture();
    const address = value.request.scopes[0]!.address;
    const datumCbor = "d8799f4100ff";
    const outputCbor = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCbor)),
    ).to_canonical_cbor_hex();
    const publication = (outRef: string) => ({
      outRef,
      outputCbor,
      datumCbor:
        CML.PlutusData.from_cbor_hex(datumCbor).to_canonical_cbor_hex(),
      referenceScriptCbor: null,
    });
    const ours = `${"5a".repeat(32)}#3`;
    // Other parties' outputs at the address, past the snapshot's bound.
    const other = publication(`${"00".repeat(32)}#0`);
    const others = Array.from({ length: 100_001 }, (_, index) => ({
      ...other,
      outRef: `${index.toString(16).padStart(64, "0")}#0`,
    }));
    const held = [...others, publication(ours)];
    const asked: FraudProofRawL1SnapshotRequest["scopes"][] = [];
    const observer = createFraudProofAuthenticatedPublicationObserver({
      authority: {
        authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
        capture: async (request) => {
          asked.push(request.scopes);
          return {
            ...value.snapshot,
            scopes: request.scopes.map((scope) => ({
              ...scope,
              utxos: held.filter(
                ({ outRef }) =>
                  scope.outRefs === undefined || scope.outRefs.includes(outRef),
              ),
            })),
            historyUnits: [],
            history: [],
            transactions: [],
          };
        },
      },
      releaseFinality,
    });
    const observe = (expectedOutRef: string) =>
      observer.observeExact({
        headerHash: value.request.headerHash,
        kind: "proof_chunk",
        address,
        expectedOutRef,
        expectedDatumCbor: datumCbor,
      });

    await expect(observe(ours)).resolves.toEqual({
      kind: "confirmed",
      outRef: ours,
    });
    await expect(observe(`${"5a".repeat(32)}#4`)).resolves.toEqual({
      kind: "not_found",
    });
    expect(asked).toEqual([
      [{ role: "proof_chunk", address, outRefs: [ours] }],
      [{ role: "proof_chunk", address, outRefs: [`${"5a".repeat(32)}#4`] }],
    ]);
  });

  it("refuses an out-ref scope answered with outputs it was not scoped to, or without its out-refs", () => {
    const value = fixture();
    const scope = value.snapshot.scopes[0]!;
    const outRef = scope.utxos[0]!.outRef;
    const request = {
      ...value.request,
      scopes: [{ ...value.request.scopes[0]!, outRefs: [outRef] }],
    };
    const scoped = {
      ...value.snapshot,
      scopes: [{ ...scope, outRefs: [outRef] }],
    };
    expect(admit(scoped, request)).toEqual(scoped);

    const narrower = {
      ...request,
      scopes: [
        { ...value.request.scopes[0]!, outRefs: [`${"77".repeat(32)}#0`] },
      ],
    };
    expect(() =>
      admit(
        {
          ...value.snapshot,
          scopes: [{ ...scope, outRefs: narrower.scopes[0]!.outRefs }],
        },
        narrower,
      ),
    ).toThrow(/output it was not scoped to/u);
    expect(() => admit(value.snapshot, request)).toThrow(
      /missing or unknown fields/u,
    );
    expect(() =>
      admit(
        {
          ...value.snapshot,
          scopes: [{ ...scope, outRefs: [`${"77".repeat(32)}#0`] }],
        },
        request,
      ),
    ).toThrow(/not the requested out-refs/u);
    for (const outRefs of [[], [outRef, outRef], ["not-an-out-ref"]])
      expect(() =>
        admit(scoped, {
          ...value.request,
          scopes: [{ ...value.request.scopes[0]!, outRefs }],
        }),
      ).toThrow(/outRefs/u);
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
    included.provenance.tipPoint = included.cursor.point;
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
    forgedCheckpoint.provenance.boundaryPoint = chainPoint({
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
