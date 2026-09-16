import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitFraudProofRawL1Snapshot,
  admitFraudProofRawL1Transaction,
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  computeFraudProofReleaseFinalityPolicyDigest,
  createLocalKupmiosFraudProofRawL1SnapshotAuthority,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1Point,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotRequest,
  LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../src/workflow/index.js";

const DEPLOYMENT = "aa".repeat(32);
const RELEASE = "bb".repeat(32);
const HEADER = "cc".repeat(28);
const UNIT = `${"dd".repeat(28)}00`;
const OTHER_UNIT = `${"ee".repeat(28)}01`;
const SOURCE = "local-kupmios-release-v1";

const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;

const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

const chainPoint = ({
  slot,
  blockNo,
  blockHash,
}: {
  readonly slot: string;
  readonly blockNo: string;
  readonly blockHash: string;
}): FraudProofRawL1Point => ({
  slot,
  blockNo,
  blockHash,
  pointId: computeFraudProofRawL1PointId({ slot, blockNo, blockHash }),
});

const rawOutput = (
  address: string,
  assets?: Readonly<{ unit: string; quantity: bigint }>,
): string => {
  const multiasset = assets === undefined ? undefined : CML.MultiAsset.new();
  if (assets !== undefined) {
    multiasset!.set(
      CML.ScriptHash.from_hex(assets.unit.slice(0, 56)),
      CML.AssetName.from_hex(assets.unit.slice(56)),
      assets.quantity,
    );
  }
  return CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    multiasset === undefined
      ? CML.Value.from_coin(3_000_000n)
      : CML.Value.new(3_000_000n, multiasset),
  ).to_canonical_cbor_hex();
};

const fixture = (): {
  readonly request: FraudProofRawL1SnapshotRequest;
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly alternateAddressOutput: string;
} => {
  const address = credentialToAddress(
    "Preview",
    scriptHashToCredential("11".repeat(28)),
  );
  const alternateAddress = credentialToAddress(
    "Preview",
    scriptHashToCredential("22".repeat(28)),
  );
  const spentOutputCbor = rawOutput(address);
  const referenceOutputCbor = rawOutput(address);
  const createdOutputCbor = rawOutput(address, { unit: UNIT, quantity: 1n });
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("31".repeat(32)), 0n),
  );
  const referenceInputs = CML.TransactionInputList.new();
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("32".repeat(32)), 1n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(CML.TransactionOutput.from_cbor_hex(createdOutputCbor));
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  body.set_reference_inputs(referenceInputs);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(UNIT.slice(0, 56)),
    CML.AssetName.from_hex(UNIT.slice(56)),
    1n,
  );
  body.set_mint(mint);
  const witnesses = CML.TransactionWitnessSet.new();
  const txHash = CML.hash_transaction(body).to_hex();
  const included = chainPoint({
    slot: "1070",
    blockNo: "70",
    blockHash: "41".repeat(32),
  });
  const cursorPoint = chainPoint({
    slot: "1071",
    blockNo: "71",
    blockHash: "42".repeat(32),
  });
  const tip = chainPoint({
    slot: "1100",
    blockNo: "100",
    blockHash: "43".repeat(32),
  });
  const request = {
    deploymentIdentityDigest: DEPLOYMENT,
    blueprintHash: RELEASE,
    finalityPolicyDigest: releaseFinality.policyDigest,
    headerHash: HEADER,
    scopes: [{ role: "state_queue", address }],
    historyUnits: [UNIT],
  } as const satisfies FraudProofRawL1SnapshotRequest;
  const snapshot = {
    schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT,
    blueprintHash: RELEASE,
    finalityPolicyDigest: releaseFinality.policyDigest,
    headerHash: HEADER,
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: SOURCE,
      grade: "security",
      sourceMode: "local_kupo_ogmios",
      kupoCheckpoint: cursorPoint,
      ogmiosTip: tip,
    },
    cursor: {
      point: cursorPoint,
      tip,
      confirmationDepth: 30,
      rollbackCursor: computeFraudProofRawL1RollbackCursor({
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: RELEASE,
        finalityPolicyDigest: releaseFinality.policyDigest,
        sourceId: SOURCE,
        pointId: cursorPoint.pointId,
      }),
    },
    scopes: [
      {
        role: "state_queue",
        address,
        utxos: [
          {
            outRef: `${txHash}#0`,
            outputCbor: createdOutputCbor,
            datumCbor: null,
            referenceScriptCbor: null,
          },
        ],
      },
    ],
    historyUnits: [UNIT],
    history: [
      {
        unit: UNIT,
        fromGenesis: true,
        completeThroughPointId: cursorPoint.pointId,
        transactionHashes: [txHash],
      },
    ],
    transactions: [
      {
        txHash,
        bodyCbor: body.to_canonical_cbor_hex(),
        witnessSetCbor: witnesses.to_canonical_cbor_hex(),
        redeemersCbor: null,
        isValid: true,
        inclusionPoint: included,
        confirmationDepth: 31,
        resolvedInputs: [
          {
            outRef: `${"31".repeat(32)}#0`,
            outputCbor: spentOutputCbor,
            datumCbor: null,
            referenceScriptCbor: null,
          },
        ],
        resolvedReferenceInputs: [
          {
            outRef: `${"32".repeat(32)}#1`,
            outputCbor: referenceOutputCbor,
            datumCbor: null,
            referenceScriptCbor: null,
          },
        ],
      },
    ],
  } as const satisfies FraudProofRawL1Snapshot;
  return {
    request,
    snapshot,
    alternateAddressOutput: rawOutput(alternateAddress, {
      unit: UNIT,
      quantity: 1n,
    }),
  };
};

const admit = (
  snapshot: unknown,
  request: FraudProofRawL1SnapshotRequest,
): FraudProofRawL1Snapshot =>
  admitFraudProofRawL1Snapshot({
    value: snapshot,
    request,
    releaseFinality,
  });

type Mutable<T> = T extends readonly (infer Item)[]
  ? Mutable<Item>[]
  : T extends object
    ? { -readonly [Key in keyof T]: Mutable<T[Key]> }
    : T;

const mutable = <T>(value: T): Mutable<T> =>
  structuredClone(value) as Mutable<T>;

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

const localSource = (
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
