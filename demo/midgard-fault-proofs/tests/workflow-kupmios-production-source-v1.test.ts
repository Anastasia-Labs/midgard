import { readFile } from "node:fs/promises";
import { resolve } from "node:path";

import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitKupoMatchAgainstTransactionOutput,
  computeFraudProofRawL1PointId,
  computeFraudProofReleaseEconomicsPolicyDigest,
  computeFraudProofReleaseFinalityPolicyDigest,
  createLocalKupmiosHttpOgmiosRawSource,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1WebSocketLike,
  LocalKupmiosExactPointNotCanonicalError,
  OGMIOS_RAW_TRANSACTION_CBOR_FLAG,
  readAdmittedLocalKupmiosAddressUtxosAtPoint,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
  requireOgmiosRawTransactionCbor,
  validateVerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../src/workflow/index.js";

const hash = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const DEPLOYMENT = hash(1);
const RELEASE = hash(2);
const KUP0_HEAD = hash(3);
const TARGET = hash(4);
const ANCESTOR = hash(5);
const TIP = hash(6);

const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  releaseIdentityDigest: RELEASE,
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest({
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  }),
  policy: {
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  },
};

class OgmiosBoundarySocket implements FraudProofRawL1WebSocketLike {
  readonly listeners = new Map<string, ((event: never) => void)[]>();
  nextCount = 0;

  constructor(private readonly transactions: readonly unknown[] = []) {
    queueMicrotask(() => this.emit("open", {}));
  }

  addEventListener(type: string, listener: (event: never) => void): void {
    const listeners = this.listeners.get(type) ?? [];
    listeners.push(listener);
    this.listeners.set(type, listeners);
  }

  send(data: string): void {
    const request = JSON.parse(data) as {
      readonly id: number;
      readonly method: string;
    };
    const result =
      request.method === "findIntersection"
        ? {
            intersection: { slot: 380, id: ANCESTOR },
            tip: { slot: 1000, id: TIP },
          }
        : this.nextCount++ === 0
          ? { direction: "backward", point: { slot: 380, id: ANCESTOR } }
          : {
              direction: "forward",
              block: {
                slot: 400,
                id: TARGET,
                height: 71,
                ancestor: ANCESTOR,
                transactions: this.transactions,
              },
            };
    queueMicrotask(() =>
      this.emit("message", {
        data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
      }),
    );
  }

  close(): void {}

  private emit(type: string, event: unknown): void {
    for (const listener of this.listeners.get(type) ?? []) {
      listener(event as never);
    }
  }
}

const response = (
  value: unknown,
  checkpointHeaders = false,
  oversized = false,
): Response =>
  new Response(JSON.stringify(value), {
    status: 200,
    headers: checkpointHeaders
      ? {
          "content-type": "application/json",
          "x-most-recent-checkpoint": "990",
          etag: `"${KUP0_HEAD}"`,
          ...(oversized ? { "content-length": "67108865" } : {}),
        }
      : { "content-type": "application/json" },
  });

const sourceFixture = ({
  numericCoin = false,
  oversizedKupo = false,
  blockTransactions = [],
  kupoMatches = [],
}: {
  readonly numericCoin?: boolean;
  readonly oversizedKupo?: boolean;
  readonly blockTransactions?: readonly unknown[];
  readonly kupoMatches?: readonly unknown[];
} = {}) => {
  const requests: {
    readonly url: string;
    readonly init: RequestInit | undefined;
  }[] = [];
  const fetchImpl = async (
    url: string,
    init?: RequestInit,
  ): Promise<Response> => {
    requests.push({ url, init });
    if (url === "http://127.0.0.1:1337") {
      return response({
        jsonrpc: "2.0",
        id: "midgard-fraud-proof-raw-tip-v1",
        result: { slot: 1000, id: TIP, height: 100 },
      });
    }
    if (url.endsWith("/checkpoints/400")) {
      return response(
        { slot_no: 400, header_hash: TARGET },
        true,
        oversizedKupo,
      );
    }
    if (url.endsWith("/checkpoints/399")) {
      return response({ slot_no: 380, header_hash: ANCESTOR }, true);
    }
    if (url.includes("/matches/")) {
      return response(
        numericCoin
          ? [
              {
                transaction_index: 0,
                transaction_id: hash(7),
                output_index: 0,
                address:
                  "addr_test1wzj2e2d2x6ns5w50z3h2zlaurqu4h9tpuv7zpkg6dj6xefcp4x24g",
                value: { coins: 3_000_000, assets: {} },
                datum_hash: null,
                script_hash: null,
                created_at: { slot_no: 390, header_hash: hash(8) },
                spent_at: null,
                datum: null,
                script: null,
              },
            ]
          : kupoMatches,
        true,
      );
    }
    throw new Error(`unexpected request ${url}`);
  };
  const source = createLocalKupmiosHttpOgmiosRawSource({
    sourceId: "local-release-test",
    kupoHttpUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "http://127.0.0.1:1337",
    releaseFinality,
    fetchImpl,
    webSocketFactory: () => new OgmiosBoundarySocket(blockTransactions),
  });
  return { source, requests };
};

describe("production local Kupmios raw source V1", () => {
  it("pins real Kupo checkpoint headers and requests string asset quantities", async () => {
    const fixture = sourceFixture();
    const boundary = await readAdmittedLocalKupmiosBoundary({
      source: fixture.source,
    });
    expect(boundary.kupoCheckpoint).toEqual({
      slot: "400",
      blockHash: TARGET,
      blockNo: "71",
      pointId: computeFraudProofRawL1PointId({
        slot: "400",
        blockHash: TARGET,
        blockNo: "71",
      }),
    });
    expect(boundary.confirmationDepth).toBe(30);
    await expect(
      readAdmittedLocalKupmiosBoundary({ source: { ...fixture.source } }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
    await expect(
      fixture.source.scanAddressPage({
        address:
          "addr_test1wzj2e2d2x6ns5w50z3h2zlaurqu4h9tpuv7zpkg6dj6xefcp4x24g",
        throughPoint: boundary.kupoCheckpoint,
        after: null,
      }),
    ).resolves.toMatchObject({ complete: true, nextCursor: null, utxos: [] });
    const matchRequest = fixture.requests.find(({ url }) =>
      url.includes("/matches/"),
    );
    expect(matchRequest?.url).toContain("resolve_hashes&order=oldest_first");
    expect(new Headers(matchRequest?.init?.headers).get("accept")).toBe(
      "application/json;asset-quantity=string",
    );
  });

  it("rejects Kupo's numeric default rather than losing asset precision", async () => {
    const fixture = sourceFixture({ numericCoin: true });
    const boundary = (await fixture.source.readBoundary()) as {
      readonly kupoCheckpoint: {
        readonly slot: string;
        readonly blockHash: string;
        readonly blockNo: string;
        readonly pointId: string;
      };
    };
    await expect(
      fixture.source.scanAddressPage({
        address:
          "addr_test1wzj2e2d2x6ns5w50z3h2zlaurqu4h9tpuv7zpkg6dj6xefcp4x24g",
        throughPoint: boundary.kupoCheckpoint,
        after: null,
      }),
    ).rejects.toThrow(/coins must be canonical lovelace/u);
  });

  it("binds Kupo value and reference-script identity to raw output CBOR", () => {
    const script = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(Uint8Array.from([1, 2, 3])),
    );
    const address = credentialToAddress(
      "Preview",
      scriptHashToCredential("31".repeat(28)),
    );
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(3_000_000n),
      undefined,
      script,
    );
    const outputScript = output.script_ref()!;
    const match = {
      transaction_index: 0,
      transaction_id: hash(7),
      output_index: 0,
      address,
      value: { coins: "3000000", assets: {} },
      datum_hash: null,
      script_hash: outputScript.hash().to_hex(),
      created_at: { slot_no: 390, header_hash: hash(8) },
      spent_at: null,
      datum: null,
      script: outputScript.to_canonical_cbor_hex(),
    };
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match,
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).not.toThrow();
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match: { ...match, value: { coins: "2999999", assets: {} } },
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).toThrow(/value disagrees/u);
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match: {
          ...match,
          script: CML.Script.new_plutus_v3(
            CML.PlutusV3Script.from_raw_bytes(Uint8Array.from([9])),
          ).to_canonical_cbor_hex(),
        },
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).toThrow(/reference script disagrees/u);
  });

  it("fails closed when Ogmios omits raw transaction CBOR", () => {
    expect(() =>
      requireOgmiosRawTransactionCbor({
        value: { id: hash(9) },
        expectedTxHash: hash(9),
        label: "transaction",
      }),
    ).toThrow(new RegExp(OGMIOS_RAW_TRANSACTION_CBOR_FLAG, "u"));
  });

  it("rejects an oversized provider response before buffering it", async () => {
    const fixture = sourceFixture({ oversizedKupo: true });
    await expect(fixture.source.readBoundary()).rejects.toThrow(
      /exceeds the raw-source byte bound/u,
    );
  });

  it("accepts only transaction CBOR whose body hashes to the reported id", () => {
    const body = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      CML.TransactionOutputList.new(),
      0n,
    );
    const transaction = CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    );
    const txHash = CML.hash_transaction(body).to_hex();
    expect(
      requireOgmiosRawTransactionCbor({
        value: { id: txHash, cbor: transaction.to_canonical_cbor_hex() },
        expectedTxHash: txHash,
        label: "transaction",
      }),
    ).toBe(transaction.to_canonical_cbor_hex());
  });

  it("re-admits an exact ordered raw block only from the opaque concrete source", async () => {
    const transaction = (fee: bigint) => {
      const body = CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        CML.TransactionOutputList.new(),
        fee,
      );
      const value = CML.Transaction.new(
        body,
        CML.TransactionWitnessSet.new(),
        true,
      );
      return {
        id: CML.hash_transaction(body).to_hex(),
        cbor: value.to_canonical_cbor_hex(),
      };
    };
    const first = transaction(1n);
    const second = transaction(2n);
    const fixture = sourceFixture({
      blockTransactions: [first, second],
    });
    const boundary = (await fixture.source.readBoundary()) as {
      readonly kupoCheckpoint: {
        readonly slot: string;
        readonly blockHash: string;
        readonly blockNo: string;
        readonly pointId: string;
      };
    };
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: fixture.source,
        point: boundary.kupoCheckpoint,
      }),
    ).resolves.toMatchObject({
      sourceId: fixture.source.sourceId,
      point: boundary.kupoCheckpoint,
      kupoCheckpoint: { slot: 400, blockHash: TARGET },
      transactions: [
        { txHash: first.id, transactionCbor: first.cbor },
        { txHash: second.id, transactionCbor: second.cbor },
      ],
    });
    const rolledBackBlockHash = hash(0xec);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: fixture.source,
        point: {
          ...boundary.kupoCheckpoint,
          blockHash: rolledBackBlockHash,
          pointId: computeFraudProofRawL1PointId({
            ...boundary.kupoCheckpoint,
            blockHash: rolledBackBlockHash,
          }),
        },
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: { ...fixture.source },
        point: boundary.kupoCheckpoint,
      }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
  });

  it("re-admits exact resolved transaction bytes only from the concrete source", async () => {
    const address = credentialToAddress(
      "Preview",
      scriptHashToCredential("31".repeat(28)),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(3_000_000n),
      ),
    );
    const body = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      outputs,
      200_000n,
    );
    const transaction = CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    );
    const txHash = CML.hash_transaction(body).to_hex();
    const transactionCbor = transaction.to_canonical_cbor_hex();
    const kupoMatch = {
      transaction_index: 0,
      transaction_id: txHash,
      output_index: 0,
      address,
      value: { coins: "3000000", assets: {} },
      datum_hash: null,
      script_hash: null,
      created_at: { slot_no: 400, header_hash: TARGET },
      spent_at: null,
      datum: null,
      script: null,
    };
    const fixture = sourceFixture({
      blockTransactions: [{ id: txHash, cbor: transactionCbor }],
      kupoMatches: [kupoMatch],
    });
    const boundary = (await fixture.source.readBoundary()) as {
      readonly kupoCheckpoint: {
        readonly slot: string;
        readonly blockHash: string;
        readonly blockNo: string;
        readonly pointId: string;
      };
    };
    await expect(
      readAdmittedLocalKupmiosRawTransaction({
        source: fixture.source,
        txHash,
        expectedInclusionPoint: boundary.kupoCheckpoint,
        minimumConfirmationDepth: 30,
      }),
    ).resolves.toMatchObject({
      txHash,
      inclusionPoint: boundary.kupoCheckpoint,
      confirmationDepth: 30,
      resolvedInputs: [],
      resolvedReferenceInputs: [],
    });
    await expect(
      readAdmittedLocalKupmiosAddressUtxosAtPoint({
        source: fixture.source,
        address,
        point: boundary.kupoCheckpoint,
      }),
    ).resolves.toEqual([
      {
        outRef: `${txHash}#0`,
        outputCbor: outputs.get(0).to_canonical_cbor_hex(),
        datumCbor: null,
        referenceScriptCbor: null,
      },
    ]);
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit: `${"12".repeat(28)}aa`,
        point: boundary.kupoCheckpoint,
      }),
    ).resolves.toEqual({
      checkpoint: boundary.kupoCheckpoint,
      transactions: [{ txHash, inclusionPoint: boundary.kupoCheckpoint }],
    });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: { ...fixture.source },
        unit: `${"12".repeat(28)}aa`,
        point: boundary.kupoCheckpoint,
      }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
    await expect(
      readAdmittedLocalKupmiosRawTransaction({
        source: { ...fixture.source },
        txHash,
        expectedInclusionPoint: boundary.kupoCheckpoint,
        minimumConfirmationDepth: 30,
      }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
    await expect(
      readAdmittedLocalKupmiosRawTransaction({
        source: fixture.source,
        txHash,
        expectedInclusionPoint: boundary.kupoCheckpoint,
        minimumConfirmationDepth: 31,
      }),
    ).rejects.toThrow(/below release finality/u);
  });

  it("keeps raw transaction CBOR enabled in every checked-in Ogmios launch path", async () => {
    const repository = resolve(process.cwd(), "../..");
    const paths = [
      "l1-services/docker-compose.yml",
      "demo/midgard-node/scripts/run-ogmios.sh",
      "demo/midgard-node/devnet/phase4-process/compose.yaml",
    ];
    for (const path of paths) {
      await expect(
        readFile(resolve(repository, path), "utf8"),
      ).resolves.toContain(OGMIOS_RAW_TRANSACTION_CBOR_FLAG);
    }
  });
});

describe("release-bound fraud-proof economics V1", () => {
  const testnetPolicy = {
    profile: "bounded-acceptance-v1",
    requiredBondLovelace: "900000000",
    slashingPenaltyLovelace: "500000000",
    fraudProverRewardLovelace: "400000000",
    inactivitySlashingPenaltyLovelace: "100000000",
    proverCollateralFloorLovelace: "5000000",
  } as const;

  it("admits the manifest-bound testnet profile", () => {
    expect(
      validateVerifiedFraudProofReleaseEconomicsPolicy({
        schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: DEPLOYMENT,
        releaseIdentityDigest: RELEASE,
        policyDigest:
          computeFraudProofReleaseEconomicsPolicyDigest(testnetPolicy),
        policy: testnetPolicy,
      }),
    ).toMatchObject({ policy: testnetPolicy });
  });

  it("rejects a caller-selected reward or substituted policy digest", () => {
    const policy = { ...testnetPolicy, fraudProverRewardLovelace: "1" };
    expect(() =>
      validateVerifiedFraudProofReleaseEconomicsPolicy({
        schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: DEPLOYMENT,
        releaseIdentityDigest: RELEASE,
        policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(policy),
        policy,
      }),
    ).toThrow(/must equal|canonical launch profile/u);
  });

  it("rejects legacy or extended economics policy shapes", () => {
    const { proverCollateralFloorLovelace: _omitted, ...legacyPolicy } =
      testnetPolicy;
    for (const policy of [
      legacyPolicy,
      { ...testnetPolicy, extra: "forged" },
    ]) {
      expect(() =>
        validateVerifiedFraudProofReleaseEconomicsPolicy({
          schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
          deploymentIdentityDigest: DEPLOYMENT,
          releaseIdentityDigest: RELEASE,
          policyDigest: "00".repeat(32),
          policy,
        } as never),
      ).toThrow(/must contain exactly/u);
    }
  });
});
