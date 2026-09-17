import { readFile } from "node:fs/promises";

import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherLocalUserEventReferenceAuthority,
  createWatcherLocalUserEventReferenceAuthorityFromBodies,
  readWatcherUserEventReferenceEvidence,
} from "../../src/indexers/user-event-reference-authority.js";
import {
  makeWatcherL1PublicBytes,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import {
  assertWatcherLocalKupmiosNativeObservation,
  type WatcherLocalKupmiosNativeObservation,
} from "../../src/l1/local-kupmios-native-observation.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import { WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION } from "../../src/l1/native-chain-sync.js";
import {
  readWatcherResolvedBlockObservation,
  resolveWatcherBlockObservationTransactions,
  WATCHER_RESOLVED_BLOCK_OBSERVATION_SCHEMA_VERSION,
  type WatcherResolvedBlockObservation,
  type WatcherResolvedBlockObservationEvidence,
} from "../../src/l1/resolved-block-observation.js";
import {
  makeDeploymentAuthority,
  sha256,
} from "../support/deployment-authority-fixture.js";

// Unit coverage of the local consumer boundary, with existing Conway bytes.
// These upstream admission doubles do not establish end-to-end L1 acquisition;
// the W15 integration suite separately uses genuine external TLS capabilities.
const upstream = vi.hoisted(() => ({
  target: null as object | null,
  localObservation: null as object | null,
  nativeBlock: null as object | null,
  live: true,
}));
vi.mock(
  "../../src/l1/local-kupmios-native-observation.js",
  async (importOriginal) => ({
    ...(await importOriginal<
      typeof import("../../src/l1/local-kupmios-native-observation.js")
    >()),
    assertWatcherLocalKupmiosNativeObservation: vi.fn(
      (observation: object, nativeBlock: object) => {
        if (
          observation !== upstream.localObservation ||
          nativeBlock !== upstream.nativeBlock ||
          !upstream.live
        ) {
          throw new Error("local/native observation is not admitted");
        }
      },
    ),
  }),
);
vi.mock("../../src/l1/l1-adapter.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../../src/l1/l1-adapter.js")>()),
  isWatcherL1AdapterNormalizedBlock: (target: object) =>
    target === upstream.target && upstream.live,
}));
vi.mock(
  "../../src/l1/resolved-block-observation.js",
  async (importOriginal) => ({
    ...(await importOriginal<
      typeof import("../../src/l1/resolved-block-observation.js")
    >()),
    readWatcherResolvedBlockObservation: vi.fn(),
    resolveWatcherBlockObservationTransactions: vi.fn(),
  }),
);

const deploymentIdentity = makeDeploymentAuthority().result;
const fixture = async () => {
  const metadata = {
    blockHash:
      "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
    blockNo: "12069665",
    blockType: "7",
    prevHash:
      "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
    slot: "159835207",
  };
  const native = admitWatcherNativeRollForwardBlock({
    ...metadata,
    kind: "roll_forward",
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    rawBlockCbor: (
      await readFile(
        new URL("../support/conway-block.hex", import.meta.url),
        "utf8",
      )
    ).trim(),
    tip: {
      kind: "point",
      blockHash: metadata.blockHash,
      blockNo: metadata.blockNo,
      slot: metadata.slot,
    },
  });
  const point = {
    blockHash: native.blockHash,
    blockNo: native.blockNo,
    slot: native.slot,
    pointId: computeFraudProofRawL1PointId(native),
  };
  const decoded = native.transactionCbors.map((cbor) =>
    CML.Transaction.from_cbor_hex(cbor),
  );
  const targetBlock = Object.freeze<WatcherNormalizedL1Block>({
    schemaVersion: "midgard-watcher-normalized-l1-block-v1",
    network: "Preprod",
    provider: {
      schemaVersion: "midgard-watcher-authenticated-l1-provider-v1",
      network: "Preprod",
      providerId: "ordinary-local-source",
      source: {
        sourceMode: "local_node",
        authorityNodeId: "ordinary-node",
        surface: "chain_sync",
      },
      authentication: {
        kind: "cardano_node_genesis_v1",
        publicIdentitySha256: "01".repeat(32),
      },
    },
    chainPoint: {
      ...point,
      chainPointId: point.pointId,
      parentBlockHash: native.prevHash,
      pointDigest: point.pointId,
      depth: "30",
    },
    observationDigest: sha256(
      Buffer.from("ordinary-local-reference-observation"),
    ),
    blockContentDigest: sha256(Buffer.from(native.rawBlockCbor, "hex")),
    transactions: decoded.map((transaction, index) => ({
      txHash: native.transactionIds[index]!,
      transactionIndex: index.toString(),
      isValid: transaction.is_valid(),
      fullTransaction: makeWatcherL1PublicBytes(
        native.transactionCbors[index]!,
      ),
      body: makeWatcherL1PublicBytes(transaction.body().to_cbor_hex()),
      witnessSet: makeWatcherL1PublicBytes(
        transaction.witness_set().to_cbor_hex(),
      ),
      utxos: [],
      scripts: [],
      datums: [],
      redeemers: [],
    })),
  });
  upstream.target = targetBlock;
  const resolvedBlock: WatcherResolvedBlockObservation = Object.freeze({
    schemaVersion: WATCHER_RESOLVED_BLOCK_OBSERVATION_SCHEMA_VERSION,
  });
  let evidence: WatcherResolvedBlockObservationEvidence = {
    deploymentIdentityDigest: deploymentIdentity.manifestId,
    sourceId: "ordinary-local-reference-source",
    finalityDepth: "30",
    minimumConfirmationDepth: "30",
    rawBlock: {
      schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1",
      sourceId: "ordinary-local-reference-source",
      point,
      parentBlockHash: native.prevHash,
      kupoCheckpoint: {
        slot: Number(native.slot),
        blockHash: native.blockHash,
      },
      transactions: native.transactionIds.map((txHash, index) => ({
        txHash,
        transactionCbor: native.transactionCbors[index]!,
      })),
    },
  };
  const rawTransactions: FraudProofRawL1Transaction[] = decoded.flatMap(
    (transaction, index) => {
      if (!transaction.is_valid()) return [];
      const references = transaction.body().reference_inputs();
      return [
        {
          txHash: native.transactionIds[index]!,
          bodyCbor: transaction.body().to_cbor_hex(),
          witnessSetCbor: transaction.witness_set().to_cbor_hex(),
          redeemersCbor:
            transaction.witness_set().redeemers()?.to_canonical_cbor_hex() ??
            null,
          isValid: true,
          inclusionPoint: point,
          confirmationDepth: 30,
          resolvedInputs: [],
          resolvedReferenceInputs: Array.from(
            { length: references?.len() ?? 0 },
            (_, referenceIndex) => {
              const input = references!.get(referenceIndex);
              return {
                outRef: `${input.transaction_id().to_hex()}#${input.index().toString()}`,
                // This layer consumes already-resolved bytes; actual resolution is
                // tested at the shared raw-source admission boundary.
                outputCbor: decoded[0]!
                  .body()
                  .outputs()
                  .get(0)
                  .to_canonical_cbor_hex(),
                datumCbor: null,
                referenceScriptCbor: null,
              };
            },
          ),
        },
      ];
    },
  );
  vi.mocked(readWatcherResolvedBlockObservation).mockImplementation(
    (observation) => {
      if (observation !== resolvedBlock || !upstream.live)
        throw new Error("upstream observation is not live");
      return evidence;
    },
  );
  vi.mocked(resolveWatcherBlockObservationTransactions).mockResolvedValue(
    rawTransactions,
  );
  return {
    input: { targetBlock, deploymentIdentity, resolvedBlock },
    nativeBlock: native,
    rawTransactions,
    advanceDepth: () => {
      evidence = { ...evidence, finalityDepth: "60" };
    },
  };
};

beforeEach(() => {
  upstream.live = true;
  upstream.target = null;
  upstream.localObservation = null;
  upstream.nativeBlock = null;
  vi.mocked(assertWatcherLocalKupmiosNativeObservation).mockClear();
  vi.mocked(readWatcherResolvedBlockObservation).mockReset();
  vi.mocked(resolveWatcherBlockObservationTransactions).mockReset();
});

describe("local user-event reference consumer", () => {
  it("keeps pending local preimages separate from release-final raw resolution", async () => {
    const current = await fixture();
    // This upstream unit double contains the first existing ordinary Conway
    // transaction, which has no references. Native/full-block agreement is
    // tested by the local-observation owner, not established by this double.
    const pendingBlock = Object.freeze({
      ...current.input.targetBlock,
      chainPoint: { ...current.input.targetBlock.chainPoint, depth: "0" },
      transactions: [current.input.targetBlock.transactions[0]!],
    });
    const localObservation = {
      block: pendingBlock,
    } as unknown as WatcherLocalKupmiosNativeObservation;
    upstream.target = pendingBlock;
    upstream.localObservation = localObservation;
    upstream.nativeBlock = current.nativeBlock;
    const authority = createWatcherLocalUserEventReferenceAuthorityFromBodies({
      localObservation,
      nativeBlock: current.nativeBlock,
      deploymentIdentity,
      creatingTransactionBodies: [],
    });
    expect(assertWatcherLocalKupmiosNativeObservation).toHaveBeenCalledWith(
      localObservation,
      current.nativeBlock,
    );
    expect(readWatcherUserEventReferenceEvidence(authority)).toMatchObject({
      sourceMode: "local_node",
      evidenceKind: "creating_bodies",
      transactions: [
        { txHash: pendingBlock.transactions[0]!.txHash, referenceInputs: [] },
      ],
    });
    expect(readWatcherResolvedBlockObservation).not.toHaveBeenCalled();
    expect(resolveWatcherBlockObservationTransactions).not.toHaveBeenCalled();
    expect(() =>
      createWatcherLocalUserEventReferenceAuthorityFromBodies({
        localObservation: { ...localObservation },
        nativeBlock: current.nativeBlock,
        deploymentIdentity,
        creatingTransactionBodies: [],
      }),
    ).toThrow("not admitted");
    await expect(
      createWatcherLocalUserEventReferenceAuthority({
        ...current.input,
        targetBlock: pendingBlock,
      }),
    ).rejects.toThrow("differs from the complete resolved block");
    upstream.live = false;
    expect(() => readWatcherUserEventReferenceEvidence(authority)).toThrow(
      "not admitted",
    );
  });

  it("resolves the complete valid target sequence and binds the stable release minimum", async () => {
    const current = await fixture();
    const first = await createWatcherLocalUserEventReferenceAuthority(
      current.input,
    );
    const evidence = readWatcherUserEventReferenceEvidence(first);
    expect(resolveWatcherBlockObservationTransactions).toHaveBeenCalledWith(
      current.input.resolvedBlock,
      current.input.targetBlock.transactions
        .filter(({ isValid }) => isValid)
        .map(({ txHash }) => txHash),
    );
    expect(evidence.transactions.map(({ txHash }) => txHash)).toEqual(
      current.rawTransactions.map(({ txHash }) => txHash),
    );
    expect(evidence).toMatchObject({
      sourceMode: "local_node",
      confirmationDepth: "30",
    });
    current.advanceDepth();
    const fresh = await createWatcherLocalUserEventReferenceAuthority(
      current.input,
    );
    expect(fresh).not.toBe(first);
    expect(readWatcherUserEventReferenceEvidence(fresh)).toEqual(evidence);
    expect(() => readWatcherUserEventReferenceEvidence({ ...fresh })).toThrow(
      "not admitted",
    );
    upstream.live = false;
    expect(() => readWatcherUserEventReferenceEvidence(fresh)).toThrow(
      "live admitted target",
    );
  });

  it("rechecks upstream liveness after asynchronous raw resolution", async () => {
    const current = await fixture();
    vi.mocked(resolveWatcherBlockObservationTransactions).mockImplementation(
      async () => {
        upstream.live = false;
        return current.rawTransactions;
      },
    );
    await expect(
      createWatcherLocalUserEventReferenceAuthority(current.input),
    ).rejects.toThrow("live admitted target");
  });
});
