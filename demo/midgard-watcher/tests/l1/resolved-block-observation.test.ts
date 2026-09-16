import { readFile } from "node:fs/promises";

import {
  computeFraudProofRawL1PointId,
  computeFraudProofReleaseFinalityPolicyDigest,
  createLocalKupmiosHttpOgmiosRawSource,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1Transaction,
  LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
  type LocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherStateQueueObservationSource } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { WatcherLocalKupmiosNativeObservation } from "../../src/l1/local-kupmios-native-observation.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import { WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION } from "../../src/l1/native-chain-sync.js";
import {
  createWatcherResolvedBlockObservationSource,
  readWatcherResolvedBlockObservation,
  resolveWatcherBlockObservationTransactions,
} from "../../src/l1/resolved-block-observation.js";
import { makeDeploymentAuthority } from "../support/deployment-authority-fixture.js";

// These are unit tests of the extracted owner. Upstream native/local/raw
// admission has its own suites; these mocks do not establish end-to-end L1
// acquisition. All transaction bytes come from the existing Conway fixture.
const upstream = vi.hoisted(() => ({
  localObservation: null as object | null,
  nativeBlock: null as object | null,
  live: true,
  closedSurface: null as object | null,
  kupo: {},
  ogmios: {},
}));

vi.mock("@al-ft/midgard-fault-proofs", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@al-ft/midgard-fault-proofs")>()),
  readAdmittedLocalKupmiosBoundary: vi.fn(),
  readAdmittedLocalKupmiosRawBlockAtPoint: vi.fn(),
  readAdmittedLocalKupmiosRawTransaction: vi.fn(),
}));

vi.mock(
  "../../src/l1/local-kupmios-native-observation.js",
  async (importOriginal) => ({
    ...(await importOriginal<
      typeof import("../../src/l1/local-kupmios-native-observation.js")
    >()),
    assertWatcherLocalKupmiosNativeObservation: (
      observation: object,
      block: object,
    ) => {
      if (
        observation !== upstream.localObservation ||
        block !== upstream.nativeBlock
      ) {
        throw new Error(
          "local observation is not admitted for the native block",
        );
      }
    },
  }),
);

vi.mock("../../src/l1/l1-adapter.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../../src/l1/l1-adapter.js")>()),
  watcherL1TransportAttestationDetails: (context: object) => {
    if (!upstream.live || context === upstream.closedSurface) return null;
    if (context === upstream.kupo)
      return {
        provider: { source: { sourceMode: "local_node", surface: "kupo" } },
        transportEndpoint: "http://127.0.0.1:1442",
      };
    if (context === upstream.ogmios)
      return {
        provider: { source: { sourceMode: "local_node", surface: "ogmios" } },
        transportEndpoint: "ws://127.0.0.1:1337",
      };
    return null;
  },
}));

const metadata = Object.freeze({
  blockHash: "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
  blockNo: "12069665",
  blockType: "7",
  prevHash: "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
  slot: "159835207",
});
const deployment = makeDeploymentAuthority().result;
const rawSource = (
  options: { readonly kupoUrl?: string; readonly manifestId?: string } = {},
) => {
  const policy = {
    confirmationDepth: 30 as const,
    automaticRecoveryMaxDepth: 2160 as const,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1" as const,
  };
  return createLocalKupmiosHttpOgmiosRawSource({
    sourceId: "resolved-block-unit",
    kupoHttpUrl: options.kupoUrl ?? "http://127.0.0.1:1442",
    ogmiosUrl: "ws://127.0.0.1:1337",
    releaseFinality: validateVerifiedFraudProofReleaseFinalityPolicy({
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: options.manifestId ?? deployment.manifestId,
      blueprintHash: deployment.blueprintHash,
      policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
      policy,
    }),
  });
};

const fixture = async () => {
  const nativeBlock = admitWatcherNativeRollForwardBlock({
    ...metadata,
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    kind: "roll_forward",
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
    blockHash: nativeBlock.blockHash,
    blockNo: nativeBlock.blockNo,
    slot: nativeBlock.slot,
    pointId: computeFraudProofRawL1PointId(nativeBlock),
  };
  const rawBlock: LocalKupmiosRawBlockAtPoint = {
    schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
    sourceId: "fixture",
    point,
    parentBlockHash: nativeBlock.prevHash,
    kupoCheckpoint: {
      slot: Number(nativeBlock.slot),
      blockHash: nativeBlock.blockHash,
    },
    transactions: nativeBlock.transactionIds.map((txHash, index) => ({
      txHash,
      transactionCbor: nativeBlock.transactionCbors[index]!,
    })),
  };
  const tipCoordinates = {
    blockHash: "77".repeat(32),
    blockNo: (BigInt(point.blockNo) + 29n).toString(),
    slot: (BigInt(point.slot) + 600n).toString(),
  };
  vi.mocked(readAdmittedLocalKupmiosBoundary).mockResolvedValue({
    kupoCheckpoint: point,
    ogmiosTip: {
      ...tipCoordinates,
      pointId: computeFraudProofRawL1PointId(tipCoordinates),
    },
    confirmationDepth: 30,
  });
  const transactions = nativeBlock.transactionCbors.map((cbor) =>
    CML.Transaction.from_cbor_hex(cbor),
  );
  const localObservation = {
    block: {
      chainPoint: { ...point, depth: "30" },
      transactions: transactions.map((transaction, index) => ({
        txHash: nativeBlock.transactionIds[index]!,
        body: { bytesHex: transaction.body().to_cbor_hex() },
        witnessSet: { bytesHex: transaction.witness_set().to_cbor_hex() },
      })),
    },
    transportAttestations: [upstream.kupo, upstream.ogmios],
  } as unknown as WatcherLocalKupmiosNativeObservation;
  upstream.localObservation = localObservation;
  upstream.nativeBlock = nativeBlock;
  const rawTransactions: FraudProofRawL1Transaction[] = transactions.map(
    (transaction, index) => ({
      txHash: nativeBlock.transactionIds[index]!,
      bodyCbor: transaction.body().to_cbor_hex(),
      witnessSetCbor: transaction.witness_set().to_cbor_hex(),
      redeemersCbor:
        transaction.witness_set().redeemers()?.to_canonical_cbor_hex() ?? null,
      isValid: true,
      inclusionPoint: { ...point },
      confirmationDepth: 30,
      // This owner consumes already-admitted resolved records. Reuse a fixture
      // output to check copying; upstream roster/provenance is not mocked proof.
      resolvedInputs: [
        {
          outRef: `${nativeBlock.transactionIds[index]}#0`,
          outputCbor: transaction.body().outputs().get(0).to_cbor_hex(),
          datumCbor: null,
          referenceScriptCbor: null,
        },
      ],
      resolvedReferenceInputs: [],
    }),
  );
  vi.mocked(readAdmittedLocalKupmiosRawBlockAtPoint).mockResolvedValue(
    rawBlock,
  );
  vi.mocked(readAdmittedLocalKupmiosRawTransaction).mockImplementation(
    async ({ txHash }) => {
      const raw = rawTransactions.find(
        (transaction) => transaction.txHash === txHash,
      );
      if (raw === undefined) throw new Error("fixture transaction is absent");
      return raw;
    },
  );
  const input = { nativeBlock, localObservation };
  const sourceInput = {
    deploymentIdentity: deployment,
    rawSource: rawSource(),
  };
  return {
    input,
    sourceInput,
    rawBlock,
    rawTransactions,
    source: createWatcherResolvedBlockObservationSource(sourceInput),
  };
};

beforeEach(() => {
  upstream.live = true;
  upstream.closedSurface = null;
  vi.mocked(readAdmittedLocalKupmiosRawBlockAtPoint).mockReset();
  vi.mocked(readAdmittedLocalKupmiosRawTransaction).mockReset();
});

describe("shared resolved block observation owner", () => {
  it("authenticates included transaction bytes without granting release finality", async () => {
    const current = await fixture();
    const included = {
      ...current.input.localObservation,
      block: {
        ...current.input.localObservation.block,
        chainPoint: {
          ...current.input.localObservation.block.chainPoint,
          depth: "1",
        },
      },
    };
    upstream.localObservation = included;
    const input = { ...current.input, localObservation: included };
    vi.mocked(readAdmittedLocalKupmiosRawTransaction).mockImplementation(
      async ({ txHash }) => {
        const transaction = current.rawTransactions.find(
          (raw) => raw.txHash === txHash,
        )!;
        return { ...transaction, confirmationDepth: 1 };
      },
    );
    const source = createWatcherResolvedBlockObservationSource({
      ...current.sourceInput,
      minimumConfirmationDepth: 1,
    });
    const admitted = await source.observe(input);
    expect(
      readWatcherResolvedBlockObservation(admitted).minimumConfirmationDepth,
    ).toBe("1");
    expect(
      (
        await resolveWatcherBlockObservationTransactions(admitted, [
          current.rawTransactions[0]!.txHash,
        ])
      )[0]!.confirmationDepth,
    ).toBe(1);
    await expect(current.source.observe(input)).rejects.toThrow(
      "chain point/finality",
    );
  });

  it("captures the complete immutable sequence before resolving a selected subset", async () => {
    const current = await fixture();
    const observation = await current.source.observe(current.input);
    const evidence = readWatcherResolvedBlockObservation(observation);
    expect(evidence.rawBlock).toEqual(current.rawBlock);
    expect(evidence.rawBlock).not.toBe(current.rawBlock);
    expect(evidence.rawBlock.point).not.toBe(current.rawBlock.point);
    expect(evidence.rawBlock.transactions).not.toBe(
      current.rawBlock.transactions,
    );
    expect(Object.isFrozen(evidence.rawBlock.transactions[0])).toBe(true);
    expect(readAdmittedLocalKupmiosRawTransaction).not.toHaveBeenCalled();
    const selected = [
      current.input.nativeBlock.transactionIds[1]!,
      current.input.nativeBlock.transactionIds[0]!,
    ];
    const transactions = await resolveWatcherBlockObservationTransactions(
      observation,
      selected,
    );
    expect(transactions.map(({ txHash }) => txHash)).toEqual(selected);
    expect(readAdmittedLocalKupmiosRawTransaction).toHaveBeenCalledTimes(2);
    expect(readAdmittedLocalKupmiosRawTransaction).toHaveBeenCalledWith({
      source: current.sourceInput.rawSource,
      txHash: selected[0],
      expectedInclusionPoint: current.rawBlock.point,
      minimumConfirmationDepth: 30,
    });
    expect(Object.isFrozen(transactions)).toBe(true);
    expect(Object.isFrozen(transactions[0]!.inclusionPoint)).toBe(true);
    expect(Object.isFrozen(transactions[0]!.resolvedInputs[0])).toBe(true);
    expect(transactions[0]!.resolvedInputs[0]).not.toBe(
      current.rawTransactions[1]!.resolvedInputs[0],
    );
    expect(
      readWatcherResolvedBlockObservation(
        observation,
      ).rawBlock.transactions.map(({ txHash }) => txHash),
    ).toEqual(current.input.nativeBlock.transactionIds);
  });

  it("does not change queue candidate filtering when an ordinary block has no queue transition", async () => {
    const current = await fixture();
    const source = createWatcherStateQueueObservationSource(
      current.sourceInput,
    );
    await expect(
      source.observe({ ...current.input, previous: null }),
    ).resolves.toBeNull();
    expect(readAdmittedLocalKupmiosRawBlockAtPoint).toHaveBeenCalledTimes(1);
    expect(readAdmittedLocalKupmiosRawTransaction).not.toHaveBeenCalled();
  });

  it("refuses copied sources, local authority and observation handles", async () => {
    const current = await fixture();
    await expect({ ...current.source }.observe(current.input)).rejects.toThrow(
      "source was not admitted",
    );
    await expect(
      current.source.observe({
        ...current.input,
        localObservation: { ...current.input.localObservation },
      }),
    ).rejects.toThrow("not admitted for the native block");
    const observation = await current.source.observe(current.input);
    expect(() =>
      readWatcherResolvedBlockObservation({ ...observation }),
    ).toThrow("observation was not admitted");
    await expect(
      resolveWatcherBlockObservationTransactions({ ...observation }, []),
    ).rejects.toThrow("observation was not admitted");
  });

  it("rejects duplicate or unknown selections before any resolved read", async () => {
    const current = await fixture();
    const observation = await current.source.observe(current.input);
    const hash = current.input.nativeBlock.transactionIds[0]!;
    await expect(
      resolveWatcherBlockObservationTransactions(observation, [hash, hash]),
    ).rejects.toThrow("unique subset");
    await expect(
      resolveWatcherBlockObservationTransactions(observation, [
        "00".repeat(32),
      ]),
    ).rejects.toThrow("unique subset");
    expect(readAdmittedLocalKupmiosRawTransaction).not.toHaveBeenCalled();
    await expect(
      resolveWatcherBlockObservationTransactions(observation, []),
    ).resolves.toEqual([]);
  });

  it("requires the deployment and live transport endpoints of the raw source", async () => {
    const current = await fixture();
    expect(() =>
      createWatcherResolvedBlockObservationSource({
        ...current.sourceInput,
        deploymentIdentity: { ...deployment },
      }),
    ).toThrow("verifiedDeploymentIdentity");
    expect(() =>
      createWatcherResolvedBlockObservationSource({
        ...current.sourceInput,
        rawSource: { ...current.sourceInput.rawSource },
      }),
    ).toThrow("not bound to the verified deployment");
    expect(() =>
      createWatcherResolvedBlockObservationSource({
        ...current.sourceInput,
        rawSource: rawSource({ manifestId: "ab".repeat(32) }),
      }),
    ).toThrow("not bound to the verified deployment");
    const foreignEndpoint = createWatcherResolvedBlockObservationSource({
      ...current.sourceInput,
      rawSource: rawSource({ kupoUrl: "http://127.0.0.1:1443" }),
    });
    await expect(foreignEndpoint.observe(current.input)).rejects.toThrow(
      "differs from admitted watcher transports",
    );
    expect(readAdmittedLocalKupmiosRawBlockAtPoint).not.toHaveBeenCalled();
  });

  it.each(["blockHash", "slot", "blockNo", "depth"] as const)(
    "preserves the local %s binding",
    async (field) => {
      const current = await fixture();
      const localObservation = {
        ...current.input.localObservation,
        block: {
          ...current.input.localObservation.block,
          chainPoint: {
            ...current.input.localObservation.block.chainPoint,
            [field]: field === "blockHash" ? "00".repeat(32) : "29",
          },
        },
      };
      upstream.localObservation = localObservation;
      await expect(
        current.source.observe({ ...current.input, localObservation }),
      ).rejects.toThrow("chain point/finality differs");
      expect(readAdmittedLocalKupmiosRawBlockAtPoint).not.toHaveBeenCalled();
    },
  );

  it.each(["parent", "count", "order", "hash", "bytes"] as const)(
    "preserves the complete raw block %s binding",
    async (field) => {
      const current = await fixture();
      const transactions = current.rawBlock.transactions.map((transaction) => ({
        ...transaction,
      }));
      if (field === "count") transactions.pop();
      if (field === "order") transactions.reverse();
      if (field === "hash") transactions[0]!.txHash = "00".repeat(32);
      if (field === "bytes") transactions[0]!.transactionCbor = "80";
      vi.mocked(readAdmittedLocalKupmiosRawBlockAtPoint).mockResolvedValue({
        ...current.rawBlock,
        transactions,
        ...(field === "parent" ? { parentBlockHash: "00".repeat(32) } : {}),
      });
      await expect(current.source.observe(current.input)).rejects.toThrow(
        "differs from native admission",
      );
      expect(readAdmittedLocalKupmiosRawTransaction).not.toHaveBeenCalled();
    },
  );

  it.each([
    "hash",
    "blockHash",
    "slot",
    "blockNo",
    "depth",
    "body",
    "witness",
  ] as const)("preserves resolved transaction %s binding", async (field) => {
    const current = await fixture();
    const observation = await current.source.observe(current.input);
    const raw = current.rawTransactions[0]!;
    const candidate = { ...raw, inclusionPoint: { ...raw.inclusionPoint } };
    if (field === "hash") candidate.txHash = "00".repeat(32);
    if (field === "blockHash")
      candidate.inclusionPoint.blockHash = "00".repeat(32);
    if (field === "slot" || field === "blockNo")
      candidate.inclusionPoint[field] = "29";
    if (field === "depth") candidate.confirmationDepth = 29;
    if (field === "body") candidate.bodyCbor = "a0";
    if (field === "witness") candidate.witnessSetCbor = "a10080";
    vi.mocked(readAdmittedLocalKupmiosRawTransaction).mockResolvedValue(
      candidate,
    );
    await expect(
      resolveWatcherBlockObservationTransactions(observation, [raw.txHash]),
    ).rejects.toThrow(
      field === "body" || field === "witness"
        ? "bytes differ"
        : "differs from the admitted block point",
    );
  });

  it.each(["kupo", "ogmios"] as const)(
    "identifies a closed %s authority separately from endpoint substitution",
    async (surface) => {
      const current = await fixture();
      upstream.closedSurface = upstream[surface];
      await expect(current.source.observe(current.input)).rejects.toThrow(
        `transport authority is closed or absent: ${surface}`,
      );
    },
  );

  it("rechecks live authority after asynchronous reads and before later use", async () => {
    const current = await fixture();
    const observation = await current.source.observe(current.input);
    upstream.live = false;
    expect(() => readWatcherResolvedBlockObservation(observation)).toThrow(
      "transport authority is closed or absent: kupo, ogmios",
    );
    await expect(
      resolveWatcherBlockObservationTransactions(observation, []),
    ).rejects.toThrow("transport authority is closed or absent: kupo, ogmios");
    upstream.live = true;
    vi.mocked(readAdmittedLocalKupmiosRawTransaction).mockImplementation(
      async () => {
        upstream.live = false;
        return current.rawTransactions[0]!;
      },
    );
    await expect(
      resolveWatcherBlockObservationTransactions(observation, [
        current.rawTransactions[0]!.txHash,
      ]),
    ).rejects.toThrow("transport authority is closed or absent: kupo, ogmios");
    upstream.live = true;
    vi.mocked(readAdmittedLocalKupmiosRawBlockAtPoint).mockImplementation(
      async () => {
        upstream.live = false;
        return current.rawBlock;
      },
    );
    await expect(current.source.observe(current.input)).rejects.toThrow(
      "transport authority is closed or absent: kupo, ogmios",
    );
  });
});
