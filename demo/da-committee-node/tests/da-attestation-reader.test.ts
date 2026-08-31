import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import type { DaAttestationCandidateRecord } from "../src/domain.js";
import {
  type DaAttestationChainReader,
  LucidDaAttestationChainReader,
  MultiDaAttestationChainReader,
  type OnChainDaParams,
} from "../src/l1/da-attestation-reader.js";
import {
  type CanonicalChainPoint,
  FileChainSyncCursorStore,
  LocalNodeChainAuthority,
} from "../src/l1/provider.js";
import { bytesToHex } from "../src/utils/hex.js";
import { minimalConfig, tempDir } from "./helpers.js";

describe("LucidDaAttestationChainReader", () => {
  it("fetches and decodes the unique DA params UTxO", async () => {
    const dir = await tempDir();
    // Q63 (F04 §4) floors both governed thresholds at two, so the smallest
    // representable committee has two sorted-unique members.
    const committeeHex = "01".repeat(32) + "02".repeat(32);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: "00".repeat(32),
      signerPublicKey: "01".repeat(32),
    });
    const daParamsDatum: SDK.DaParamsDatum = {
      committee: committeeHex,
      committee_signers_hash: bytesToHex(
        blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
      ),
      da_threshold: 2n,
      owners: ["22".repeat(28), "33".repeat(28)],
      update_threshold: 2n,
    };
    const paramsUnit = toUnit(
      config.daParamsGovernorPolicyId,
      SDK.DA_PARAMS_ASSET_NAME,
    );
    const lucid = fakeLucid([
      {
        txHash: "aa".repeat(32),
        outputIndex: 0,
        address: config.daParamsGovernorAddress,
        assets: { lovelace: 5_000_000n, [paramsUnit]: 1n },
        datum: Data.to(daParamsDatum as never, SDK.DaParamsDatum as never),
      },
    ]);
    const reader = new LucidDaAttestationChainReader({
      lucid,
      config,
      providerSource: "fake",
      ...pointResolvers("fake"),
    });
    await expect(reader.fetchDaParams()).resolves.toMatchObject({
      outRef: `${"aa".repeat(32)}#0`,
      committeeHex,
      threshold: 2,
      observedChainPoint: {
        network: "Preview",
        slot: 99,
        blockHash: "ab".repeat(32),
      },
    });
  });

  it("fails closed when a policy-matched DA attestation has a foreign header", async () => {
    const dir = await tempDir();
    const committeeHex = "01".repeat(32);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: "00".repeat(32),
      signerPublicKey: committeeHex,
    });
    const headerHash = "12".repeat(28);
    const unit = toUnit(
      config.daAttestationPolicyId,
      SDK.daAttestationAssetName(headerHash),
    );
    const datum: SDK.DaAttestationDatum = {
      header_hash: headerHash,
      availability_commitment: availabilityCommitment(headerHash),
      da_threshold: 2n,
      committee_signers_hash: "34".repeat(32),
      rescue_beneficiary: {
        paymentCredential: { PublicKeyCredential: ["56".repeat(28)] },
        stakeCredential: null,
      },
      attested_signers: "80" + "00".repeat(31),
      attestation_count: 1n,
    };
    const foreignDatum: SDK.DaAttestationDatum = {
      ...datum,
      header_hash: "99".repeat(28),
      availability_commitment: availabilityCommitment("99".repeat(28)),
    };
    const lucid = fakeLucid([
      {
        txHash: "bb".repeat(32),
        outputIndex: 0,
        address: config.daAttestationAddress,
        assets: { lovelace: 5_000_000n, [unit]: 1n },
        datum: Data.to(datum as never, SDK.DaAttestationDatum as never),
      },
      {
        txHash: "cc".repeat(32),
        outputIndex: 0,
        address: config.daAttestationAddress,
        assets: { lovelace: 5_000_000n, [unit]: 1n },
        datum: Data.to(foreignDatum as never, SDK.DaAttestationDatum as never),
      },
    ]);
    const reader = new LucidDaAttestationChainReader({
      lucid,
      config,
      providerSource: "fake",
      ...pointResolvers("fake"),
    });
    await expect(
      reader.fetchDaAttestationCandidates(headerHash),
    ).rejects.toThrow(/has header hash .* expected/u);
  });

  it("fails closed on malformed bytes at a policy-matched DA UTxO", async () => {
    const dir = await tempDir();
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: "00".repeat(32),
      signerPublicKey: "01".repeat(32),
    });
    const headerHash = "12".repeat(28);
    const unit = toUnit(
      config.daAttestationPolicyId,
      SDK.daAttestationAssetName(headerHash),
    );
    const reader = new LucidDaAttestationChainReader({
      lucid: fakeLucid([
        {
          txHash: "dd".repeat(32),
          outputIndex: 0,
          address: config.daAttestationAddress,
          assets: { lovelace: 5_000_000n, [unit]: 1n },
          datum: "00",
        },
      ]),
      config,
      providerSource: "fake",
      ...pointResolvers("fake"),
    });

    await expect(
      reader.fetchDaAttestationCandidates(headerHash),
    ).rejects.toThrow();
  });

  it("rejects an empty local query result when its indexer point is stale", async () => {
    const dir = await tempDir();
    const committeeHex = "01".repeat(32);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: "00".repeat(32),
      signerPublicKey: committeeHex,
    });
    const canonical = chainPoint("chain-sync:node-a", 100, "ab");
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      {
        next: async () => ({
          event: { direction: "roll_forward", point: canonical },
          tip: canonical,
        }),
      },
      new FileChainSyncCursorStore(`${dir}/cursor.json`, "11".repeat(32)),
    );
    const reader = new LucidDaAttestationChainReader({
      lucid: fakeLucid([]),
      config,
      providerSource: "query:node-a:0",
      inclusionPointResolver: async () =>
        chainPoint("query:node-a:0", 99, "cd"),
      queryPointResolver: async () => chainPoint("query:node-a:0", 99, "cd"),
      localAuthority: authority,
    });

    await expect(
      reader.fetchDaAttestationCandidates("12".repeat(28)),
    ).rejects.toThrow(/stale or on a mismatched chain point/u);
  });

  it("requires DA params and candidate agreement across L1 readers", async () => {
    const daParams = daParamsFixture();
    const candidate = candidateFixture({
      observedChainPoint: {
        ...chainPoint("provider-a", 100, "ab"),
        depth: 10,
        finalized: true,
      },
    });
    const reader = new MultiDaAttestationChainReader([
      fakeReader({
        daParams,
        candidates: [candidate],
      }),
      fakeReader({
        daParams,
        candidates: [
          {
            ...candidate,
            observedChainPoint: {
              ...chainPoint("provider-b", 100, "ab"),
              depth: 3,
              finalized: false,
            },
          },
        ],
      }),
    ]);

    await expect(reader.fetchDaParams()).resolves.toEqual(daParams);
    await expect(
      reader.fetchDaAttestationCandidates(candidate.headerHash),
    ).resolves.toMatchObject([
      {
        outRef: candidate.outRef,
        observedChainPoint: {
          providerSource: "provider-a,provider-b",
          depth: 3,
          finalized: false,
        },
      },
    ]);
  });

  it("fails closed when DA params providers disagree", async () => {
    const reader = new MultiDaAttestationChainReader([
      fakeReader({
        daParams: daParamsFixture({ threshold: 2 }),
        candidates: [],
      }),
      fakeReader({
        daParams: daParamsFixture({ threshold: 3 }),
        candidates: [],
      }),
    ]);

    await expect(reader.fetchDaParams()).rejects.toThrow(
      /DA params provider disagreement/,
    );
  });

  it("fails closed when DA attestation candidate providers disagree", async () => {
    const daParams = daParamsFixture();
    const candidate = candidateFixture();
    const reader = new MultiDaAttestationChainReader([
      fakeReader({ daParams, candidates: [candidate] }),
      fakeReader({
        daParams,
        candidates: [{ ...candidate, bitmap: "80" + "00".repeat(31) }],
      }),
    ]);

    await expect(
      reader.fetchDaAttestationCandidates(candidate.headerHash),
    ).rejects.toThrow(/candidate provider disagreement/);
  });

  it("fails closed when external readers report incompatible current chain points", async () => {
    const daParams = daParamsFixture();
    const candidate = candidateFixture();
    const reader = new MultiDaAttestationChainReader([
      fakeReader({
        daParams,
        candidates: [candidate],
        queryPoint: chainPoint("provider-a", 100, "ab"),
      }),
      fakeReader({
        daParams,
        candidates: [candidate],
        queryPoint: chainPoint("provider-b", 99, "cd"),
      }),
    ]);

    await expect(
      reader.fetchDaAttestationCandidates(candidate.headerHash),
    ).rejects.toThrow(/chain-point disagreement/u);
  });

  it("fails closed when readers agree on data but not observation provenance", async () => {
    const daParams = daParamsFixture();
    const candidate = candidateFixture();
    const reader = new MultiDaAttestationChainReader([
      fakeReader({
        daParams,
        candidates: [candidate],
        queryPoint: chainPoint("provider-a", 100, "ab"),
      }),
      fakeReader({
        daParams,
        candidates: [
          {
            ...candidate,
            observedChainPoint: chainPoint("provider-b", 99, "cd"),
          },
        ],
        queryPoint: chainPoint("provider-b", 100, "ab"),
      }),
    ]);

    await expect(
      reader.fetchDaAttestationCandidates(candidate.headerHash),
    ).rejects.toThrow(/observation provenance disagreement/u);
  });
});

const availabilityCommitment = (
  headerHash: string,
): SDK.DaAvailabilityCommitmentV1 =>
  SDK.buildDaAvailabilityCommitmentV1({
    deploymentIdentity: "99".repeat(28),
    headerHash,
    payload: Buffer.from("public retained DA"),
    bondOwner: "76".repeat(28),
    responseGeometry: SDK.availabilityResponseGeometryV1({
      chunkByteLength: 14_020,
      trancheByteLength: 4 * 1_024 * 1_024,
      maxTrancheCount: 16,
    }),
  });

const fakeLucid = (utxos: readonly UTxO[]) =>
  ({
    utxosAtWithUnit: async (address: string, unit: string) =>
      utxos.filter(
        (utxo) => utxo.address === address && (utxo.assets[unit] ?? 0n) > 0n,
      ),
  }) as never;

const chainPoint = (
  providerSource: string,
  slot: number,
  blockByte: string,
): CanonicalChainPoint => ({
  network: "Preview",
  slot,
  blockHash: blockByte.repeat(32),
  providerSource,
  observedAt: "2026-07-28T00:00:00.000Z",
});

const pointResolvers = (providerSource: string) => ({
  inclusionPointResolver: async (_utxo: UTxO) =>
    chainPoint(providerSource, 99, "ab"),
  queryPointResolver: async () => chainPoint(providerSource, 100, "cd"),
});

/**
 * A floor-compliant DA params fixture: a three-member sorted-unique committee
 * over a two-member owner set, so thresholds of both 2 and 3 stay within the
 * Q63 (F04 §4) governed bounds.
 */
const daParamsFixture = ({
  threshold = 2,
}: {
  readonly threshold?: number;
} = {}): OnChainDaParams => {
  const committeeHex = "01".repeat(32) + "02".repeat(32) + "03".repeat(32);
  const owners = ["22".repeat(28), "33".repeat(28)];
  const rawDatum: SDK.DaParamsDatum = {
    committee: committeeHex,
    committee_signers_hash: bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    ),
    da_threshold: BigInt(threshold),
    owners,
    update_threshold: 2n,
  };
  return {
    outRef: `${"aa".repeat(32)}#0`,
    committeeHex,
    committeeSignersHash: rawDatum.committee_signers_hash,
    threshold,
    ownerCount: owners.length,
    updateThreshold: 2,
    rawDatum,
  };
};

const candidateFixture = ({
  bitmap = "00".repeat(32),
  observedChainPoint = chainPoint("provider-a", 100, "ab"),
}: {
  readonly bitmap?: string;
  readonly observedChainPoint?: DaAttestationCandidateRecord["observedChainPoint"];
} = {}): DaAttestationCandidateRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "12".repeat(28),
  outRef: `${"bb".repeat(32)}#0`,
  datumCbor: "d87980",
  attestationCount: bitmap.startsWith("80") ? 1 : 0,
  threshold: 2,
  committeeSignersHash: "34".repeat(32),
  bitmap,
  observedChainPoint,
  status: bitmap.startsWith("80") ? "signed" : "initialized",
});

const fakeReader = ({
  daParams,
  candidates,
  queryPoint,
}: {
  readonly daParams: OnChainDaParams;
  readonly candidates: readonly DaAttestationCandidateRecord[];
  readonly queryPoint?: CanonicalChainPoint;
}): DaAttestationChainReader => ({
  fetchDaParams: async () => daParams,
  fetchDaAttestationCandidates: async () => candidates,
  ...(queryPoint === undefined ? {} : { currentQueryPoint: () => queryPoint }),
});
