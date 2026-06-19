import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  LucidDaAttestationChainReader,
  MultiDaAttestationChainReader,
  type DaAttestationChainReader,
  type OnChainDaParams,
} from "../src/l1/da-attestation-reader.js";
import type { DaAttestationCandidateRecord } from "../src/domain.js";
import { bytesToHex } from "../src/utils/hex.js";
import { minimalConfig, tempDir } from "./helpers.js";

describe("LucidDaAttestationChainReader", () => {
  it("fetches and decodes the unique DA params UTxO", async () => {
    const dir = await tempDir();
    const committeeHex = "01".repeat(32);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: "00".repeat(32),
      signerPublicKey: committeeHex,
    });
    const daParamsDatum: SDK.DaParamsDatum = {
      committee: committeeHex,
      committee_signers_hash: bytesToHex(
        blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
      ),
      da_threshold: 1n,
      owners: [],
      update_threshold: 1n,
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
    });
    await expect(reader.fetchDaParams()).resolves.toMatchObject({
      outRef: `${"aa".repeat(32)}#0`,
      committeeHex,
      threshold: 1,
    });
  });

  it("fetches DA attestation candidates by header hash and filters malformed/foreign datums", async () => {
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
      da_threshold: 2n,
      committee_signers_hash: "34".repeat(32),
      attested_signers: "80" + "00".repeat(31),
      attestation_count: 1n,
    };
    const foreignDatum: SDK.DaAttestationDatum = {
      ...datum,
      header_hash: "99".repeat(28),
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
    });
    await expect(reader.fetchDaAttestationCandidates(headerHash)).resolves.toEqual([
      expect.objectContaining({
        headerHash,
        outRef: `${"bb".repeat(32)}#0`,
        attestationCount: 1,
        threshold: 2,
        status: "signed",
      }),
    ]);
  });

  it("requires DA params and candidate agreement across L1 readers", async () => {
    const daParams = daParamsFixture();
    const candidate = candidateFixture({
      observedChainPoint: { providerSource: "provider-a" },
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
            observedChainPoint: { providerSource: "provider-b" },
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
        },
      },
    ]);
  });

  it("fails closed when DA params providers disagree", async () => {
    const reader = new MultiDaAttestationChainReader([
      fakeReader({
        daParams: daParamsFixture({ threshold: 1 }),
        candidates: [],
      }),
      fakeReader({
        daParams: daParamsFixture({ threshold: 2 }),
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
});

const fakeLucid = (utxos: readonly UTxO[]) =>
  ({
    utxosAtWithUnit: async (address: string, unit: string) =>
      utxos.filter(
        (utxo) => utxo.address === address && (utxo.assets[unit] ?? 0n) > 0n,
      ),
  }) as never;

const daParamsFixture = ({
  threshold = 1,
}: {
  readonly threshold?: number;
} = {}): OnChainDaParams => {
  const committeeHex = "01".repeat(32);
  const rawDatum: SDK.DaParamsDatum = {
    committee: committeeHex,
    committee_signers_hash: bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    ),
    da_threshold: BigInt(threshold),
    owners: [],
    update_threshold: 1n,
  };
  return {
    outRef: `${"aa".repeat(32)}#0`,
    committeeHex,
    committeeSignersHash: rawDatum.committee_signers_hash,
    threshold,
    ownerCount: 0,
    updateThreshold: 1,
    rawDatum,
  };
};

const candidateFixture = ({
  bitmap = "00".repeat(32),
  observedChainPoint = { providerSource: "provider-a" },
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
}: {
  readonly daParams: OnChainDaParams;
  readonly candidates: readonly DaAttestationCandidateRecord[];
}): DaAttestationChainReader => ({
  fetchDaParams: async () => daParams,
  fetchDaAttestationCandidates: async () => candidates,
});
