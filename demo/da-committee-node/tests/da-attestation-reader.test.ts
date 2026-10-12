import type {
  FactStore,
  StoredBlock,
  StoredOutput,
  UtxoRead,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { followerDaAttestationReader } from "../src/l1/da-attestation-reader.js";
import { bytesToHex } from "../src/utils/hex.js";

const scriptAddress = (hash: string): string =>
  credentialToAddress("Preprod", { type: "Script", hash });

const config = {
  deploymentFingerprint: "dd".repeat(32),
  daParamsGovernorPolicyId: "55".repeat(28),
  daParamsGovernorAddress: scriptAddress("56".repeat(28)),
  daAttestationPolicyId: "33".repeat(28),
  daAttestationAddress: scriptAddress("34".repeat(28)),
};
const elsewhere = scriptAddress("77".repeat(28));

const committeeHex = "01".repeat(32) + "02".repeat(32);
const paramsDatum: SDK.DaParamsDatum = {
  committee: committeeHex,
  committee_signers_hash: bytesToHex(
    blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
  ),
  da_threshold: 2n,
  owners: ["22".repeat(28), "33".repeat(28)],
  update_threshold: 2n,
};

const headerHash = "12".repeat(28);
const attestationDatum = (
  header: string,
  count: bigint,
): SDK.DaAttestationDatum => ({
  header_hash: header,
  availability_commitment: SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: "99".repeat(28),
    headerHash: header,
    payload: Buffer.from("public retained DA"),
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: 14_020,
      trancheByteLength: 4 * 1_024 * 1_024,
      maxTrancheCount: 16,
    }),
  }),
  da_threshold: 2n,
  committee_signers_hash: "34".repeat(32),
  rescue_beneficiary: {
    paymentCredential: { PublicKeyCredential: ["56".repeat(28)] },
    stakeCredential: null,
  },
  attested_signers: "80" + "00".repeat(31),
  attestation_count: count,
});

const output = (
  txByte: string,
  address: string,
  policyId: string,
  assetName: string,
  datum: Buffer | null,
  slot: number | null = 120,
): StoredOutput => ({
  outRef: { txHash: Buffer.from(txByte.repeat(32), "hex"), index: 0 },
  output: {
    address: Buffer.from(getAddressDetails(address).address.hex, "hex"),
    paymentCredential: null,
    stakeCredential: null,
    lovelace: 5_000_000n,
    assets: new Map([[policyId, new Map([[assetName, 1n]])]]),
    datumHash: null,
    datum,
    scriptRef: null,
  },
  created: slot === null ? null : { slot, txIndex: 0 },
  seedSlot: slot === null ? 7 : null,
  spent: null,
});

const paramsOutput = (
  txByte: string,
  address = config.daParamsGovernorAddress,
) =>
  output(
    txByte,
    address,
    config.daParamsGovernorPolicyId,
    SDK.DA_PARAMS_ASSET_NAME,
    Buffer.from(
      Data.to(paramsDatum as never, SDK.DaParamsDatum as never),
      "hex",
    ),
  );

const attestationOutput = (txByte: string, datum: SDK.DaAttestationDatum) =>
  output(
    txByte,
    config.daAttestationAddress,
    config.daAttestationPolicyId,
    SDK.daAttestationAssetName(headerHash),
    Buffer.from(
      Data.to(datum as never, SDK.DaAttestationDatum as never),
      "hex",
    ),
  );

/** A fact store holding `rows` live, with one block at slot 120. */
const facts = (
  rows: readonly StoredOutput[] | UtxoRead,
): Pick<FactStore, "liveUtxos" | "blockAtOrBeforeSlot"> & {
  asked: unknown[];
} => {
  const asked: unknown[] = [];
  return {
    asked,
    liveUtxos: async (query) => {
      asked.push(query);
      if (!Array.isArray(rows)) return rows as UtxoRead;
      const unit = query as { policyId: Buffer; assetName: Buffer };
      return {
        kind: "ok",
        utxos: (rows as readonly StoredOutput[]).filter(
          (row) =>
            row.output.assets
              .get(unit.policyId.toString("hex"))
              ?.has(unit.assetName.toString("hex")) === true,
        ),
      };
    },
    blockAtOrBeforeSlot: async (slot) =>
      slot >= 120
        ? ({
            slot: 120,
            height: 900,
            hash: Buffer.from("ab".repeat(32), "hex"),
          } as StoredBlock)
        : null,
  };
};

describe("the DA params and attestations, read from the follower's facts", () => {
  it("decodes the one DA params output at the configured address, with its block", async () => {
    const store = facts([paramsOutput("aa"), paramsOutput("bb", elsewhere)]);
    await expect(
      followerDaAttestationReader(store, config).fetchDaParams(),
    ).resolves.toMatchObject({
      outRef: `${"aa".repeat(32)}#0`,
      committeeHex,
      committeeSignersHash: paramsDatum.committee_signers_hash,
      threshold: 2,
      ownerCount: 2,
      updateThreshold: 2,
      observedChainPoint: {
        slot: 120,
        blockHash: "ab".repeat(32),
        blockHeight: 900,
        providerSource: "l1_follower",
      },
    });
    expect(store.asked).toEqual([
      {
        by: "unit",
        policyId: Buffer.from(config.daParamsGovernorPolicyId, "hex"),
        assetName: Buffer.from(SDK.DA_PARAMS_ASSET_NAME, "hex"),
      },
    ]);
  });

  it.each([
    ["none", []],
    ["two", [paramsOutput("aa"), paramsOutput("bb")]],
  ] as const)(
    "refuses %s DA params outputs at the configured address",
    async (_label, rows) => {
      await expect(
        followerDaAttestationReader(facts(rows), config).fetchDaParams(),
      ).rejects.toThrow(/expected exactly one DA params UTxO/u);
    },
  );

  it("passes on a refused read rather than reading nothing", async () => {
    await expect(
      followerDaAttestationReader(
        facts({ kind: "not_initialized", detail: "no cursor" } as UtxoRead),
        config,
      ).fetchDaParams(),
    ).rejects.toThrow("DA params read refused: not_initialized: no cursor");
  });

  it("classifies each attestation output by its count, sorted by output", async () => {
    const store = facts([
      attestationOutput("cc", attestationDatum(headerHash, 2n)),
      attestationOutput("bb", attestationDatum(headerHash, 1n)),
      attestationOutput("dd", attestationDatum(headerHash, 0n)),
    ]);
    const records = await followerDaAttestationReader(
      store,
      config,
    ).fetchDaAttestationCandidates(headerHash);
    expect(
      records.map(({ outRef, status, attestationCount, threshold }) => ({
        outRef,
        status,
        attestationCount,
        threshold,
      })),
    ).toEqual([
      {
        outRef: `${"bb".repeat(32)}#0`,
        status: "signed",
        attestationCount: 1,
        threshold: 2,
      },
      {
        outRef: `${"cc".repeat(32)}#0`,
        status: "threshold",
        attestationCount: 2,
        threshold: 2,
      },
      {
        outRef: `${"dd".repeat(32)}#0`,
        status: "initialized",
        attestationCount: 0,
        threshold: 2,
      },
    ]);
    expect(records[0]).toMatchObject({
      deploymentFingerprint: config.deploymentFingerprint,
      headerHash,
      committeeSignersHash: "34".repeat(32),
      bitmap: "80" + "00".repeat(31),
    });
  });

  it("refuses an attestation output under the header's token whose datum names another header", async () => {
    const store = facts([
      attestationOutput("bb", attestationDatum(headerHash, 1n)),
      attestationOutput("cc", attestationDatum("99".repeat(28), 1n)),
    ]);
    await expect(
      followerDaAttestationReader(store, config).fetchDaAttestationCandidates(
        headerHash,
      ),
    ).rejects.toThrow(/has header hash .* expected/u);
  });

  it("refuses an attestation output with no inline datum", async () => {
    const store = facts([
      output(
        "bb",
        config.daAttestationAddress,
        config.daAttestationPolicyId,
        SDK.daAttestationAssetName(headerHash),
        null,
      ),
    ]);
    await expect(
      followerDaAttestationReader(store, config).fetchDaAttestationCandidates(
        headerHash,
      ),
    ).rejects.toThrow(/has no inline datum/u);
  });
});
