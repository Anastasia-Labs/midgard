import "./fabricated-deposit.q39-fabricated-deposit-proof-plan.js";

import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { authenticateFabricatedHistoryWitness } from "../src/fabricated-history-witness.js";
import { fabricatedDepositBlockEvidenceFromVerifiedPayload } from "../src/prepare-fabricated-deposit.js";
import {
  createFabricatedDepositEvidenceAuthority,
  FABRICATED_DEPOSIT_ARTIFACT,
  type FabricatedDepositArtifact,
  requireFabricatedDepositArtifact,
} from "../src/workflow/fabricated-deposit-evidence.js";
import { normalizeJournalJson } from "../src/workflow/journal.js";
import {
  AUTHENTIC_DEPOSIT_ID,
  buildDepositsBlockFixture,
  DA_PROVENANCE,
  type DepositsBlockFixture,
  FI_LEAF,
  historyEnvironment,
  historyWitness,
  hubOracleUtxoFixture,
  KEY_AUTHENTIC_DEPOSIT_ID,
  MM_LEAF,
  NONCE_AUTHENTIC_DEPOSIT_ID,
  rootHistoryUtxo,
  VALUE_AUTHENTIC_DEPOSIT_INFO,
} from "./fabricated-deposit.build-deposits-block-fixture.js";
import {
  depositEventUtxoFixture,
  historyLucid,
  historyOpening,
} from "./fabricated-deposit.q39-fabricated-deposit-evidence-admission.js";
import { h28, h32 } from "./helpers/canonical-block-evidence-fixture.js";

describe("Q39 fabricated-deposit production evidence authority", () => {
  const artifactDigestForTest = (
    value: Omit<FabricatedDepositArtifact, "artifactDigest">,
  ): string =>
    createHash("sha256")
      .update(FABRICATED_DEPOSIT_ARTIFACT)
      .update("\0")
      .update(value.headerHash)
      .update("\0")
      .update(value.owner)
      .update("\0")
      .update(value.depositIndex.toString())
      .update("\0")
      .update(JSON.stringify(value.depositInclusion))
      .update("\0")
      .update(JSON.stringify(value.authenticContent))
      .update("\0")
      .update(JSON.stringify(value.l1Evidence))
      .digest("hex");

  const canonicalEvidence = async (
    fixture: DepositsBlockFixture,
  ): Promise<CanonicalBlockEvidence> => {
    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayload({
      observation: fixture.observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
      minimumConfirmationDepth: 1,
    });
    const deposits = evidence.entries.map(([keyCbor, valueCbor]) => ({
      key: Data.from(keyCbor, SDK.OutputReference),
      value: Data.from(valueCbor, SDK.DepositInfo),
      keyBytes: Buffer.from(keyCbor, "hex"),
      valueBytes: Buffer.from(valueCbor, "hex"),
    }));
    return {
      ...evidence,
      observation: fixture.observation,
      header: fixture.header,
      reconstruction: { deposits },
    } as unknown as CanonicalBlockEvidence;
  };

  it("roundtrips an absence fault through journal normalization and rejects artifact tampering", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [FI_LEAF] });
    const authority = createFabricatedDepositEvidenceAuthority({
      history: historyEnvironment,
      lucid: historyLucid([rootHistoryUtxo()]),
      network: "Preview",
      hubOraclePolicyId: h28(0x16),
      minimumConfirmationDepth: 1,
    });
    const evidence = await canonicalEvidence(fixture);
    const detections = await authority.detect(evidence, h28(0x44));
    expect(detections).toHaveLength(1);
    expect(detections[0]!.detection.violationId).toBe("fabricated-deposit");
    expect(detections[0]!.artifact.l1Evidence).toEqual({
      kind: "absent_identity",
      historyOutRef: `${h32(0xa3)}#0`,
      retainedDataOutRef: null,
    });
    await expect(
      authority.readmit(
        JSON.parse(
          JSON.stringify(normalizeJournalJson(detections[0]!.artifact)),
        ),
      ),
    ).resolves.toEqual(detections[0]!.artifact);
    await expect(
      authority.readmit(
        normalizeJournalJson({
          ...detections[0]!.artifact,
          depositInclusion: {
            ...detections[0]!.artifact.depositInclusion,
            depositsPhasRoot: h32(0x77),
          },
        }),
      ),
    ).rejects.toThrow(/digest mismatch/u);
    await expect(
      authority.readmit({
        ...detections[0]!.artifact,
        depositIndex: 1,
      }),
    ).rejects.toThrow(/digest mismatch/u);
    const { artifactDigest: _digest, ...body } = detections[0]!.artifact;
    const substitutedBody = {
      ...body,
      authenticContent: { openingCbor: historyOpening() },
      l1Evidence: {
        kind: "present_event" as const,
        historyOutRef: `${h32(0x77)}#0`,
        retainedDataOutRef: null,
      },
    };
    await expect(
      authority.readmit({
        ...substitutedBody,
        artifactDigest: artifactDigestForTest(substitutedBody),
      }),
    ).rejects.toThrow(/History facts changed before capture/u);
    await expect(
      authority.readmit({ ...detections[0]!.artifact, extra: true }),
    ).rejects.toThrow(/unknown, missing, or non-string/u);
    await expect(
      authority.readmit(
        Object.assign(Object.create(null), detections[0]!.artifact),
      ),
    ).rejects.toThrow(/unknown, missing, or non-string/u);
    expect(() =>
      requireFabricatedDepositArtifact(
        { ...detections[0]!.artifact },
        h28(0x44),
        fixture.headerHash,
      ),
    ).toThrow(/not re-authenticated/u);
  });

  it.each([false, true])(
    "uses the governed deposit spending address for ordinary lookup and readmission (stake credential: %s)",
    async (withStake) => {
      const hubPolicy = h28(0x16);
      const hubUnit = toUnit(hubPolicy, SDK.HUB_ORACLE_ASSET_NAME);
      const hubAddress = credentialToAddress("Preview", {
        type: "Script",
        hash: hubPolicy,
      });
      const eventAddress = credentialToAddress(
        "Preview",
        { type: "Script", hash: h28(0xe1) },
        withStake ? { type: "Key", hash: h28(0xe2) } : undefined,
      );
      const hubDatum = {
        ...Data.from(hubOracleUtxoFixture().datum!, SDK.HubOracleDatum),
        deposit_addr: await Effect.runPromise(
          SDK.addressDataFromBech32(eventAddress),
        ),
      };
      const hub = {
        ...hubOracleUtxoFixture(),
        address: hubAddress,
        datum: Data.to(hubDatum, SDK.HubOracleDatum),
        assets: { lovelace: 5_000_000n, [hubUnit]: 1n },
      };
      const event = { ...depositEventUtxoFixture(), address: eventAddress };
      const eventUnit = toUnit(hubDatum.deposit, NONCE_AUTHENTIC_DEPOSIT_ID);
      const mintPolicyAddress = credentialToAddress("Preview", {
        type: "Script",
        hash: hubDatum.deposit,
      });
      expect(eventAddress).not.toBe(mintPolicyAddress);
      expect(event.assets[eventUnit]).toBe(1n);
      let liveEventAddress = eventAddress;
      const queries: string[] = [];
      const authority = createFabricatedDepositEvidenceAuthority({
        history: historyEnvironment,
        lucid: {
          utxosByOutRef: async () => {
            throw new Error("Unexpected identity-nonce lookup");
          },
          utxosAtWithUnit: async (address: string, unit: string) =>
            address === hubAddress && unit === hubUnit ? [hub] : [],
          utxosAt: async (address: string) => {
            queries.push(address);
            return address === liveEventAddress
              ? [{ ...event, address: liveEventAddress }]
              : [];
          },
        } as unknown as LucidEvolution,
        network: "Preview",
        hubOraclePolicyId: hubPolicy,
        minimumConfirmationDepth: 1,
      });
      const ordinary = await buildDepositsBlockFixture({
        leaves: [
          {
            key: KEY_AUTHENTIC_DEPOSIT_ID,
            value: VALUE_AUTHENTIC_DEPOSIT_INFO,
          },
        ],
      });
      const ordinaryEvidence = await canonicalEvidence(ordinary);
      await expect(
        authority.detect(ordinaryEvidence, h28(0x44)),
      ).resolves.toEqual([]);
      // Reuse the existing mismatch case solely to exercise persisted-event lookup.
      const existing = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
      const detections = await authority.detect(
        await canonicalEvidence(existing),
        h28(0x44),
      );
      expect(detections).toHaveLength(1);
      await expect(
        authority.readmit(
          JSON.parse(
            JSON.stringify(normalizeJournalJson(detections[0]!.artifact)),
          ),
        ),
      ).resolves.toEqual(detections[0]!.artifact);
      expect(queries).toContain(eventAddress);
      expect(queries).not.toContain(mintPolicyAddress);
      liveEventAddress = mintPolicyAddress;
      await expect(
        authority.detect(ordinaryEvidence, h28(0x44)),
      ).rejects.toThrow("no unique authenticated witness");
      await expect(authority.readmit(detections[0]!.artifact)).rejects.toThrow(
        "no unique authenticated witness",
      );
    },
  );

  it("returns no detection for an authentic due event whose content matches the block", async () => {
    const fixture = await buildDepositsBlockFixture({
      leaves: [
        { key: KEY_AUTHENTIC_DEPOSIT_ID, value: VALUE_AUTHENTIC_DEPOSIT_INFO },
      ],
    });
    const authority = createFabricatedDepositEvidenceAuthority({
      history: historyEnvironment,
      lucid: historyLucid([depositEventUtxoFixture()]),
      network: "Preview",
      hubOraclePolicyId: h28(0x16),
      minimumConfirmationDepth: 1,
    });
    expect(
      await authority.detect(await canonicalEvidence(fixture), h28(0x44)),
    ).toEqual([]);
  });

  it("requires an authenticated gap for arbitrary and historically consumed identities", async () => {
    const authority = createFabricatedDepositEvidenceAuthority({
      history: historyEnvironment,
      lucid: historyLucid([]),
      network: "Preview",
      hubOraclePolicyId: h28(0x16),
      minimumConfirmationDepth: 1,
    });
    for (const leaves of [
      [FI_LEAF],
      [
        {
          key: KEY_AUTHENTIC_DEPOSIT_ID,
          value: VALUE_AUTHENTIC_DEPOSIT_INFO,
        },
      ],
    ]) {
      const fixture = await buildDepositsBlockFixture({ leaves });
      const authenticated = createFabricatedDepositEvidenceAuthority({
        history: historyEnvironment,
        lucid: historyLucid([rootHistoryUtxo()]),
        network: "Preview",
        hubOraclePolicyId: h28(0x16),
        minimumConfirmationDepth: 1,
      });
      expect(
        await authenticated.detect(await canonicalEvidence(fixture), h28(0x44)),
      ).toHaveLength(1);
      await expect(
        authority.detect(await canonicalEvidence(fixture), h28(0x44)),
      ).rejects.toThrow(/no unique authenticated witness/u);
    }
  });
});

it("authenticates an equal-key filler as absence without counting its funds or using nonce liveness", async () => {
  const anchor = depositEventUtxoFixture();
  const node = Data.from(anchor.datum!, SDK.EventHistoryNode);
  const filler = {
    ...anchor,
    datum: Data.to(
      { ...node, payload: { Filler: { refund_key: h28(0x44) } } },
      SDK.EventHistoryNode,
    ),
  };
  const result = await authenticateFabricatedHistoryWitness(
    historyWitness(filler),
    "Deposit",
    AUTHENTIC_DEPOSIT_ID,
  );
  expect(result.witness.kind).toBe("Absent");
  expect(result.captured).toBeUndefined();
  expect(result.witness.anchor.utxo.assets.lovelace).toBe(5_000_000n);
});
