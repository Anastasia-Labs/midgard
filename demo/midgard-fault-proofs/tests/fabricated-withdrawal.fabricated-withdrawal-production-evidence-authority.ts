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
import { fabricatedWithdrawalBlockEvidenceFromVerifiedPayload } from "../src/prepare-fabricated-withdrawal.js";
import {
  createFabricatedWithdrawalEvidenceAuthority,
  requireFabricatedWithdrawalArtifact,
} from "../src/workflow/fabricated-withdrawal-evidence.js";
import { normalizeJournalJson } from "../src/workflow/journal.js";
import {
  buildWithdrawalsBlockFixture,
  DA_PROVENANCE,
  FI_LEAF,
  historyEnvironment,
  hubOracleUtxoFixture,
  KEY_AUTHENTIC_WITHDRAWAL_ID,
  MM_LEAF,
  NONCE_AUTHENTIC_WITHDRAWAL_ID,
  VALUE_AUTHENTIC_WITHDRAWAL_INFO,
  type WithdrawalsBlockFixture,
} from "./fabricated-withdrawal.build-withdrawals-block-fixture.js";
import {
  historyLucid,
  historyOpening,
  rootHistoryUtxo,
  withdrawalEventDatum,
  withdrawalEventUtxoFixture,
} from "./fabricated-withdrawal.q40-fabricated-withdrawal-evidence-admission.js";
import { h28, h32 } from "./helpers/canonical-block-evidence-fixture.js";

describe("fabricated-withdrawal production evidence authority", () => {
  const canonicalEvidence = async (
    fixture: WithdrawalsBlockFixture,
  ): Promise<CanonicalBlockEvidence> => {
    const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload(
      {
        observation: fixture.observation,
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
        minimumConfirmationDepth: 1,
      },
    );
    const withdrawals = evidence.entries.map(([keyCbor, valueCbor]) => ({
      key: Data.from(keyCbor, SDK.OutputReference),
      value: Data.from(valueCbor, SDK.WithdrawalInfo),
      keyBytes: Buffer.from(keyCbor, "hex"),
      valueBytes: Buffer.from(valueCbor, "hex"),
    }));
    return {
      ...evidence,
      observation: fixture.observation,
      header: fixture.header,
      reconstruction: { withdrawals },
    } as unknown as CanonicalBlockEvidence;
  };

  it.each([false, true])(
    "uses the governed withdrawal spending address for ordinary lookup and readmission (stake credential: %s)",
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
        withdrawal_addr: await Effect.runPromise(
          SDK.addressDataFromBech32(eventAddress),
        ),
      };
      const hub = {
        ...hubOracleUtxoFixture(),
        address: hubAddress,
        datum: Data.to(hubDatum, SDK.HubOracleDatum),
        assets: { lovelace: 5_000_000n, [hubUnit]: 1n },
      };
      const event = { ...withdrawalEventUtxoFixture(), address: eventAddress };
      const eventUnit = toUnit(
        hubDatum.withdrawal,
        NONCE_AUTHENTIC_WITHDRAWAL_ID,
      );
      const mintPolicyAddress = credentialToAddress("Preview", {
        type: "Script",
        hash: hubDatum.withdrawal,
      });
      expect(eventAddress).not.toBe(mintPolicyAddress);
      expect(event.assets[eventUnit]).toBe(1n);
      let liveEventAddress = eventAddress;
      const queries: string[] = [];
      const authority = createFabricatedWithdrawalEvidenceAuthority({
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
      const ordinary = await buildWithdrawalsBlockFixture({
        leaves: [
          {
            key: KEY_AUTHENTIC_WITHDRAWAL_ID,
            value: VALUE_AUTHENTIC_WITHDRAWAL_INFO,
          },
        ],
      });
      const ordinaryEvidence = await canonicalEvidence(ordinary);
      await expect(
        authority.detect(ordinaryEvidence, h28(0x44)),
      ).resolves.toEqual([]);
      // Reuse the existing mismatch case solely to exercise persisted-event lookup.
      const existing = await buildWithdrawalsBlockFixture({
        leaves: [MM_LEAF],
      });
      const detections = await authority.detect(
        await canonicalEvidence(existing),
        h28(0x44),
      );
      expect(detections).toHaveLength(1);
      expect(detections[0]!.artifact.authenticContent.openingCbor).toBe(
        historyOpening(),
      );
      expect(detections[0]!.artifact.authenticContent.openingCbor).not.toBe(
        event.datum,
      );
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
      liveEventAddress = eventAddress;
      const altered = withdrawalEventDatum();
      altered.event.info.body.l2_owner = h28(0xe3);
      event.datum = withdrawalEventUtxoFixture({ datum: altered }).datum;
      await expect(authority.readmit(detections[0]!.artifact)).rejects.toThrow(
        "History facts changed before capture",
      );
    },
  );

  it("roundtrips an authenticated list-absence fault through journal normalization", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [FI_LEAF] });
    const authority = createFabricatedWithdrawalEvidenceAuthority({
      history: historyEnvironment,
      lucid: historyLucid([rootHistoryUtxo()]),
      network: "Preview",
      hubOraclePolicyId: h28(0x16),
      minimumConfirmationDepth: 1,
    });
    const detections = await authority.detect(
      await canonicalEvidence(fixture),
      h28(0x44),
    );
    expect(detections).toHaveLength(1);
    expect(detections[0]!.detection.violationId).toBe("fabricated-withdrawal");
    expect(detections[0]!.artifact.l1Evidence).toEqual({
      kind: "absent_identity",
      historyOutRef: `${h32(0xa3)}#0`,
      retainedDataOutRef: null,
    });
    const readmitted = await authority.readmit(
      JSON.parse(JSON.stringify(normalizeJournalJson(detections[0]!.artifact))),
    );
    expect(
      requireFabricatedWithdrawalArtifact(
        readmitted,
        h28(0x44),
        fixture.headerHash,
      ),
    ).toBe(readmitted);
    await expect(
      authority.readmit(
        normalizeJournalJson({
          ...readmitted,
          withdrawalInclusion: {
            ...readmitted.withdrawalInclusion,
            withdrawalsPhasRoot: h32(0x77),
          },
        }),
      ),
    ).rejects.toThrow(/digest mismatch/u);
    await expect(
      authority.readmit({ ...readmitted, withdrawalIndex: 1 }),
    ).rejects.toThrow(/digest mismatch/u);
    expect(() =>
      requireFabricatedWithdrawalArtifact(
        { ...readmitted },
        h28(0x44),
        fixture.headerHash,
      ),
    ).toThrow(/not re-authenticated/u);
  });
});
