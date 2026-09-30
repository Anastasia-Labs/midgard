import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  fabricatedDepositBlockEvidenceFromVerifiedPayload,
  type FabricatedDepositL1Witness,
} from "../src/prepare-fabricated-deposit.js";
import {
  AUTHENTIC_DEPOSIT_ID,
  AUTHENTIC_INCLUSION_TIME,
  buildDepositsBlockFixture,
  DA_PROVENANCE,
  DATUM_AUTHENTIC_DEPOSIT_EVENT,
  DEPOSIT_POLICY_ID,
  FABRICATED_DEPOSIT_ID,
  FI_HEADER_HASH,
  FI_LEAF,
  HASH_AUTHENTIC_DEPOSIT_INFO,
  HASH_DIVERTED_DEPOSIT_INFO,
  HEADER_END_TIME,
  HEADER_START_TIME,
  historyWitness,
  hubOracleUtxoFixture,
  KEY_AUTHENTIC_DEPOSIT_ID,
  MM_DEPOSITS_ROOT,
  MM_HEADER_HASH,
  MM_LEAF,
  NONCE_AUTHENTIC_DEPOSIT_ID,
  syntheticUtxo,
  VALUE_DIVERTED_DEPOSIT_INFO,
} from "./fabricated-deposit.build-deposits-block-fixture.js";
import { h28 } from "./helpers/canonical-block-evidence-fixture.js";

export const presentEventWitness = ({
  observedEventAssetName = NONCE_AUTHENTIC_DEPOSIT_ID,
  eventDatumCbor = DATUM_AUTHENTIC_DEPOSIT_EVENT,
}: {
  readonly observedEventAssetName?: string;
  readonly eventDatumCbor?: string;
} = {}): FabricatedDepositL1Witness =>
  historyWitness(
    depositEventUtxoFixture({
      assetName: observedEventAssetName,
      datum: Data.from(eventDatumCbor, SDK.DepositDatum),
    }),
  );

/** A canonical `DepositDatum` with an arbitrary identity, content and window. */
export const depositEventDatum = ({
  id = AUTHENTIC_DEPOSIT_ID,
  paymentKeyByte = 0x1c,
  inclusionTime = AUTHENTIC_INCLUSION_TIME,
}: {
  readonly id?: SDK.OutputReference;
  readonly paymentKeyByte?: number;
  readonly inclusionTime?: bigint;
} = {}): SDK.DepositDatum => ({
  event: {
    id,
    info: {
      l2_address: {
        paymentCredential: {
          PublicKeyCredential: [h28(paymentKeyByte)] as [string],
        },
        stakeCredential: null,
      },
      l2_network_id: 0n,
      l2_datum: null,
    },
  },
  inclusion_time: inclusionTime,
  witness: h28(0x57),
});

export const depositEventUtxoFixture = ({
  policyId = DEPOSIT_POLICY_ID,
  assetName = NONCE_AUTHENTIC_DEPOSIT_ID,
  datum = depositEventDatum(),
}: {
  readonly policyId?: string;
  readonly assetName?: string;
  readonly datum?: SDK.DepositDatum;
} = {}): UTxO => ({
  ...syntheticUtxo({
    txIdByte: 0xa2,
    outputIndex: 1,
    datum: Data.to(
      {
        position: { Key: [NONCE_AUTHENTIC_DEPOSIT_ID] },
        next: null,
        protected_until: 0n,
        payload: {
          Order: {
            facts: {
              event_id: datum.event.id,
              inclusion_time: datum.inclusion_time,
              location: { Inline: { payload: historyPayload(datum) } },
              structural_lovelace: 2_000_000n,
              structural_refund_key: h28(0x44),
            },
          },
        },
      },
      SDK.EventHistoryNode,
    ),
    assets: { [toUnit(policyId, assetName)]: 1n },
  }),
  address: credentialToAddress("Preview", {
    type: "Script",
    hash: DEPOSIT_POLICY_ID,
  }),
});

/** Read-only public-output fixture. A nonce lookup must never authorize absence. */
export const historyLucid = (nodes: readonly UTxO[]): LucidEvolution =>
  ({
    utxosByOutRef: async () => {
      throw new Error("Unexpected identity-nonce lookup");
    },
    utxosAtWithUnit: async (address: string, unit: string) => {
      const hub = hubOracleUtxoFixture();
      return hub.address === address && hub.assets[unit] === 1n ? [hub] : [];
    },
    utxosAt: async (address: string) =>
      nodes.filter((u) => u.address === address),
  }) as unknown as LucidEvolution;

// ## Measured-state twins for the submit-side handoffs
//
// Built from the Aiken constants rather than from a local block, so the step-04
// handoff bytes can be compared against the Aiken scenarios' exact CBOR.

export const historyAssets: SDK.Value = new Map([
  ["", new Map([["", 3_000_000n]])],
]);

const historyPayload = (datum: SDK.DepositDatum): SDK.EventHistoryPayload => ({
  DepositPayload: { event: datum.event },
});

export const historyOpening = (
  datum = depositEventDatum(),
  assets = historyAssets,
) =>
  Data.to(
    {
      RetainedEventData: {
        payload: historyPayload(datum),
        original_assets: assets,
      },
    },
    SDK.FabricatedDepositAuthenticContentOpening,
  );

export const historyCommitment = SDK.eventHistoryCommitment(
  "30".repeat(28),
  "Deposit",
  {
    event_id: AUTHENTIC_DEPOSIT_ID,
    inclusion_time: AUTHENTIC_INCLUSION_TIME,
  },
  historyPayload(depositEventDatum()),
  historyAssets,
);

export const fiStep03State: SDK.FabricatedDepositStep03State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: FI_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_deposit_id: FABRICATED_DEPOSIT_ID,
  committed_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
  verdict: "DepositIdentityAbsent",
};

export const mmStep03State: SDK.FabricatedDepositStep03State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: MM_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_deposit_id: AUTHENTIC_DEPOSIT_ID,
  committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
  verdict: {
    DepositEventObserved: {
      commitment: historyCommitment,
    },
  },
};

describe("Q39 fabricated-deposit evidence admission", () => {
  it("admits a deposits-bearing block and extracts its committed leaves", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayload({
      observation: fixture.observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.grade).toBe("security");
    expect(evidence.provenance.l1.trustClass).toBe("authenticated_cardano_l1");
    expect(evidence.provenance.da.trustClass).toBe(
      "public_or_permissionless_da",
    );
    expect(evidence.headerHash).toBe(fixture.headerHash);
    // The counted deposits_root of the MM scenario, measured in Aiken.
    expect(evidence.committedDepositsRoot).toBe(MM_DEPOSITS_ROOT);
    expect(evidence.depositCount).toBe(1n);
    expect(evidence.headerStartTime).toBe(HEADER_START_TIME);
    expect(evidence.headerEndTime).toBe(HEADER_END_TIME);
    expect(evidence.entries).toEqual([
      [KEY_AUTHENTIC_DEPOSIT_ID, VALUE_DIVERTED_DEPOSIT_INFO],
    ]);
  });

  it("refuses operator-private DA provenance", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    await expect(
      fabricatedDepositBlockEvidenceFromVerifiedPayload({
        observation: fixture.observation,
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: {
          trustClass: "operator_admin_api",
          sourceId: "node-admin",
          grade: "diagnostic",
          diagnosticLabel: "operator diagnostic",
        },
      }),
    ).rejects.toBeInstanceOf(SDK.CanonicalEvidenceRejection);
  });

  it("refuses a payload whose embedded header is not the observed one", async () => {
    const observed = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    const other = await buildDepositsBlockFixture({ leaves: [FI_LEAF] });
    expect(other.headerHash).not.toBe(observed.headerHash);
    await expect(
      fabricatedDepositBlockEvidenceFromVerifiedPayload({
        observation: observed.observation,
        payloadEnvelopeCbor: other.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toMatchObject({ code: "header_hash_mismatch" });
  });
});
