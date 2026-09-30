import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
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
  fabricatedWithdrawalBlockEvidenceFromVerifiedPayload,
  type FabricatedWithdrawalL1Witness,
} from "../src/prepare-fabricated-withdrawal.js";
import {
  AUTHENTIC_INCLUSION_TIME,
  AUTHENTIC_WITHDRAWAL_ID,
  buildWithdrawalsBlockFixture,
  DA_PROVENANCE,
  DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
  FABRICATED_WITHDRAWAL_ID,
  FI_HEADER_HASH,
  FI_LEAF,
  HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
  HASH_DIVERTED_WITHDRAWAL_CONTENT,
  HEADER_END_TIME,
  HEADER_START_TIME,
  historyWitness,
  hubOracleUtxoFixture,
  KEY_AUTHENTIC_WITHDRAWAL_ID,
  MM_HEADER_HASH,
  MM_LEAF,
  MM_WITHDRAWALS_ROOT,
  NONCE_AUTHENTIC_WITHDRAWAL_ID,
  syntheticUtxo,
  VALUE_DIVERTED_WITHDRAWAL_INFO,
  WITHDRAWAL_POLICY_ID,
} from "./fabricated-withdrawal.build-withdrawals-block-fixture.js";
import { h28 } from "./helpers/canonical-block-evidence-fixture.js";

export const rootHistoryUtxo = (): UTxO => ({
  ...syntheticUtxo({
    txIdByte: 0xa3,
    outputIndex: 0,
    datum: Data.to(
      {
        position: "Root",
        next: null,
        protected_until: 0n,
        payload: "RootContent",
      },
      SDK.EventHistoryNode,
    ),
    assets: { [WITHDRAWAL_POLICY_ID]: 1n },
  }),
  address: credentialToAddress("Preview", {
    type: "Script",
    hash: WITHDRAWAL_POLICY_ID,
  }),
});

export const absentIdentityWitness = (
  authenticated = true,
): FabricatedWithdrawalL1Witness => {
  const anchor = rootHistoryUtxo();
  return historyWitness(
    authenticated ? anchor : { ...anchor, assets: { lovelace: 5_000_000n } },
  );
};

export const presentEventWitness = ({
  observedEventAssetName = NONCE_AUTHENTIC_WITHDRAWAL_ID,
  eventDatumCbor = DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
}: {
  readonly observedEventAssetName?: string;
  readonly eventDatumCbor?: string;
} = {}): FabricatedWithdrawalL1Witness =>
  historyWitness(
    withdrawalEventUtxoFixture({
      assetName: observedEventAssetName,
      datum: Data.from(eventDatumCbor, SDK.WithdrawalOrderDatum),
    }),
  );

export const l1AddressOf = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] as [string] },
  stakeCredential: null,
});

/** `step-01.ak`'s `authentic_withdrawal_info_v1`, as typed data. */
export const authenticWithdrawalInfo = (): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: { transactionId: "7e".repeat(32), outputIndex: 1n },
    l2_owner: h28(0x9c),
    l2_value: new Map([
      [h28(0x4b), new Map([["6d6964676172642d746f6b656e", 42n]])],
    ]),
    l1_address: l1AddressOf(0x2b),
    l1_datum: "NoDatum",
  },
  signature: ["ad".repeat(32), "be".repeat(64)],
  validity: "WithdrawalIsValid",
});

/** A canonical withdrawal event datum with an arbitrary identity and window. */
export const withdrawalEventDatum = ({
  id = AUTHENTIC_WITHDRAWAL_ID,
  info = authenticWithdrawalInfo(),
  inclusionTime = AUTHENTIC_INCLUSION_TIME,
}: {
  readonly id?: SDK.OutputReference;
  readonly info?: SDK.WithdrawalInfo;
  readonly inclusionTime?: bigint;
} = {}): SDK.WithdrawalOrderDatum => ({
  event: { id, info },
  inclusion_time: inclusionTime,
  witness: h28(0x57),
  refund_address: l1AddressOf(0x2b),
  refund_datum: "NoDatum",
});

/** Legacy content fixture encoded canonically before conversion to a history payload. */
export const eventDatumBytes = (datum: SDK.WithdrawalOrderDatum): string =>
  SDK.withdrawalEventDatumBytes(datum);

export const withdrawalEventUtxoFixture = ({
  policyId = WITHDRAWAL_POLICY_ID,
  assetName = NONCE_AUTHENTIC_WITHDRAWAL_ID,
  datum = withdrawalEventDatum(),
}: {
  readonly policyId?: string;
  readonly assetName?: string;
  readonly datum?: SDK.WithdrawalOrderDatum;
} = {}): UTxO => ({
  ...syntheticUtxo({
    txIdByte: 0xa2,
    outputIndex: 1,
    datum: Data.to(
      {
        position: { Key: [NONCE_AUTHENTIC_WITHDRAWAL_ID] },
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
    hash: WITHDRAWAL_POLICY_ID,
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

const historyPayload = (
  datum: SDK.WithdrawalOrderDatum,
): SDK.EventHistoryPayload => ({
  WithdrawalPayload: {
    event: datum.event,
    refund_address: datum.refund_address,
    refund_datum: datum.refund_datum,
  },
});

export const historyOpening = (
  datum = withdrawalEventDatum(),
  assets = historyAssets,
) =>
  replacePlutusConstrFieldCbor(
    Data.to(
      {
        RetainedEventData: {
          payload: historyPayload(datum),
          original_assets: assets,
        },
      },
      SDK.FabricatedWithdrawalAuthenticContentOpening,
    ),
    [0],
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(historyPayload(datum), SDK.EventHistoryPayload),
    ),
  );

export const historyCommitment = SDK.eventHistoryCommitment(
  "30".repeat(28),
  "Withdrawal",
  {
    event_id: AUTHENTIC_WITHDRAWAL_ID,
    inclusion_time: AUTHENTIC_INCLUSION_TIME,
  },
  historyPayload(withdrawalEventDatum()),
  historyAssets,
);

export const fiStep03State: SDK.FabricatedWithdrawalStep03State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: FI_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: FABRICATED_WITHDRAWAL_ID,
  committed_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
  verdict: "WithdrawalIdentityAbsent",
};

export const mmStep03State: SDK.FabricatedWithdrawalStep03State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: MM_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
  committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
  verdict: {
    WithdrawalEventObserved: {
      commitment: historyCommitment,
    },
  },
};

describe("Q40 fabricated-withdrawal evidence admission", () => {
  it("admits a withdrawals-bearing block and extracts its committed leaves", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload(
      {
        observation: fixture.observation,
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      },
    );
    expect(evidence.grade).toBe("security");
    expect(evidence.provenance.l1.trustClass).toBe("authenticated_cardano_l1");
    expect(evidence.provenance.da.trustClass).toBe(
      "public_or_permissionless_da",
    );
    expect(evidence.headerHash).toBe(fixture.headerHash);
    // The counted withdrawals_root of the MM scenario, measured in Aiken.
    expect(evidence.committedWithdrawalsRoot).toBe(MM_WITHDRAWALS_ROOT);
    expect(evidence.withdrawalCount).toBe(1n);
    expect(evidence.headerStartTime).toBe(HEADER_START_TIME);
    expect(evidence.headerEndTime).toBe(HEADER_END_TIME);
    expect(evidence.entries).toEqual([
      [KEY_AUTHENTIC_WITHDRAWAL_ID, VALUE_DIVERTED_WITHDRAWAL_INFO],
    ]);
  });

  it("refuses operator-private DA provenance", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    await expect(
      fabricatedWithdrawalBlockEvidenceFromVerifiedPayload({
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
    const observed = await buildWithdrawalsBlockFixture({
      leaves: [MM_LEAF],
    });
    const other = await buildWithdrawalsBlockFixture({ leaves: [FI_LEAF] });
    expect(other.headerHash).not.toBe(observed.headerHash);
    await expect(
      fabricatedWithdrawalBlockEvidenceFromVerifiedPayload({
        observation: observed.observation,
        payloadEnvelopeCbor: other.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toMatchObject({ code: "header_hash_mismatch" });
  });
});
