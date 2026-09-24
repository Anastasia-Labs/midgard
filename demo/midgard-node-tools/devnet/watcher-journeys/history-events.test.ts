import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as canonical,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { canonicalBlockEvidenceFromVerifiedPayload } from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { prepareFabricatedDepositFromCommittedLeaves } from "@al-ft/midgard-fault-proofs/test-support/prepare-fabricated-deposit";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  datumToHash,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import {
  buildJourneyFabricatedDeposit,
  buildJourneyRepeatedWithdrawal,
  buildJourneyWithdrawalEvent,
  captureStagedHistoryEvent,
} from "./history-events.js";

// Pure content fixtures: these checks do not claim a deployed chain, a new L1
// observation, or live journey acceptance. Both carriage modes use the shared reader.
const fixture = async (external: boolean, rawDatum?: string) => {
  const policyId = "ab".repeat(28);
  const hubPolicyId = "cd".repeat(28);
  const address = credentialToAddress("Custom", {
    type: "Script",
    hash: policyId,
  });
  const ownerAddress = credentialToAddress("Custom", {
    type: "Key",
    hash: "12".repeat(28),
  });
  const owner = await Effect.runPromise(
    SDK.addressDataFromBech32(ownerAddress),
  );
  const event: SDK.DepositEvent = {
    id: { transactionId: "34".repeat(32), outputIndex: 0n },
    info: {
      l2_address: owner,
      l2_network_id: 0n,
      l2_datum: rawDatum === undefined ? null : "fa".repeat(40),
    },
  };
  const key = await Effect.runPromise(SDK.eventHistoryKey(event.id));
  const payload: SDK.EventHistoryPayload = { DepositPayload: { event } };
  const retained: SDK.EventHistoryData = {
    event_key: key,
    event_payload: Data.from(Data.to(payload, SDK.EventHistoryPayload)),
    reclaim_auth: owner.paymentCredential,
  };
  const payloadCbor = canonical(
    rawDatum === undefined
      ? Data.to(payload, SDK.EventHistoryPayload)
      : Data.to(payload, SDK.EventHistoryPayload).replace(
          Data.to("fa".repeat(40)),
          rawDatum,
        ),
  );
  const retainedCbor = canonical(
    replacePlutusConstrFieldCbor(
      SDK.encodeEventHistoryData(retained),
      [1],
      payloadCbor,
    ),
  );
  const deployment = {
    policyId,
    address,
    retentionAddress: "retention-fixture",
    inlineLimitBytes: 512n,
  };
  const root: UTxO = {
    txHash: "01".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 3_000_000n, [policyId]: 1n },
    datum: Data.to(
      {
        position: "Root",
        next: key,
        protected_until: 0n,
        payload: "RootContent",
      },
      SDK.EventHistoryNode,
    ),
  };
  const orderOutput: UTxO = {
    txHash: "02".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 10_000_000n, [policyId + key]: 1n },
    datum: Data.to(
      {
        position: { Key: [key] },
        next: null,
        protected_until: 0n,
        payload: {
          Order: {
            facts: {
              event_id: event.id,
              inclusion_time: 150n,
              location: external
                ? {
                    External: {
                      storage_datum_hash: datumToHash(retainedCbor),
                    },
                  }
                : { Inline: { payload } },
              structural_lovelace: 2_000_000n,
              structural_refund_key: "12".repeat(28),
            },
          },
        },
      },
      SDK.EventHistoryNode,
    ),
  };
  if (!external)
    orderOutput.datum = replacePlutusConstrFieldCbor(
      orderOutput.datum!,
      [3, 0, 2, 0],
      payloadCbor,
    );
  const retainedDataUtxo: UTxO = {
    txHash: "03".repeat(32),
    outputIndex: 0,
    address: deployment.retentionAddress,
    assets: { lovelace: 3_000_000n },
    datum: retainedCbor,
  };
  const [order] = await Effect.runPromise(
    SDK.utxosToDepositUTxOs(
      [root, orderOutput],
      external ? [retainedDataUtxo] : [],
      deployment,
    ),
  );
  const predecessor = await buildCanonicalBlockFixture({
    transactions: [],
    startTime: 0n,
    endTime: 100n,
  });
  const scriptAddress = await Effect.runPromise(
    SDK.addressDataFromBech32(address),
  );
  const hubDatum: SDK.HubOracleDatum = {
    registered_operators: policyId,
    active_operators: policyId,
    retired_operators: policyId,
    scheduler: policyId,
    state_queue: policyId,
    fraud_proof_catalogue: policyId,
    fraud_proof: policyId,
    deposit: policyId,
    withdrawal: policyId,
    tx_order: policyId,
    settlement: policyId,
    payout: policyId,
    registered_operators_addr: scriptAddress,
    active_operators_addr: scriptAddress,
    retired_operators_addr: scriptAddress,
    scheduler_addr: scriptAddress,
    state_queue_addr: scriptAddress,
    fraud_proof_catalogue_addr: scriptAddress,
    fraud_proof_addr: scriptAddress,
    deposit_addr: scriptAddress,
    withdrawal_addr: scriptAddress,
    tx_order_addr: scriptAddress,
    settlement_addr: scriptAddress,
    payout_addr: scriptAddress,
    reserve_addr: scriptAddress,
    reserve_observer: policyId,
  };
  const hubOracleUtxo: UTxO = {
    txHash: "04".repeat(32),
    outputIndex: 0,
    address: credentialToAddress("Custom", {
      type: "Script",
      hash: hubPolicyId,
    }),
    assets: {
      lovelace: 3_000_000n,
      [hubPolicyId + SDK.HUB_ORACLE_ASSET_NAME]: 1n,
    },
    datum: Data.to(hubDatum, SDK.HubOracleDatum),
  };
  return {
    root,
    orderOutput,
    order: order!,
    policyId,
    predecessor,
    hubPolicyId,
    hubOracleUtxo,
    deployment,
    retainedDataUtxo,
  };
};

it.each([false, true])(
  "binds diverted deposit content to authenticated history (external=%s) and refuses its honest control",
  async (external) => {
    const f = await fixture(external);
    expect(f.order.originalAssets).toEqual({ lovelace: 8_000_000n });
    for (const honest of [false, true]) {
      const block = await buildJourneyFabricatedDeposit({
        predecessor: f.predecessor,
        operatorVkey: "56".repeat(28),
        endTime: 200n,
        blockSlot: 0n,
        deposit: { order: f.order, policyId: f.policyId },
        honest,
      });
      await canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(block),
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        daProvenance: {
          trustClass: "public_or_permissionless_da",
          sourceId: "history-content/unit-fixture",
          grade: "security",
        },
      });
      const proof = prepareFabricatedDepositFromCommittedLeaves({
        headerHash: block.headerHash,
        committedDepositsRoot: block.header.depositsRoot,
        depositCount: block.header.depositCount,
        headerStartTime: block.header.startTime,
        headerEndTime: block.header.endTime,
        entries: block.payload.block_body.deposits,
        witness: {
          observation: authenticatedHeaderObservation(block),
          hubOraclePolicyId: f.hubPolicyId,
          hubOracleUtxo: f.hubOracleUtxo,
          network: "Custom",
          history: {
            retentionAddress: f.deployment.retentionAddress,
            inlineLimitBytes: 512n,
            maxPayloadBytes: 5000n,
            maxPayloadNodes: 512n,
          },
          anchor: f.order.utxo,
          ...(external ? { retainedDataUtxo: f.retainedDataUtxo } : {}),
        },
        minimumConfirmationDepth: 30,
      });
      if (honest) await expect(proof).rejects.toThrow();
      else expect((await proof).headerHash).toBe(block.headerHash);
    }
  },
);

it("refuses an out-of-window event and preserves captured original Value through plain JSON", async () => {
  const f = await fixture(false);
  await expect(
    buildJourneyFabricatedDeposit({
      predecessor: f.predecessor,
      operatorVkey: "56".repeat(28),
      endTime: 120n,
      blockSlot: 0n,
      deposit: { order: f.order, policyId: f.policyId },
      honest: true,
    }),
  ).rejects.toThrow("outside the committed block interval");
  const captured = JSON.parse(
    JSON.stringify(
      captureStagedHistoryEvent({ order: f.order, policyId: f.policyId }),
    ),
  );
  const opening = Data.from(captured.openingCbor, SDK.EventHistoryOpening);
  expect(SDK.valueToAssets(opening.original_assets)).toEqual({
    lovelace: 8_000_000n,
  });
});

it.each([false, true])(
  "preserves raw deposit capture and honest leaves with repeated map pairs (external=%s)",
  async (external) => {
    const rawDatum = "a302a20102010301000102";
    const f = await fixture(external, rawDatum);
    const input = { order: f.order, policyId: f.policyId };
    const captured = JSON.parse(
      JSON.stringify(captureStagedHistoryEvent(input)),
    );
    const payload = canonical(plutusConstrFieldCbor(captured.openingCbor, [0]));
    expect(payload).toBe(f.order.history.payloadCbor);
    expect(payload).toContain(rawDatum);
    expect(
      SDK.opensEventHistoryCommitmentCbor(
        Data.from(captured.commitmentCbor, SDK.EventHistoryCommitment),
        payload,
        plutusConstrFieldCbor(captured.openingCbor, [1]),
      ),
    ).toBe(true);
    const block = await buildJourneyFabricatedDeposit({
      predecessor: f.predecessor,
      operatorVkey: "56".repeat(28),
      endTime: 200n,
      blockSlot: 0n,
      deposit: input,
      honest: true,
    });
    expect(block.payload.block_body.deposits[0]?.[1]).toBe(
      f.order.infoCbor.toString("hex"),
    );
    expect(
      block.payload.block_body.utxos.some(([, output]) =>
        output.includes(rawDatum),
      ),
    ).toBe(true);
    expect(() =>
      captureStagedHistoryEvent({
        ...input,
        order: {
          ...f.order,
          infoCbor: Buffer.from(
            f.order.infoCbor
              .toString("hex")
              .replace(rawDatum, "a302a20102010901000102"),
            "hex",
          ),
        },
      }),
    ).toThrow("content differs");
  },
);

it.each([false, true])(
  "preserves withdrawal capture, address edits and repeated raw leaves (external=%s)",
  async (external) => {
    const f = await fixture(external);
    const marker = "fa".repeat(40);
    const rawDatum = "a302a20102010301000102";
    const payload: SDK.EventHistoryPayload = {
      WithdrawalPayload: {
        event: {
          id: f.order.event.id,
          info: {
            body: {
              l2_outref: f.order.event.id,
              l2_owner: "12".repeat(28),
              l2_value: new Map(),
              l1_address: f.order.event.info.l2_address,
              l1_datum: { InlineDatum: { data: marker } },
            },
            signature: ["aa", "bb"],
            validity: "NonExistentWithdrawalUtxo",
          },
        },
        refund_address: f.order.event.info.l2_address,
        refund_datum: "NoDatum",
      },
    };
    const rawPayload = canonical(
      Data.to(payload, SDK.EventHistoryPayload).replace(
        Data.to(marker),
        rawDatum,
      ),
    );
    const retained = replacePlutusConstrFieldCbor(
      f.retainedDataUtxo.datum!,
      [1],
      rawPayload,
    );
    const node = Data.from(f.orderOutput.datum!, SDK.EventHistoryNode);
    if (node.payload === "RootContent" || !("Order" in node.payload))
      throw new Error("Expected Order");
    node.payload.Order.facts.location = external
      ? { External: { storage_datum_hash: datumToHash(canonical(retained)) } }
      : { Inline: { payload } };
    const datum = external
      ? Data.to(node, SDK.EventHistoryNode)
      : replacePlutusConstrFieldCbor(
          Data.to(node, SDK.EventHistoryNode),
          [3, 0, 2, 0],
          rawPayload,
        );
    const [order] = await Effect.runPromise(
      SDK.utxosToWithdrawalUTxOs(
        [f.root, { ...f.orderOutput, datum }],
        external ? [{ ...f.retainedDataUtxo, datum: retained }] : [],
        f.deployment,
      ),
    );
    const staged = { order: order!, policyId: f.policyId };
    const captured = JSON.parse(
      JSON.stringify(captureStagedHistoryEvent(staged)),
    );
    expect(canonical(plutusConstrFieldCbor(captured.openingCbor, [0]))).toBe(
      rawPayload,
    );
    const base = {
      predecessor: f.predecessor,
      operatorVkey: "56".repeat(28),
      endTime: 200n,
      blockSlot: 0n,
    };
    const honest = await buildJourneyWithdrawalEvent({
      ...base,
      category: "withdrawalMistag",
      withdrawals: [staged],
      honest: true,
    });
    expect(honest.payload.block_body.withdrawals[0]?.[1]).toBe(
      order!.infoCbor.toString("hex"),
    );
    const altered = await buildJourneyWithdrawalEvent({
      ...base,
      category: "fabricatedWithdrawal",
      withdrawals: [staged],
    });
    expect(
      plutusConstrFieldCbor(
        altered.payload.block_body.withdrawals[0]![1],
        [0, 4],
      ),
    ).toBe(plutusConstrFieldCbor(order!.infoCbor.toString("hex"), [0, 4]));
    const repeated = await buildJourneyRepeatedWithdrawal({
      ...base,
      predecessor: honest,
      settled: honest,
      endTime: 300n,
    });
    expect(repeated.payload.block_body.withdrawals).toEqual(
      honest.payload.block_body.withdrawals,
    );
  },
);
