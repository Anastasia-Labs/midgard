import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
} from "@al-ft/midgard-core";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { captureTransitionTraceL1Events } from "../../src/transition-trace/l1-events.js";
import {
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
} from "../../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import { TRANSITION_HISTORY_FIXTURE_PARAMETERS } from "../helpers/transition-history-fixture.js";
import { buildRetainedPlutusIdentityFixture } from "./retained-reason-classifier.build-retained-plutus-fixture.js";

/** Unit-only raw transaction fixture, admitted through the real snapshot owner. */
export const captureRetainedPlutusIdentityOrigins = async (
  fixture: Pick<
    Awaited<ReturnType<typeof buildRetainedPlutusIdentityFixture>>,
    "block" | "transaction" | "orderKey"
  >,
  options: Readonly<{
    omitEvent?: boolean;
    advanceCapture?: boolean;
    inclusionTime?: bigint;
  }> = {},
) => {
  const hubPolicy = "61".repeat(28);
  const depositPolicy = "62".repeat(28);
  const withdrawalPolicy = "63".repeat(28);
  const orderPolicy = "64".repeat(28);
  const dummy = "65".repeat(28);
  const addressData = (hash: string) => ({
    paymentCredential: { ScriptCredential: [hash] as [string] },
    stakeCredential: null,
  });
  const address = (hash: string) =>
    credentialToAddress("Preprod", scriptHashToCredential(hash));
  const hub: SDK.HubOracleDatum = {
    registered_operators: dummy,
    active_operators: dummy,
    retired_operators: dummy,
    scheduler: dummy,
    state_queue: dummy,
    fraud_proof_catalogue: dummy,
    fraud_proof: dummy,
    deposit: depositPolicy,
    withdrawal: withdrawalPolicy,
    tx_order: orderPolicy,
    settlement: dummy,
    payout: dummy,
    registered_operators_addr: addressData(dummy),
    active_operators_addr: addressData(dummy),
    retired_operators_addr: addressData(dummy),
    scheduler_addr: addressData(dummy),
    state_queue_addr: addressData(dummy),
    fraud_proof_catalogue_addr: addressData(dummy),
    fraud_proof_addr: addressData(dummy),
    deposit_addr: addressData(depositPolicy),
    withdrawal_addr: addressData(withdrawalPolicy),
    tx_order_addr: addressData(orderPolicy),
    settlement_addr: addressData(dummy),
    reserve_addr: addressData(dummy),
    payout_addr: addressData(dummy),
    reserve_observer: dummy,
  };
  const submitted = deriveMidgardForcedTxProofSource(
    decodeMidgardNativeTxFullFromCanonicalCbor(
      fixture.transaction.canonicalCbor,
    ),
  );
  const order: SDK.TxOrderDatum = {
    event: {
      id: fixture.orderKey,
      tx: {
        tx_id: fixture.transaction.txId,
        transaction_commitment:
          computeMidgardForcedTxProofCommitment(submitted).toString("hex"),
        submitted_source: {
          compact_cbor: submitted.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            submitted.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            submitted.fieldPreimageLengthsCbor.toString("hex"),
        },
      },
    },
    inclusion_time: options.inclusionTime ?? 1_749_999_999_000n,
    witness: dummy,
    refund_address: addressData(dummy),
    refund_datum: "NoDatum",
  };
  const hubUnit = hubPolicy + SDK.HUB_ORACLE_ASSET_NAME;
  const orderUnit = orderPolicy + "01";
  const entries = [
    {
      address: address(hubPolicy),
      unit: hubUnit,
      datumCbor: Data.to(hub, SDK.HubOracleDatum),
    },
    ...(options.omitEvent
      ? []
      : [
          {
            address: address(orderPolicy),
            unit: orderUnit,
            datumCbor: Data.to(order, SDK.TxOrderDatum),
          },
        ]),
  ];
  const outputs = CML.TransactionOutputList.new();
  const mint = CML.Mint.new();
  for (const entry of entries) {
    const assets = CML.MultiAsset.new();
    const policy = CML.ScriptHash.from_hex(entry.unit.slice(0, 56));
    const name = CML.AssetName.from_hex(entry.unit.slice(56));
    assets.set(policy, name, 1n);
    mint.set(policy, name, 1n);
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(entry.address),
      CML.Value.new(3_000_000n, assets),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(entry.datumCbor)),
    );
    outputs.add(output);
  }
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("31".repeat(32)), 0n),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  body.set_mint(mint);
  const txHash = CML.hash_transaction(body).to_hex();
  const spent = CML.TransactionOutput.new(
    CML.Address.from_bech32(address(dummy)),
    CML.Value.from_coin(10_000_000n),
  ).to_canonical_cbor_hex();
  const point = (slot: string, blockNo: string, byte: string) => {
    const value = { slot, blockNo, blockHash: byte.repeat(32) };
    return { ...value, pointId: computeFraudProofRawL1PointId(value) };
  };
  const included = point("1070", "70", "41");
  const cursor = options.advanceCapture
    ? point("1072", "72", "44")
    : point("1071", "71", "42");
  const tip = options.advanceCapture
    ? point("1101", "101", "45")
    : point("1100", "100", "43");
  const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
  const finality = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: "d1".repeat(32),
    blueprintHash: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  };
  const authority: FraudProofRawL1SnapshotAuthority = {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request) =>
      ({
        schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
        deploymentIdentityDigest: request.deploymentIdentityDigest,
        blueprintHash: request.blueprintHash,
        finalityPolicyDigest: request.finalityPolicyDigest,
        headerHash: request.headerHash,
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId: "retained-origin-unit-fixture",
          grade: "security",
          sourceMode: "local_chain_follower",
          boundaryPoint: cursor,
          tipPoint: tip,
        },
        cursor: {
          point: cursor,
          tip,
          confirmationDepth: 30,
          rollbackCursor: computeFraudProofRawL1RollbackCursor({
            ...request,
            sourceId: "retained-origin-unit-fixture",
            pointId: cursor.pointId,
          }),
        },
        scopes: request.scopes.map((scope) => ({
          ...scope,
          utxos: entries.flatMap((entry, index) =>
            entry.address === scope.address
              ? [
                  {
                    outRef: txHash + "#" + index.toString(),
                    outputCbor: outputs.get(index).to_canonical_cbor_hex(),
                    datumCbor: CML.PlutusData.from_cbor_hex(
                      entry.datumCbor,
                    ).to_canonical_cbor_hex(),
                    referenceScriptCbor: null,
                  },
                ]
              : [],
          ),
        })),
        historyUnits: request.historyUnits,
        history: request.historyUnits.map((unit) => ({
          unit,
          fromGenesis: true,
          completeThroughPointId: cursor.pointId,
          transactionHashes: [txHash],
        })),
        transactions: [
          {
            txHash,
            bodyCbor: body.to_cbor_hex(),
            witnessSetCbor:
              CML.TransactionWitnessSet.new().to_canonical_cbor_hex(),
            redeemersCbor: null,
            isValid: true,
            inclusionPoint: included,
            confirmationDepth: options.advanceCapture ? 32 : 31,
            resolvedInputs: [
              {
                outRef: "31".repeat(32) + "#0",
                outputCbor: spent,
                datumCbor: null,
                referenceScriptCbor: null,
              },
            ],
            resolvedReferenceInputs: [],
          },
        ],
      }) satisfies FraudProofRawL1Snapshot,
  };
  // Only fields consumed by raw event capture are supplied; this does not claim
  // that a deployment manifest or an actual Cardano transaction was admitted.
  const binding = {
    network: "Preprod",
    releaseFinality: finality,
    resolvedContracts: {
      hubOraclePolicyId: hubPolicy,
      contracts: {
        transitionTrace: { history: TRANSITION_HISTORY_FIXTURE_PARAMETERS },
      },
    },
    definition: { headerHash: fixture.block.headerHash },
  } as Parameters<typeof captureTransitionTraceL1Events>[0]["binding"];
  return captureTransitionTraceL1Events({ binding, authority });
};
