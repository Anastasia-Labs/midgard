import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { type WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { WatcherLocalKupmiosNativeObservation } from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import { h32 } from "../support/deployment-authority-fixture.js";
import {
  fixture,
  headerFixture,
  RELEASE_DEPTH,
  RELEASE_DEPTH_TEXT,
  value,
} from "./authenticated-state-queue-observation.fixture.js";

export const appendFixture = ({
  initial,
  previous,
  nodeHeader = headerFixture(),
  assetHeaderHash,
  daAvailability = "Unattested",
}: {
  initial: ReturnType<typeof fixture>;
  previous: WatcherAuthenticatedStateQueueObservation;
  nodeHeader?: SDK.Header;
  assetHeaderHash?: string;
  daAvailability?: SDK.DaAvailabilityStateQueueStatus;
}) => {
  const stateQueueAddress = credentialToAddress(
    initial.authority.network,
    scriptHashToCredential(
      initial.authority.protocolScriptHashes.stateQueueSpend,
    ),
  );
  const headerCborHex = Data.to(nodeHeader, SDK.Header);
  const computedHeaderHash = computeHash28(
    Buffer.from(headerCborHex, "hex"),
  ).toString("hex");
  const nodeAssetHeaderHash = assetHeaderHash ?? computedHeaderHash;
  const stateQueueNode: SDK.StateQueueNode = {
    proven_fraud: null,
    header: nodeHeader,
    da_attestation: daAvailability,
  };
  const rootDatumCborHex = Data.to(
    SDK.nodeViewToLinkedListDatum({
      key: "Empty",
      next: { Key: { key: nodeAssetHeaderHash } },
      data: Data.to([]),
    }),
    SDK.LinkedListDatum,
  );
  const nodeDatumCborHex = Data.to(
    SDK.nodeViewToLinkedListDatum({
      key: { Key: { key: nodeAssetHeaderHash } },
      next: "Empty",
      data: Data.castTo(stateQueueNode, SDK.StateQueueNode),
    }),
    SDK.LinkedListDatum,
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(stateQueueAddress),
      value(
        initial.authority.protocolScriptHashes.stateQueueMint,
        SDK.STATE_QUEUE_ROOT_ASSET_NAME,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(rootDatumCborHex)),
      undefined,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(stateQueueAddress),
      value(
        initial.authority.protocolScriptHashes.stateQueueMint,
        `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${nodeAssetHeaderHash}`,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(nodeDatumCborHex)),
      undefined,
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(initial.raw.txHash),
      0n,
    ),
  );
  const references = CML.TransactionInputList.new();
  references.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(initial.raw.txHash),
      1n,
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  body.set_reference_inputs(references);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(
      initial.authority.protocolScriptHashes.stateQueueMint,
    ),
    CML.AssetName.from_hex(
      `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${nodeAssetHeaderHash}`,
    ),
    1n,
  );
  body.set_mint(mint);
  const redeemerCbor = Data.to(
    {
      CommitBlockHeader: {
        yield_to_ref_input_index: 0n,
        new_block_output_index: 1n,
        continued_latest_block_output_index: 0n,
        operator: nodeHeader.operatorVkey,
        scheduler_ref_input_index: 0n,
        active_operators_input_index: 0n,
        active_operators_redeemer_index: 0n,
        m_confirmed_state_ref_input_index: null,
        m_head_state_queue_node_ref_input_index: null,
      },
    },
    SDK.StateQueueRedeemer,
  );
  const canonicalRedeemerCbor =
    CML.PlutusData.from_cbor_hex(redeemerCbor).to_canonical_cbor_hex();
  const witnessSet = CML.TransactionWitnessSet.new();
  const witnessRedeemers = CML.LegacyRedeemerList.new();
  witnessRedeemers.add(
    CML.LegacyRedeemer.new(
      CML.RedeemerTag.Mint,
      0n,
      CML.PlutusData.from_cbor_hex(redeemerCbor),
      CML.ExUnits.new(0n, 0n),
    ),
  );
  witnessSet.set_redeemers(
    CML.Redeemers.new_arr_legacy_redeemer(witnessRedeemers),
  );
  const bodyCbor = body.to_canonical_cbor_hex();
  const witnessSetCbor = witnessSet.to_canonical_cbor_hex();
  const txHash = CML.hash_transaction(body).to_hex();
  const transaction = CML.Transaction.new(body, witnessSet, true, undefined);
  const blockHash = h32("a2");
  const appendPoint = Object.freeze({
    blockHash,
    blockNo: "101",
    slot: "1001",
    pointId: computeFraudProofRawL1PointId({
      blockHash,
      blockNo: "101",
      slot: "1001",
    }),
  });
  const nativeBlock = Object.freeze({
    blockHash,
    blockNo: appendPoint.blockNo,
    blockType: "7",
    prevHash: initial.nativeBlock.blockHash,
    protocolMajor: "9",
    rawBlockCbor: "80",
    rawHeaderCbor: "80",
    schemaVersion: "midgard-watcher-native-block-admission-v1",
    slot: appendPoint.slot,
    transactionIds: Object.freeze([txHash]),
    transactionCbors: Object.freeze([transaction.to_canonical_cbor_hex()]),
  }) as WatcherNativeBlockAdmission;
  const localObservation = {
    block: {
      chainPoint: {
        blockHash,
        blockNo: appendPoint.blockNo,
        chainPointId: h32("b3"),
        depth: RELEASE_DEPTH_TEXT,
        parentBlockHash: nativeBlock.prevHash,
        pointDigest: h32("b4"),
        slot: appendPoint.slot,
      },
      transactions: [
        {
          txHash,
          body: { bytesHex: bodyCbor },
          witnessSet: { bytesHex: witnessSetCbor },
          redeemers: [
            {
              purpose: "mint",
              index: "0",
              bytes: { bytesHex: canonicalRedeemerCbor },
            },
          ],
        },
      ],
    },
  } as unknown as WatcherLocalKupmiosNativeObservation;
  const initialBody = CML.TransactionBody.from_cbor_hex(initial.raw.bodyCbor);
  const raw = Object.freeze({
    txHash,
    bodyCbor,
    witnessSetCbor,
    redeemersCbor: witnessSet.redeemers()!.to_canonical_cbor_hex(),
    isValid: true,
    inclusionPoint: appendPoint,
    confirmationDepth: RELEASE_DEPTH,
    resolvedInputs: Object.freeze([
      {
        outRef: `${initial.raw.txHash}#0`,
        outputCbor: initialBody.outputs().get(0).to_canonical_cbor_hex(),
        datumCbor: initialBody
          .outputs()
          .get(0)
          .datum()!
          .as_datum()!
          .to_canonical_cbor_hex(),
        referenceScriptCbor: null,
      },
    ]),
    resolvedReferenceInputs: Object.freeze([
      {
        outRef: `${initial.raw.txHash}#1`,
        outputCbor: initialBody.outputs().get(1).to_canonical_cbor_hex(),
        datumCbor: initialBody
          .outputs()
          .get(1)
          .datum()!
          .as_datum()!
          .to_canonical_cbor_hex(),
        referenceScriptCbor: null,
      },
    ]),
  }) satisfies FraudProofRawL1Transaction;
  return {
    nativeBlock,
    localObservation,
    raw,
    previous,
    stateQueueNodeCborHex: Data.to(stateQueueNode, SDK.StateQueueNode),
    linkedListDatumCborHex: body
      .outputs()
      .get(1)
      .datum()!
      .as_datum()!
      .to_canonical_cbor_hex(),
    headerCborHex,
    headerHash: computedHeaderHash,
  };
};
