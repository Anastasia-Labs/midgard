import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  createCostModels,
  credentialToAddress,
  Data,
  PROTOCOL_PARAMETERS_DEFAULT,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { watcherDeploymentProtocolScriptAuthority } from "../../src/runtime/deployment-identity.js";
import { h28, h32 } from "./deployment-authority-fixture.js";

// Transport-bound unit fixture only. Init/Commit shapes are copied from the
// ordinary SQ observation tests; no Plutus evaluation, wallet signature, Cardano
// consensus validity, emulator validity or public-chain inclusion is claimed.
// Native subprocess framing, TCP identity, Kupo/Ogmios byte agreement, creating
// transaction resolution and the SQ source's private admission all remain real.
type ProtocolAuthority = ReturnType<
  typeof watcherDeploymentProtocolScriptAuthority
>;

const value = (policyId: string, assetName: string): CML.Value => {
  const multiasset = CML.MultiAsset.new();
  multiasset.set(
    CML.ScriptHash.from_hex(policyId),
    CML.AssetName.from_hex(assetName),
    1n,
  );
  return CML.Value.new(2_000_000n, multiasset);
};

const transactionWithScriptData = (
  body: CML.TransactionBody,
  witnessSet: CML.TransactionWitnessSet,
): string => {
  const languages = CML.LanguageList.new();
  languages.add(CML.Language.PlutusV3);
  const redeemers = witnessSet.redeemers();
  if (redeemers === undefined)
    throw new Error("Synthetic SQ transaction has no redeemers");
  const scriptDataHash = CML.calc_script_data_hash(
    redeemers,
    CML.PlutusDataList.new(),
    createCostModels(PROTOCOL_PARAMETERS_DEFAULT.costModels),
    languages,
  );
  if (scriptDataHash === undefined)
    throw new Error("Synthetic SQ transaction has no script-data commitment");
  body.set_script_data_hash(scriptDataHash);
  return CML.Transaction.new(body, witnessSet, true).to_canonical_cbor_hex();
};

export const initializationTransaction = (
  authority: ProtocolAuthority,
): string => {
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.correctionLockSpend),
  );
  const rootDatum = Data.to(
    SDK.nodeViewToLinkedListDatum({
      key: "Empty",
      next: "Empty",
      data: Data.to([]),
    }),
    SDK.LinkedListDatum,
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(stateQueueAddress),
      value(
        authority.protocolScriptHashes.stateQueueMint,
        SDK.STATE_QUEUE_ROOT_ASSET_NAME,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(rootDatum)),
      undefined,
    ),
  );
  {
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(correctionLockAddress),
        value(
          authority.protocolScriptHashes.hubOracleMint,
          SDK.CORRECTION_LOCK_ASSET_NAME,
        ),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(
            Data.to("Idle", SDK.CorrectionLockDatum),
          ),
        ),
        undefined,
      ),
    );
  }
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    170_000n,
  );
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(authority.protocolScriptHashes.stateQueueMint),
    CML.AssetName.from_hex(SDK.STATE_QUEUE_ROOT_ASSET_NAME),
    1n,
  );
  mint.set(
    CML.ScriptHash.from_hex(authority.protocolScriptHashes.hubOracleMint),
    CML.AssetName.from_hex(SDK.CORRECTION_LOCK_ASSET_NAME),
    1n,
  );
  body.set_mint(mint);
  const policies = [
    authority.protocolScriptHashes.stateQueueMint,
    authority.protocolScriptHashes.hubOracleMint,
  ].sort();
  const stateQueuePolicyIndex = policies.indexOf(
    authority.protocolScriptHashes.stateQueueMint,
  );
  const redeemerCbor = Data.to(
    { InitV1: { output_index: 0n } },
    SDK.StateQueueRedeemer,
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  const witnessRedeemers = CML.LegacyRedeemerList.new();
  witnessRedeemers.add(
    CML.LegacyRedeemer.new(
      CML.RedeemerTag.Mint,
      BigInt(stateQueuePolicyIndex),
      CML.PlutusData.from_cbor_hex(redeemerCbor),
      CML.ExUnits.new(0n, 0n),
    ),
  );
  witnessSet.set_redeemers(
    CML.Redeemers.new_arr_legacy_redeemer(witnessRedeemers),
  );
  return transactionWithScriptData(body, witnessSet);
};

export const createSyntheticStateQueueHeader = (): SDK.Header => ({
  prevUtxosRoot: h32("01"),
  utxosRoot: h32("02"),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: h32("03"),
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: h32("04"),
  eventToStepRoot: h32("05"),
  validationTracesRoot: h32("08"),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: h28("06"),
  operatorVkey: h28("07"),
  protocolVersion: 1n,
});

export const commitTransaction = (
  authority: ProtocolAuthority,
  initializationTxHash: string,
  nodeHeader: SDK.Header,
): string => {
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const headerCborHex = Data.to(nodeHeader, SDK.Header);
  const computedHeaderHash = computeHash28(
    Buffer.from(headerCborHex, "hex"),
  ).toString("hex");
  const nodeAssetHeaderHash = computedHeaderHash;
  const stateQueueNode: SDK.StateQueueNode = {
    proven_fraud: null,
    header: nodeHeader,
    da_attestation: "Unattested",
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
        authority.protocolScriptHashes.stateQueueMint,
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
        authority.protocolScriptHashes.stateQueueMint,
        `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${nodeAssetHeaderHash}`,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(nodeDatumCborHex)),
      undefined,
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(initializationTxHash),
      0n,
    ),
  );
  const references = CML.TransactionInputList.new();
  references.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(initializationTxHash),
      1n,
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  body.set_reference_inputs(references);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(authority.protocolScriptHashes.stateQueueMint),
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
  return transactionWithScriptData(body, witnessSet);
};

export const closeAll = async (
  actions: readonly (() => void | Promise<void>)[],
) => {
  const results = await Promise.allSettled(
    actions.map((action) => Promise.resolve().then(action)),
  );
  const errors = results.flatMap((result) =>
    result.status === "rejected" ? [result.reason] : [],
  );
  if (errors.length > 0)
    throw new AggregateError(errors, "Synthetic SQ fixture cleanup failed");
};
