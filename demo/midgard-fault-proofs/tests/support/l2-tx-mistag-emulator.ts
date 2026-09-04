import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  HUB_ORACLE_ASSET_NAME,
  type NativeTxInclusionCarriage,
  Proof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { L2TxMistagContracts } from "../../src/l2-tx-mistag/contracts.js";
import {
  L2TxMistagStep01SpendRedeemer,
  L2TxMistagStep02Datum,
} from "../../src/l2-tx-mistag/schemas.js";
import {
  requireL2TxMistagReferenceScript,
  requireL2TxMistagThreadUtxo,
} from "../../src/l2-tx-mistag/submit-common.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../../src/runtime.js";
import type { SubmitStep01TxInclusion } from "../../src/submit-step-01.js";
import {
  nativeTxFromCoreCompact,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
} from "../../src/submit-step-01.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../../src/witness-reference-scripts.js";
import { trieRootHex } from "./emulator/catalogue.js";
import type { FaultProofEmulatorHarness } from "./emulator/harness.js";
import { l2TransactionSourceCbor as l2TransactionSourceCborV1 } from "./emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";

export type L2TxMistagBlockFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly nativeTxId: string;
  readonly compactCbor: string;
  readonly inclusion: SubmitStep01TxInclusion;
};

/**
 * Per-family materializer. It deliberately does not call shared `makeNativeTx`,
 * whose invariant is `TxIsValid`.
 */
export const buildL2TxMistagBlockFixture = async (
  validity: "TxIsValid" | "TxIsInvalid",
): Promise<L2TxMistagBlockFixture> => {
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: encodeCbor([
        Buffer.from("61".repeat(32), "hex"),
      ]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 7n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
  const l2TransactionSourceCbor = l2TransactionSourceCborV1(nativeTx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(l2TransactionSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const proofCbor = proof.toCBOR().toString("hex");
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    nativeTxId,
    compactCbor: compactCbor.toString("hex"),
    inclusion: {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      l2TransactionSourceCbor,
      transactionsPhasRoot: trieRootHex(trie),
      txMembershipProof: Data.from(proofCbor, Proof),
      txMembershipProofCbor: proofCbor,
    },
  };
};

export const l2TxMistagCategory = (harness: FaultProofEmulatorHarness) => {
  const category = harness.catalogue.categories.l2TxMistag;
  if (category === undefined || harness.contracts.l2TxMistag === undefined) {
    throw new Error("l2-tx-mistag harness was not enabled");
  }
  if (category.categoryId !== FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.l2TxMistag) {
    throw new Error("l2-tx-mistag harness category id drifted");
  }
  return category;
};

export const publishL2TxMistagReferenceScripts = async ({
  harness,
}: {
  readonly harness: FaultProofEmulatorHarness;
}): Promise<readonly [UTxO, UTxO]> => {
  const contracts = harness.contracts.l2TxMistag;
  if (contracts === undefined) {
    throw new Error("l2-tx-mistag contracts were not enabled");
  }
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    published.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.proverLucid,
          script: step.spendingScript,
          label: `l2-tx-mistag step 0${(index + 1).toString()}`,
        })
      ).utxo,
    );
  }
  return [published[0]!, published[1]!];
};

/**
 * Emulator-only forced spend used to reach the validator's exact check after
 * the production submitter has correctly refused the same evidence offchain.
 * It retains every binding/layout/reference-script check and omits only the
 * `validity_code != 0` detection gate.
 */
export const forceL2TxMistagStep01ForAdversarialTest = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: FaultProofEmulatorHarness["proverLucid"];
  readonly blueprint: unknown;
  readonly contracts: L2TxMistagContracts;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const { threadUtxo, threadToken } = await requireL2TxMistagThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  requireNativeTxMatchesCompactCbor(txInclusion);
  const reference = requireL2TxMistagReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "test state-queue out-ref"),
    label: "l2-tx-mistag adversarial state-queue block",
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOraclePolicyId),
    ),
    unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "l2-tx-mistag adversarial hub oracle",
  });
  if (
    resolveFraudulentHeaderHash({
      stateQueuePolicyId: contracts.stateQueuePolicyId,
      fraudulentBlockUtxo: stateQueueBlockUtxo,
    }) !== threadToken.fraudulentHeaderHash
  ) {
    throw new Error("forced l2-tx-mistag spend names the wrong block");
  }
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const rewardAddress = phasMembershipRewardAddress(network, phasScript);
  const phasCarriage = witnessWithdrawalValidatorCarriage({
    script: phasScript,
    referenceUtxo: witnessReferenceScripts.phasMembershipWithdraw,
    label: "forced l2-tx-mistag PHAS membership",
  });
  const state = {
    bad_tx_id: txInclusion.nativeTxId,
    committed_validity_code: txInclusion.nativeTx.validity_code,
  };
  const outputDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    L2TxMistagStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: outputDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "forced l2-tx-mistag step-01");
    const common = {
      input_index: requireInputIndex(ctx, threadUtxo, "forced step-01"),
      output_index: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "forced step-01 output",
      ),
      hub_ref_input_index: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "forced step-01 hub",
      ),
      state_queue_node_ref_input_index: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "forced step-01 state queue",
      ),
      native_tx_id: txInclusion.nativeTxId,
      l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
      transactions_phas_root: txInclusion.transactionsPhasRoot,
      tx_membership_proof: txInclusion.txMembershipProof,
      inclusion_proof_script_withdraw_redeemer_index:
        requireWithdrawalRedeemerIndex(
          ctx,
          rewardAddress,
          "forced step-01 membership",
        ),
    };
    const carriage: NativeTxInclusionCarriage = {
      RedeemerCarriedInclusion: [common],
    };
    return Data.to({ Continue: [carriage] }, L2TxMistagStep01SpendRedeemer);
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubOracleUtxo,
      stateQueueBlockUtxo,
      reference,
      ...phasCarriage.referenceInputs,
    ])
    .withdraw(
      rewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.l2TransactionSourceCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await phasCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  const txHash = await (await unsigned.sign.withWallet().complete()).submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return txHash;
};
