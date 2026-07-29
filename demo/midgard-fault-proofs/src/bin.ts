#!/usr/bin/env node

import { realpathSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { formatUnknownError } from "@al-ft/midgard-core";

import {
  inspectContractsFromFiles,
  parseNetwork,
} from "./inspect-contracts.js";
import { inxSubmitStep01FromFiles } from "./inx-submit-step-01.js";
import { inxSubmitStep02FromFiles } from "./inx-submit-step-02.js";
import { inxSubmitStep03FromFiles } from "./inx-submit-step-03.js";
import { inxSubmitStep04FromFiles } from "./inx-submit-step-04.js";
import { stringifyJson } from "./json-file.js";
import { neSubmitStep01FromFiles } from "./ne-submit-step-01.js";
import { neSubmitStep02FromFiles } from "./ne-submit-step-02.js";
import { neSubmitStep03FromFiles } from "./ne-submit-step-03.js";
import { neSubmitStep04FromFiles } from "./ne-submit-step-04.js";
import { nriSubmitStep01FromFiles } from "./nri-submit-step-01.js";
import { nriSubmitStep02FromFiles } from "./nri-submit-step-02.js";
import { nriSubmitStep03FromFiles } from "./nri-submit-step-03.js";
import { nriSubmitStep04FromFiles } from "./nri-submit-step-04.js";
import {
  prepareDoubleSpendFromFile,
  prepareDoubleSpendFromNode,
  prepareSampleDoubleSpend,
} from "./prepare-double-spend.js";
import {
  prepareInputNoIdxFromFile,
  prepareInputNoIdxFromNode,
} from "./prepare-input-no-idx.js";
import {
  prepareInvalidRangeFromFile,
  prepareInvalidRangeFromNode,
} from "./prepare-invalid-range.js";
import {
  prepareNoReferenceInputFromFile,
  prepareNoReferenceInputFromNode,
} from "./prepare-no-reference-input.js";
import {
  prepareNonExistentInputFromFile,
  prepareNonExistentInputFromNode,
} from "./prepare-non-existent-input.js";
import {
  prepareZeroInputFromFile,
  prepareZeroInputFromNode,
} from "./prepare-zero-input.js";
import { submitRemoveFraudulentBlockFromFiles } from "./remove-fraudulent-block.js";
import { type ProviderKind } from "./runtime.js";
import {
  type SubmitInitFraudCategory,
  submitInitFromFiles,
} from "./submit-init.js";
import { submitInvalidRangeStep01FromFiles } from "./submit-invalid-range-step-01.js";
import { submitInvalidRangeStep02FromFiles } from "./submit-invalid-range-step-02.js";
import { submitStep01FromFiles } from "./submit-step-01.js";
import { submitStep02FromFiles } from "./submit-step-02.js";
import { submitStep03FromFiles } from "./submit-step-03.js";
import { submitStep04FromFiles } from "./submit-step-04.js";
import { submitZeroInputStep01FromFiles } from "./submit-zero-input-step-01.js";
import { submitZeroInputStep02FromFiles } from "./submit-zero-input-step-02.js";

export type ParsedArgs = {
  readonly command: string | undefined;
  readonly blueprintPath: string | undefined;
  readonly deploymentInfoPath: string | undefined;
  readonly network: string | undefined;
  readonly provider: ProviderKind | undefined;
  readonly blockfrostApiUrl: string | undefined;
  readonly blockfrostKey: string | undefined;
  readonly kupoUrl: string | undefined;
  readonly ogmiosUrl: string | undefined;
  readonly walletSeedPhrase: string | undefined;
  readonly walletSeedPhraseEnv: string | undefined;
  readonly walletPrivateKey: string | undefined;
  readonly walletPrivateKeyEnv: string | undefined;
  readonly fraudulentBlockOutRef: string | undefined;
  readonly fraudulentHeaderHash: string | undefined;
  readonly threadOutRef: string | undefined;
  readonly stateQueueBlockOutRef: string | undefined;
  readonly txInclusionPath: string | undefined;
  readonly tx1InputsPath: string | undefined;
  readonly tx2InputsPath: string | undefined;
  readonly doubleSpentInputIndex: string | undefined;
  readonly midgardNodeUrl: string | undefined;
  readonly midgardNodeAdminKey: string | undefined;
  readonly midgardNodeAdminKeyEnv: string | undefined;
  readonly stateQueueLeaseTtlMs: string | undefined;
  readonly transactionsPath: string | undefined;
  readonly sampleDoubleSpend: boolean;
  readonly headerHash: string | undefined;
  readonly expectedTransactionsRoot: string | undefined;
  readonly tx1Id: string | undefined;
  readonly tx2Id: string | undefined;
  readonly outputDir: string | undefined;
  readonly allowIncompatibleOutput: boolean;
  readonly awaitConfirmation: boolean;
  readonly fraudCategory: SubmitInitFraudCategory | undefined;
  readonly blockValidFrom: string | undefined;
  readonly blockValidTo: string | undefined;
  readonly txId: string | undefined;
  readonly badTxId: string | undefined;
  readonly badInputIndex: string | undefined;
  readonly badReferenceInputIndex: string | undefined;
  readonly prevUtxosRoot: string | undefined;
  readonly prevBlockPayloadPath: string | undefined;
  readonly inputsPreimagePath: string | undefined;
  readonly referenceInputsPreimagePath: string | undefined;
  readonly ledgerNonMembershipProofPath: string | undefined;
  readonly txsNonMembershipProofPath: string | undefined;
  readonly outputsPreimagePath: string | undefined;
};

const usage = `Usage:
  midgard-fault-proofs prepare-double-spend (--midgard-node-url <url> | --transactions-file <path> | --sample-double-spend) --header-hash <hex> [--expected-transactions-root <hex>] [--tx1-id <hex> --tx2-id <hex>] [--output-dir <path>] [--allow-incompatible-output]
  midgard-fault-proofs prepare-invalid-range (--midgard-node-url <url> | --transactions-file <path>) --header-hash <hex> --block-valid-from <posixMs> --block-valid-to <posixMs> [--expected-transactions-root <hex>] [--tx-id <hex>] [--output-dir <path>] [--allow-incompatible-output]
  midgard-fault-proofs prepare-non-existent-input (--midgard-node-url <url> | --transactions-file <path>) --header-hash <hex> [--bad-tx-id <hex>] [--bad-input-index <n>] [--prev-utxos-root <hex> --prev-block-payload-file <daPayloadV2.hex>] [--expected-transactions-root <hex>] [--output-dir <path>]
  midgard-fault-proofs prepare-no-reference-input (--midgard-node-url <url> | --transactions-file <path>) --header-hash <hex> [--bad-tx-id <hex>] [--bad-reference-input-index <n>] [--prev-utxos-root <hex> --prev-block-payload-file <daPayloadV2.hex>] [--expected-transactions-root <hex>] [--output-dir <path>]
  midgard-fault-proofs prepare-input-no-idx (--midgard-node-url <url> | --transactions-file <path>) --header-hash <hex> [--bad-tx-id <hex>] [--bad-input-index <n>] [--expected-transactions-root <hex>] [--output-dir <path>]
  midgard-fault-proofs prepare-zero-input (--midgard-node-url <url> | --transactions-file <path>) --header-hash <hex> --expected-transactions-root <hex> [--tx-id <hex>] [--output-dir <path>]
  midgard-fault-proofs inspect-contracts --blueprint <path> --deployment-info <path> [--network <Mainnet|Preview|Preprod>]
  midgard-fault-proofs submit-init --blueprint <path> --deployment-info <path> --fraudulent-block-out-ref <txHash#outputIndex> [--fraud-category <doubleSpend|invalidRange|transitionTrace|nonExistentInput|zeroInput|noReferenceInput>] [--fraudulent-header-hash <hex>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --tx1-inputs <raw-input-cbor-list.json> --double-spent-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --tx2-inputs <raw-input-cbor-list.json> --double-spent-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-range-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-range-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --inputs-preimage <path> --bad-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --ledger-non-membership-proof <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --txs-non-membership-proof <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --reference-inputs-preimage <path> --bad-reference-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --ledger-non-membership-proof <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --txs-non-membership-proof <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <badTxInclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --inputs-preimage <path> --bad-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <producingTxInclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --outputs-preimage <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-zero-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-zero-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs remove-fraudulent-block --blueprint <path> --deployment-info <path> --fraudulent-header-hash <hex> [--fraud-category <doubleSpend|invalidRange|transitionTrace|nonExistentInput|zeroInput|noReferenceInput>] [--midgard-node-url <url> --midgard-node-admin-key <key> | --midgard-node-admin-key-env <envVar>] [--state-queue-lease-ttl-ms <n>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
`;

export const parseFraudCategory = (
  value: string | undefined,
): SubmitInitFraudCategory | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (
    value === "doubleSpend" ||
    value === "invalidRange" ||
    value === "transitionTrace" ||
    value === "nonExistentInput" ||
    value === "nonExistentInputNoIndex" ||
    value === "zeroInput" ||
    value === "noReferenceInput"
  ) {
    return value;
  }
  throw new Error(
    '--fraud-category must be one of "doubleSpend", "invalidRange", "transitionTrace", "nonExistentInput", "nonExistentInputNoIndex", "zeroInput", or "noReferenceInput".',
  );
};

export const parseArgs = (argv: readonly string[]): ParsedArgs => {
  const [, , command, ...rest] = argv;
  if (command === "--help" || command === "-h") {
    console.log(usage);
    process.exit(0);
  }
  let blueprintPath: string | undefined;
  let deploymentInfoPath: string | undefined;
  let network: string | undefined;
  let provider: ProviderKind | undefined;
  let blockfrostApiUrl: string | undefined;
  let blockfrostKey: string | undefined;
  let kupoUrl: string | undefined;
  let ogmiosUrl: string | undefined;
  let walletSeedPhrase: string | undefined;
  let walletSeedPhraseEnv: string | undefined;
  let walletPrivateKey: string | undefined;
  let walletPrivateKeyEnv: string | undefined;
  let fraudulentBlockOutRef: string | undefined;
  let fraudulentHeaderHash: string | undefined;
  let threadOutRef: string | undefined;
  let stateQueueBlockOutRef: string | undefined;
  let txInclusionPath: string | undefined;
  let tx1InputsPath: string | undefined;
  let tx2InputsPath: string | undefined;
  let doubleSpentInputIndex: string | undefined;
  let midgardNodeUrl: string | undefined;
  let midgardNodeAdminKey: string | undefined;
  let midgardNodeAdminKeyEnv: string | undefined;
  let stateQueueLeaseTtlMs: string | undefined;
  let transactionsPath: string | undefined;
  let sampleDoubleSpend = false;
  let headerHash: string | undefined;
  let expectedTransactionsRoot: string | undefined;
  let tx1Id: string | undefined;
  let tx2Id: string | undefined;
  let outputDir: string | undefined;
  let allowIncompatibleOutput = false;
  let awaitConfirmation = true;
  let fraudCategory: SubmitInitFraudCategory | undefined;
  let blockValidFrom: string | undefined;
  let blockValidTo: string | undefined;
  let txId: string | undefined;
  let badTxId: string | undefined;
  let badInputIndex: string | undefined;
  let prevUtxosRoot: string | undefined;
  let prevBlockPayloadPath: string | undefined;
  let inputsPreimagePath: string | undefined;
  let referenceInputsPreimagePath: string | undefined;
  let ledgerNonMembershipProofPath: string | undefined;
  let txsNonMembershipProofPath: string | undefined;
  let outputsPreimagePath: string | undefined;
  let badReferenceInputIndex: string | undefined;

  for (let index = 0; index < rest.length; index += 1) {
    const arg = rest[index];
    switch (arg) {
      case "--blueprint":
        blueprintPath = rest[++index];
        break;
      case "--deployment-info":
        deploymentInfoPath = rest[++index];
        break;
      case "--network":
        network = rest[++index];
        break;
      case "--provider": {
        const value = rest[++index];
        if (value !== "Blockfrost" && value !== "Kupmios") {
          throw new Error(
            '--provider must be either "Blockfrost" or "Kupmios".',
          );
        }
        provider = value;
        break;
      }
      case "--blockfrost-api-url":
        blockfrostApiUrl = rest[++index];
        break;
      case "--blockfrost-key":
        blockfrostKey = rest[++index];
        break;
      case "--kupo-url":
        kupoUrl = rest[++index];
        break;
      case "--ogmios-url":
        ogmiosUrl = rest[++index];
        break;
      case "--wallet-seed-phrase":
        walletSeedPhrase = rest[++index];
        break;
      case "--wallet-seed-phrase-env":
        walletSeedPhraseEnv = rest[++index];
        break;
      case "--wallet-private-key":
        walletPrivateKey = rest[++index];
        break;
      case "--wallet-private-key-env":
        walletPrivateKeyEnv = rest[++index];
        break;
      case "--fraudulent-block-out-ref":
        fraudulentBlockOutRef = rest[++index];
        break;
      case "--fraudulent-header-hash":
        fraudulentHeaderHash = rest[++index];
        break;
      case "--thread-out-ref":
        threadOutRef = rest[++index];
        break;
      case "--state-queue-block-out-ref":
        stateQueueBlockOutRef = rest[++index];
        break;
      case "--tx-inclusion":
        txInclusionPath = rest[++index];
        break;
      case "--tx1-inputs":
        tx1InputsPath = rest[++index];
        break;
      case "--tx2-inputs":
        tx2InputsPath = rest[++index];
        break;
      case "--double-spent-input-index":
        doubleSpentInputIndex = rest[++index];
        break;
      case "--midgard-node-url":
        midgardNodeUrl = rest[++index];
        break;
      case "--midgard-node-admin-key":
        midgardNodeAdminKey = rest[++index];
        break;
      case "--midgard-node-admin-key-env":
        midgardNodeAdminKeyEnv = rest[++index];
        break;
      case "--state-queue-lease-ttl-ms":
        stateQueueLeaseTtlMs = rest[++index];
        break;
      case "--transactions-file":
        transactionsPath = rest[++index];
        break;
      case "--sample-double-spend":
        sampleDoubleSpend = true;
        break;
      case "--header-hash":
        headerHash = rest[++index];
        break;
      case "--expected-transactions-root":
        expectedTransactionsRoot = rest[++index];
        break;
      case "--tx1-id":
        tx1Id = rest[++index];
        break;
      case "--tx2-id":
        tx2Id = rest[++index];
        break;
      case "--output-dir":
        outputDir = rest[++index];
        break;
      case "--allow-incompatible-output":
        allowIncompatibleOutput = true;
        break;
      case "--fraud-category":
        fraudCategory = parseFraudCategory(rest[++index]);
        break;
      case "--block-valid-from":
        blockValidFrom = rest[++index];
        break;
      case "--block-valid-to":
        blockValidTo = rest[++index];
        break;
      case "--tx-id":
        txId = rest[++index];
        break;
      case "--bad-tx-id":
        badTxId = rest[++index];
        break;
      case "--bad-input-index":
        badInputIndex = rest[++index];
        break;
      case "--bad-reference-input-index":
        badReferenceInputIndex = rest[++index];
        break;
      case "--prev-utxos-root":
        prevUtxosRoot = rest[++index];
        break;
      case "--prev-block-payload-file":
        prevBlockPayloadPath = rest[++index];
        break;
      case "--inputs-preimage":
        inputsPreimagePath = rest[++index];
        break;
      case "--reference-inputs-preimage":
        referenceInputsPreimagePath = rest[++index];
        break;
      case "--ledger-non-membership-proof":
        ledgerNonMembershipProofPath = rest[++index];
        break;
      case "--txs-non-membership-proof":
        txsNonMembershipProofPath = rest[++index];
        break;
      case "--outputs-preimage":
        outputsPreimagePath = rest[++index];
        break;
      case "--no-await-confirmation":
        awaitConfirmation = false;
        break;
      case "--help":
      case "-h":
        console.log(usage);
        process.exit(0);
      default:
        throw new Error(`Unknown argument: ${arg}`);
    }
  }

  return {
    command,
    blueprintPath,
    deploymentInfoPath,
    network,
    provider,
    blockfrostApiUrl,
    blockfrostKey,
    kupoUrl,
    ogmiosUrl,
    walletSeedPhrase,
    walletSeedPhraseEnv,
    walletPrivateKey,
    walletPrivateKeyEnv,
    fraudulentBlockOutRef,
    fraudulentHeaderHash,
    threadOutRef,
    stateQueueBlockOutRef,
    txInclusionPath,
    tx1InputsPath,
    tx2InputsPath,
    doubleSpentInputIndex,
    midgardNodeUrl,
    midgardNodeAdminKey,
    midgardNodeAdminKeyEnv,
    stateQueueLeaseTtlMs,
    transactionsPath,
    sampleDoubleSpend,
    headerHash,
    expectedTransactionsRoot,
    tx1Id,
    tx2Id,
    outputDir,
    allowIncompatibleOutput,
    awaitConfirmation,
    fraudCategory,
    blockValidFrom,
    blockValidTo,
    txId,
    badTxId,
    badInputIndex,
    badReferenceInputIndex,
    prevUtxosRoot,
    prevBlockPayloadPath,
    inputsPreimagePath,
    referenceInputsPreimagePath,
    ledgerNonMembershipProofPath,
    txsNonMembershipProofPath,
    outputsPreimagePath,
  };
};

export const buildRemoveFraudulentBlockCliConfig = (args: ParsedArgs) => {
  if (args.blueprintPath === undefined) {
    throw new Error(`Missing required --blueprint <path>.\n${usage}`);
  }
  if (args.deploymentInfoPath === undefined) {
    throw new Error(`Missing required --deployment-info <path>.\n${usage}`);
  }
  if (args.fraudulentHeaderHash === undefined) {
    throw new Error(
      `Missing required --fraudulent-header-hash <hex>.\n${usage}`,
    );
  }
  return {
    blueprintPath: args.blueprintPath,
    deploymentInfoPath: args.deploymentInfoPath,
    network: parseNetwork(args.network),
    provider: args.provider,
    blockfrostApiUrl: args.blockfrostApiUrl,
    blockfrostKey: args.blockfrostKey,
    kupoUrl: args.kupoUrl,
    ogmiosUrl: args.ogmiosUrl,
    walletSeedPhrase: args.walletSeedPhrase,
    walletSeedPhraseEnv: args.walletSeedPhraseEnv,
    walletPrivateKey: args.walletPrivateKey,
    walletPrivateKeyEnv: args.walletPrivateKeyEnv,
    fraudCategory: args.fraudCategory,
    fraudulentHeaderHash: args.fraudulentHeaderHash,
    awaitConfirmation: args.awaitConfirmation,
    midgardNodeUrl: args.midgardNodeUrl,
    midgardNodeAdminKey: args.midgardNodeAdminKey,
    midgardNodeAdminKeyEnv: args.midgardNodeAdminKeyEnv,
    stateQueueLeaseTtlMs:
      args.stateQueueLeaseTtlMs === undefined
        ? undefined
        : Number(args.stateQueueLeaseTtlMs),
  };
};

const writeJson = (value: unknown): void => {
  process.stdout.write(stringifyJson(value));
};

export const isCliEntrypoint = ({
  moduleUrl,
  argvPath,
}: {
  readonly moduleUrl: string;
  readonly argvPath: string | undefined;
}): boolean => {
  if (argvPath === undefined) {
    return false;
  }
  try {
    return realpathSync(fileURLToPath(moduleUrl)) === realpathSync(argvPath);
  } catch {
    return false;
  }
};

export const main = async (): Promise<void> => {
  const args = parseArgs(process.argv);
  if (
    args.command !== "prepare-double-spend" &&
    args.command !== "prepare-invalid-range" &&
    args.command !== "prepare-non-existent-input" &&
    args.command !== "prepare-no-reference-input" &&
    args.command !== "prepare-input-no-idx" &&
    args.command !== "prepare-zero-input" &&
    args.command !== "inspect-contracts" &&
    args.command !== "submit-init" &&
    args.command !== "submit-step-01" &&
    args.command !== "submit-step-02" &&
    args.command !== "submit-step-03" &&
    args.command !== "submit-step-04" &&
    args.command !== "submit-invalid-range-step-01" &&
    args.command !== "submit-invalid-range-step-02" &&
    args.command !== "submit-non-existent-input-step-01" &&
    args.command !== "submit-non-existent-input-step-02" &&
    args.command !== "submit-non-existent-input-step-03" &&
    args.command !== "submit-non-existent-input-step-04" &&
    args.command !== "submit-no-reference-input-step-01" &&
    args.command !== "submit-no-reference-input-step-02" &&
    args.command !== "submit-no-reference-input-step-03" &&
    args.command !== "submit-no-reference-input-step-04" &&
    args.command !== "submit-input-no-idx-step-01" &&
    args.command !== "submit-input-no-idx-step-02" &&
    args.command !== "submit-input-no-idx-step-03" &&
    args.command !== "submit-input-no-idx-step-04" &&
    args.command !== "submit-zero-input-step-01" &&
    args.command !== "submit-zero-input-step-02" &&
    args.command !== "remove-fraudulent-block"
  ) {
    throw new Error(
      `Expected command "prepare-double-spend", "prepare-invalid-range", "prepare-non-existent-input", "prepare-no-reference-input", "prepare-zero-input", "inspect-contracts", "submit-init", "submit-step-01", "submit-step-02", "submit-step-03", "submit-step-04", "submit-invalid-range-step-01", "submit-invalid-range-step-02", "submit-non-existent-input-step-01", "submit-non-existent-input-step-02", "submit-non-existent-input-step-03", "submit-non-existent-input-step-04", "submit-no-reference-input-step-01", "submit-no-reference-input-step-02", "submit-no-reference-input-step-03", "submit-no-reference-input-step-04", "submit-input-no-idx-step-01", "submit-input-no-idx-step-02", "submit-input-no-idx-step-03", "submit-input-no-idx-step-04", "submit-zero-input-step-01", "submit-zero-input-step-02", or "remove-fraudulent-block".\n${usage}`,
    );
  }

  if (args.command === "prepare-double-spend") {
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash <hex>.\n${usage}`);
    }
    const inputModes = [
      args.midgardNodeUrl !== undefined,
      args.transactionsPath !== undefined,
      args.sampleDoubleSpend,
    ].filter(Boolean).length;
    if (inputModes !== 1) {
      throw new Error(
        `Provide exactly one of --midgard-node-url, --transactions-file, or --sample-double-spend.\n${usage}`,
      );
    }
    const output =
      args.midgardNodeUrl !== undefined
        ? await prepareDoubleSpendFromNode({
            midgardNodeUrl: args.midgardNodeUrl,
            headerHash: args.headerHash,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            tx1Id: args.tx1Id,
            tx2Id: args.tx2Id,
            outputDir: args.outputDir,
            allowIncompatibleOutput: args.allowIncompatibleOutput,
          })
        : args.transactionsPath !== undefined
          ? await prepareDoubleSpendFromFile({
              transactionsPath: args.transactionsPath,
              headerHash: args.headerHash,
              expectedTransactionsRoot: args.expectedTransactionsRoot,
              tx1Id: args.tx1Id,
              tx2Id: args.tx2Id,
              outputDir: args.outputDir,
              allowIncompatibleOutput: args.allowIncompatibleOutput,
            })
          : await prepareSampleDoubleSpend({
              headerHash: args.headerHash,
              expectedTransactionsRoot: args.expectedTransactionsRoot,
              outputDir: args.outputDir,
              allowIncompatibleOutput: args.allowIncompatibleOutput,
            });
    writeJson(output);
    return;
  }

  if (args.command === "prepare-invalid-range") {
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash <hex>.\n${usage}`);
    }
    if (args.blockValidFrom === undefined) {
      throw new Error(
        `Missing required --block-valid-from <posixMs>.\n${usage}`,
      );
    }
    if (args.blockValidTo === undefined) {
      throw new Error(`Missing required --block-valid-to <posixMs>.\n${usage}`);
    }
    const inputModes = [
      args.midgardNodeUrl !== undefined,
      args.transactionsPath !== undefined,
    ].filter(Boolean).length;
    if (inputModes !== 1) {
      throw new Error(
        `Provide exactly one of --midgard-node-url or --transactions-file.\n${usage}`,
      );
    }
    const output =
      args.midgardNodeUrl !== undefined
        ? await prepareInvalidRangeFromNode({
            midgardNodeUrl: args.midgardNodeUrl,
            headerHash: args.headerHash,
            blockValidFrom: args.blockValidFrom,
            blockValidTo: args.blockValidTo,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            txId: args.txId,
            outputDir: args.outputDir,
            allowIncompatibleOutput: args.allowIncompatibleOutput,
          })
        : await prepareInvalidRangeFromFile({
            transactionsPath: args.transactionsPath!,
            headerHash: args.headerHash,
            blockValidFrom: args.blockValidFrom,
            blockValidTo: args.blockValidTo,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            txId: args.txId,
            outputDir: args.outputDir,
            allowIncompatibleOutput: args.allowIncompatibleOutput,
          });
    writeJson(output);
    return;
  }

  if (args.command === "prepare-non-existent-input") {
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash <hex>.\n${usage}`);
    }
    const inputModes = [
      args.midgardNodeUrl !== undefined,
      args.transactionsPath !== undefined,
    ].filter(Boolean).length;
    if (inputModes !== 1) {
      throw new Error(
        `Provide exactly one of --midgard-node-url or --transactions-file.\n${usage}`,
      );
    }
    const output =
      args.midgardNodeUrl !== undefined
        ? await prepareNonExistentInputFromNode({
            midgardNodeUrl: args.midgardNodeUrl,
            headerHash: args.headerHash,
            badTxId: args.badTxId,
            badInputIndex: args.badInputIndex,
            prevUtxosRoot: args.prevUtxosRoot,
            prevBlockPayloadPath: args.prevBlockPayloadPath,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            outputDir: args.outputDir,
          })
        : await prepareNonExistentInputFromFile({
            transactionsPath: args.transactionsPath!,
            headerHash: args.headerHash,
            badTxId: args.badTxId,
            badInputIndex: args.badInputIndex,
            prevUtxosRoot: args.prevUtxosRoot,
            prevBlockPayloadPath: args.prevBlockPayloadPath,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            outputDir: args.outputDir,
          });
    writeJson(output);
    return;
  }

  if (args.command === "prepare-no-reference-input") {
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash <hex>.\n${usage}`);
    }
    const inputModes = [
      args.midgardNodeUrl !== undefined,
      args.transactionsPath !== undefined,
    ].filter(Boolean).length;
    if (inputModes !== 1) {
      throw new Error(
        `Provide exactly one of --midgard-node-url or --transactions-file.\n${usage}`,
      );
    }
    const output =
      args.midgardNodeUrl !== undefined
        ? await prepareNoReferenceInputFromNode({
            midgardNodeUrl: args.midgardNodeUrl,
            headerHash: args.headerHash,
            badTxId: args.badTxId,
            badReferenceInputIndex: args.badReferenceInputIndex,
            prevUtxosRoot: args.prevUtxosRoot,
            prevBlockPayloadPath: args.prevBlockPayloadPath,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            outputDir: args.outputDir,
          })
        : await prepareNoReferenceInputFromFile({
            transactionsPath: args.transactionsPath!,
            headerHash: args.headerHash,
            badTxId: args.badTxId,
            badReferenceInputIndex: args.badReferenceInputIndex,
            prevUtxosRoot: args.prevUtxosRoot,
            prevBlockPayloadPath: args.prevBlockPayloadPath,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            outputDir: args.outputDir,
          });
    writeJson(output);
    return;
  }

  if (args.command === "prepare-input-no-idx") {
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash <hex>.\n${usage}`);
    }
    const inputModes = [
      args.midgardNodeUrl !== undefined,
      args.transactionsPath !== undefined,
    ].filter(Boolean).length;
    if (inputModes !== 1) {
      throw new Error(
        `Provide exactly one of --midgard-node-url or --transactions-file.\n${usage}`,
      );
    }
    const output =
      args.midgardNodeUrl !== undefined
        ? await prepareInputNoIdxFromNode({
            midgardNodeUrl: args.midgardNodeUrl,
            headerHash: args.headerHash,
            badTxId: args.badTxId,
            badInputIndex: args.badInputIndex,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            outputDir: args.outputDir,
          })
        : await prepareInputNoIdxFromFile({
            transactionsPath: args.transactionsPath!,
            headerHash: args.headerHash,
            badTxId: args.badTxId,
            badInputIndex: args.badInputIndex,
            expectedTransactionsRoot: args.expectedTransactionsRoot,
            outputDir: args.outputDir,
          });
    writeJson(output);
    return;
  }

  if (args.command === "prepare-zero-input") {
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash <hex>.\n${usage}`);
    }
    const inputModes = [
      args.midgardNodeUrl !== undefined,
      args.transactionsPath !== undefined,
    ].filter(Boolean).length;
    if (inputModes !== 1) {
      throw new Error(
        `Provide exactly one of --midgard-node-url or --transactions-file.\n${usage}`,
      );
    }
    if (args.expectedTransactionsRoot === undefined) {
      throw new Error(
        `Missing required --expected-transactions-root <hex>.\n${usage}`,
      );
    }
    const expectedTransactionsRoot = args.expectedTransactionsRoot;
    const output =
      args.midgardNodeUrl !== undefined
        ? await prepareZeroInputFromNode({
            midgardNodeUrl: args.midgardNodeUrl,
            headerHash: args.headerHash,
            expectedTransactionsRoot,
            txId: args.txId,
            outputDir: args.outputDir,
          })
        : await prepareZeroInputFromFile({
            transactionsPath: args.transactionsPath!,
            headerHash: args.headerHash,
            expectedTransactionsRoot,
            txId: args.txId,
            outputDir: args.outputDir,
          });
    writeJson(output);
    return;
  }

  if (args.blueprintPath === undefined) {
    throw new Error(`Missing required --blueprint <path>.\n${usage}`);
  }
  if (args.deploymentInfoPath === undefined) {
    throw new Error(`Missing required --deployment-info <path>.\n${usage}`);
  }

  if (args.command === "submit-init") {
    if (args.fraudulentBlockOutRef === undefined) {
      throw new Error(
        `Missing required --fraudulent-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitInitFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      fraudCategory: args.fraudCategory,
      fraudulentBlockOutRef: args.fraudulentBlockOutRef,
      fraudulentHeaderHash: args.fraudulentHeaderHash,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-range-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitInvalidRangeStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-range-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitInvalidRangeStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-zero-input-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitZeroInputStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-zero-input-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitZeroInputStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.tx1InputsPath === undefined) {
      throw new Error(`Missing required --tx1-inputs <path>.\n${usage}`);
    }
    if (args.doubleSpentInputIndex === undefined) {
      throw new Error(
        `Missing required --double-spent-input-index <n>.\n${usage}`,
      );
    }
    const output = await submitStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      tx1InputsPath: args.tx1InputsPath,
      doubleSpentInputIndex: args.doubleSpentInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.tx2InputsPath === undefined) {
      throw new Error(`Missing required --tx2-inputs <path>.\n${usage}`);
    }
    if (args.doubleSpentInputIndex === undefined) {
      throw new Error(
        `Missing required --double-spent-input-index <n>.\n${usage}`,
      );
    }
    const output = await submitStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      tx2InputsPath: args.tx2InputsPath,
      doubleSpentInputIndex: args.doubleSpentInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-non-existent-input-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await neSubmitStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-non-existent-input-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.inputsPreimagePath === undefined) {
      throw new Error(`Missing required --inputs-preimage <path>.\n${usage}`);
    }
    if (args.badInputIndex === undefined) {
      throw new Error(`Missing required --bad-input-index <n>.\n${usage}`);
    }
    const output = await neSubmitStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      inputsPreimagePath: args.inputsPreimagePath,
      badInputIndex: args.badInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-non-existent-input-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.ledgerNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --ledger-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await neSubmitStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      ledgerNonMembershipProofPath: args.ledgerNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-non-existent-input-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txsNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --txs-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await neSubmitStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      txsNonMembershipProofPath: args.txsNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await nriSubmitStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.referenceInputsPreimagePath === undefined) {
      throw new Error(
        `Missing required --reference-inputs-preimage <path>.\n${usage}`,
      );
    }
    if (args.badReferenceInputIndex === undefined) {
      throw new Error(
        `Missing required --bad-reference-input-index <n>.\n${usage}`,
      );
    }
    const output = await nriSubmitStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      referenceInputsPreimagePath: args.referenceInputsPreimagePath,
      badReferenceInputIndex: args.badReferenceInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.ledgerNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --ledger-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await nriSubmitStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      ledgerNonMembershipProofPath: args.ledgerNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txsNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --txs-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await nriSubmitStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      txsNonMembershipProofPath: args.txsNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await inxSubmitStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.inputsPreimagePath === undefined) {
      throw new Error(`Missing required --inputs-preimage <path>.\n${usage}`);
    }
    if (args.badInputIndex === undefined) {
      throw new Error(`Missing required --bad-input-index <n>.\n${usage}`);
    }
    const output = await inxSubmitStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      inputsPreimagePath: args.inputsPreimagePath,
      badInputsIndex: args.badInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await inxSubmitStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      producingTxInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.outputsPreimagePath === undefined) {
      throw new Error(`Missing required --outputs-preimage <path>.\n${usage}`);
    }
    const output = await inxSubmitStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      outputsPreimagePath: args.outputsPreimagePath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "remove-fraudulent-block") {
    const output = await submitRemoveFraudulentBlockFromFiles(
      buildRemoveFraudulentBlockCliConfig(args),
    );

    writeJson(output);
    return;
  }

  const output = await inspectContractsFromFiles({
    blueprintPath: args.blueprintPath,
    deploymentInfoPath: args.deploymentInfoPath,
    network: parseNetwork(args.network),
  });

  writeJson(output);
};

if (
  isCliEntrypoint({ moduleUrl: import.meta.url, argvPath: process.argv[1] })
) {
  main().catch((error: unknown) => {
    process.stderr.write(
      `midgard-fault-proofs: ${formatUnknownError(error)}\n`,
    );
    process.exitCode = 1;
  });
}
