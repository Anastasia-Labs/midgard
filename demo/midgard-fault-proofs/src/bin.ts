#!/usr/bin/env node

import { formatUnknownError } from "@al-ft/midgard-core";

import {
  inspectContractsFromFiles,
  parseNetwork,
} from "./inspect-contracts.js";
import { stringifyJson } from "./json-file.js";
import { neSubmitInitFromFiles } from "./ne-submit-init.js";
import { neSubmitStep01FromFiles } from "./ne-submit-step-01.js";
import { neSubmitStep02FromFiles } from "./ne-submit-step-02.js";
import { neSubmitStep03FromFiles } from "./ne-submit-step-03.js";
import { neSubmitStep04FromFiles } from "./ne-submit-step-04.js";
import {
  prepareDoubleSpendFromFile,
  prepareDoubleSpendFromNode,
  prepareSampleDoubleSpend,
} from "./prepare-double-spend.js";
import { prepareNonExistentInputFromNode } from "./prepare-non-existent-input.js";
import { submitRemoveFraudulentBlockFromFiles } from "./remove-fraudulent-block.js";
import { type ProviderKind } from "./runtime.js";
import { submitInitFromFiles } from "./submit-init.js";
import { submitNonExistentInputTx } from "./submit-non-existent-input-tx.js";
import { submitStep01FromFiles } from "./submit-step-01.js";
import { submitStep02FromFiles } from "./submit-step-02.js";
import { submitStep03FromFiles } from "./submit-step-03.js";
import { submitStep04FromFiles } from "./submit-step-04.js";

type ParsedArgs = {
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
  readonly fraudCategory: string | undefined;
  readonly threadOutRef: string | undefined;
  readonly stateQueueBlockOutRef: string | undefined;
  readonly txInclusionPath: string | undefined;
  readonly tx1InputsPath: string | undefined;
  readonly tx2InputsPath: string | undefined;
  readonly doubleSpentInputIndex: string | undefined;
  readonly inputsPath: string | undefined;
  readonly badInputIndex: string | undefined;
  readonly nonMembershipProofPath: string | undefined;
  readonly midgardNodeUrl: string | undefined;
  readonly transactionsPath: string | undefined;
  readonly phantomInputTxId: string | undefined;
  readonly phantomInputIndex: string | undefined;
  readonly badTxId: string | undefined;
  readonly prevUtxosRoot: string | undefined;
  readonly sampleDoubleSpend: boolean;
  readonly headerHash: string | undefined;
  readonly expectedTransactionsRoot: string | undefined;
  readonly tx1Id: string | undefined;
  readonly tx2Id: string | undefined;
  readonly outputDir: string | undefined;
  readonly allowIncompatibleOutput: boolean;
  readonly awaitConfirmation: boolean;
};

const usage = `Usage:
  midgard-fault-proofs prepare-double-spend (--midgard-node-url <url> | --transactions-file <path> | --sample-double-spend) --header-hash <hex> [--expected-transactions-root <hex>] [--tx1-id <hex> --tx2-id <hex>] [--output-dir <path>] [--allow-incompatible-output]
  midgard-fault-proofs prepare-non-existent-input --midgard-node-url <url> --header-hash <hex> [--bad-tx-id <hex>] [--bad-input-index <n>] [--prev-utxos-root <hex>] [--expected-transactions-root <hex>] [--output-dir <path>]
      # --prev-utxos-root is the disputed block header's prevUtxosRoot (defaults to the empty root). When non-empty (the chain already has blocks), the ledger non-membership proof is fetched from the node via GET /ledger-non-membership.
  midgard-fault-proofs submit-non-existent-input-tx --midgard-node-url <url> [--phantom-input-tx-id <hex>] [--phantom-input-index <n>]
  midgard-fault-proofs inspect-contracts --blueprint <path> --deployment-info <path> [--network <Mainnet|Preview|Preprod>]
  midgard-fault-proofs submit-init --blueprint <path> --deployment-info <path> --fraudulent-block-out-ref <txHash#outputIndex> [--fraudulent-header-hash <hex>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --tx1-inputs <raw-input-cbor-list.json> --double-spent-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --tx2-inputs <raw-input-cbor-list.json> --double-spent-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs ne-submit-init --blueprint <path> --deployment-info <path> --fraudulent-block-out-ref <txHash#outputIndex> [--fraudulent-header-hash <hex>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs ne-submit-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [...wallet/provider flags]
  midgard-fault-proofs ne-submit-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --inputs <output-references.json> --bad-input-index <n> [...wallet/provider flags]
  midgard-fault-proofs ne-submit-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --non-membership-proof <ledger-non-membership-proof.json> [...wallet/provider flags]
  midgard-fault-proofs ne-submit-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --non-membership-proof <txs-non-membership-proof.json> [...wallet/provider flags]
  midgard-fault-proofs remove-fraudulent-block --blueprint <path> --deployment-info <path> --fraudulent-header-hash <hex> [--fraud-category <doubleSpend|nonExistentInput>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
`;

const parseArgs = (argv: readonly string[]): ParsedArgs => {
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
  let fraudCategory: string | undefined;
  let threadOutRef: string | undefined;
  let stateQueueBlockOutRef: string | undefined;
  let txInclusionPath: string | undefined;
  let tx1InputsPath: string | undefined;
  let tx2InputsPath: string | undefined;
  let doubleSpentInputIndex: string | undefined;
  let inputsPath: string | undefined;
  let badInputIndex: string | undefined;
  let nonMembershipProofPath: string | undefined;
  let midgardNodeUrl: string | undefined;
  let transactionsPath: string | undefined;
  let phantomInputTxId: string | undefined;
  let phantomInputIndex: string | undefined;
  let badTxId: string | undefined;
  let prevUtxosRoot: string | undefined;
  let sampleDoubleSpend = false;
  let headerHash: string | undefined;
  let expectedTransactionsRoot: string | undefined;
  let tx1Id: string | undefined;
  let tx2Id: string | undefined;
  let outputDir: string | undefined;
  let allowIncompatibleOutput = false;
  let awaitConfirmation = true;

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
      case "--fraud-category":
        fraudCategory = rest[++index];
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
      case "--inputs":
        inputsPath = rest[++index];
        break;
      case "--bad-input-index":
        badInputIndex = rest[++index];
        break;
      case "--non-membership-proof":
        nonMembershipProofPath = rest[++index];
        break;
      case "--midgard-node-url":
        midgardNodeUrl = rest[++index];
        break;
      case "--transactions-file":
        transactionsPath = rest[++index];
        break;
      case "--phantom-input-tx-id":
        phantomInputTxId = rest[++index];
        break;
      case "--phantom-input-index":
        phantomInputIndex = rest[++index];
        break;
      case "--bad-tx-id":
        badTxId = rest[++index];
        break;
      case "--prev-utxos-root":
        prevUtxosRoot = rest[++index];
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
    fraudCategory,
    threadOutRef,
    stateQueueBlockOutRef,
    txInclusionPath,
    tx1InputsPath,
    tx2InputsPath,
    doubleSpentInputIndex,
    inputsPath,
    badInputIndex,
    nonMembershipProofPath,
    midgardNodeUrl,
    transactionsPath,
    phantomInputTxId,
    phantomInputIndex,
    badTxId,
    prevUtxosRoot,
    sampleDoubleSpend,
    headerHash,
    expectedTransactionsRoot,
    tx1Id,
    tx2Id,
    outputDir,
    allowIncompatibleOutput,
    awaitConfirmation,
  };
};

const writeJson = (value: unknown): void => {
  process.stdout.write(stringifyJson(value));
};

const main = async (): Promise<void> => {
  const args = parseArgs(process.argv);
  if (
    args.command !== "prepare-double-spend" &&
    args.command !== "prepare-non-existent-input" &&
    args.command !== "inspect-contracts" &&
    args.command !== "submit-non-existent-input-tx" &&
    args.command !== "submit-init" &&
    args.command !== "submit-step-01" &&
    args.command !== "submit-step-02" &&
    args.command !== "submit-step-03" &&
    args.command !== "submit-step-04" &&
    args.command !== "ne-submit-init" &&
    args.command !== "ne-submit-step-01" &&
    args.command !== "ne-submit-step-02" &&
    args.command !== "ne-submit-step-03" &&
    args.command !== "ne-submit-step-04" &&
    args.command !== "remove-fraudulent-block"
  ) {
    throw new Error(
      `Expected command "prepare-double-spend", "prepare-non-existent-input", "submit-non-existent-input-tx", "inspect-contracts", "submit-init", "submit-step-01", "submit-step-02", "submit-step-03", "submit-step-04", "ne-submit-init", "ne-submit-step-01", "ne-submit-step-02", "ne-submit-step-03", "ne-submit-step-04", or "remove-fraudulent-block".\n${usage}`,
    );
  }

  if (args.command === "prepare-non-existent-input") {
    if (args.midgardNodeUrl === undefined) {
      throw new Error(`Missing required --midgard-node-url <url>.\n${usage}`);
    }
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash <hex>.\n${usage}`);
    }
    writeJson(
      await prepareNonExistentInputFromNode({
        midgardNodeUrl: args.midgardNodeUrl,
        headerHash: args.headerHash,
        badTxId: args.badTxId,
        badInputIndex:
          args.badInputIndex === undefined
            ? undefined
            : Number(args.badInputIndex),
        prevUtxosRoot: args.prevUtxosRoot,
        expectedTransactionsRoot: args.expectedTransactionsRoot,
        outputDir: args.outputDir,
      }),
    );
    return;
  }

  if (args.command === "submit-non-existent-input-tx") {
    if (args.midgardNodeUrl === undefined) {
      throw new Error(`Missing required --midgard-node-url <url>.\n${usage}`);
    }
    writeJson(
      await submitNonExistentInputTx({
        midgardNodeUrl: args.midgardNodeUrl,
        phantomTxId: args.phantomInputTxId,
        phantomIndex:
          args.phantomInputIndex === undefined
            ? undefined
            : Number(args.phantomInputIndex),
      }),
    );
    return;
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
      fraudulentBlockOutRef: args.fraudulentBlockOutRef,
      fraudulentHeaderHash: args.fraudulentHeaderHash,
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

  const providerWalletConfig = {
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
    awaitConfirmation: args.awaitConfirmation,
  } as const;
  const blueprintPath = args.blueprintPath;
  const deploymentInfoPath = args.deploymentInfoPath;

  if (args.command === "ne-submit-init") {
    if (args.fraudulentBlockOutRef === undefined) {
      throw new Error(
        `Missing required --fraudulent-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    writeJson(
      await neSubmitInitFromFiles({
        ...providerWalletConfig,
        blueprintPath,
        deploymentInfoPath,
        fraudulentBlockOutRef: args.fraudulentBlockOutRef,
        fraudulentHeaderHash: args.fraudulentHeaderHash,
      }),
    );
    return;
  }

  if (args.command === "ne-submit-step-01") {
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
    writeJson(
      await neSubmitStep01FromFiles({
        ...providerWalletConfig,
        blueprintPath,
        deploymentInfoPath,
        threadOutRef: args.threadOutRef,
        stateQueueBlockOutRef: args.stateQueueBlockOutRef,
        txInclusionPath: args.txInclusionPath,
      }),
    );
    return;
  }

  if (args.command === "ne-submit-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.inputsPath === undefined) {
      throw new Error(`Missing required --inputs <path>.\n${usage}`);
    }
    if (args.badInputIndex === undefined) {
      throw new Error(`Missing required --bad-input-index <n>.\n${usage}`);
    }
    writeJson(
      await neSubmitStep02FromFiles({
        ...providerWalletConfig,
        blueprintPath,
        deploymentInfoPath,
        threadOutRef: args.threadOutRef,
        inputsPath: args.inputsPath,
        badInputIndex: args.badInputIndex,
      }),
    );
    return;
  }

  if (
    args.command === "ne-submit-step-03" ||
    args.command === "ne-submit-step-04"
  ) {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.nonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --non-membership-proof <path>.\n${usage}`,
      );
    }
    const fromFiles =
      args.command === "ne-submit-step-03"
        ? neSubmitStep03FromFiles
        : neSubmitStep04FromFiles;
    writeJson(
      await fromFiles({
        ...providerWalletConfig,
        blueprintPath,
        deploymentInfoPath,
        threadOutRef: args.threadOutRef,
        nonMembershipProofPath: args.nonMembershipProofPath,
      }),
    );
    return;
  }

  if (args.command === "remove-fraudulent-block") {
    if (args.fraudulentHeaderHash === undefined) {
      throw new Error(
        `Missing required --fraudulent-header-hash <hex>.\n${usage}`,
      );
    }
    if (
      args.fraudCategory !== undefined &&
      args.fraudCategory !== "doubleSpend" &&
      args.fraudCategory !== "nonExistentInput"
    ) {
      throw new Error(
        `--fraud-category must be "doubleSpend" or "nonExistentInput".\n${usage}`,
      );
    }
    const output = await submitRemoveFraudulentBlockFromFiles({
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
      fraudulentHeaderHash: args.fraudulentHeaderHash,
      fraudCategory: args.fraudCategory,
      awaitConfirmation: args.awaitConfirmation,
    });

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

main().catch((error: unknown) => {
  process.stderr.write(`midgard-fault-proofs: ${formatUnknownError(error)}\n`);
  process.exitCode = 1;
});
