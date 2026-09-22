import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { appendFile, copyFile, readFile, writeFile } from "node:fs/promises";
import { isAbsolute, join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { referenceScriptAuthUnit } from "@al-ft/midgard-sdk";
import {
  generateSeedPhrase,
  Lucid,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import {
  cardanoProtocolParametersIdentityFromProvider,
  queryLocalOgmiosProtocolParameters,
} from "midgard-node/commands/contract-deployment-info";
import {
  awaitReferenceScriptPublicationReadiness,
  type PublishedWorkflowDeploymentResume,
  publishWorkflowDeploymentOnChain,
} from "midgard-node/tests/helpers/published-workflow-deployment";
import {
  type PublicationSchedule,
  synchronizePublicationIndexer,
} from "midgard-node/tests/helpers/reference-publication-chain";
import { WatcherLocalKupmios } from "midgard-watcher";
import { expect, it } from "vitest";

import {
  readJourneyArtifact,
  writeJourneyArtifact,
  writeJourneyFile,
} from "./artifacts.js";
import { verifyJourneyConfiguration } from "./configuration.js";
import { readOgmiosTipSlot } from "./ledger-tip.js";
import { createLiveWorkflowChain } from "./live-chain.js";
import { loadJourneyContext } from "./live-context.js";
import { journeyNativeNodeQuery } from "./native-node.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined)(
  "publishes the actual installed deployment on the isolated Cardano devnet",
  async () => {
    if (runDirectory === undefined || !isAbsolute(runDirectory))
      throw new Error("An absolute run directory is required");
    const runEnv = Object.fromEntries(
      (await readFile(join(runDirectory, "run.env"), "utf8"))
        .trim()
        .split("\n")
        .map((line) => {
          const separator = line.indexOf("=");
          return [line.slice(0, separator), line.slice(separator + 1)];
        }),
    );
    const genesis = JSON.parse(
      await readFile(
        join(runDirectory, "genesis/shelley-genesis.json"),
        "utf8",
      ),
    );
    expect(genesis.networkMagic).toBe(424242);
    expect(genesis.networkId).toBe("Testnet");
    expect(genesis).toMatchObject({
      slotLength: 1,
      activeSlotsCoeff: 0.05,
      epochLength: 432000,
      securityParam: 2160,
    });
    const byronGenesis = JSON.parse(
      await readFile(join(runDirectory, "genesis/byron-genesis.json"), "utf8"),
    );
    expect(byronGenesis.protocolConsts.k).toBe(2160);
    const slotConfig = {
      zeroTime: Date.parse(genesis.systemStart),
      zeroSlot: 0,
      slotLength: genesis.slotLength * 1000,
    };
    const kupoUrl = `http://127.0.0.1:${runEnv.MIDGARD_PHASE4_KUPO_PORT}`;
    const ogmiosUrl = `http://127.0.0.1:${runEnv.MIDGARD_PHASE4_OGMIOS_PORT}`;
    const provider = new WatcherLocalKupmios(
      kupoUrl,
      ogmiosUrl,
      await journeyNativeNodeQuery(runDirectory),
    );
    const serviceDeadline = Date.now() + 180_000;
    while (true) {
      const ready = await fetch(`${ogmiosUrl}/health`)
        .then(async (response) => {
          if (!response.ok) return false;
          const health = await response.json();
          return (
            health.connectionStatus === "connected" &&
            health.currentEra === "conway"
          );
        })
        .catch(() => false);
      if (ready) break;
      if (Date.now() >= serviceDeadline)
        throw new Error("Cardano/Ogmios did not become ready");
      await pause(1000);
    }
    await verifyJourneyConfiguration({ runDirectory, ogmiosUrl });
    const parameters = await cardanoProtocolParametersIdentityFromProvider(
      provider,
      await queryLocalOgmiosProtocolParameters(ogmiosUrl),
    );
    await writeFile(
      join(runDirectory, "work/verified-protocol-parameters.json"),
      JSON.stringify(parameters, null, 2),
    );
    if (existsSync(join(runDirectory, "deploymentInfo/live-deployment.json"))) {
      const reopened = await loadJourneyContext(runDirectory);
      const blueprintPath = process.env.MIDGARD_REAL_BLUEPRINT_PATH;
      if (blueprintPath === undefined)
        throw new Error("Frozen blueprint required");
      expect(await readFile(blueprintPath, "utf8")).toBe(
        reopened.deployment.blueprintJson,
      );
      const prepared = await readJourneyArtifact<
        Pick<PublishedWorkflowDeploymentResume, "authPolicy">
      >(join(runDirectory, "work/deployment-prepared.json"));
      const publisher = reopened.deployment.publisherLucid;
      const { canonicalSlot, authorityKind } =
        await awaitReferenceScriptPublicationReadiness({
          authPolicy: prepared.authPolicy,
          publisherAddress: await publisher.wallet().address(),
          synchronize: () => synchronizePublicationIndexer(ogmiosUrl, kupoUrl),
          awaitSlot: async (slots) => {
            await pause(slots * slotConfig.slotLength);
          },
        });
      const outputs = await publisher.utxosAt(
        await publisher.wallet().address(),
      );
      for (const receipt of reopened.deployment.receipts) {
        const unit = referenceScriptAuthUnit(
          prepared.authPolicy.policyId,
          receipt.role,
        );
        expect(
          outputs.reduce(
            (quantity, output) => quantity + (output.assets[unit] ?? 0n),
            0n,
          ),
        ).toBe(1n);
        expect(
          outputs.some(
            (output) =>
              output.txHash === receipt.outRef.txHash &&
              output.outputIndex === receipt.outRef.outputIndex &&
              output.assets[unit] === 1n,
          ),
        ).toBe(true);
      }
      await writeJourneyArtifact(
        join(runDirectory, "work/publication-readiness.json"),
        {
          verifiedAt: new Date().toISOString(),
          authorityKind,
          observedCanonicalSlot: canonicalSlot,
          authorityExpirySlot: prepared.authPolicy.expiresAtSlot,
          authorityPolicyId: prepared.authPolicy.policyId,
          manifestId: reopened.deployment.manifest.manifestId,
          uniqueRetainedRoles: reopened.deployment.receipts.length,
        },
      );
      console.info(
        `Reopened finalized deployment ${reopened.deployment.manifest.manifestId}`,
      );
      return;
    }

    const accountsPath = join(runDirectory, "secrets/journey-accounts.json");
    const accounts: Record<string, { seedPhrase: string; address: string }> =
      existsSync(accountsPath)
        ? JSON.parse(await readFile(accountsPath, "utf8"))
        : Object.fromEntries(
            ["operator", "publisher", "cosigner", "availability"].map(
              (role) => {
                const seedPhrase = generateSeedPhrase();
                return [
                  role,
                  {
                    seedPhrase,
                    address: walletFromSeed(seedPhrase, { network: "Custom" })
                      .address,
                  },
                ];
              },
            ),
          );
    // Exclusive creation prevents accidentally replacing signers for a live run.
    if (!existsSync(accountsPath))
      await writeFile(
        join(runDirectory, "secrets/journey-accounts.json"),
        JSON.stringify(accounts),
        { mode: 0o600, flag: "wx" },
      );
    const cli = (...args: string[]) =>
      execFileSync(
        "docker",
        [
          "run",
          "--rm",
          "--user",
          `${process.getuid!()}:${process.getgid!()}`,
          "--volume",
          `${runDirectory}:/run`,
          "--entrypoint",
          "cardano-cli",
          runEnv.MIDGARD_PHASE4_CARDANO_NODE_IMAGE!,
          "latest",
          ...args,
        ],
        { encoding: "utf8" },
      ).trim();
    execFileSync("docker", [
      "run",
      "--rm",
      "--volume",
      `${runDirectory}/cardano/ipc:/ipc`,
      "--entrypoint",
      "sh",
      runEnv.MIDGARD_PHASE4_POSTGRES_IMAGE!,
      "-ec",
      "chmod 0666 /ipc/node.socket",
    ]);
    if (!existsSync(join(runDirectory, "work/fund-journeys.tx"))) {
      const magic = String(genesis.networkMagic);
      const socket = "/run/cardano/ipc/node.socket";
      const genesisAddress = cli(
        "genesis",
        "initial-addr",
        "--verification-key-file",
        "/run/genesis/utxo-keys/utxo1/utxo.vkey",
        "--testnet-magic",
        magic,
      );
      cli(
        "query",
        "utxo",
        "--socket-path",
        socket,
        "--testnet-magic",
        magic,
        "--address",
        genesisAddress,
        "--out-file",
        "/run/work/genesis-utxos.json",
      );
      const genesisUtxos = JSON.parse(
        await readFile(join(runDirectory, "work/genesis-utxos.json"), "utf8"),
      );
      const [fundingInput] = Object.keys(genesisUtxos);
      if (fundingInput === undefined)
        throw new Error("No devnet genesis funding input");
      cli(
        "transaction",
        "build",
        "--socket-path",
        socket,
        "--testnet-magic",
        magic,
        "--tx-in",
        fundingInput,
        ...Object.values(accounts).flatMap(({ address }) => [
          "--tx-out",
          `${address}+4000000000000`,
        ]),
        "--change-address",
        genesisAddress,
        "--out-file",
        "/run/work/fund-journeys.txbody",
      );
      cli(
        "transaction",
        "sign",
        "--tx-body-file",
        "/run/work/fund-journeys.txbody",
        "--signing-key-file",
        "/run/genesis/utxo-keys/utxo1/utxo.skey",
        "--testnet-magic",
        magic,
        "--out-file",
        "/run/work/fund-journeys.tx",
      );
      cli(
        "transaction",
        "submit",
        "--socket-path",
        socket,
        "--testnet-magic",
        magic,
        "--tx-file",
        "/run/work/fund-journeys.tx",
      );
    }
    const fundingTxHash: unknown = JSON.parse(
      cli("transaction", "txid", "--tx-file", "/run/work/fund-journeys.tx"),
    ).txhash;
    if (
      typeof fundingTxHash !== "string" ||
      !/^[0-9a-f]{64}$/.test(fundingTxHash)
    )
      throw new Error(
        "cardano-cli returned an invalid funding transaction hash",
      );
    await provider.awaitTx(fundingTxHash, 500);
    const operatorLucid = await Lucid(provider, "Custom", {
      slotConfig,
      evaluator: createScalusEvaluator(),
    });
    const publisherLucid = await Lucid(provider, "Custom", {
      slotConfig,
      evaluator: createScalusEvaluator(),
    });
    operatorLucid.selectWallet.fromSeed(accounts.operator!.seedPhrase);
    publisherLucid.selectWallet.fromSeed(accounts.publisher!.seedPhrase);
    const blueprint = process.env.MIDGARD_REAL_BLUEPRINT_PATH;
    if (blueprint === undefined) throw new Error("Frozen blueprint required");
    await copyFile(blueprint, join(runDirectory, "deploymentInfo/plutus.json"));
    const startedAt = Date.now();
    const resumePath = join(runDirectory, "work/deployment-resume.json");
    const preparedPath = join(runDirectory, "work/deployment-prepared.json");
    const publicationsPath = join(
      runDirectory,
      "work/reference-publications.ndjson",
    );
    const initializationPath = join(runDirectory, "work/initialization.cbor");
    const resume = existsSync(resumePath)
      ? await readJourneyArtifact<PublishedWorkflowDeploymentResume>(resumePath)
      : existsSync(preparedPath)
        ? {
            ...(await readJourneyArtifact<
              Pick<PublishedWorkflowDeploymentResume, "nonce" | "authPolicy">
            >(preparedPath)),
            publications: existsSync(publicationsPath)
              ? (await readFile(publicationsPath, "utf8"))
                  .trim()
                  .split("\n")
                  .map((line) => JSON.parse(line))
              : [],
            ...(existsSync(initializationPath)
              ? {
                  initializationCbor: await readFile(
                    initializationPath,
                    "utf8",
                  ),
                }
              : {}),
          }
        : undefined;
    const deployment = await publishWorkflowDeploymentOnChain({
      network: "Custom",
      accounts: {
        operator: accounts.operator!,
        publisher: accounts.publisher!,
        cosigner: accounts.cosigner!,
      },
      operatorLucid,
      publisherLucid,
      chain: createLiveWorkflowChain({
        lucid: operatorLucid,
        slotLength: slotConfig.slotLength,
        readTipSlot: () => readOgmiosTipSlot(ogmiosUrl),
      }),
      protocolParameters: parameters.snapshot,
      publicationJournalPath: join(
        runDirectory,
        "work/publication-transactions.ndjson",
      ),
      publicationSchedule: await readJourneyArtifact<PublicationSchedule>(
        join(runDirectory, "work/publication-schedule.json"),
      ),
      publicationSynchronize: () =>
        synchronizePublicationIndexer(ogmiosUrl, kupoUrl),
      publicationMaxTargetsPerBatch: 8,
      resume,
      onPrepared: async (context) => {
        await writeJourneyArtifact(
          join(runDirectory, "work/deployment-prepared.json"),
          context,
        );
      },
      onInitialization: async (signedCbor) => {
        await writeJourneyFile(
          join(runDirectory, "work/initialization.cbor"),
          signedCbor,
        );
      },
      onPublication: async (receipt) => {
        console.info(
          `Published ${receipt.role} after ${((Date.now() - startedAt) / 1000).toFixed(1)}s`,
        );
        await appendFile(
          join(runDirectory, "work/reference-publications.ndjson"),
          `${JSON.stringify(receipt)}\n`,
        );
      },
    });
    const {
      chain: _chain,
      operatorLucid: _operator,
      publisherLucid: _publisher,
      ...persisted
    } = deployment;
    await writeJourneyFile(
      join(runDirectory, "deploymentInfo/live-deployment.json"),
      JSON.stringify(
        {
          ...persisted,
          references: [...deployment.references],
        },
        (_, value) =>
          typeof value === "bigint" ? { bigint: value.toString() } : value,
      ),
    );
    await writeJourneyFile(
      join(runDirectory, "deploymentInfo/manifest.json"),
      JSON.stringify(deployment.manifest, null, 2),
    );
    console.info(
      `Live deployment completed in ${((Date.now() - startedAt) / 1000).toFixed(1)}s`,
    );
  },
);
