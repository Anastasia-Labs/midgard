import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { isAbsolute, join } from "node:path";

import {
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  Lucid,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";
import { WatcherLocalKupmios } from "midgard-watcher";

import { readOgmiosTipSlot } from "./ledger-tip.js";
import { createLiveWorkflowChain } from "./live-chain.js";
import { journeyNativeNodeQuery } from "./native-node.js";

type Deployment = Awaited<ReturnType<typeof publishWorkflowDeploymentOnChain>>;

type PersistedDeployment = Omit<
  Deployment,
  "chain" | "operatorLucid" | "publisherLucid" | "references"
> & {
  references: [string, UTxO][];
};

/** Reopen run-owned artifacts and real providers after a stopped journey. */
/**
 * L1 depth the journey watcher requires before a block is finalized. Finality
 * anchors evidence: the finalized audit anchor of a completed workflow, its
 * incident records and its evidence stamp. It no longer gates the actions the
 * journey drives. It is the compiled deployment profile's release depth (3
 * for local-devnet-testing), which the watcher process config must match.
 */
export const JOURNEY_FINALITY_DEPTH =
  DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

/**
 * L1 depth at which the journey watcher acts on an observation. Once a step's
 * inputs are in a block this deep, the next step is built and submitted; a
 * rollback invalidates cached authority, re-observes canonical state, and
 * reconciles submitted attempts before resuming. A valid attempt can reuse
 * its signed bytes under fresh authorization. Release depth remains unchanged.
 */
export const JOURNEY_ACTION_DEPTH = 1;

export const loadJourneyContext = async (runDirectory: string) => {
  if (!isAbsolute(runDirectory))
    throw new Error("Journey run directory must be absolute");
  const runEnv = Object.fromEntries(
    (await readFile(join(runDirectory, "run.env"), "utf8"))
      .trim()
      .split("\n")
      .map((line) => {
        const separator = line.indexOf("=");
        return [line.slice(0, separator), line.slice(separator + 1)];
      }),
  );
  if (runEnv.MIDGARD_PHASE4_RUN_DIR !== runDirectory)
    throw new Error("Run environment changed its directory");
  const genesisBytes = await readFile(
    join(runDirectory, "genesis/shelley-genesis.json"),
  );
  const genesis = JSON.parse(genesisBytes.toString("utf8"));
  if (genesis.networkMagic !== 424242 || genesis.networkId !== "Testnet")
    throw new Error("Journey requires its isolated devnet genesis");
  const customNetwork = {
    networkMagic: genesis.networkMagic as number,
    slotConfig: {
      zeroTime: Date.parse(genesis.systemStart),
      zeroSlot: 0,
      slotLength: genesis.slotLength * 1000,
    },
  };
  const kupoUrl = `http://127.0.0.1:${runEnv.MIDGARD_PHASE4_KUPO_PORT}`;
  const ogmiosUrl = `http://127.0.0.1:${runEnv.MIDGARD_PHASE4_OGMIOS_PORT}`;
  const provider = new WatcherLocalKupmios(
    kupoUrl,
    ogmiosUrl,
    await journeyNativeNodeQuery(runDirectory),
    // Slow block inclusion must not abort an otherwise healthy shared session.
    { awaitTxTimeoutMs: 10 * 60_000 },
  );
  const accounts: Record<
    "operator" | "publisher" | "cosigner" | "availability",
    { seedPhrase: string; address: string }
  > = JSON.parse(
    await readFile(join(runDirectory, "secrets/journey-accounts.json"), "utf8"),
  );
  const persisted: PersistedDeployment = JSON.parse(
    await readFile(
      join(runDirectory, "deploymentInfo/live-deployment.json"),
      "utf8",
    ),
    (_, value) =>
      value !== null &&
      typeof value === "object" &&
      Object.keys(value).length === 1 &&
      typeof value.bigint === "string"
        ? BigInt(value.bigint)
        : value,
  );
  const verified = verifyFinalizedDeploymentManifest(persisted.manifest);
  if (
    verified.network !== "Custom" ||
    createHash("sha256").update(persisted.blueprintJson).digest("hex") !==
      persisted.manifest.artifacts.blueprintHash
  ) {
    throw new Error(
      "Stored deployment differs from its frozen blueprint or network",
    );
  }
  const references = new Map(persisted.references);
  for (const receipt of persisted.receipts) {
    const reference = references.get(receipt.contractName);
    if (
      reference?.scriptRef == null ||
      validatorToScriptHash(reference.scriptRef) !== receipt.scriptHash ||
      reference.txHash !== receipt.outRef.txHash ||
      reference.outputIndex !== receipt.outRef.outputIndex
    ) {
      throw new Error(`Stored reference changed: ${receipt.role}`);
    }
  }
  const operatorLucid = await Lucid(provider, "Custom", {
    slotConfig: customNetwork.slotConfig,
    evaluator: createScalusEvaluator(),
  });
  const publisherLucid = await Lucid(provider, "Custom", {
    slotConfig: customNetwork.slotConfig,
    evaluator: createScalusEvaluator(),
  });
  operatorLucid.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisherLucid.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  const deployment: Deployment = {
    ...persisted,
    references,
    operatorLucid,
    publisherLucid,
    chain: {
      ...createLiveWorkflowChain({
        lucid: operatorLucid,
        slotLength: customNetwork.slotConfig.slotLength,
        readTipSlot: () => readOgmiosTipSlot(ogmiosUrl),
      }),
      blockHeight: async () => {
        const response = await fetch(ogmiosUrl, {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({
            jsonrpc: "2.0",
            method: "queryNetwork/blockHeight",
            id: null,
          }),
        });
        const { result } = (await response.json()) as { result?: unknown };
        if (typeof result !== "number" || !Number.isSafeInteger(result))
          throw new Error("Ogmios did not report a block height");
        return result;
      },
    },
  };
  return {
    runDirectory,
    runEnv,
    customNetwork,
    genesisIdentitySha256: createHash("sha256")
      .update(genesisBytes)
      .digest("hex"),
    kupoUrl,
    ogmiosUrl,
    provider,
    accounts,
    deployment,
  };
};
