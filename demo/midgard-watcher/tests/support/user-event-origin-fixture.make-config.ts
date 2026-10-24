import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import {
  buildEventHistoryDeployments,
  buildHubOracleMintingValidator,
  buildTxOrderValidators,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { type Script, validatorToScriptHash } from "@lucid-evolution/lucid";
import { deriveWatcherInitializationFrame } from "midgard-node/tests/helpers/derived-watcher-initialization-frame";

import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  makeWatcherAuthorityContracts,
  makeWatcherDeploymentAuthorityFixture,
  WATCHER_EMULATOR_HISTORY_RECIPE,
} from "./deployment-authority-fixture.js";

// Explicitly synthetic local test chain. The initialization and reference frames
// are the real node initialization on a deterministic emulator, derived once per
// run from the current blueprint. Header keys/signatures are dummy
// fixture data: this proves native framing/W12/application authority wiring only,
// and makes no consensus validity, public-chain inclusion or historical claim.
export const GENESIS_BYTES = JSON.stringify({ networkMagic: 1 });

const GENESIS = createHash("sha256").update(GENESIS_BYTES).digest("hex");

export const makeConfig = (
  NODE_CONFIG_PATH: string,
  GENESIS_CONFIG_PATH: string,
) =>
  Object.freeze({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: Object.freeze({
      source: Object.freeze({
        sourceMode: "local_node",
        authorityNodeId: "watcher-node",
        chainSync: Object.freeze({
          kind: "cardano_node_socket",
          socketPath: "/run/cardano/node.socket",
          nodeConfigPath: NODE_CONFIG_PATH,
          genesisConfigPath: GENESIS_CONFIG_PATH,
          genesisIdentitySha256: GENESIS,
        }),
      }),
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
      finality: Object.freeze({
        depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
      }),
    }),
    da: Object.freeze({
      peers: Object.freeze([
        {
          identity: "da-peer-a",
          multiaddr:
            "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
        },
      ]),
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
    }),
    storage: Object.freeze({
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
      rollbackAuthorityKeySource: Object.freeze({
        kind: "environment",
        variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
      }),
    }),
    proverWallet: Object.freeze({
      keySource: Object.freeze({
        kind: "environment",
        variable: "MIDGARD_WATCHER_PROVER_KEY",
      }),
    }),
    deadlines: Object.freeze({
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    }),
  });

/** The node initialization frame the origin deployment and chains replay. */
export const originInitialization = deriveWatcherInitializationFrame;

export const blueprintBytes = readFileSync(
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
    fileURLToPath(
      new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
    ),
);

const blueprint = parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString("utf8")) as unknown,
);

export const buildWatcherOriginFixtureHistoryDeployments = (
  oneShot: string,
  hubOraclePolicyId: string,
) =>
  buildEventHistoryDeployments({
    blueprint,
    network: "Preprod",
    hubOraclePolicyId,
    initializationNonce: parseOutRefLabel(oneShot),
    protectionDurationMs: BigInt(
      WATCHER_EMULATOR_HISTORY_RECIPE.protectionDurationMs,
    ),
    bounds: {
      inlineLimitBytes: BigInt(
        WATCHER_EMULATOR_HISTORY_RECIPE.bounds.inlineLimitBytes,
      ),
      maxPayloadBytes: BigInt(
        WATCHER_EMULATOR_HISTORY_RECIPE.bounds.maxPayloadBytes,
      ),
      maxPayloadNodes: BigInt(
        WATCHER_EMULATOR_HISTORY_RECIPE.bounds.maxPayloadNodes,
      ),
    },
  });

/**
 * The synthetic origin deployment. `scripts` replaces named applied
 * contracts (for example the state queue of an emulator ledger the
 * deployment follows); every other contract is the synthetic one.
 */
export const makeOriginDeployment = async (
  ruleBundleCommitment?: string,
  scripts: Readonly<Record<string, Script>> = {},
) => {
  const { canonicalOneShotOutRef } = await originInitialization();
  const contractSet = makeWatcherAuthorityContracts();
  const hub = buildHubOracleMintingValidator({
    blueprint,
    oneShotOutRef: parseOutRefLabel(canonicalOneShotOutRef),
  });
  const input = {
    blueprint,
    network: "Preprod" as const,
    hubOraclePolicyId: hub.policyId,
  };
  const history = buildWatcherOriginFixtureHistoryDeployments(
    canonicalOneShotOutRef,
    hub.policyId,
  );
  const deposit = history.deposit.list;
  const withdrawal = history.withdrawal.list;
  const { txOrder, fieldPreimageCertificate } = buildTxOrderValidators(input);
  const applied = {
    hubOracleMint: hub.mintingScript,
    depositMint: deposit.mintingScript,
    depositSpend: deposit.spendingScript,
    withdrawalMint: withdrawal.mintingScript,
    withdrawalSpend: withdrawal.spendingScript,
    depositHistoryRetentionSpend: history.deposit.retention.spendingScript,
    depositHistoryRetirementWithdraw:
      history.deposit.retirement.withdrawalScript,
    withdrawalHistoryRetentionSpend:
      history.withdrawal.retention.spendingScript,
    withdrawalHistoryRetirementWithdraw:
      history.withdrawal.retirement.withdrawalScript,
    txOrderMint: txOrder.mintingScript,
    txOrderSpend: txOrder.spendingScript,
    fieldPreimageCertificateMint: fieldPreimageCertificate.mintingScript,
    ...scripts,
  };
  for (const [name, script] of Object.entries(applied)) {
    contractSet.contracts[name] = {
      ...contractSet.contracts[name],
      contract: { type: script.type, cborHex: script.script },
      scriptHash: validatorToScriptHash(script),
    };
  }
  for (const [role, name] of Object.entries(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  )) {
    contractSet.referenceScripts[role] = {
      ...contractSet.referenceScripts[role],
      scriptHash: contractSet.contracts[name].scriptHash,
    };
  }
  return makeWatcherDeploymentAuthorityFixture({
    contractSet,
    blueprintHash: createHash("sha256").update(blueprintBytes).digest("hex"),
    hubOracleOneShotOutRef: canonicalOneShotOutRef,
    eventHistoryRecipe: WATCHER_EMULATOR_HISTORY_RECIPE,
    ...(ruleBundleCommitment === undefined ? {} : { ruleBundleCommitment }),
  });
};

export type MadeOriginDeployment = Awaited<
  ReturnType<typeof makeOriginDeployment>
>;

export type Point = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
  pointId: string;
}>;

export type SyntheticUserEventBlock = Readonly<{
  point: Point;
  parentPoint: Point;
  nativeBlock: ReturnType<typeof admitWatcherNativeRollForwardBlock>;
}>;

export type Creating = Readonly<{
  txHash: string;
  transactionCbor: string;
  creatingPoint: Point;
  predecessorPoint: Point;
  creatingTransactionIndex: number;
  inputOutRefs: readonly string[];
  outputs: readonly CreatingOutput[];
}>;

export type CreatingOutput = Readonly<{
  transaction_index: number;
  transaction_id: string;
  output_index: number;
  address: string;
  value: Readonly<{ coins: string; assets: Readonly<Record<string, string>> }>;
  datum_hash: string | null;
  datum_type?: "hash" | "inline";
  script_hash: string | null;
  created_at: Readonly<{ slot_no: number; header_hash: string }>;
  datum: string | null;
  script: Readonly<{ language: string; script: string }> | null;
}>;

export type Consumption = Readonly<{
  transaction_id: string;
  input_index: number;
  slot_no: number;
  header_hash: string;
}>;

// Keep only primitive data after indexing; CML child getters allocate independent
// WASM wrappers, so release every wrapper before leaving this synchronous scope.
export const withFixtureCml = <Result>(
  read: (
    own: <T extends { free(): void } | undefined>(value: T) => T,
  ) => Result,
): Result => {
  const allocated: { free(): void }[] = [];
  const own = <T extends { free(): void } | undefined>(value: T): T => {
    if (value !== undefined) allocated.push(value);
    return value;
  };
  try {
    return read(own);
  } finally {
    for (const value of allocated.reverse()) value.free();
  }
};

export const pointAt = (
  blockHash: string,
  blockNo: bigint,
  slot: bigint,
): Point => {
  const point = {
    blockHash,
    blockNo: blockNo.toString(),
    slot: slot.toString(),
  };
  return Object.freeze({
    ...point,
    pointId: computeFraudProofRawL1PointId(point),
  });
};
