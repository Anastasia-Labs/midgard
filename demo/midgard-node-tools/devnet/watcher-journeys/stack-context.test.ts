import { createHash } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, relative } from "node:path";

import {
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { AllowListRefusal } from "@al-ft/midgard-core/deployment-profile";
import type { Script, UTxO } from "@lucid-evolution/lucid";
import { walletFromSeed } from "@lucid-evolution/lucid";
import { entropyToMnemonic } from "bip39";
import { makeWatcherDeploymentAuthorityFixture } from "midgard-watcher/tests/support/deployment-authority-fixture";
import { afterAll, describe, expect, it } from "vitest";

import { l1KupmiosEnvironment } from "../../src/environment.js";
import { stackPaths } from "../../src/full-stack/deployment.js";
import { configureHostNativeLedger } from "../../src/full-stack/native-ledger.js";
import { StackProcesses } from "../../src/full-stack/process.js";
import { deriveStackWallets } from "../../src/full-stack/wallets.js";
import {
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "../../tests/full-stack-fixtures.js";
import { journeyNetwork } from "./journey-network.js";
import {
  assertStackJourneyOperators,
  readStackJourneyDeployment,
  resolveStackJourneyLayout,
  resolveStackJourneyReferences,
  STACK_JOURNEY_SEED_ENV,
  stackComposeProject,
  type StackJourneyRole,
} from "./stack-context.js";

const roles = Object.keys(STACK_JOURNEY_SEED_ENV) as StackJourneyRole[];

/** A recorded stack: the example configuration, its environment and the drill's keys. */
const recordedStack = async () => {
  const { config } = await stackFixture();
  const env: Record<string, string> = {
    ...stackEnvironment(config),
    KUPO_PORT: "21442",
    OGMIOS_PORT: "21337",
  };
  roles.forEach((role, index) => {
    env[STACK_JOURNEY_SEED_ENV[role]] = entropyToMnemonic(
      (0x70 + index).toString(16).padStart(32, "0"),
    );
  });
  return { config, env, processes: new StackProcesses(config, env) };
};

/** Every secret value of an environment: anything but a port or a count. */
const secretsOf = (env: Readonly<Record<string, string>>) =>
  Object.values(env).filter((value) => value.length > 16);

const refusal = (action: () => unknown): Error => {
  try {
    action();
  } catch (cause) {
    return cause as Error;
  }
  throw new Error("Expected a refusal");
};

afterAll(removeStackFixtures);

describe("stack journey layout", () => {
  it("reads the files the stack writes, the endpoints it publishes and the drill's own keys", async () => {
    const { config, env, processes } = await recordedStack();
    const layout = resolveStackJourneyLayout(processes, "drill-1");
    expect(layout.manifestPath).toBe(stackPaths(processes).manifest);
    const host = configureHostNativeLedger(
      new StackProcesses(config, { ...env }),
    );
    expect(layout.node).toEqual({
      socketPath: host.L1_NODE_SOCKET_PATH,
      binaryPath: host.L1_NODE_TRANSPORT_BINARY_PATH,
      nodeConfigPath: host.L1_NODE_CONFIG_PATH,
    });
    const endpoints = l1KupmiosEnvironment(env);
    expect([layout.kupoUrl, layout.ogmiosUrl]).toEqual([
      endpoints.kupoUrl,
      endpoints.ogmiosUrl,
    ]);
    const stackWallets = deriveStackWallets(config, env);
    expect(layout.proverAddress).toBe(stackWallets.prover);
    for (const role of roles)
      expect(layout.wallets[role].address).toBe(
        walletFromSeed(env[STACK_JOURNEY_SEED_ENV[role]]!, {
          network: journeyNetwork().network,
        }).address,
      );
    const recorded = JSON.stringify(layout);
    for (const secret of secretsOf(env)) expect(recorded).not.toContain(secret);
  });

  it("keeps each drill run in its own resumable directory inside the stack run", async () => {
    const { config, processes } = await recordedStack();
    const first = resolveStackJourneyLayout(processes, "drill-1");
    const again = resolveStackJourneyLayout(processes, "drill-1");
    const second = resolveStackJourneyLayout(processes, "drill-2");
    expect(again.directory).toBe(first.directory);
    expect(second.directory).not.toBe(first.directory);
    for (const { directory } of [first, second]) {
      const inside = relative(config.runDirectory, directory);
      expect(inside).not.toBe("");
      expect(inside.startsWith("..")).toBe(false);
    }
    for (const runId of ["", "../escape", "a/b", ".hidden"])
      expect(() => resolveStackJourneyLayout(processes, runId)).toThrow(
        /run id/u,
      );
  });

  it("attaches to the watcher in the stack's own Compose project, with its mounted configuration", async () => {
    const { config, env } = await recordedStack();
    const named = resolveStackJourneyLayout(
      new StackProcesses(config, {
        ...env,
        COMPOSE_PROJECT_NAME: "midgard-node-0a1b2c",
      }),
      "drill",
    );
    expect(named.watcher).toEqual({
      composeProject: "midgard-node-0a1b2c",
      processConfigPath: join(
        config.watcher.configDirectory,
        "watcher-process.json",
      ),
    });
    // Without a name, Compose names the project after the node directory.
    expect(stackComposeProject("/srv/Midgard Node.v2", {})).toBe(
      "midgardnodev2",
    );
    expect(stackComposeProject("/srv/_midgard-node", {})).toBe("midgard-node");
    for (const project of ["Midgard", "-midgard", "midgard node"])
      expect(() =>
        stackComposeProject("/srv/midgard-node", {
          COMPOSE_PROJECT_NAME: project,
        }),
      ).toThrow(/Compose project/u);
    expect(() => stackComposeProject("/srv/___", {})).toThrow(
      /Compose project/u,
    );
  });

  it("refuses a missing drill key by its name and never echoes a secret", async () => {
    for (const role of roles) {
      const { config, env } = await recordedStack();
      const name = STACK_JOURNEY_SEED_ENV[role];
      const { [name]: _removed, ...rest } = env;
      const error = refusal(() =>
        resolveStackJourneyLayout(new StackProcesses(config, rest), "drill"),
      );
      expect(error.message).toContain(name);
      for (const secret of secretsOf(env))
        expect(error.message).not.toContain(secret);
    }
  });

  it("refuses a drill key shared with a stack wallet or another drill wallet", async () => {
    const { config, env } = await recordedStack();
    const sharedWithStack = {
      ...env,
      [STACK_JOURNEY_SEED_ENV.operator]: env[config.wallets.operator!.seedEnv]!,
    };
    const sharedWithDrill = {
      ...env,
      [STACK_JOURNEY_SEED_ENV.funding]:
        env[STACK_JOURNEY_SEED_ENV.ledgerOwner]!,
    };
    for (const shared of [sharedWithStack, sharedWithDrill]) {
      const error = refusal(() =>
        resolveStackJourneyLayout(new StackProcesses(config, shared), "drill"),
      );
      expect(error.message).toMatch(/shared/u);
      for (const secret of secretsOf(shared))
        expect(error.message).not.toContain(secret);
    }
  });
});

describe("stack journey operators", () => {
  it("admits the drill only when the allow-list names both distinct operators", async () => {
    const { processes } = await recordedStack();
    const layout = resolveStackJourneyLayout(processes, "drill");
    const keys = {
      honest: layout.honestOperatorKeyHash,
      drill: layout.wallets.operator.keyHash,
    };
    expect(() =>
      assertStackJourneyOperators([keys.honest, keys.drill], keys),
    ).not.toThrow();
    expect(() => assertStackJourneyOperators(null, keys)).not.toThrow();
    expect(() => assertStackJourneyOperators([keys.honest], keys)).toThrow(
      AllowListRefusal,
    );
    expect(() => assertStackJourneyOperators([keys.drill], keys)).toThrow(
      AllowListRefusal,
    );
    expect(() =>
      assertStackJourneyOperators(null, { ...keys, drill: keys.honest }),
    ).toThrow(/our node's own operator key/u);
  });
});

describe("stack journey deployment", () => {
  const blueprintJson = JSON.stringify({ preamble: { title: "recorded" } });
  const fixture = makeWatcherDeploymentAuthorityFixture({
    network: journeyNetwork().network,
    blueprintHash: createHash("sha256").update(blueprintJson).digest("hex"),
  });
  const manifest = fixture.signedIdentity.manifest as DeploymentManifest;
  const directories: string[] = [];
  afterAll(() =>
    Promise.all(
      directories.map((path) => rm(path, { recursive: true, force: true })),
    ),
  );
  const recordedRelease = async (
    manifestJson: unknown,
    blueprint = blueprintJson,
  ) => {
    const directory = await mkdtemp(join(tmpdir(), "stack-journey-release-"));
    directories.push(directory);
    const manifestPath = join(directory, "contract-deployment-info.json");
    const blueprintPath = join(directory, "plutus.json");
    await writeFile(manifestPath, JSON.stringify(manifestJson));
    await writeFile(blueprintPath, blueprint);
    return { manifestPath, blueprintPath };
  };

  it("opens the compiled profile's manifest with the blueprint it commits to", async () => {
    const opened = await readStackJourneyDeployment({
      ...(await recordedRelease(manifest)),
      handshakeMagic: journeyNetwork().magic,
    });
    expect(opened.manifest.manifestId).toBe(manifest.manifestId);
    expect(opened.blueprintJson).toBe(blueprintJson);
  });

  it("refuses a node on another network, another blueprint, or an edited manifest", async () => {
    await expect(
      readStackJourneyDeployment({
        ...(await recordedRelease(manifest)),
        handshakeMagic: journeyNetwork().magic + 1,
      }),
    ).rejects.toThrow(/node magic/u);
    await expect(
      readStackJourneyDeployment({
        ...(await recordedRelease(manifest, `${blueprintJson} `)),
        handshakeMagic: journeyNetwork().magic,
      }),
    ).rejects.toThrow(/blueprint/u);
    await expect(
      readStackJourneyDeployment({
        ...(await recordedRelease({
          ...manifest,
          updatedAt: "2026-07-29T00:00:00.000Z",
          genesis: { ...manifest.genesis, headerHash: "11".repeat(28) },
        })),
        handshakeMagic: journeyNetwork().magic,
      }),
    ).rejects.toThrow();
  });

  const referenceUtxos = (): UTxO[] =>
    Object.entries(manifest.referenceScripts).map(([role, reference]) => {
      const contract = manifest.contracts[
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ]
      ] as unknown as { contract: { type: Script["type"]; cborHex: string } };
      const [txHash, outputIndex] = reference.outRef.split("#");
      return {
        txHash: txHash!,
        outputIndex: Number(outputIndex),
        address: "addr_test1vqrecorded",
        assets: { lovelace: 20_000_000n, [reference.roleUnit]: 1n },
        scriptRef: {
          type: contract.contract.type,
          script: contract.contract.cborHex,
        },
      };
    });
  const lookup = (utxos: UTxO[]) => async () => utxos;

  it("resolves every confirmed reference by contract from its recorded out-ref", async () => {
    const references = await resolveStackJourneyReferences(
      manifest,
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
      lookup(referenceUtxos()),
    );
    expect(new Set(references.keys())).toEqual(
      new Set(
        Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
      ),
    );
    for (const [role, reference] of Object.entries(manifest.referenceScripts)) {
      const utxo = references.get(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ],
      )!;
      expect(`${utxo.txHash}#${utxo.outputIndex}`).toBe(reference.outRef);
    }
  });

  it("refuses a reference that is missing, carries another script, or lacks its role token", async () => {
    const genuine = referenceUtxos();
    const [first] = genuine;
    const missing = genuine.slice(1);
    const other = genuine.find(
      ({ scriptRef }) => scriptRef!.script !== first!.scriptRef!.script,
    )!;
    const swapped = [{ ...first!, scriptRef: other.scriptRef }, ...missing];
    const unauthenticated = [
      { ...first!, assets: { lovelace: first!.assets.lovelace! } },
      ...missing,
    ];
    for (const utxos of [missing, swapped, unauthenticated])
      await expect(
        resolveStackJourneyReferences(
          manifest,
          DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
          lookup(utxos),
        ),
      ).rejects.toThrow(/deployment reference/u);
  });
});
