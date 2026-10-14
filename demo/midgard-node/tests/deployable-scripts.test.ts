import "./utils.js";

import { createHash } from "node:crypto";

import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { collectScriptDescriptors } from "../src/commands/contract-deployment-info.js";
import {
  MANIFEST_ORDER,
  PUBLICATION_ORDER,
  REFERENCE_SCRIPT_COMMAND_NAMES,
} from "../src/deployable-scripts.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "../src/deployment-manifest.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {} from "../src/transactions/reference-scripts.js";
import {
  dataCanonicityContracts,
  dataCanonicityRoles,
  HISTORY_CATALOGUE_ADDITIONS,
  historyContracts,
  historyRoles,
  POOL_CATALOGUE_ADDITIONS,
  poolContracts,
  poolRoles,
  withRetiredBondRole,
} from "./helpers/deployable-catalogue-additions.js";
import {
  manifestDeployableScripts,
  nodeRuntimeReferenceScriptTargets,
  referenceScriptTargetsByCommand,
} from "./helpers/deployable-catalogue-before-canonical.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const orderDigest = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value)).digest("hex");

const CONTRACT_BY_ROLE: Readonly<Record<string, string>> =
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE;

const ROLE_BY_CONTRACT = new Map(
  Object.entries(CONTRACT_BY_ROLE).map(([role, contract]) => [contract, role]),
);

// These digests pin ORDER, not script bytes: the manifest order is the key
// order of `contract-deployment-info.json` (and so its digest), and the
// publication order decides which reference scripts share a publication batch.
// Script bytes are pinned by the bundle digests in `midgard-contracts.test.ts`.
//
// Every pin below, including the pre-history and pre-pool generations, was
// re-derived from the current catalogue when the missingNativeScriptTx and
// missingNativeScriptUtxo families (15 contracts and roles) were removed;
// missingScriptSource supersedes these families. The earlier digests and
// counts included those roles, so the older generations now describe the
// catalogue as it was before each addition, less those 15 roles.
//
// They were re-derived the same way when the cross-block duplicate-event
// family (two contracts and roles) was deleted. Inserting its two adjacent
// step entries back at one position reproduced every previous digest of
// every generation, so the deletion removed exactly those two entries and
// moved nothing else.
//
// They were re-derived the same way when the post-UTxO minimum-Ada mode
// retired its step-02 UTxO yield and step-05 (two contracts and roles).
// Inserting the yield before the correction-lock spend and step-05 after
// step-04 (in the manifest, the publication order and the node-runtime
// command) reproduced every previous digest of every generation. So did the
// three retired final-4 claim yields, inserted back after the L2 replay yield.
const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "c68fb6aace2567420095fced89151672b2206e10435ed16c65e8838170262f51";
const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "eef64fd401b648b79f4219d6c7e9c0df78be513a0f045c5a65a9e4fb409e214a";
const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "c39117d840cbd3a7cc1d953cc6ad8048df698b8d829cf136a81583f2215dfc4d";
const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "ff9281ce5a3d8877fcf777994a6f65cc19ac38a247bf6de458cd5b2adc1f2521";
const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "07122773835fd062fc5ef6544f497865c16425442d3c13ec9e9ceca40e7e6ca6";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "d3cb72a65020d36c60c76be7589f9e3b68baf99892c4ae8f2e02b8272f9e5343";
const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "0670accf7467bd79d8bd1588066d45fa2081de36384d891b529f80b15e93955e";
const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "eaf82736cf5f9ec5495b5e0e6a1512192c3558adfae05662cd4adad1c84f4ab5";
const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "eef250aa0610302c1afb12201769fb6a0083c355bff80f401843aeaf06aac3f3";
const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3cc8bac05e38516f6ef5292b2c8d97dfb0e4a0e8c2d9a6553b461f7ee283efd8";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "8be714ebe04c5b70477715280435c4df87b93872cffb073867a6740b69ac5c1f";
const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "81b8e4d09c0d25e1a024cd3b28240dfaf6188ca4d49f0b93369341b7dfcf67c8";
const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "cdadb143599388810488d20c274cfb31e1f85f8526bfe4a6dfc62e033a0add91";
const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "a44d27371ef984600d8c78c5d1fc0eb1037b133c906da91293dbd0bbff5f55e2";
const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "ddee6dae4ef81c8b64091a531745e1dc0fc14fb88d97db91dedd36351430c51c";
const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "4aeac0be36e07f0a6aa9b9a38526aac2440a712981ee0da5b575b1a5ee3e473f";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
const MANIFEST_CONTRACT_ORDER_DIGEST =
  "96d5746ccd18de9194f4c72cdb37dbc63f616298624ccf2095a624f76639267a";
const MANIFEST_ROLE_ORDER_DIGEST =
  "ab98acc6f11bd0624b7da66732a409a2dd27ed09d5066317ca9dbf6c0acde969";
const REAL_PUBLICATION_ORDER_DIGEST =
  "33bd457dc0ef00b5b1cd51a522f96a1eb11e429e047efcc14b61d3fdef879174";
const REAL_COMMAND_ORDER_DIGEST =
  "ee0a86066bdc3d75891594e0dccdc8be69d263796a9da511eeb8d0a6882b27e2";
const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "d0122b56094f45603ab0ef5850f6a8dd08eccf26d1ef2d4fd8a8cbd0f0b6a344";
const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "9e2e5454b0bbdc8b93c3f6e4eac3f09be41dd9fb20caecb8fe2d5334401a250a";

const commandOrder = (contracts: SDK.MidgardValidators) => {
  const byCommand = referenceScriptTargetsByCommand(contracts);
  return REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
    commandName,
    byCommand[commandName].map(({ name }) => name),
  ]);
};

describe("deployable-script catalogue", () => {
  let real: SDK.MidgardValidators;
  let placeholder: SDK.MidgardValidators;

  beforeAll(async () => {
    real = await loadRealMidgardContractsForTest({
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
    placeholder = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
  }, 120_000);

  it("orders publication over every catalogue section exactly once", () => {
    const publicationSections = PUBLICATION_ORDER.flatMap((step) =>
      typeof step === "string" ? [step] : step.interleaveByChain,
    );
    expect(new Set(publicationSections).size).toEqual(
      publicationSections.length,
    );
    expect([...publicationSections].sort()).toEqual([...MANIFEST_ORDER].sort());
  });

  it("pins the manifest order of contracts and roles", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts);
      expect(manifest).toHaveLength(515);
      expect(orderDigest(manifest.map(({ contract }) => contract))).toEqual(
        MANIFEST_CONTRACT_ORDER_DIGEST,
      );
      expect(orderDigest(manifest.map(({ role }) => role ?? null))).toEqual(
        MANIFEST_ROLE_ORDER_DIGEST,
      );
    }
  });

  it("pins the publication and per-command orders", () => {
    const realTargets = nodeRuntimeReferenceScriptTargets(real);
    expect(realTargets).toHaveLength(510);
    expect(orderDigest(realTargets.map(({ name }) => name))).toEqual(
      REAL_PUBLICATION_ORDER_DIGEST,
    );
    expect(orderDigest(commandOrder(real))).toEqual(REAL_COMMAND_ORDER_DIGEST);

    // The always-succeeds CEK stand-in is the tx-order spend script and is
    // not published as a distinct deployed script.
    const placeholderTargets = nodeRuntimeReferenceScriptTargets(placeholder);
    expect(placeholderTargets.map(({ name }) => name)).toEqual(
      realTargets
        .map(({ name }) => name)
        .filter(
          (name) => name !== "V1 immutable CEK program-material publication",
        ),
    );
    expect(orderDigest(placeholderTargets.map(({ name }) => name))).toEqual(
      PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
    );
    expect(orderDigest(commandOrder(placeholder))).toEqual(
      PLACEHOLDER_COMMAND_ORDER_DIGEST,
    );
  });

  it("preserves every pre-data-canonicity order after excluding exactly its four additions", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts).filter(
        ({ contract }) => !dataCanonicityContracts.has(contract),
      );
      expect(manifest).toHaveLength(511);
      expect(orderDigest(manifest.map(({ contract }) => contract))).toEqual(
        PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST,
      );
      expect(orderDigest(manifest.map(({ role }) => role ?? null))).toEqual(
        PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST,
      );
      const without = (names: readonly string[]) =>
        names.filter((name) => !dataCanonicityRoles.has(name));
      const targets = without(
        nodeRuntimeReferenceScriptTargets(contracts).map(({ name }) => name),
      );
      expect(targets).toHaveLength(contracts === real ? 506 : 505);
      expect(orderDigest(targets)).toEqual(
        contracts === real
          ? PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST
          : PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
      );
      const byCommand = referenceScriptTargetsByCommand(contracts);
      expect(
        orderDigest(
          REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
            commandName,
            without(byCommand[commandName].map(({ name }) => name)),
          ]),
        ),
      ).toEqual(
        contracts === real
          ? PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST
          : PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      );
    }
  });

  it("preserves every original published order after excluding exactly the six history, two pool and four data-canonicity additions and restoring the retired bond yield", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts).filter(
        ({ contract }) =>
          !historyContracts.has(contract) &&
          !poolContracts.has(contract) &&
          !dataCanonicityContracts.has(contract),
      );
      const roles = withRetiredBondRole(
        manifest.map(({ role }) => role ?? null),
      );
      expect(roles).toHaveLength(504);
      expect(orderDigest(roles)).toEqual(
        PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST,
      );
      const targets = withRetiredBondRole(
        nodeRuntimeReferenceScriptTargets(contracts)
          .map(({ name }) => name)
          .filter(
            (name) =>
              !historyRoles.has(name) &&
              !poolRoles.has(name) &&
              !dataCanonicityRoles.has(name),
          ),
      );
      expect(targets).toHaveLength(contracts === real ? 499 : 498);
      expect(orderDigest(targets)).toEqual(
        contracts === real
          ? PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST
          : PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
      );
      const byCommand = referenceScriptTargetsByCommand(contracts);
      expect(
        orderDigest(
          REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
            commandName,
            withRetiredBondRole(
              byCommand[commandName]
                .map(({ name }) => name)
                .filter(
                  (name) =>
                    !historyRoles.has(name) &&
                    !poolRoles.has(name) &&
                    !dataCanonicityRoles.has(name),
                ),
            ),
          ]),
        ),
      ).toEqual(
        contracts === real
          ? PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST
          : PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      );
    }
  });

  it("preserves every pre-pool published order after excluding exactly the two pool and four data-canonicity additions and restoring the retired bond yield", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts).filter(
        ({ contract }) =>
          !poolContracts.has(contract) &&
          !dataCanonicityContracts.has(contract),
      );
      const roles = withRetiredBondRole(
        manifest.map(({ role }) => role ?? null),
      );
      expect(roles).toHaveLength(510);
      expect(orderDigest(roles)).toEqual(PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST);
      const targets = withRetiredBondRole(
        nodeRuntimeReferenceScriptTargets(contracts)
          .map(({ name }) => name)
          .filter(
            (name) => !poolRoles.has(name) && !dataCanonicityRoles.has(name),
          ),
      );
      expect(targets).toHaveLength(contracts === real ? 505 : 504);
      expect(orderDigest(targets)).toEqual(
        contracts === real
          ? PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST
          : PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
      );
      const byCommand = referenceScriptTargetsByCommand(contracts);
      expect(
        orderDigest(
          REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
            commandName,
            withRetiredBondRole(
              byCommand[commandName]
                .map(({ name }) => name)
                .filter(
                  (name) =>
                    !poolRoles.has(name) && !dataCanonicityRoles.has(name),
                ),
            ),
          ]),
        ),
      ).toEqual(
        contracts === real
          ? PRE_POOL_REAL_COMMAND_ORDER_DIGEST
          : PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      );
    }
  });

  it("places the DA bond pool right after the DA params governor, in both orders", () => {
    for (const contracts of [real, placeholder]) {
      const manifestContracts = manifestDeployableScripts(contracts).map(
        ({ contract }) => contract,
      );
      const governorMint = manifestContracts.indexOf("daParamsGovernorMint");
      expect(
        manifestContracts.slice(governorMint + 1, governorMint + 3),
      ).toEqual(["daBondPoolSpend", "daBondPoolMint"]);
      const targetNames = nodeRuntimeReferenceScriptTargets(contracts).map(
        ({ name }) => name,
      );
      const governorMintRole = targetNames.indexOf(
        "da-params-governor minting",
      );
      expect(
        targetNames.slice(governorMintRole + 1, governorMintRole + 3),
      ).toEqual(["da-bond-pool spending", "da-bond-pool minting"]);
    }
  });

  it("publishes all six history additions with their exact roles, purposes and command coverage", () => {
    expect(historyContracts.size).toBe(6);
    expect(historyRoles.size).toBe(6);
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts);
      expect(
        manifest
          .filter(({ contract }) => historyContracts.has(contract))
          .map(({ contract, role, purpose, commands }) => ({
            contract,
            role,
            purpose,
            commands,
          })),
      ).toEqual(HISTORY_CATALOGUE_ADDITIONS);
      const targets = nodeRuntimeReferenceScriptTargets(contracts);
      const byCommand = referenceScriptTargetsByCommand(contracts);
      for (const addition of HISTORY_CATALOGUE_ADDITIONS) {
        const matching = targets.filter(({ name }) => name === addition.role);
        expect(matching).toHaveLength(1);
        expect(CONTRACT_BY_ROLE[addition.role]).toBe(addition.contract);
        expect(matching[0]!.script).toEqual(
          manifest.find(({ contract }) => contract === addition.contract)!
            .script,
        );
        for (const commandName of REFERENCE_SCRIPT_COMMAND_NAMES) {
          expect(
            byCommand[commandName].filter(({ name }) => name === addition.role),
          ).toHaveLength(
            commandName === "node-runtime" ||
              (addition.commands as readonly string[]).includes(commandName)
              ? 1
              : 0,
          );
        }
      }
    }
  });

  it("publishes both pool additions with their exact roles, purposes and command coverage", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts);
      expect(
        manifest
          .filter(({ contract }) => poolContracts.has(contract))
          .map(({ contract, role, purpose, commands }) => ({
            contract,
            role,
            purpose,
            commands,
          })),
      ).toEqual(POOL_CATALOGUE_ADDITIONS);
      const targets = nodeRuntimeReferenceScriptTargets(contracts);
      const byCommand = referenceScriptTargetsByCommand(contracts);
      for (const addition of POOL_CATALOGUE_ADDITIONS) {
        const matching = targets.filter(({ name }) => name === addition.role);
        expect(matching).toHaveLength(1);
        expect(CONTRACT_BY_ROLE[addition.role]).toBe(addition.contract);
        expect(matching[0]!.script).toEqual(
          manifest.find(({ contract }) => contract === addition.contract)!
            .script,
        );
        for (const commandName of REFERENCE_SCRIPT_COMMAND_NAMES) {
          expect(
            byCommand[commandName].filter(({ name }) => name === addition.role),
          ).toHaveLength(
            commandName === "node-runtime" ||
              (addition.commands as readonly string[]).includes(commandName)
              ? 1
              : 0,
          );
        }
      }
    }
  });

  it("gives every descriptor exactly the manifest role of its contract", () => {
    const descriptors = collectScriptDescriptors(real);
    expect(new Set(descriptors.map(({ name }) => name)).size).toEqual(
      descriptors.length,
    );
    for (const descriptor of descriptors) {
      expect(descriptor.referenceScriptTargetName).toEqual(
        ROLE_BY_CONTRACT.get(descriptor.name),
      );
    }
    expect(
      Object.values(CONTRACT_BY_ROLE).filter(
        (contract) => !descriptors.some(({ name }) => name === contract),
      ),
    ).toEqual([]);
  });

  it("publishes each role once with its manifest descriptor's script", () => {
    const descriptorByName = new Map(
      collectScriptDescriptors(real).map((descriptor) => [
        descriptor.name,
        descriptor,
      ]),
    );
    const targets = nodeRuntimeReferenceScriptTargets(real);
    expect(new Set(targets.map(({ name }) => name)).size).toEqual(
      targets.length,
    );
    for (const target of targets) {
      const contract = CONTRACT_BY_ROLE[target.name];
      expect(contract).toBeDefined();
      expect(descriptorByName.get(contract!)?.script).toEqual(target.script);
    }
  });

  it("derives every command subset from the node-runtime order", () => {
    const byCommand = referenceScriptTargetsByCommand(real);
    const nodeRuntime = byCommand["node-runtime"];
    expect(nodeRuntime).toEqual(nodeRuntimeReferenceScriptTargets(real));
    for (const commandName of REFERENCE_SCRIPT_COMMAND_NAMES) {
      const names = byCommand[commandName].map(({ name }) => name);
      expect(names.length).toBeGreaterThan(0);
      expect(names).toEqual(
        nodeRuntime
          .map(({ name }) => name)
          .filter((name) => names.includes(name)),
      );
    }
  });
});
