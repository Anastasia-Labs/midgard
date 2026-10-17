import "./utils.js";

import { createHash } from "node:crypto";

import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { collectScriptDescriptors } from "../src/commands/contract-deployment-info.js";
import { REFERENCE_SCRIPT_COMMAND_NAMES } from "../src/deployable-scripts.js";
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
import { expectPreMigrationDeploymentBoundary } from "./helpers/deployment-member-baseline.js";
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
// command) reproduced every previous digest of every generation.
const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "5cdbd41d41634b7bd02aafaf62cf15035b25599bde880222ba0da1d25c2c8b18";
const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "4a23e447ea2fe9df745640e74a2c3628d80ac5e6eb44dc39724cd991cb9c0937";
const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "a7012751eab4c9e44be24b002e7e9494c55bce7a9ddd65c9b8d78bc90f051cd5";
const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "4a23e447ea2fe9df745640e74a2c3628d80ac5e6eb44dc39724cd991cb9c0937";
const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "a7012751eab4c9e44be24b002e7e9494c55bce7a9ddd65c9b8d78bc90f051cd5";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "28d08643f7840068e00ba61215a358bbf8b5ce566246e0e9522612669a1f2405";
const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "74a37f65006a36395395ffff714fbab82037920e87489cd790c5ffd306e579a0";
const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "083f0de35ccae69878c670801600e4d3dfc9341a0219180b00335d513531aa8f";
const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "74a37f65006a36395395ffff714fbab82037920e87489cd790c5ffd306e579a0";
const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "083f0de35ccae69878c670801600e4d3dfc9341a0219180b00335d513531aa8f";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "4e180e188ea962ef48cf36aba48bc91605e36cebd629f560c17c3dfefd32a55d";
const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "81234e912abc4213be500ccaccb3df78b0dec861da8c5894fb99c20a55b878b3";
const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "26fdf9dd3caaa051fa3dac10135dbec3616e7fe217ed2ff8a8f4be351b4dd6e5";
const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "686d7eab0c30a091da54831ca27d896f4c03f61f6d3c9c22ececa79df74166dd";
const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "26fdf9dd3caaa051fa3dac10135dbec3616e7fe217ed2ff8a8f4be351b4dd6e5";
const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "686d7eab0c30a091da54831ca27d896f4c03f61f6d3c9c22ececa79df74166dd";

// Projecting out all four data-canonicity additions leaves 510/502 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
// All pins were re-derived when script-decode replaced the CEK program-material
// publication (real set only), its traversal step and four material yields
// (traversal, program and Data task yields before selection-authenticate, two
// material yields after selection-successor), and again when the scripts
// larger than one transaction (script-decode spend and mint, CEK core BLS
// budget) left the publication orders. Restoring those entries in place
// reproduced every earlier digest and count. Both contract sets publish the
// same roles, so their pins coincide.
const MANIFEST_CONTRACT_ORDER_DIGEST =
  "69c026fbe97f1cec8466399c7739340ea79cd27560e03a291982bc67b276c4c9";
const MANIFEST_ROLE_ORDER_DIGEST =
  "981bf2f0af0bef4b6a7544978dfc36ca627faaed6f2aed851ed6712d622fe7fe";
const REAL_PUBLICATION_ORDER_DIGEST =
  "9b8c11ab7d18bd918e0ac07ad4ea4f2e72aed71aae017cba728e586c3300c84a";
const REAL_COMMAND_ORDER_DIGEST =
  "481b3db6b933c86cb215238de99db324e18b83b49fda1445f852e6637f14e472";
const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "9b8c11ab7d18bd918e0ac07ad4ea4f2e72aed71aae017cba728e586c3300c84a";
const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "481b3db6b933c86cb215238de99db324e18b83b49fda1445f852e6637f14e472";

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

  it("preserves the complete pre-migration script binding and publication order", () => {
    expectPreMigrationDeploymentBoundary(real);
  });

  it("pins the manifest order of contracts and roles", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts);
      expect(manifest).toHaveLength(514);
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
    expect(realTargets).toHaveLength(506);
    expect(orderDigest(realTargets.map(({ name }) => name))).toEqual(
      REAL_PUBLICATION_ORDER_DIGEST,
    );
    expect(orderDigest(commandOrder(real))).toEqual(REAL_COMMAND_ORDER_DIGEST);

    // Both contract sets publish the same roles in the same order.
    const placeholderTargets = nodeRuntimeReferenceScriptTargets(placeholder);
    expect(placeholderTargets.map(({ name }) => name)).toEqual(
      realTargets.map(({ name }) => name),
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
      expect(manifest).toHaveLength(510);
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
      expect(targets).toHaveLength(502);
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
      expect(roles).toHaveLength(503);
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
      expect(targets).toHaveLength(495);
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
      expect(roles).toHaveLength(509);
      expect(orderDigest(roles)).toEqual(PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST);
      const targets = withRetiredBondRole(
        nodeRuntimeReferenceScriptTargets(contracts)
          .map(({ name }) => name)
          .filter(
            (name) => !poolRoles.has(name) && !dataCanonicityRoles.has(name),
          ),
      );
      expect(targets).toHaveLength(501);
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
