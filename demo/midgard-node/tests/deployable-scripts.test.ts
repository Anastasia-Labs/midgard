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
const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "4f373a62d32ff07a72e4e1dfb8057b29fdaa4ef5786f7c4cde6825a8d08a04ea";
const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "a035b336335bcea9f0478dbf225d6c91631bf22b3be9c5f5acb7168566bc0b44";
const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "4c7fc91d18b00c27f53ecc22381181d279226a64ce02fc6a5cf711a98df31be7";
const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "0091acb4a1456776550938df8846756aaae0112297542fe98e77f808e94e05a3";
const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "d1b479b92bd3ee47c68aff589d549fdea1fc959bd41cd4bbf60d782f759e405e";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "eb55585e1b71c403c2edff8cc5007a3399440e365ecbf3df6d292f57efb936ea";
const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "a5198e6e4d97642166f1fd7d13c5fbf5929c9acf1737a80ae8c9a47484301c66";
const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "2135eb8f379730297fe2d1a4d6f932908c79d4425426c591c27f97868c3c4b46";
const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "b63468b12ab98c89930f2a6618ef46191f3c7032c71d8c2570fa8b3119c611ca";
const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "266151b5fdf2b4e4412abed4b13d00ee418cf06ac4f55a36479fd4fdab0d2b2a";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
const MANIFEST_CONTRACT_ORDER_DIGEST =
  "8487c7406a27382538634cbcd35baed89217d3d77f4973a5d0af8f0667809d0c";
const MANIFEST_ROLE_ORDER_DIGEST =
  "74a82a793b2941de4c0172bdb327f8ab26556ec38df318ad19219d2e64ae70a2";
const REAL_PUBLICATION_ORDER_DIGEST =
  "6dd38059d89f20032cbadd73262300093fbe2a611a69eabd0bd6a1ac1458163f";
const REAL_COMMAND_ORDER_DIGEST =
  "b609358821e3e3881ce6779ac0200cbfd79f563c3c03496d90b2660ed452856b";
const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "217040c490ac21e8509dfc24bc261eab62d45092e6f126810902ef2f617b9aa6";
const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "38cd9bc02bc5238e5a3597684ca355351d8d09a34ec97d2ac842fcb19a23ad16";

const HISTORY_CATALOGUE_ADDITIONS = [
  {
    contract: "depositHistoryRetentionSpend",
    role: "deposit history retention",
    purpose: "spend",
    commands: ["deposit"],
  },
  {
    contract: "depositHistoryRetirementWithdraw",
    role: "deposit history retirement",
    purpose: "withdraw",
    commands: ["deposit"],
  },
  {
    contract: "withdrawalHistoryRetentionSpend",
    role: "withdrawal history retention",
    purpose: "spend",
    commands: ["withdrawal"],
  },
  {
    contract: "withdrawalHistoryRetirementWithdraw",
    role: "withdrawal history retirement",
    purpose: "withdraw",
    commands: ["withdrawal"],
  },
  {
    contract: "fraudProofTransitionTraceL1EventWithdraw",
    role: "V1 fraud-proof transition-trace final-6 L1 event yield",
    purpose: "withdraw",
    commands: [],
  },
  {
    contract: "fraudProofTransitionTraceForcedTimingWithdraw",
    role: "V1 fraud-proof transition-trace final-6 forced timing yield",
    purpose: "withdraw",
    commands: [],
  },
] as const;

// The pooled DA committee bond: its spend and its one-shot mint, which the
// atomic protocol init runs to create the pool.
const POOL_CATALOGUE_ADDITIONS = [
  {
    contract: "daBondPoolSpend",
    role: "da-bond-pool spending",
    purpose: "spend",
    commands: ["da"],
  },
  {
    contract: "daBondPoolMint",
    role: "da-bond-pool minting",
    purpose: "mint",
    commands: ["protocol-init", "da"],
  },
] as const;

const poolContracts = new Set<string>(
  POOL_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
const poolRoles = new Set<string>(
  POOL_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

const historyContracts = new Set<string>(
  HISTORY_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
const historyRoles = new Set<string>(
  HISTORY_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The per-header bond yield sat right after the availability mint in every
// order and in the one command (`da`) that published the mint.
const RETIRED_BOND_ROLE = "availability-challenge bond withdrawal";

const withRetiredBondRole = <T extends string | null>(
  names: readonly T[],
): readonly (T | string)[] => {
  const mint = names.indexOf("availability-challenge minting" as T);
  if (mint < 0) return names;
  expect(names).not.toContain(RETIRED_BOND_ROLE);
  return [
    ...names.slice(0, mint + 1),
    RETIRED_BOND_ROLE,
    ...names.slice(mint + 1),
  ];
};

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
      expect(manifest).toHaveLength(520);
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
    expect(realTargets).toHaveLength(513);
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

  it("preserves every original published order after excluding exactly the six history and two pool additions and restoring the retired bond yield", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts).filter(
        ({ contract }) =>
          !historyContracts.has(contract) && !poolContracts.has(contract),
      );
      const roles = withRetiredBondRole(
        manifest.map(({ role }) => role ?? null),
      );
      expect(roles).toHaveLength(513);
      expect(orderDigest(roles)).toEqual(
        PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST,
      );
      const targets = withRetiredBondRole(
        nodeRuntimeReferenceScriptTargets(contracts)
          .map(({ name }) => name)
          .filter((name) => !historyRoles.has(name) && !poolRoles.has(name)),
      );
      expect(targets).toHaveLength(contracts === real ? 506 : 505);
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
                  (name) => !historyRoles.has(name) && !poolRoles.has(name),
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

  it("preserves every pre-pool published order after excluding exactly the two pool additions and restoring the retired bond yield", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts).filter(
        ({ contract }) => !poolContracts.has(contract),
      );
      const roles = withRetiredBondRole(
        manifest.map(({ role }) => role ?? null),
      );
      expect(roles).toHaveLength(519);
      expect(orderDigest(roles)).toEqual(PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST);
      const targets = withRetiredBondRole(
        nodeRuntimeReferenceScriptTargets(contracts)
          .map(({ name }) => name)
          .filter((name) => !poolRoles.has(name)),
      );
      expect(targets).toHaveLength(contracts === real ? 512 : 511);
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
                .filter((name) => !poolRoles.has(name)),
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
