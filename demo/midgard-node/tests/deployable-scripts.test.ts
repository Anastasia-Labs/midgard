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
  caseContracts,
  caseRoles,
  cekResolverContracts,
  cekResolverRoles,
  cekStagedContracts,
  cekStagedRoles,
  dataCanonicityContracts,
  dataCanonicityRoles,
  HISTORY_CATALOGUE_ADDITIONS,
  historyContracts,
  historyRoles,
  phaseResolverContracts,
  phaseResolverRoles,
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
import * as orderPins from "./helpers/deployable-catalogue-order-pins.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

/** The manifest and publication lengths before the staged-operand additions. */
const PRE_CEK_STAGED_LENGTHS = [694, 689] as const;

const orderDigest = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value)).digest("hex");

const CONTRACT_BY_ROLE: Readonly<Record<string, string>> =
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE;

const ROLE_BY_CONTRACT = new Map(
  Object.entries(CONTRACT_BY_ROLE).map(([role, contract]) => [contract, role]),
);

const commandOrder = (contracts: SDK.MidgardValidators) => {
  const byCommand = referenceScriptTargetsByCommand(contracts);
  return REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
    commandName,
    byCommand[commandName].map(({ name }) => name),
  ]);
};

/**
 * Every order of `sets` (the real contract set first) with the staged-operand
 * additions and exactly the given contracts and roles excluded: the manifest
 * and publication lengths, and
 * the manifest, publication and per-command order digests an older
 * generation pinned.
 */
const expectOrdersWithout = (
  [real, ...others]: readonly SDK.MidgardValidators[],
  {
    contracts,
    roles,
    lengths: [manifestLength, targetLength],
    pins,
    restoreRetiredBond,
  }: {
    readonly contracts: readonly ReadonlySet<string>[];
    readonly roles: readonly ReadonlySet<string>[];
    readonly lengths: readonly [number, number];
    readonly pins: Readonly<Record<string, string>>;
    /** Older generations still listed the retired per-header bond yield. */
    readonly restoreRetiredBond?: boolean;
  },
) => {
  const excludedContracts = [cekStagedContracts, ...contracts];
  const excludedRoles = [cekStagedRoles, ...roles];
  const restore = <T extends string | null>(names: readonly T[]) =>
    restoreRetiredBond === true ? withRetiredBondRole(names) : names;
  const without = (names: readonly string[]) =>
    restore(
      names.filter((name) => !excludedRoles.some((set) => set.has(name))),
    );
  for (const contracts of [real!, ...others]) {
    const manifest = manifestDeployableScripts(contracts).filter(
      ({ contract }) => !excludedContracts.some((set) => set.has(contract)),
    );
    const roles = restore(manifest.map(({ role }) => role ?? null));
    expect(roles).toHaveLength(manifestLength);
    // The older generations' contract names are not pinned (see the pins).
    if (pins.manifestContract !== undefined)
      expect(orderDigest(manifest.map(({ contract }) => contract))).toEqual(
        pins.manifestContract,
      );
    expect(orderDigest(roles)).toEqual(pins.manifestRole);
    const targets = without(
      nodeRuntimeReferenceScriptTargets(contracts).map(({ name }) => name),
    );
    expect(targets).toHaveLength(targetLength);
    const kind = contracts === real ? "real" : "placeholder";
    expect(orderDigest(targets)).toEqual(pins[`${kind}Publication`]);
    const byCommand = referenceScriptTargetsByCommand(contracts);
    expect(
      orderDigest(
        REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
          commandName,
          without(byCommand[commandName].map(({ name }) => name)),
        ]),
      ),
    ).toEqual(pins[`${kind}Command`]);
  }
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

  it("pins the manifest order of contracts and roles", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts);
      expect(manifest).toHaveLength(
        PRE_CEK_STAGED_LENGTHS[0] + cekStagedContracts.size,
      );
      expect(orderDigest(manifest.map(({ contract }) => contract))).toEqual(
        orderPins.MANIFEST_CONTRACT_ORDER_DIGEST,
      );
      expect(orderDigest(manifest.map(({ role }) => role ?? null))).toEqual(
        orderPins.MANIFEST_ROLE_ORDER_DIGEST,
      );
    }
  });

  it("pins the publication and per-command orders", () => {
    const realTargets = nodeRuntimeReferenceScriptTargets(real);
    expect(realTargets).toHaveLength(
      PRE_CEK_STAGED_LENGTHS[1] + cekStagedRoles.size,
    );
    expect(orderDigest(realTargets.map(({ name }) => name))).toEqual(
      orderPins.REAL_PUBLICATION_ORDER_DIGEST,
    );
    expect(orderDigest(commandOrder(real))).toEqual(
      orderPins.REAL_COMMAND_ORDER_DIGEST,
    );

    // Both contract sets publish the same roles in the same order.
    const placeholderTargets = nodeRuntimeReferenceScriptTargets(placeholder);
    expect(placeholderTargets.map(({ name }) => name)).toEqual(
      realTargets.map(({ name }) => name),
    );
    expect(orderDigest(placeholderTargets.map(({ name }) => name))).toEqual(
      orderPins.PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
    );
    expect(orderDigest(commandOrder(placeholder))).toEqual(
      orderPins.PLACEHOLDER_COMMAND_ORDER_DIGEST,
    );
  });

  it("preserves every pre-staged-operand order after excluding exactly the staged-operand additions", () => {
    expectOrdersWithout([real, placeholder], {
      contracts: [],
      roles: [],
      lengths: PRE_CEK_STAGED_LENGTHS,
      pins: {
        manifestContract:
          orderPins.PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST,
        manifestRole: orderPins.PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST,
        realPublication: orderPins.PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST,
        placeholderPublication:
          orderPins.PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
        realCommand: orderPins.PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST,
        placeholderCommand:
          orderPins.PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      },
    });
  });

  it("preserves every pre-phase-resolver order after excluding exactly the phase resolver additions", () => {
    expectOrdersWithout([real, placeholder], {
      contracts: [phaseResolverContracts],
      roles: [phaseResolverRoles],
      lengths: [678, 673],
      pins: {
        manifestContract:
          orderPins.PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST,
        manifestRole: orderPins.PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST,
        realPublication:
          orderPins.PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST,
        placeholderPublication:
          orderPins.PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
        realCommand: orderPins.PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST,
        placeholderCommand:
          orderPins.PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      },
    });
  });

  it("preserves every pre-CEK-resolver order after excluding exactly its five and the phase resolver additions", () => {
    expectOrdersWithout([real, placeholder], {
      contracts: [phaseResolverContracts, cekResolverContracts],
      roles: [phaseResolverRoles, cekResolverRoles],
      lengths: [673, 668],
      pins: {
        manifestContract:
          orderPins.PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST,
        manifestRole: orderPins.PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST,
        realPublication:
          orderPins.PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST,
        placeholderPublication:
          orderPins.PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
        realCommand: orderPins.PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST,
        placeholderCommand:
          orderPins.PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      },
    });
  });

  it("preserves every pre-case order after excluding exactly its six, the five CEK resolver and the phase resolver additions", () => {
    expectOrdersWithout([real, placeholder], {
      contracts: [phaseResolverContracts, cekResolverContracts, caseContracts],
      roles: [phaseResolverRoles, cekResolverRoles, caseRoles],
      lengths: [667, 662],
      pins: {
        manifestContract: orderPins.PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST,
        manifestRole: orderPins.PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST,
        realPublication: orderPins.PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST,
        placeholderPublication:
          orderPins.PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
        realCommand: orderPins.PRE_CASE_REAL_COMMAND_ORDER_DIGEST,
        placeholderCommand: orderPins.PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      },
    });
  });

  it("preserves every pre-data-canonicity order after excluding exactly its four, the six case, the five CEK resolver and the phase resolver additions", () => {
    expectOrdersWithout([real, placeholder], {
      contracts: [
        phaseResolverContracts,
        cekResolverContracts,
        caseContracts,
        dataCanonicityContracts,
      ],
      roles: [
        phaseResolverRoles,
        cekResolverRoles,
        caseRoles,
        dataCanonicityRoles,
      ],
      lengths: [663, 658],
      pins: {
        manifestContract:
          orderPins.PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST,
        manifestRole: orderPins.PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST,
        realPublication:
          orderPins.PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST,
        placeholderPublication:
          orderPins.PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
        realCommand: orderPins.PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST,
        placeholderCommand:
          orderPins.PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      },
    });
  });

  it("preserves every original published order after excluding exactly the six history, two pool, four data-canonicity, six case, five CEK resolver and the phase resolver additions and restoring the retired bond yield", () => {
    expectOrdersWithout([real, placeholder], {
      contracts: [
        historyContracts,
        poolContracts,
        phaseResolverContracts,
        cekResolverContracts,
        caseContracts,
        dataCanonicityContracts,
      ],
      roles: [
        historyRoles,
        poolRoles,
        phaseResolverRoles,
        cekResolverRoles,
        caseRoles,
        dataCanonicityRoles,
      ],
      lengths: [656, 651],
      restoreRetiredBond: true,
      pins: {
        manifestRole: orderPins.PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST,
        realPublication: orderPins.PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST,
        placeholderPublication:
          orderPins.PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
        realCommand: orderPins.PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST,
        placeholderCommand:
          orderPins.PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      },
    });
  });

  it("preserves every pre-pool published order after excluding exactly the two pool, four data-canonicity, six case, five CEK resolver and the phase resolver additions and restoring the retired bond yield", () => {
    expectOrdersWithout([real, placeholder], {
      contracts: [
        poolContracts,
        phaseResolverContracts,
        cekResolverContracts,
        caseContracts,
        dataCanonicityContracts,
      ],
      roles: [
        poolRoles,
        phaseResolverRoles,
        cekResolverRoles,
        caseRoles,
        dataCanonicityRoles,
      ],
      lengths: [662, 657],
      restoreRetiredBond: true,
      pins: {
        manifestRole: orderPins.PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST,
        realPublication: orderPins.PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST,
        placeholderPublication:
          orderPins.PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
        realCommand: orderPins.PRE_POOL_REAL_COMMAND_ORDER_DIGEST,
        placeholderCommand: orderPins.PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      },
    });
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
