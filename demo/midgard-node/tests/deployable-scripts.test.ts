import "./utils.js";

import { createHash } from "node:crypto";

import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { collectScriptDescriptors } from "../src/commands/contract-deployment-info.js";
import {
  MANIFEST_ORDER,
  manifestDeployableScripts,
  PUBLICATION_ORDER,
  REFERENCE_SCRIPT_COMMAND_NAMES,
} from "../src/deployable-scripts.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "../src/deployment-manifest.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  nodeRuntimeReferenceScriptTargets,
  referenceScriptTargetsByCommand,
} from "../src/transactions/reference-scripts.js";
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
const PRE_HISTORY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "43d52375d2b77f0587bfcd6553bc6a77366b507828e903e23efe73342ee0666f";
const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "c460b989ef9dd6547f1000710fcecf5ceced42ca7b35c6042467ea0965a2f811";
const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "8ac96b9961ba6f7a4ef0d8106873af671837fa072f5cdbeac89a5435279e20a0";
const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "1ef6485370b266f59efcd99a16f09fffa5d5f4fde003a2826b942a7129956537";
const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "210e25d9bbc4d5eccff6abdb92c685383988695e8ba0e7477774e3694b2175a1";
const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3dc67cab67e108538859e783c53623c1a38c199dfad3f75b1d13b3980d7b7783";

// New pins are derived only after projecting out exactly these six entries
// reproduces every original order digest and the original 528/521 counts.
const MANIFEST_CONTRACT_ORDER_DIGEST =
  "73d8db770d72ccb908c3e0cc2758fc33f77e3bd0e7f692336d0091aed615f909";
const MANIFEST_ROLE_ORDER_DIGEST =
  "a2e24084e3de768007b27ed80abab4251a853f878bb991e08053776467c32014";
const REAL_PUBLICATION_ORDER_DIGEST =
  "14a252bc64c1f79cd33583c7dd277d0a0ded120f6a799f1e470049fea7ba6c8d";
const REAL_COMMAND_ORDER_DIGEST =
  "67860e5e44507dbaff6bc705ceb6992ff11e993b74c28d1eeee4325f98231fd0";
const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "4983f9529c62525d4ec65b3e5c718d458d47c3d076f60ef9be46811e1c375a65";
const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "0dc4a59b6bb9b94cfb1487d65c5baba836c2a3e328ee106d78abc7183698dafe";

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

const historyContracts = new Set<string>(
  HISTORY_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
const historyRoles = new Set<string>(
  HISTORY_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

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
  });

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
      expect(manifest).toHaveLength(534);
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
    expect(realTargets).toHaveLength(527);
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

  it("preserves every original order after excluding exactly the six history additions", () => {
    for (const contracts of [real, placeholder]) {
      const manifest = manifestDeployableScripts(contracts).filter(
        ({ contract }) => !historyContracts.has(contract),
      );
      expect(manifest).toHaveLength(528);
      expect(orderDigest(manifest.map(({ contract }) => contract))).toEqual(
        PRE_HISTORY_MANIFEST_CONTRACT_ORDER_DIGEST,
      );
      expect(orderDigest(manifest.map(({ role }) => role ?? null))).toEqual(
        PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST,
      );
      const targets = nodeRuntimeReferenceScriptTargets(contracts).filter(
        ({ name }) => !historyRoles.has(name),
      );
      expect(targets).toHaveLength(contracts === real ? 521 : 520);
      expect(orderDigest(targets.map(({ name }) => name))).toEqual(
        contracts === real
          ? PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST
          : PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST,
      );
      const byCommand = referenceScriptTargetsByCommand(contracts);
      expect(
        orderDigest(
          REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
            commandName,
            byCommand[commandName]
              .map(({ name }) => name)
              .filter((name) => !historyRoles.has(name)),
          ]),
        ),
      ).toEqual(
        contracts === real
          ? PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST
          : PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST,
      );
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
