/** Current pinned blueprint reapplication, not a full installed/L1 lifecycle. */
import { readFileSync } from "node:fs";

import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  mintingPolicyToId,
  scriptFromNative,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { type ContractDeploymentInfoEntry } from "../src/inspect-contracts.js";
import {
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  resolveFaultProofDeploymentContracts,
} from "../src/runtime.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "../src/transition-trace/yield-references.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

const rawBlueprint: unknown = JSON.parse(
  readFileSync(realBlueprintPath, "utf8"),
);
const blueprint = SDK.parseFaultProofBlueprint(rawBlueprint);
const bounds = {
  inlineLimitBytes: "512",
  maxPayloadBytes: "5000",
  maxPayloadNodes: "512",
};
const policy = scriptFromNative({ type: "sig", keyHash: "11".repeat(28) });
const referenceScriptAuthPolicy = {
  policyId: mintingPolicyToId(policy),
  nativeScript: { type: "Native", cborHex: policy.script },
  tokenNames: SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
};

for (const category of [
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "transitionTrace",
] as const)
  describe(`${category} applied bounds resolution`, () => {
    let fixture: {
      referenceScriptAuthPolicy: typeof referenceScriptAuthPolicy;
      contracts: Record<string, ContractDeploymentInfoEntry>;
    };
    beforeAll(async () => {
      const params = {
        blueprint,
        network: "Preview" as const,
        hubOraclePolicyId: "aa".repeat(28),
        fraudProofCataloguePolicyId: "bb".repeat(28),
        referenceScriptAuthPolicyId: referenceScriptAuthPolicy.policyId,
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
      };
      const built =
        category === "fabricatedDeposit"
          ? await Effect.runPromise(
              SDK.buildFabricatedDepositFaultProofContracts(params),
            )
          : category === "fabricatedWithdrawal"
            ? await Effect.runPromise(
                SDK.buildFabricatedWithdrawalFaultProofContracts(params),
              )
            : await Effect.runPromise(
                SDK.buildTransitionTraceFaultProofContracts(params),
              );
      const chain =
        "fabricatedDeposit" in built
          ? built.fabricatedDeposit
          : "fabricatedWithdrawal" in built
            ? built.fabricatedWithdrawal
            : built.transitionTrace;
      const ids = SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS;
      const names = SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER;
      const scriptHash = (name: (typeof names)[number]) =>
        name === category
          ? chain.firstStep.spendingScriptHash
          : "dd".repeat(28);
      const trie = await Trie.fromList(
        names.map((name) => ({
          key: Buffer.from(Data.to(ids[name]), "hex"),
          value: Buffer.from(Data.to(scriptHash(name)), "hex"),
        })),
      );
      const entries = await Promise.all(
        names.map(
          async (name) =>
            [
              name,
              {
                categoryId: ids[name],
                scriptHash: scriptHash(name),
                membershipProofCbor: (
                  await trie.prove(Buffer.from(Data.to(ids[name]), "hex"))
                )
                  .toCBOR()
                  .toString("hex"),
              },
            ] as const,
        ),
      );
      const catalogue = {
        root: Buffer.from(trie.hash).toString("hex"),
        categories: Object.fromEntries(
          entries,
        ) as SDK.FraudProofCatalogueDeploymentInfo["categories"],
      };
      fixture = {
        referenceScriptAuthPolicy,
        contracts: {
          hubOracleMint: { scriptHash: params.hubOraclePolicyId },
          stateQueueMint: { scriptHash: "cc".repeat(28) },
          fraudProofCatalogueMint: {
            scriptHash: params.fraudProofCataloguePolicyId,
            fraudProofCatalogue: catalogue,
          },
          fraudProofMint: { scriptHash: built.fraudProof.policyId },
          fraudProofSpend: { scriptHash: built.fraudProof.spendingScriptHash },
          ...("transitionTrace" in built
            ? Object.fromEntries(
                Object.entries(TRANSITION_TRACE_YIELD_REFERENCES).map(
                  ([key, reference]) => [
                    reference.entry,
                    {
                      scriptHash:
                        built.transitionTrace.yields[
                          key as keyof typeof TRANSITION_TRACE_YIELD_REFERENCES
                        ].withdrawalScriptHash,
                    },
                  ],
                ),
              )
            : {}),
          ...Object.fromEntries(
            FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY[category].map(
              (name, index) => [
                name,
                {
                  scriptHash: chain.steps[index]!.spendingScriptHash,
                  ...(index === 0
                    ? {
                        eventHistoryBounds: bounds,
                        ...("retentionAddresses" in chain.history
                          ? {
                              eventHistoryRetentionAddresses:
                                chain.history.retentionAddresses,
                            }
                          : {
                              eventHistoryRetentionAddress:
                                chain.history.retentionAddress,
                            }),
                      }
                    : {}),
                },
              ],
            ),
          ),
        },
      };
    });
    const resolve = (deploymentInfo: unknown) =>
      resolveFaultProofDeploymentContracts({
        blueprint: rawBlueprint,
        deploymentInfo,
        network: "Preview",
        categoryName: category,
        requireStateQueueMint: true,
        requireFraudProofSpend: true,
      });
    it("resolves the exact chain from explicit recorded parameters and catalogue identity", async () => {
      const result = await resolve(fixture);
      expect(result.contracts[category]?.history).toMatchObject({
        inlineLimitBytes: 512n,
        maxPayloadBytes: 5000n,
        maxPayloadNodes: 512n,
      });
    });
    it("refuses a retention address that is not derived by the same applied family", async () => {
      const changed = structuredClone(fixture);
      const name = FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY[category][0]!;
      changed.contracts[name] = {
        ...changed.contracts[name]!,
        ...(category === "transitionTrace"
          ? {
              eventHistoryRetentionAddresses: {
                ...changed.contracts[name]!.eventHistoryRetentionAddresses!,
                deposit: credentialToAddress("Preview", {
                  type: "Script",
                  hash: "ee".repeat(28),
                }),
              },
            }
          : {
              eventHistoryRetentionAddress: credentialToAddress("Preview", {
                type: "Script",
                hash: "ee".repeat(28),
              }),
            }),
      };
      await expect(resolve(changed)).rejects.toThrow(
        /retained-data address(?:es)? do(?:es)? not match/,
      );
    });
    it("refuses omitted bounds rather than choosing defaults", async () => {
      const changed = structuredClone(fixture);
      const name = FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY[category][0]!;
      const { eventHistoryBounds: omitted, ...entry } =
        changed.contracts[name]!;
      expect(omitted).toEqual(bounds);
      changed.contracts[name] = entry;
      await expect(resolve(changed)).rejects.toThrow(/eventHistoryBounds/);
    });
    it("refuses valid-looking substituted bounds when reapplication changes deployed scripts", async () => {
      const changed = structuredClone(fixture);
      const name = FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY[category][0]!;
      changed.contracts[name] = {
        ...changed.contracts[name]!,
        eventHistoryBounds: { ...bounds, maxPayloadNodes: "513" },
      };
      await expect(resolve(changed)).rejects.toThrow(/mismatch/);
    });
  });
