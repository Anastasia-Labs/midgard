import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import { DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { REFERENCE_SCRIPT_AUTH_TOKEN_NAMES } from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import {
  mintingPolicyToId,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it as unitIt, vi } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifest,
  type DeploymentManifestIdentityContext,
} from "../src/commands/contract-deployment-info.js";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestJsonValue,
} from "../src/deployment-manifest.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  assertDeploymentManifestMatchesConfig,
  buildRealTxOrderContracts,
  eventHistoryBoundsFromExplicitEnvironment,
  readRuntimeDeploymentManifestFile,
  withRealStateQueueAndOperatorContracts,
} from "../src/services/midgard-contracts.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";
import {
  TEST_AVAILABILITY_CHALLENGE,
  TEST_AVAILABILITY_PARAMETERS,
} from "./helpers/availability-challenge.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./helpers/cardano-protocol-parameters.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";
import {
  collectScriptInventory,
  scriptInventoryId,
} from "./helpers/script-inventory.js";

describe("midgard contracts registry", () => {
  const oneShotOutRef = {
    txHash: "00".repeat(32),
    outputIndex: 0,
  } as const;
  const cardanoSnapshot = TEST_CARDANO_PROTOCOL_PARAMETERS;
  const daVkey = "11".repeat(32);
  const manifestIdentityContext: DeploymentManifestIdentityContext = {
    availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    cardanoProtocolParameters: {
      snapshot: cardanoSnapshot,
      digest: computeDeploymentManifestJsonDigest(cardanoSnapshot),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest: computeDeploymentManifestJsonDigest(
        normalizeDeploymentManifestJsonValue([]),
      ),
    },
    da: {
      committeeVkeys: [daVkey],
      committeeSignersHash: computeDeploymentManifestDaCommitteeSignersHash([
        daVkey,
      ]),
      threshold: 1,
      transportProfile: {
        protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
        envelopeEncoding: "identity" as const,
        zstdLevel: 3,
        limits: DA_TRANSPORT_LIMITS,
        retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
      },
    },
    artifacts: {
      blueprintHash: "22".repeat(32),
    },
  };

  it.effect("resolves real state_queue and hub_oracle scripts", () =>
    Effect.gen(function* () {
      const placeholderContracts = yield* AlwaysSucceedsContract;
      const resolved = yield* withRealStateQueueAndOperatorContracts(
        "Preprod",
        placeholderContracts,
        { ...oneShotOutRef },
        {
          referenceScriptAuth: placeholderContracts.referenceScriptAuth,
          availabilityChallengeParameters: TEST_AVAILABILITY_PARAMETERS,
          eventHistoryProtectionDurationMs: 2_000n,
          eventHistoryBounds: {
            inlineLimitBytes: 512n,
            maxPayloadBytes: 5000n,
            maxPayloadNodes: 512n,
          },
        },
      );

      // Applied recipes include the completed-fraud queue marker and explicit
      // history bounds/retention metadata, both authenticated timing yields, and
      // the paired list/retention/retirement deployment recipes;
      // normalize bigint parameters as decimal strings.
      expect(
        createHash("sha256")
          .update(
            JSON.stringify(normalizeDeploymentManifestJsonValue(resolved)),
          )
          .digest("hex"),
      ).toBe(
        "aa57086c9b9b2708e73ea4f605f271e1199bc78c9e685a4372e57cab50c41bff",
      );
      // The queue/correction subset is pinned independently of the full registry.
      // Includes every applied CBOR, hash, policy id, address, and queue yield.
      expect(
        createHash("sha256")
          .update(
            JSON.stringify({
              stateQueue: resolved.stateQueue,
              correctionLock: resolved.correctionLock,
            }),
          )
          .digest("hex"),
      ).toBe(
        "7ea695e5bf105e14c6e1bbce64f93f31afdc337ab1c9ab8ccac401b7d62ffd64",
      );
      // The always-succeeds stand-in is a real hazard here: it satisfies every
      // spend, so a role that silently kept it would pass any behavioural test
      // built on this registry. Rather than name a handful of roles and assert
      // `not.toEqual(placeholder)` — which any non-placeholder value satisfies
      // — take the whole reachable script inventory and pin the exact set that
      // is still the stand-in.
      const placeholderCbors = new Set(
        collectScriptInventory(placeholderContracts).map(({ cbor }) => cbor),
      );
      const inventory = collectScriptInventory(resolved);
      expect(inventory.length).toBeGreaterThan(500);

      /**
       * Reviewed contract, not a snapshot of current output:
       *
       * - `referenceScriptAuth:minting` is the stand-in this call site passes
       *   in itself (a real deployment supplies the timelocked native policy).
       * - `escapeHatch` has no compiled validator yet.
       * - `hubOracle:spending` is not a script at all: the hub oracle UTxO
       *   lives at the mint policy's own address, so the spending slot carries
       *   the stand-in bytes and the real policy id (see the exception below).
       *
       * Every other role must resolve to a real applied validator.
       */
      const EXPECTED_STAND_INS = [
        "escapeHatch:minting",
        "escapeHatch:spending",
        "hubOracle:spending",
        "referenceScriptAuth:minting",
      ];
      expect(
        inventory
          .filter(({ cbor }) => placeholderCbors.has(cbor))
          .map(scriptInventoryId)
          .sort(),
      ).toEqual(EXPECTED_STAND_INS);

      // Script and declared hash must be the same script. `hubOracle:spending`
      // is the one documented exception: it deliberately declares the mint
      // policy id beside the stand-in bytes.
      expect(
        inventory
          .filter(
            (entry) =>
              scriptInventoryId(entry) !== "hubOracle:spending" &&
              validatorToScriptHash(entry.script) !== entry.declaredHash,
          )
          .map(scriptInventoryId),
      ).toEqual([]);
      expect(resolved.hubOracle.spendingScriptHash).toEqual(
        resolved.hubOracle.policyId,
      );

      expect(resolved.hubOracle.policyId).toEqual(
        mintingPolicyToId(resolved.hubOracle.mintingScript),
      );
      expect(resolved.stateQueue.policyId).toEqual(
        mintingPolicyToId(resolved.stateQueue.mintingScript),
      );

      // The validation-trace dispute control scripts are six distinct roles;
      // collapsing any two would let one leg's redeemer drive another's leg.
      const validationControlHashes = [
        resolved.fraudProofs.validationTraceDispute.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.source.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.game.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.boundary.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.timeout.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.award.spendingScriptHash,
      ];
      expect(new Set(validationControlHashes).size).toEqual(6);

      const txOrderContracts = yield* buildRealTxOrderContracts(
        "Preprod",
        resolved.hubOracle.policyId,
      );
      expect(txOrderContracts.txOrder.policyId).toEqual(
        mintingPolicyToId(txOrderContracts.txOrder.mintingScript),
      );
      expect(txOrderContracts.fieldPreimageCertificate.policyId).toEqual(
        mintingPolicyToId(
          txOrderContracts.fieldPreimageCertificate.mintingScript,
        ),
      );
      expect(
        txOrderContracts.fieldPreimageCertificate.spendingScriptHash,
      ).toHaveLength(56);
      expect(
        txOrderContracts.cekProgramMaterial.spendingScriptHash,
      ).toHaveLength(56);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects invalid one-shot hub-oracle outref configuration", () =>
    Effect.gen(function* () {
      const placeholderContracts = yield* AlwaysSucceedsContract;
      const result = yield* Effect.either(
        withRealStateQueueAndOperatorContracts(
          "Preprod",
          placeholderContracts,
          {
            txHash: "zz",
            outputIndex: -1,
          },
          {
            referenceScriptAuth: placeholderContracts.referenceScriptAuth,
            availabilityChallengeParameters: TEST_AVAILABILITY_PARAMETERS,
            eventHistoryProtectionDurationMs: 2_000n,
            eventHistoryBounds: {
              inlineLimitBytes: 512n,
              maxPayloadBytes: 5000n,
              maxPayloadNodes: 512n,
            },
          },
        ),
      );
      expect(result._tag).toEqual("Left");
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  unitIt.each([
    {
      title: "correction_lock.spend.spend",
      mutation: "missing",
      error: /not found in blueprint/,
    },
    {
      title: "state_queue.mint.mint",
      mutation: "missing",
      error: /not found in blueprint/,
    },
    {
      title: "state_queue.spend.spend",
      mutation: "extra",
      error: /declares 5 parameter/,
    },
    {
      title: "state_queue_yields.merge.withdraw",
      mutation: "fewer",
      error: /declares 4 parameter/,
    },
    {
      title: "state_queue.mint.mint",
      mutation: "shape",
      error: /hub_oracle_script_hash.*must be an integer/,
    },
  ] as const)(
    "refuses $mutation deployment metadata for $title",
    async ({ title, mutation, error }) => {
      const dir = await mkdtemp(join(tmpdir(), "midgard-queue-blueprint-"));
      const blueprintPath = join(dir, "plutus.json");
      const raw = JSON.parse(
        await readFile(
          new URL("../../../onchain/aiken/plutus.json", import.meta.url),
          "utf8",
        ),
      ) as {
        validators: {
          title: string;
          compiledCode: string;
          parameters?: { title: string; schema?: { $ref: string } }[];
        }[];
      };
      const entry = raw.validators.find(
        (validator) => validator.title === title,
      );
      if (entry === undefined || entry.parameters === undefined)
        throw new Error(`Missing blueprint fixture ${title}`);
      if (mutation === "missing")
        raw.validators = raw.validators.filter(
          (validator) => validator.title !== title,
        );
      else if (mutation === "extra") entry.parameters.push({ title: "extra" });
      else if (mutation === "fewer") entry.parameters.pop();
      else entry.parameters[0]!.schema = { $ref: "#/definitions/Int" };
      try {
        await writeFile(blueprintPath, JSON.stringify(raw));
        vi.stubEnv("MIDGARD_REAL_BLUEPRINT_PATH", blueprintPath);
        const contracts = withRealEventHistoryForTest(
          await Effect.runPromise(
            AlwaysSucceedsContract.pipe(
              Effect.provide(AlwaysSucceedsContract.Default),
            ),
          ),
          { txHash: "ab".repeat(32), outputIndex: 0 },
        );
        await expect(
          Effect.runPromise(
            withRealStateQueueAndOperatorContracts(
              "Preprod",
              contracts,
              oneShotOutRef,
              {
                referenceScriptAuth: contracts.referenceScriptAuth,
                availabilityChallengeParameters: TEST_AVAILABILITY_PARAMETERS,
                eventHistoryProtectionDurationMs: 2_000n,
                eventHistoryBounds: {
                  inlineLimitBytes: 512n,
                  maxPayloadBytes: 5000n,
                  maxPayloadNodes: 512n,
                },
              },
            ),
          ),
        ).rejects.toThrow(error);
      } finally {
        vi.unstubAllEnvs();
        await rm(dir, { recursive: true, force: true });
      }
    },
  );

  unitIt(
    "fails closed for every existing non-V1 or tampered manifest",
    async () => {
      const dir = await mkdtemp(join(tmpdir(), "midgard-runtime-manifest-"));
      const missingPath = join(dir, "missing.json");
      const unsupportedPath = join(dir, "unsupported.json");
      const tamperedPath = join(dir, "tampered.json");
      try {
        expect(() =>
          readRuntimeDeploymentManifestFile(missingPath, true),
        ).toThrow(/does not exist/);
        await writeFile(
          unsupportedPath,
          JSON.stringify({ schemaVersion: "unsupported", contracts: {} }),
        );
        expect(() =>
          readRuntimeDeploymentManifestFile(unsupportedPath, false),
        ).toThrow(/schemaVersion must be midgard-deployment-manifest-v1/);
        expect(() =>
          readRuntimeDeploymentManifestFile(unsupportedPath, true),
        ).toThrow(/schemaVersion must be midgard-deployment-manifest-v1/);

        const contracts = withRealEventHistoryForTest(
          await Effect.runPromise(
            AlwaysSucceedsContract.pipe(
              Effect.provide(AlwaysSucceedsContract.Default),
            ),
          ),
          { txHash: "ab".repeat(32), outputIndex: 0 },
        );
        const nativeScriptCbor = "820501";
        const authPolicy = {
          policyId: validatorToScriptHash({
            type: "Native" as const,
            script: nativeScriptCbor,
          }),
          nativeScript: {
            type: "Native" as const,
            cborHex: nativeScriptCbor,
            expiresAtSlot: 1,
            expiresAtUnixTime: 1,
            timelockDurationMs: 1,
          },
          tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
          postTimelockAudit: {
            required: true as const,
            rule: "test fixture",
          },
        };
        const referenceScriptOutRefs = new Map(
          Object.values(
            DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
          ).map((contractName, outputIndex) => [
            contractName,
            { txHash: "33".repeat(32), outputIndex },
          ]),
        );
        const fraudProofCatalogue = await Effect.runPromise(
          buildFraudProofCatalogueDeploymentInfo(
            fraudProofsToIndexedValidators(contracts.fraudProofs),
          ),
        );
        const deploymentInfo = buildContractDeploymentInfoFromContracts(
          contracts,
          authPolicy,
          referenceScriptOutRefs,
          fraudProofCatalogue,
        );
        const manifest = buildDeploymentManifest(deploymentInfo, {
          network: "Preprod",
          ...manifestIdentityContext,
          referenceScriptDeployAddress: "addr_test1reference",
          hubOracleOneShotTxHash: "ab".repeat(32),
          hubOracleOneShotOutputIndex: 0,
          hubOracleOneShotStatus: "consumed_by_init",
          now: new Date("2026-07-24T00:00:00.000Z"),
          steps: {
            initProtocol: { status: "complete" },
            availabilityRegistration: { status: "complete" },
          },
        });
        const commonConfig = {
          NETWORK: "Preprod" as const,
          L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: "addr_test1reference",
          HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
          HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: 0,
        };
        expect(() =>
          assertDeploymentManifestMatchesConfig(manifest, tamperedPath, {
            ...commonConfig,
            MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: "public-preprod-launch-v1",
            OPERATOR_REQUIRED_BOND_LOVELACE: 100_000_000_000n,
            OPERATOR_SLASHING_PENALTY_LOVELACE: 25_000_000_000n,
          }),
        ).toThrow(
          /economics\.profile manifest=bounded-acceptance-v1 config=public-preprod-launch-v1; economics\.requiredBondLovelace manifest=900000000 config=100000000000; economics\.slashingPenaltyLovelace manifest=500000000 config=25000000000/u,
        );

        const publicManifest = buildDeploymentManifest(deploymentInfo, {
          network: "Preprod",
          ...manifestIdentityContext,
          economics:
            DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE[
              "public-preprod-launch-v1"
            ],
          referenceScriptDeployAddress: "addr_test1reference",
          hubOracleOneShotTxHash: "ab".repeat(32),
          hubOracleOneShotOutputIndex: 0,
          hubOracleOneShotStatus: "consumed_by_init",
          now: new Date("2026-07-24T00:00:00.000Z"),
          steps: {
            initProtocol: { status: "complete" },
            availabilityRegistration: { status: "complete" },
          },
        });
        expect(() =>
          assertDeploymentManifestMatchesConfig(publicManifest, tamperedPath, {
            ...commonConfig,
            MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: "bounded-acceptance-v1",
            OPERATOR_REQUIRED_BOND_LOVELACE: 900_000_000n,
            OPERATOR_SLASHING_PENALTY_LOVELACE: 500_000_000n,
          }),
        ).toThrow(
          /economics\.profile manifest=public-preprod-launch-v1 config=bounded-acceptance-v1; economics\.requiredBondLovelace manifest=100000000000 config=900000000; economics\.slashingPenaltyLovelace manifest=25000000000 config=500000000/u,
        );
        await writeFile(
          tamperedPath,
          JSON.stringify({
            ...manifest,
            network: "Preview",
          }),
        );
        expect(() =>
          readRuntimeDeploymentManifestFile(tamperedPath, false),
        ).toThrow(/Deployment manifest id mismatch/);
      } finally {
        await rm(dir, { recursive: true });
      }
    },
  );
});

unitIt(
  "requires explicit history bounds before deriving fresh contracts",
  () => {
    try {
      vi.stubEnv("MIDGARD_EVENT_HISTORY_INLINE_LIMIT_BYTES", undefined);
      vi.stubEnv("MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_BYTES", "5000");
      vi.stubEnv("MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_NODES", "512");
      expect(() => eventHistoryBoundsFromExplicitEnvironment()).toThrow(
        /inlineLimitBytes/,
      );
      vi.stubEnv("MIDGARD_EVENT_HISTORY_INLINE_LIMIT_BYTES", "512");
      expect(eventHistoryBoundsFromExplicitEnvironment()).toEqual({
        inlineLimitBytes: 512n,
        maxPayloadBytes: 5000n,
        maxPayloadNodes: 512n,
      });
      vi.stubEnv("MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_BYTES", "511");
      expect(() => eventHistoryBoundsFromExplicitEnvironment()).toThrow(
        /inline bound/,
      );
    } finally {
      vi.unstubAllEnvs();
    }
  },
);
